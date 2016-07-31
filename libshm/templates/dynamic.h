#pragma once

#include <string>
#include <unordered_set>
#include <unordered_map>
#include <memory>

extern "C" {
#include <libshm/c/watcher.h>
}

$!setvar("ctypes", "('int', 'float', 'double', 'bool', 'string')")!$
$!setvar("dataLocs", "('shm', 'cached')")!$
	<!--(macro stdtype)-->
<!--(if ctype == 'string')-->std::string<!--(else)-->$!ctype!$<!--(end)-->
	<!--(end)-->
namespace cuauv {

/**
 * @brief Namespace Shm contains the interface to dynamic shm
 *
 * This implementation of dynamic shm allows access to shm groups and variables
 * with strings instead of needing to directly specify struct and struct field
 * names. This makes it possible to build clients that can determine how they
 * should access shm at runtime instead of compile time. This is useful for
 * clients like, for example seriald, which needs to access shm groups based on
 * which serial devices need to be synced to shm.
 */
namespace dshm {
	class Group;

/**
 * @brief Allows access to a shm variable in a shm group
 *
 * A Var object represents a single shm variable in a shm group. Var objects
 * are owned by a Group object and are accessed through pointers dealt by the
 * Group object. Such pointers refer to a subclass of Var that represents a shm
 * variable of a specific type, like int, bool, or string.
 */
class Var {
		friend class Group;

		public:
			/// Contains every type of shm variable
			enum class Type {
				<!--(for ctype in ctypes)-->
				$!ctype.upper()!$,
				<!--(end)-->
			};

			/// Returns the name of the shm variable
			std::string name() const;

			/// Returns the type of the shm variable
			Type type() const;

			/// Returns the group that this var is linked to
			Group* group() const;

			/// Returns the variable's shm path in the form 'group.name'
			std::string path() const;

			<!--(for ctype in ctypes)-->
				<!--(for dataLoc in dataLocs)-->
			virtual $!stdtype(ctype=ctype)!$ $!dataLoc!$$!ctype.capitalize()!$() const;
				<!--(end)-->

			<!--(end)-->
			<!--(for ctype in ctypes)-->
			virtual void setShm($!stdtype(ctype=ctype)!$);
			virtual void setCache($!stdtype(ctype=ctype)!$);

			<!--(end)-->
			void setShm(const char*);
			void setCache(const char*);

		protected:
			/**
			 * Non-public constructor called by Var subclasses
			 *
			 * Subclasses of Var which specialize in a shm type call this constructor
			 */
			Var(std::string name, Type type, Group* group):
				m_group{group}, m_changed{false}, m_name{name}, m_type{type} {}

			/**
			 * Called by Group to write the cached value to shm if it has been
			 * written to since the last pull(). Group calls this on all of its
			 * Var objects while holding the lock on a shm group so watchers
			 * are only notified once all cached values are written to the
			 * group
			 */
			virtual void pushNoLock() = 0;

			/**
			 * Called by Group to read the changes in its shm group into the
			 * cache of its Vars. Var caches are only updated if their cached
			 * values were not modified before pulling and if they differ from
			 * the shm value, in which case true is returned. Otherwise, false
			 * is returned
			 */
			virtual bool pullNoLock() = 0;

			Group* m_group;
			bool m_changed; ///< true if Var's cached value modified and not synced to shm

		private:
			const std::string m_name;
			const Type m_type;

			/// Called when the wrong type of setter/getter is called
			void throwWrongTypeException(Type t) const;
	};

	class Group {
		<!--(for ctype in ctypes)-->
		friend class $!ctype.capitalize()!$Var;
		<!--(end)-->
		
		public:
			/// Returns the name of the underlying shm group
			std::string name() const;

			/**
			 * @brief Returns the groups Var object that links to the given shm
			 * variable name
			 *
			 * Var objects are returned as pointers because Group owns them and
			 * destroys them when the Group itself is destroyed
			 */
			Var* var(std::string name) const;

			/// Returns a set with a Var object pointer for each shm variable in the group
			std::unordered_set<Var*> vars() const;

			/**
			 * @brief Atomically writes modified Var cache values to the shm
			 * group
			 *
			 * The shm group is locked for the duration of the push so that
			 * watchers on the group are only notified once
			 */
			void push();

			/**
			 * @brief Syncs modified shm variables to the caches of the group's
			 * Vars
			 *
			 * Only Vars that have do not have pending changes are overwritten
			 * by new shm values. This is to ensure that new cache values are
			 * not overwritten by old shm values
			 */
			std::unordered_set<Var*> pull();

			/// Watches the underlying shm group with the given watcher
			virtual void watch(watcher_t watcher) = 0;

			/// Unwatches the underlying shm group with the given watcher
			virtual void unwatch(watcher_t watcher) = 0;

		protected:
			/// Called by Group subclasses with different sets of Vars
			Group(std::string name, std::unordered_map<std::string, std::unique_ptr<Var>> vars);

			/// Called by Group subclass constructor to initialize Var caches
			void loadVarCaches();

			/// Called by Vars after they write to shm
			virtual void onWriteNoLock() = 0;

			/// Locks the shm group
			virtual void lockShm() = 0;

			/// Unlocks the shm group
			virtual void unlockShm() = 0;

		private:
			const std::string m_name;
			const std::unordered_map<std::string, std::unique_ptr<Var>> m_vars;
	};

	/// Returns a new Group object with the given shm group name
	std::unique_ptr<Group> newGroup(std::string name);

	/// Returns a set of Group objects, one for each shm group
	std::unordered_set<std::unique_ptr<Group>> newGroupSet();
}
}
