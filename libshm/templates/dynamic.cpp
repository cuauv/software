#include <string>
#include <stdexcept>
#include <vector>
#include <functional>

extern "C" {
// somehow, including shm as a system library works better
#include <libshm/c/shm.h>
}

#include "dynamic.h"

namespace cuauv {
namespace dshm {
	using ::shm;

	std::string Var::name() const {
		return m_name;
	}

	Var::Type Var::type() const {
		return m_type;
	}

	Group* Var::group() const {
		return m_group;
	}

	std::string Var::path() const {
		return m_group->name() + "." + m_name;
	}

	$!setvar("ctypes", "('int', 'float', 'double', 'bool', 'string')")!$
	$!setvar("dataLocs", "('shm', 'cached')")!$
	$!setvar("shmtypes", "{'int': 'int', 'float': 'float', 'double': 'double', 'bool': 'int32_t'}")!$
	<!--(macro stdtype)-->
<!--(if ctype == 'string')-->std::string<!--(else)-->$!ctype!$<!--(end)-->
	<!--(end)-->
	<!--(for ctype in ctypes)-->
		<!--(for dataLoc in dataLocs)-->
	$!stdtype(ctype=ctype)!$ Var::$!dataLoc!$$!ctype.capitalize()!$() const {
		throwWrongTypeException(Type::$!ctype.upper()!$);
			<!--(if ctype in ('int', 'float', 'double'))-->
		return 0;
			<!--(elif ctype == 'bool')-->
		return false;
			<!--(else)-->
		return "";
			<!--(end)-->
	}

		<!--(end)-->
	void Var::setShm($!stdtype(ctype=ctype)!$) {
		throwWrongTypeException(Type::$!ctype.upper()!$);
	}

	void Var::setCache($!stdtype(ctype=ctype)!$) {
		throwWrongTypeException(Type::$!ctype.upper()!$);
	}

	<!--(end)-->
	void Var::setShm(const char* value) {
		setShm(std::string(value));
	}

	void Var::setCache(const char* value) {
		setCache(std::string(value));
	}

	void Var::throwWrongTypeException(Type t) const {
		std::string typeStr;
		switch(t) {
			case Type::INT:
				typeStr = "an int";
				break;
			case Type::FLOAT:
				typeStr = "a float";
				break;
			case Type::DOUBLE:
				typeStr = "a double";
				break;
			case Type::BOOL:
				typeStr = "a bool";
				break;
			case Type::STRING:
				typeStr = "a string";
				break;
		}

		throw std::invalid_argument(m_group->name() + "." + name() + " is not " + typeStr);
	}

	<!--(for ctype in (x for x in ctypes if x != 'string'))-->
	class $!ctype.capitalize()!$Var : public Var {
		public:
			$!ctype.capitalize()!$Var(std::string name, Group* group, $!shmtypes[ctype]!$* shmField):
				Var(name, Type::$!ctype.upper()!$, group), m_shmField{shmField}, m_cachedValue{} {}

			$!ctype!$ shm$!ctype.capitalize()!$() const override {
				$!ctype!$ value;
				m_group->lockShm();
				value = *m_shmField;
				m_group->unlockShm();
				return value;
			}

			void setShm($!ctype!$ value) override {
				m_group->lockShm();
				*m_shmField = value;
				m_group->onWriteNoLock();
				m_group->unlockShm();
			}

			$!ctype!$ cached$!ctype.capitalize()!$() const override {
				return m_cachedValue;
			}

			void setCache($!ctype!$ value) override {
				m_cachedValue = value;
				m_changed = true;
			}

		protected:
			void pushNoLock() override {
				if (m_changed) {
					*m_shmField = m_cachedValue;
					m_changed = false;
				}
			}

			bool pullNoLock() override {
				if (!m_changed && m_cachedValue != *m_shmField) {
					m_cachedValue = *m_shmField;
					return true;
				}
				return false;
			}

		private:
			$!shmtypes[ctype]!$* m_shmField;
			$!shmtypes[ctype]!$ m_cachedValue;
	};

	<!--(end)-->
	class StringVar : public Var {
		public:
			StringVar(std::string name, Group* group, char* shmField, size_t maxLen):
				Var(name, Type::STRING, group), m_shmField{shmField}, m_maxLen{maxLen} {}

			std::string shmString() const override {
				m_group->lockShm();
				std::string value(m_shmField);
				m_group->unlockShm();
				return value;
			}

			void setShm(std::string value) override {
				checkLength(value);

				size_t i = 0;
				for (char c : value) {
					m_shmField[i] = c;
					++i;
				}
				m_shmField[i] = '\0';
				m_group->onWriteNoLock();
			}

			std::string cachedString() const override {
				return m_cachedValue;
			}

			void setCache(std::string value) override {
				checkLength(value);

				m_cachedValue = value;
				m_changed = true;
			}

		protected:
			void pushNoLock() override {
				if (m_changed) {
					size_t i = 0;
					for (char c : m_cachedValue) {
						m_shmField[i] = c;
						++i;
					}
				}
				m_changed = false;
			}

			bool pullNoLock() override {
				if (!m_changed && m_cachedValue != std::string(m_shmField)) {
					m_cachedValue = std::string(m_shmField, m_maxLen);
					return true;
				}
				return false;
			}

		private:
			char* m_shmField;
			std::string m_cachedValue;
			size_t m_maxLen;

			void checkLength(std::string value) {
				if (value.size() + 1 > m_maxLen) {
					throw std::invalid_argument("String '" + value + "' exceeds the length of " +
							m_group->name() + "." + name());
				}
			}
	};

	Group::Group(std::string name, std::unordered_map<std::string, std::unique_ptr<Var>> vars):
		m_name{name}, m_vars{std::move(vars)} {}

	std::string Group::name() const {
		return m_name;
	}

	Var* Group::var(std::string name) const {
		auto it = m_vars.find(name);
		if (it != m_vars.end()) {
			return it->second.get();
		} else {
			throw std::invalid_argument("Var '" + name + "." + m_name + "' does not exist");
		}
	}

	std::unordered_set<Var*> Group::vars() const {

		std::unordered_set<Var*> varSet(m_vars.size());
		for (auto& elem : m_vars) {
			varSet.insert(elem.second.get());
		}

		return varSet;
	}

	void Group::push() {
		lockShm();
		for (auto& var : m_vars) {
			var.second->pushNoLock();
		}
		onWriteNoLock();
		unlockShm();
	}

	std::unordered_set<Var*> Group::pull() {
		std::unordered_set<Var*> changedVars;
		lockShm();
		for (auto& var : m_vars) {
			if (var.second->pullNoLock()) {
				changedVars.insert(var.second.get());
			}
		}
		unlockShm();

		return changedVars;
	}

	void Group::loadVarCaches() {
		lockShm();
		for (auto& v : m_vars) {
			v.second->pullNoLock();
		}
		unlockShm();
	}

	struct Groups {
		<!--(for g in groups)-->
		struct $!g['groupname'].capitalize()!$ : public Group {

			// We can't initialize the map in the constructor because you can't construct
			// a map with initializer lists with unique_ptr in them
			static std::unordered_map<std::string, std::unique_ptr<Var>> genVars(Group* thisPtr) {
				std::unordered_map<std::string, std::unique_ptr<Var>> varMap;
				<!--(for _, v in sorted(g['vars'].items()))-->
				varMap.emplace("$!v['name']!$", std::make_unique<$!v['type'].capitalize()!$Var>("$!v['name']!$", thisPtr, <!--(if v['type'] != 'string')-->&<!--(end)-->shm->$!g['groupname']!$.g.$!v['name']!$<!--(if v['type'] == 'string')-->, $!v['length']!$<!--(end)-->));
				<!--(end)-->
				return varMap;
			}

			$!g['groupname'].capitalize()!$(): Group("$!g['groupname']!$", genVars(this)) {
				loadVarCaches();
			}

			void watch(watcher_t watcher) override {
				shm_watch($!g['groupname']!$, watcher);
			}

			void unwatch(watcher_t watcher) override {
				shm_unwatch($!g['groupname']!$, watcher);
			}

			void onWriteNoLock() override {
				shm->$!g['groupname']!$.m.f = 1;
				shm->$!g['groupname']!$.m.stream = 1;
				shm->$!g['groupname']!$.m.last_client = 0;
				shm_notify(shm->$!g['groupname']!$.m.w);
			}

			void lockShm() override {
				shm_lock($!g['groupname']!$);
			}

			void unlockShm() override {
				shm_unlock($!g['groupname']!$);
			}
		};

		<!--(end)-->
	};

	std::unordered_map<std::string, std::function<std::unique_ptr<Group>()>> strGroupMap{
		<!--(for g in groups)-->
		{"$!g['groupname']!$", [] { return std::make_unique<Groups::$!g['groupname'].capitalize()!$>(); } },
		<!--(end)-->
	};

	std::unique_ptr<Group> newGroup(std::string name) {
		auto it = strGroupMap.find(name);
		if (it != strGroupMap.end()) {
			return it->second();
		} else {
			throw std::invalid_argument("Group '" + name + "' does not exist");
		}
	}

	std::unordered_set<std::unique_ptr<Group>> newGroupSet() {
		std::unordered_set<std::unique_ptr<Group>> groupSet(strGroupMap.size());
		for (auto& pair : strGroupMap) {
			groupSet.emplace(pair.second());
		}

		return groupSet;
	}
}
}
