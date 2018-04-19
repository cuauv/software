#pragma once

#include <list>
#include <map>
#include <string>
#include <functional>

namespace cuauv {
namespace serial {
namespace cli {

/// @brief A singleton class for parsing command line arguments
class CommandDispatcher {
    public:
        /**
         * Parses the command line, and calls the appropriate handler function
         *
         * @param argc the number of command line arguments
         * @param argv the command line arguments
         *
         * @return the program's return code
         */
        static int run(int argc, char **argv);

        /// @brief A descriptor of a command which can be run
        struct CommandInfo {
            /// The command's name
            std::string name;
            /// The command's help message
            std::string description;
            /**
             * The command's handler function
             * The input is a map from argument name to argument value
             * If an optional argument was not specified, it does not appear
             * in the map
             */
            std::function<int(std::map<std::string, std::string>)> func;
            
            /// @brief A descriptor of an argument to a command
            struct Arg {
                /// The argument's name
                std::string name;
                /// The argument's help message
                std::string description;
                /**
                 * true if the argument is required, false if not
                 * All required argument must come before all optional arguments
                 */
                bool required;
            };
            
            /// The ordered list of arguments to the command
            std::list<Arg> args;
        };

        /// @brief A class which is used to register a command
        class Register {
            public:
                /**
                 * This constructor register a command with the dispatcher singleton
                 *
                 * @param info The command's descriptor
                 */
                explicit Register(CommandInfo info);
        };
        
    private:
        /// The list of commands, where each key is the command name
        std::map<std::string, CommandInfo> m_info;
        /// Default constructor, set to private to enforce the singleton property
        CommandDispatcher() {}

        /**
         * Returns the singleton instance of this class
         *
         * @return the one and only CommandDispatcher instance
         */
        static CommandDispatcher* getInstance();

        /**
         * The actual implementation of the run() function
         *
         * @param argc the number of command line arguments
         * @param argv the command line arguments
         *
         * @return the program's return code
         */
        int _run(int argc, char **argv);

        /**
         * Prints a full usage message
         *
         * @param progName the name with which this program was called
         */
        void printFullUsage(const std::string& progName) const;

        /**
         * Prints a short usage message for a single command
         *
         * @param cmd the command to print usage for
         */
        void printShortCommandUsage(const CommandInfo& cmd) const;

        /**
         * Prints a detailed usage message for a single command
         *
         * @param progName the name with which this program was called
         * @param cmd the command to print usage for
         */
        void printCommandUsage(const std::string& progName, const CommandInfo& cmd) const;
};

// At this point, preprocessor and global constructor abuse begins
// BEGIN_COMMAND_DEF defines a global instance of a CommandDispatcher::Register object,
// and starts filling fields in the CommandInfo struct.
// ARG_REQUIRED and ARG_OPTIONAL add entries to the CommandInfo's argument list
// END_COMMAND_DEF ends the argument list and finishes off the object declaration
// When the program runs, the Register constructor will run before main() does, and
// populate the m_info map in the CommandDispatcher singleton
#define BEGIN_COMMAND_DEF(name, desc, func) \
    static CommandDispatcher::Register __cmd_reg_##name {CommandDispatcher::CommandInfo {#name, desc, &func, {
#define ARG_REQUIRED(name, desc) {#name, desc, true},
#define ARG_OPTIONAL(name, desc) {#name, desc, false},
#define END_COMMAND_DEF() {"", "", false}}}};

}}} // end namespace cuauv::serial::cli
