#pragma once

#include <string>
#include <map>
#include <unordered_set>

#include "Variable.h"

namespace cuauv {
namespace serial {

/**
 * @brief The data given by a device's configuration string
 */
class DeviceInfo {
    public:
        /**
         * Parses the device config string, sent as part of the Hello packet
         *
         * @param protocolVersion the device's protocol version
         * @param resetFlags the device's most recent reset flags
         * @param deviceConfig the config string sent by the device
         */
        DeviceInfo(uint8_t protocolVersion, uint8_t resetFlags, std::string deviceConfig);

        /**
         * Returns the device's name
         *
         * @return the device name, as given by the device
         */
        std::string name() const;

        /**
         * Returns the device's type
         *
         * @return the device type, as given by the device
         */
        std::string type() const;

        /**
         * Returns the device's protocol version
         *
         * @return the device's protocol version
         */
        uint8_t protocolVersion() const;

        /**
         * Returns the device's reset flags for the most recent reset
         *
         * @return the device's reset flags
         */
        uint8_t resetFlags() const;

        /**
         * Returns the set of writable variables
         *
         * @return the set of all writable variables on the device
         */
        std::unordered_set<Variable> writeVariables() const;

        /**
         * Returns the defaults values of all writeable variables, as specified by the device
         *
         * If the device does not specify a default value for a given variable, that variable
         * is omitted from this set
         *
         * @return the set of default values specified by the device
         */
        std::map<std::string, BoundVariable> writeDefaults() const;

        /**
         * Returns a set of all readable variables
         *
         * Note that every element in this set also appears in one of the pollGroups
         *
         * @return the set of all readable variables on the device
         */
        std::unordered_set<Variable> readVariables() const;

        /**
         * Returns the defaults values of all readable variables, as specified by the device
         *
         * If the device does not specify a default value for a given variable, that variable
         * is omitted from this set
         *
         * @return the set of default values specified by the device
         */
        std::map<std::string, BoundVariable> readDefaults() const;

        /**
         * Returns a the poll groups on the device
         *
         * The map's key is the polling interval in ms, and the map's value
         * is the set of readable variables covered by the poll group
         *
         * Note that every Variable in the pollGroups given here is also returned in readVariables()
         *
         * @return a map representing all poll groups on the device
         */
        std::map<int, std::unordered_set<Variable>> pollGroups() const;

        /**
         * Converts the device info object into a json-formatted string
         *
         * @returns the DeviceInfo's string representation
         */
        std::string asString() const;

    private:
        /// The device's protocol version
        uint8_t m_protocolVersion;
        /// The device's reset flags
        uint8_t m_resetFlags;

        /// The device's name
        std::string m_name;
        /// The device's type
        std::string m_type;
        
        /// The set of all writeable variables
        std::unordered_set<Variable> m_writeVariables;

        /// The set of default values for writeable variables
        std::map<std::string, BoundVariable> m_writeDefaults;

        /// The set of all readable variables
        std::unordered_set<Variable> m_readVariables;

        /// The set of default values for readable variables
        std::map<std::string, BoundVariable> m_readDefaults;

        /// The set of read groups. The map's key is the read interval in ms, and the value is the set of variables in the group
        std::map<int, std::unordered_set<Variable>> m_pollGroups;
};

}} // end namespace cuauv::serial
