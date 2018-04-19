#include "DeviceInfo.h"

#include <stdexcept>

#include "proto/cpp/DeviceConfig.pb.h"

namespace cuauv {
namespace serial {

/**
 * Converts a variable type from the protobuf enum to the Variable enum
 *
 * @param type the protobuf variable type
 *
 * @returns the Variable variable type
 *
 * @throws std::logic_error for invalid protobuf variable types
 */
static Variable::Type protoTypeToVarType(proto::DeviceConfig::Variable::Type type) {
    switch (type) {
        case proto::DeviceConfig::Variable::UINT8:
            return Variable::Type::UINT8;
        case proto::DeviceConfig::Variable::INT8:
            return Variable::Type::INT8;
        case proto::DeviceConfig::Variable::UINT16:
            return Variable::Type::UINT16;
        case proto::DeviceConfig::Variable::INT16:
            return Variable::Type::INT16;
        case proto::DeviceConfig::Variable::FLOAT:
            return Variable::Type::FLOAT;
        default:
            throw std::logic_error("Unexpected DeviceConfig variable type. Did the .proto file change?");
    }
}

/**
 * Converts a variable type from the Variable enum to the protobuf enum
 *
 * @param type the Variable variable type
 *
 * @returns the protobuf variable type
 *
 * @throws std::logic_error for invalid Variable types
 */
static proto::DeviceConfig::Variable::Type varTypeToProtoType(Variable::Type type) {
    switch (type) {
        case Variable::Type::UINT8:
            return proto::DeviceConfig::Variable::UINT8;
        case Variable::Type::INT8:
            return proto::DeviceConfig::Variable::INT8;
        case Variable::Type::UINT16:
            return proto::DeviceConfig::Variable::UINT16;
        case Variable::Type::INT16:
            return proto::DeviceConfig::Variable::INT16;
        case Variable::Type::FLOAT:
            return proto::DeviceConfig::Variable::FLOAT;
        default:
            throw std::logic_error("Unexpected Variable type");
    }
}

DeviceInfo::DeviceInfo(uint8_t protocolVersion, uint8_t resetFlags, std::string device_config) {
    m_protocolVersion = protocolVersion;
    m_resetFlags = resetFlags;

    proto::DeviceConfig cfg;
    if (!cfg.ParseFromString(device_config)) {
        throw std::invalid_argument("device_config was not a valid DeviceConfig object");
    }

    m_name = cfg.name();
    m_type = cfg.type();
    
    for (auto cvar : cfg.write_variables()) {
        Variable var(cvar.name(), protoTypeToVarType(cvar.type()), cvar.base_register(), false);
        m_writeVariables.insert(var);
        if (var.type() == Variable::Type::FLOAT) {
            if (cvar.has_float_default()) {
                m_writeDefaults.insert(std::make_pair(var.name(), var.bind(cvar.float_default())));
            }
        } else {
            if (cvar.has_int_default()) {
                m_writeDefaults.insert(std::make_pair(var.name(), var.bind(cvar.int_default())));
            }
        }
    }

    for (auto group : cfg.read_groups()) {
        std::unordered_set<Variable> groupVars;
        for (auto cvar : group.read_variables()) {
            Variable var(cvar.name(), protoTypeToVarType(cvar.type()), cvar.base_register(), true);
            groupVars.insert(var);
            m_readVariables.insert(var);

            if (var.type() == Variable::Type::FLOAT) {
                if (cvar.has_float_default()) {
                    m_readDefaults.insert(std::make_pair(var.name(), var.bind(cvar.float_default())));
                }
            } else {
                if (cvar.has_int_default()) {
                    m_readDefaults.insert(std::make_pair(var.name(), var.bind(cvar.int_default())));
                }
            }
        }

        m_pollGroups.insert(std::pair<int, std::unordered_set<Variable>>(group.interval_ms(), groupVars));
    }
}

std::string DeviceInfo::asString() const {
    proto::DeviceConfig cfg;
    cfg.set_name(m_name);
    cfg.set_type(m_type);

    for (auto var : m_writeVariables) {
        auto cfg_var = cfg.add_write_variables();
        cfg_var->set_name(var.name());
        cfg_var->set_type(varTypeToProtoType(var.type()));
        cfg_var->set_base_register(var.startReg());

        auto it = m_writeDefaults.find(var.name());
        if (it != m_writeDefaults.end()) {
            if (var.type() == Variable::Type::FLOAT) {
                cfg_var->set_float_default(it->second.getFloat());
            } else {
                cfg_var->set_int_default(it->second.getInt());
            }
        }
    }

    for (auto group : m_pollGroups) {
        auto cfg_group = cfg.add_read_groups();
        cfg_group->set_interval_ms(group.first);
        for (auto var : group.second) {
            auto cfg_var = cfg_group->add_read_variables();
            cfg_var->set_name(var.name());
            cfg_var->set_type(varTypeToProtoType(var.type()));
            cfg_var->set_base_register(var.startReg());

            auto it = m_readDefaults.find(var.name());
            if (it != m_readDefaults.end()) {
                if (var.type() == Variable::Type::FLOAT) {
                    cfg_var->set_float_default(it->second.getFloat());
                } else {
                    cfg_var->set_int_default(it->second.getInt());
                }
            }
        }
    }

    std::string result;
    if (!cfg.SerializeToString(&result)) {
        throw std::logic_error("Unexpected DeviceConfig serialization error. Did the .proto file change?");
    }
    return result;
}

std::string DeviceInfo::name() const {
    return m_name;
}

std::string DeviceInfo::type() const {
    return m_type;
}

uint8_t DeviceInfo::protocolVersion() const {
    return m_protocolVersion;
}

uint8_t DeviceInfo::resetFlags() const {
    return m_resetFlags;
}

std::unordered_set<Variable> DeviceInfo::writeVariables() const {
    return m_writeVariables;
}

std::map<std::string, BoundVariable> DeviceInfo::writeDefaults() const {
    return m_writeDefaults;
}

std::unordered_set<Variable> DeviceInfo::readVariables() const {
    return m_readVariables;
}

std::map<std::string, BoundVariable> DeviceInfo::readDefaults() const {
    return m_readDefaults;
}

std::map<int, std::unordered_set<Variable>> DeviceInfo::pollGroups() const {
    return m_pollGroups;
}

}} // end namespace cuauv::serial
