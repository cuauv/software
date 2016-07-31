#include "Variable.h"

namespace cuauv {
namespace serial {

Variable::Variable(std::string name, Type type, reg_t start, bool readOnly) :
                                        m_name(name),
                                        m_type(type),
                                        m_start(start),
                                        m_readOnly(readOnly)
{
}

std::string Variable::name() const {
    return m_name;
}

Variable::Type Variable::type() const {
    return m_type;
}

Variable::reg_t Variable::startReg() const {
    return m_start;
}

uint8_t Variable::size() const {
    switch (m_type) {
        case Type::UINT8:
            return 1;
        case Type::INT8:
            return 1;
        case Type::UINT16:
            return 2;
        case Type::INT16:
            return 2;
        case Type::FLOAT:
            return 4;
        default:
            throw std::out_of_range("unexpected Variable type");
    }
}

bool Variable::isReadOnly() const {
    return m_readOnly;
}

void Variable::addToSet(RegisterSet& set) const {
    set.insertRange(startReg(), size());
}

bool Variable::isPartOf(const RegisterRange& range) const {
    uint8_t min_reg = startReg();
    uint8_t max_reg = min_reg + size() - 1;

    if (range.startRegister() > max_reg) return false;
    if (range.startRegister() + range.numRegisters() - 1 < min_reg) return false;
    return true;
}

bool Variable::isPartOf(const BoundRegisterRange& range) const {
    uint8_t min_reg = startReg();
    uint8_t max_reg = min_reg + size() - 1;

    if (range.startRegister() > max_reg) return false;
    if (range.startRegister() + range.numRegisters() - 1 < min_reg) return false;
    return true;
}

bool Variable::isPartOf(const RegisterSet& set) const {
    uint8_t min_reg = startReg();
    uint8_t max_reg = min_reg + size() - 1;

    for (auto reg = min_reg; reg <= max_reg; reg++) {
        if (set.contains(reg)) return true;
    }
    return false;
}

bool Variable::isPartOf(const BoundRegisterSet& set) const {
    uint8_t min_reg = startReg();
    uint8_t max_reg = min_reg + size() - 1;

    for (auto reg = min_reg; reg <= max_reg; reg++) {
        if (set.contains(reg)) return true;
    }
    return false;
}

bool Variable::operator==(const Variable& other) const {
    if (m_name != other.m_name) return false;
    if (m_type != other.m_type) return false;
    if (m_start != other.m_start) return false;
    if (m_readOnly != other.m_readOnly) return false;
    return true;
}

BoundVariable Variable::bind(float floatVal) const {
    // BoundVariable will throw an exception if necessary
    return BoundVariable(*this, floatVal);
}

BoundVariable Variable::bind(int intVal) const {
    switch (m_type) {
        case Type::UINT8:
            return BoundVariable(*this, (uint8_t)intVal);
        case Type::INT8:
            return BoundVariable(*this, (int8_t)intVal);
        case Type::UINT16:
            return BoundVariable(*this, (uint16_t)intVal);
        case Type::INT16:
            return BoundVariable(*this, (int16_t)intVal);
        default:
            throw std::invalid_argument("bound int to a non-integral Variable");
    }
}

BoundVariable Variable::bind(const BoundRegisterSet& set) const {
    uint8_t arr[4];
    for (uint8_t i = 0; i < size(); i++) {
        arr[i] = set[i + startReg()];
    }
    return BoundVariable(*this, arr);
}

BoundVariable Variable::bind(const BoundRegisterRange& range) const {
    uint8_t arr[4];
    for (uint8_t i = 0; i < size(); i++) {
        arr[i] = range.at(i + startReg());
    }
    return BoundVariable(*this, arr);
}

BoundVariable::BoundVariable(const Variable& var, float floatVal) : Variable(var) {
    if (type() != Type::FLOAT) {
        throw std::invalid_argument("bound float to a non-FLOAT Variable");
    }
    m_val.f = floatVal;
}

BoundVariable::BoundVariable(const Variable& var, uint8_t uint8Val) : Variable(var) {
    if (type() != Type::UINT8) {
        throw std::invalid_argument("bound uint8_t to a non-UINT8 Variable");
    }
    m_val.u8 = uint8Val;
}

BoundVariable::BoundVariable(const Variable& var, int8_t int8Val) : Variable(var) {
    if (type() != Type::INT8) {
        throw std::invalid_argument("bound int8_t to a non-INT8 Variable");
    }
    m_val.i8 = int8Val;
}

BoundVariable::BoundVariable(const Variable& var, uint16_t uint16Val) : Variable(var) {
    if (type() != Type::UINT16) {
        throw std::invalid_argument("bound uint16_t to a non-UINT16 Variable");
    }
    m_val.u16 = uint16Val;
}

BoundVariable::BoundVariable(const Variable& var, int16_t int16Val) : Variable(var) {
    if (type() != Type::INT16) {
        throw std::invalid_argument("bound int16_t to a non-INT16 Variable");
    }
    m_val.i16 = int16Val;
}

BoundVariable::BoundVariable(const Variable& var, uint8_t arrVal[4]) : Variable(var) {
    m_val.arr[0] = arrVal[0];
    m_val.arr[1] = arrVal[1];
    m_val.arr[2] = arrVal[2];
    m_val.arr[3] = arrVal[3];
}

int BoundVariable::getInt() const {
    switch (type()) {
        case Type::UINT8:
            return m_val.u8;
        case Type::INT8:
            return m_val.i8;
        case Type::UINT16:
            return m_val.u16;
        case Type::INT16:
            return m_val.i16;
        default:
            throw std::invalid_argument("attempted to get int from non-integral Variable");
    }
}

float BoundVariable::getFloat() const {
    if (type() != Type::FLOAT) {
        throw std::invalid_argument("attempted to get float from a non-FLOAT Variable");
    }
    return m_val.f;
}

void BoundVariable::bindInSet(BoundRegisterSet& set) const {
    for (int i = 0; i < size(); i++) {
        set[startReg() + i] = m_val.arr[i];
    }
}

void BoundVariable::bindInRange(BoundRegisterRange& range) const {
    for (int i = 0; i < size(); i++) {
        range.at(startReg() + i) = m_val.arr[i];
    }
}

}} // end namespace cuauv::serial
