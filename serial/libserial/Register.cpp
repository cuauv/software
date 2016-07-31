#include "Register.h"

#include <stdexcept>

namespace cuauv {
namespace serial {

RegisterRange::RegisterRange(uint8_t startRegister, uint16_t numRegisters) :
                                m_startRegister(startRegister),
                                m_numRegisters(numRegisters)
{
    int maxReg = this->startRegister() + this->numRegisters();
    if (maxReg > 255) throw std::invalid_argument("RegisterRange too big");
}

uint8_t RegisterRange::startRegister() const {
    return m_startRegister;
}

uint16_t RegisterRange::numRegisters() const {
    return m_numRegisters;
}

BoundRegisterRange::BoundRegisterRange(uint8_t startRegister, const std::vector<uint8_t>& regs) :
                                m_startRegister(startRegister),
                                m_regs(regs)
{
    int maxReg = this->startRegister() + this->numRegisters();
    if (maxReg > 255) throw std::invalid_argument("RegisterRange too big");
}

BoundRegisterRange::BoundRegisterRange(uint8_t startRegister, uint16_t numRegisters) :
                                m_startRegister(startRegister),
                                m_regs(numRegisters)
{
    int maxReg = this->startRegister() + this->numRegisters();
    if (maxReg > 255) throw std::invalid_argument("RegisterRange too big");
}

uint8_t BoundRegisterRange::startRegister() const {
    return m_startRegister;
}

uint16_t BoundRegisterRange::numRegisters() const {
    return m_regs.size();
}

uint8_t& BoundRegisterRange::at(uint8_t reg) {
    // std::vector will bounds check for us
    return m_regs.at(reg - m_startRegister);
}

const uint8_t& BoundRegisterRange::at(uint8_t reg) const {
    return m_regs.at(reg - m_startRegister);
}

std::vector<uint8_t>::const_iterator BoundRegisterRange::begin() const {
    return m_regs.begin();
}

std::vector<uint8_t>::const_iterator BoundRegisterRange::end() const {
    return m_regs.end();
}

RegisterSet::RegisterSet() {
}

RegisterSet::RegisterSet(const std::vector<uint8_t>& reg_list) :
           m_regs(reg_list.begin(), reg_list.end())
{
}

uint16_t RegisterSet::numRegisters() const {
    return m_regs.size();
}

bool RegisterSet::rangeIsSmaller() const {
    if (numRegisters() == 0) return false;

    uint8_t min = *m_regs.begin();
    uint8_t max = *m_regs.rbegin();

    uint16_t range_size = max - min + 1;
    // range is smaller if at least 1/2 of the range is filled
    return ((float) numRegisters()) / range_size >= 0.5;
}

bool RegisterSet::contains(uint8_t reg) const {
    return m_regs.find(reg) != m_regs.end();
}

void RegisterSet::insert(uint8_t reg) {
    m_regs.insert(reg);
}

void RegisterSet::insertRange(uint8_t reg, uint8_t num) {
    for (int i = 0; i < num; i++) {
        m_regs.insert(reg + i);
    }
}

RegisterRange RegisterSet::toRange() const {
    // Deal with empty register sets
    if (numRegisters() == 0) return RegisterRange(0, 0);

    uint8_t min = *m_regs.begin();
    uint8_t max = *m_regs.rbegin();

    return RegisterRange(min, max - min + 1);
}

std::set<uint8_t>::const_iterator RegisterSet::begin() const {
    return m_regs.begin();
}

std::set<uint8_t>::const_iterator RegisterSet::end() const {
    return m_regs.end();
}

BoundRegisterSet::BoundRegisterSet()
{

}

BoundRegisterSet::BoundRegisterSet(const std::vector<uint8_t>& indices, const std::vector<uint8_t>& values) {
    if (indices.size() != values.size()) throw std::invalid_argument("Differing number of indices and values in BoundRegisterSet constructor");
    
    auto iit = indices.begin();
    auto vit = values.begin();
    for (; iit != indices.end(); iit++, vit++) {
        m_regs.insert(std::make_pair(*iit, *vit));
    }
}

BoundRegisterSet::BoundRegisterSet(const RegisterSet& indices, const std::vector<uint8_t>& values) {
    if (indices.numRegisters() != values.size()) throw std::invalid_argument("Differing number of indices and values in BoundRegisterSet constructor");

    auto iit = indices.begin();
    auto vit = values.begin();
    for (; iit != indices.end(); iit++, vit++) {
        m_regs.insert(std::make_pair(*iit, *vit));
    }
}

uint16_t BoundRegisterSet::numRegisters() const {
    return m_regs.size();
}

bool BoundRegisterSet::rangeIsSmaller() const {
    if (m_regs.size() == 0) return false;
    uint8_t min = m_regs.begin()->first;
    uint8_t max = m_regs.rbegin()->first;

    uint16_t range_size = max - min + 1;
    // range is smaller if at least 1/2 of the range is filled
    return ((float) numRegisters()) / range_size >= 0.5;
}

bool BoundRegisterSet::contains(uint8_t reg) const {
    return m_regs.find(reg) != m_regs.end();
}

void BoundRegisterSet::insert(uint8_t reg, uint8_t value) {
    m_regs[reg] = value;
}

uint8_t& BoundRegisterSet::operator[](uint8_t reg) {
    return m_regs[reg];
}

const uint8_t& BoundRegisterSet::operator[](uint8_t reg) const {
    // map doesn't have a const [], hack it with the .at function
    // Feels cleaner to do it like this rather than add a .at to BoundRegisterSet
    return m_regs.at(reg);
}

BoundRegisterRange BoundRegisterSet::toBoundRange() const {
    if (m_regs.size() == 0) return BoundRegisterRange(0, 0);
    uint8_t min = m_regs.begin()->first; // regs are in sorted ascending order
    uint8_t max = m_regs.rbegin()->first;
    
    BoundRegisterRange range(min, max - min + 1);
    for (auto reg : m_regs) {
        range.at(reg.first) = reg.second;
    }

    return range;
}

BoundRegisterSet::key_iterator BoundRegisterSet::key_begin() const {
    return m_regs.begin();
}

BoundRegisterSet::key_iterator BoundRegisterSet::key_end() const {
    return m_regs.end();
}

BoundRegisterSet::value_iterator BoundRegisterSet::value_begin() const {
    return m_regs.begin();
}

BoundRegisterSet::value_iterator BoundRegisterSet::value_end() const {
    return m_regs.end();
}

}} // end namespace cuauv::serial
