#pragma once

#include <vector>
#include <set>
#include <map>
#include <iterator>
#include <cstdint>

namespace cuauv {
namespace serial {

/**
 * @brief A continugous range of registers, without specific values
 */
class RegisterRange {
    public:
        /**
         * Constructs a contiguous register range
         *
         * @throws std::invalid_argument if the range extends beyond the number
         * of possible registers
         *
         * @param startRegister the starting register number
         * @param numRegisters the number of registers in the range
         */
        RegisterRange(uint8_t startRegister, uint16_t numRegisters);

        /**
         * Gets the starting register number
         *
         * @returns the starting register number
         */
        uint8_t startRegister() const;

        /**
         * Gets the number of registers contained in this range
         *
         * @returns the number of registers in the range
         */
        uint16_t numRegisters() const;
        
    private:
        /// The starting register number
        uint8_t m_startRegister;
        /// The number of registers
        uint16_t m_numRegisters;
};

/**
 * @brief A contiguous range of registers, with values
 */
class BoundRegisterRange {
    public:
        /**
         * Constructs a bound register range from a list of registers
         *
         * @throws std::invalid_argument if the range extends beyond the
         * number of possible registers
         *
         * @param startRegister the starting register number
         * @param regs the list of register values
         */
        BoundRegisterRange(uint8_t startRegister, const std::vector<uint8_t>& regs);

        /**
         * Constructs a bound register range, with garbage initial values.
         *
         * The values initially stored in the range are unpredictable, so any registers
         * one actually cares about should be overwritten.
         *
         * @param startRegister the starting register number
         * @param numRegisters the number of registers in the range
         */
        BoundRegisterRange(uint8_t startRegister, uint16_t numRegisters);

        /**
         * Gets the starting register number
         *
         * @returns the starting register number
         */
        uint8_t startRegister() const;
        
        /**
         * Gets the number of registers contained in this range
         *
         * @returns the number of registers in the range
         */
        uint16_t numRegisters() const;

        /**
         * Gets a reference to a register's value
         *
         * @param reg the register number
         *
         * @returns a reference to that register's value
         *
         * @throws std::out_of_range if reg is not contained in this range
         */
        uint8_t& at(uint8_t reg);

        /**
         * Gets a const reference to a register's value
         *
         * @param reg the register number
         *
         * @returns a const reference to that register's value
         *
         * @throws std::out_of_range if reg is not contained in this range
         */
        const uint8_t& at(uint8_t reg) const;

        /// Const iterator type for the list of values in this object
        typedef std::vector<uint8_t>::const_iterator const_iterator;

        /**
         * Gets an iterator to the beginning of this range
         *
         * @returns a const iterator to the beginning of this register range
         */
        const_iterator begin() const;

        /**
         * Gets an iterator to the end of this range
         *
         * @returns a const iterator to the end of this register range
         */
        const_iterator end() const;

    private:
        /// The starting register number
        uint8_t m_startRegister;
        /// The values of the registers
        std::vector<uint8_t> m_regs;
};

/**
 * @brief A set of registers, not necessarily contiguous
 */
class RegisterSet {
    public:
        RegisterSet();
        explicit RegisterSet(const std::vector<uint8_t>& reg_list);
        /**
         * Gets the number of registers contained in this set
         *
         * @returns the number of registers contained in this set
         */
        uint16_t numRegisters() const;

        /**
         * Determines whether representing the registers as a range would be more efficient
         *
         * @returns true if representing as a range is better
         */
        bool rangeIsSmaller() const;

        /**
         * Returns whether a register is contained in this set
         *
         * @param reg the register number to check for
         *
         * @returns true if the register is contained
         */
        bool contains(uint8_t reg) const;

        /**
         * Inserts a register number into the set
         * 
         * @param reg the register number to insert
         */
        void insert(uint8_t reg);

        /**
         * Inserts a contiguous register number range into the set
         * 
         * @param reg the first register number to insert
         * @param num the number of registers to insert
         */
        void insertRange(uint8_t reg, uint8_t num);

        /**
         * Converts this set into a contiguous range of registers
         *
         * @returns a RegisterRange containing all registers in this set
         */
        RegisterRange toRange() const;

        /// Const iterator type for the set of registers contained in this object
        typedef std::set<uint8_t>::const_iterator const_iterator;

        /**
         * Gets an iterator to the first register in this set
         *
         * @returns an iterator to the first register in this set
         */
        const_iterator begin() const;

        /**
         * Gets an iterator to the end of the register set
         *
         * @returns an interator to the end of the register set
         */
        const_iterator end() const;

    private:
        /// The set of registers contained in this set
        std::set<uint8_t> m_regs;
};

/// @brief A set of registers, with a value bound to each register
class BoundRegisterSet {
    public:
        /**
         * Constructs an empty BoundRegisterSet
         */
        BoundRegisterSet();

        BoundRegisterSet(const std::vector<uint8_t>& indices, const std::vector<uint8_t>& values);

        /**
         * Constructs a BoundRegisterSet from a set of indices and a list of values in ascending index order
         *
         * @throws std::invalid_argument if the size of the two inputs is mismatched
         *
         * @param indices the set of indices associated with the values array
         * @param values the list of values associated with each index.
         */
        BoundRegisterSet(const RegisterSet& indices, const std::vector<uint8_t>& values);
        /**
         * Gets the number of registers contained in this set
         *
         * @returns the number of registers contained in this set
         */
        uint16_t numRegisters() const;

        /**
         * Determines whether representing the registers as a range would be more efficient
         *
         * @returns true if representing as a range is better
         */
        bool rangeIsSmaller() const;

        /**
         * Returns whether a register is contained in this set
         *
         * @param reg the register number to check for
         *
         * @returns true if the register is contained
         */
        bool contains(uint8_t reg) const;

        /**
         * Inserts a register and value into the set
         * 
         * @param reg the register to insert
         * @param value the value to insert
         */
        void insert(uint8_t reg, uint8_t value);

        /**
         * Gets a reference to a register. Inserts the register if it does not already exist.
         *
         * @param reg the register to access
         *
         * @returns a reference to that register's value
         */
        uint8_t& operator[](uint8_t reg);

        /**
         * Gets a const reference to a register.
         *
         * @param reg the register to access
         *
         * @returns a const reference to that register's value
         */
        const uint8_t& operator[](uint8_t reg) const;
        
        /**
         * Converts this set into a range of bound registers
         *
         * Registers in the range which are not contained in this set will have an unpredictable value.
         *
         * @returns a BoundRegisterRange with the values held in this set
         */
        BoundRegisterRange toBoundRange() const;

        /// @brief Iterator type for accessing the keys in a bound register set
        class key_iterator : public std::iterator<std::forward_iterator_tag, uint8_t> {
            public:
                key_iterator(std::map<uint8_t, uint8_t>::const_iterator it) : m_it(it){}
                key_iterator(const key_iterator& mit) : m_it(mit.m_it) {}
                key_iterator& operator++() {++m_it; return *this;}
                key_iterator operator++(int) {key_iterator tmp(*this); operator++(); return tmp;}
                bool operator==(const key_iterator& rhs) {return m_it==rhs.m_it;}
                bool operator!=(const key_iterator& rhs) {return m_it!=rhs.m_it;}
                const uint8_t& operator*() {return m_it->first;}
            private:
                std::map<uint8_t, uint8_t>::const_iterator m_it;
        };

        /// @brief Iterator type for accessing the values in a bound register set
        class value_iterator : public std::iterator<std::forward_iterator_tag, uint8_t> {
            public:
                value_iterator(std::map<uint8_t, uint8_t>::const_iterator it) : m_it(it){}
                value_iterator(const value_iterator& mit) : m_it(mit.m_it) {}
                value_iterator& operator++() {++m_it; return *this;}
                value_iterator operator++(int) {value_iterator tmp(*this); operator++(); return tmp;}
                bool operator==(const value_iterator& rhs) {return m_it==rhs.m_it;}
                bool operator!=(const value_iterator& rhs) {return m_it!=rhs.m_it;}
                const uint8_t& operator*() {return m_it->second;}
            private:
                std::map<uint8_t, uint8_t>::const_iterator m_it;
        };

        /**
         * Gets an iterator for the first register number contained in the set
         *
         * @returns an iterator for first register number contained in the set
         */
        key_iterator key_begin() const;

        /**
         * Gets an iterator for the end of the register numbers contained in this set
         *
         * @returns an iterator for the end of the register numbers contained in this set
         */
        key_iterator key_end() const;

        /**
         * Gets an iterator for the first register value contained in the set
         *
         * Iteration order is guaranteed to be the same as for the key iterator
         *
         * @returns an iterator for the first register value contained in this set
         */
        value_iterator value_begin() const;
        
        /**
         * Gets an iterator for the end of the register values contained in this set
         *
         * @returns an iterator for the end of the register values contained in this set
         */
        value_iterator value_end() const;
    private:
        /// The registers in this set
        std::map<uint8_t, uint8_t> m_regs;
};

}} // end namespace cuauv::serial
