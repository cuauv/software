#pragma once

#include <string>
#include <stdexcept>
#include <stdint.h>
#include <type_traits>

#include "util.h"
#include "Register.h"

namespace cuauv
{
namespace serial
{

class BoundVariable;

/** 
 * @brief An immutable object representing a device variable
 *
 * Only libserial itself can create new Variable objects, clients
 * are limited to only the copy constructor
 */
class Variable {
    // Allow DeviceInfo objects to call the main constructor
    friend class DeviceInfo;

    public:
        /// The type of a register specifier
        typedef uint8_t reg_t;

        /// The set of types a device variable can take on
        enum class Type {
            UINT8,
            INT8,
            UINT16,
            INT16,
            FLOAT
        };

        /**
         * Get the name of the variable
         * @return The variable's name
         */
        std::string name() const;

        /**
         * Get the type of the variable
         * @return The variable's type
         */
        Type type() const;

        /**
         * Get the starting register of the variable
         * @return The starting register
         */
        reg_t startReg() const;

        /**
         * Get the size in registers of the variable
         * @return The variable's size
         */
        uint8_t size() const;

        /**
         * Returns whether the variable is read-only
         * @return true if the variable is read-only
         */
        bool isReadOnly() const;

        /**
         * Adds all registers contained by this variable into a set
         *
         * @param set the set to add the registers to
         */
        void addToSet(RegisterSet& set) const;

        /**
         * Checks whether any part of this Variable is contained in range
         *
         * Note that this does not necessarily mean the entire variable is
         * in the range, just that any part is. This means that binding may
         * still fail, even if this returns true.
         *
         * @param range the register range to check
         *
         * @returns true if any part of this Variable is in the range
         */
        bool isPartOf(const RegisterRange& range) const;

        /**
         * Checks whether any part of this Variable is contained in range
         *
         * Note that this does not necessarily mean the entire variable is
         * in the range, just that any part is. This means that binding may
         * still fail, even if this returns true.
         *
         * @param range the register range to check
         *
         * @returns true if any part of this Variable is in the range
         */
        bool isPartOf(const BoundRegisterRange& range) const;

        /**
         * Checks whether any part of this Variable is contained in set
         *
         * Note that this does not necessarily mean the entire variable is
         * in the set, just that any part is. This means that binding may
         * still fail, even if this returns true.
         *
         * @param set the register set to check
         *
         * @returns true if any part of this Variable is in the set
         */
        bool isPartOf(const RegisterSet& set) const;

        /**
         * Checks whether any part of this Variable is contained in set
         *
         * Note that this does not necessarily mean the entire variable is
         * in the set, just that any part is. This means that binding may
         * still fail, even if this returns true.
         *
         * @param set the register set to check
         *
         * @returns true if any part of this Variable is in the set
         */
        bool isPartOf(const BoundRegisterSet& set) const;
        
        /**
         * Binds a value to this variable
         * 
         * This version is only for FLOAT variable types
         *
         * @throws std::invalid_argument if the variable is not an integral type
         *
         * @param floatVal the floating point value
         * @return this Variable, bound with floatVal
         */
        BoundVariable bind(float floatVal) const;

        /**
         * Binds a value to this variable
         *
         * This version is only for integral variable types
         *
         * @throws std::invalid_argument if the variable is not an integral type
         *
         * @param intVal the integral value
         * @return this Variable, bound with intVal
         */
        BoundVariable bind(int intVal) const;

        /**
         * Binds a value to this variable
         *
         * This version pulls the value out of a BoundRegisterSet
         *
         * @throws std::invalid_argument if one or more of the variable's registers don't exist in the set
         *
         * @param set the set to pull values from
         * 
         * @returns this Variable, bound with the value in set
         */
        BoundVariable bind(const BoundRegisterSet& set) const;

        /**
         * Binds a value to this variable
         *
         * This version pulls the value out of a BoundRegisterRange
         *
         * @throws std::invalid_argument if one or more of the variable's registers don't exist in the range
         *
         * @param range the range to pull values from
         * 
         * @returns this Variable, bound with the value in range
         */
        BoundVariable bind(const BoundRegisterRange& range) const;

        /**
         * Operator overload for the equality operator.
         *
         * The comparison is only valid for Variables that were produced by the same DeviceInfo object
         *
         * @param other the Variable being compared
         *
         * @returns true if both objects refer to the same device variable
         */
        bool operator==(const Variable& other) const;

    protected:
        /**
         * Constructs a new variable object
         *
         * @param name the variable's name
         * @param type the variable's type
         * @param start the starting register of this variable
         * @param readOnly whether the variable is read-only
         */
        Variable(std::string name, Type type, reg_t start, bool readOnly);

    private:
        /// The variable's name
        std::string m_name;
        /// The variable's type
        Type m_type;
        /// The register identifier for the start of the variable
        reg_t m_start;
        /// True when the variable is read only
        bool m_readOnly;
};

/**
 * @brief A variable which has an actual value bound to it
 */
class BoundVariable : public Variable {
    public:
        /**
         * Constructs a BoundVariable from an existing Variable
         * 
         * @throws std::invalid_argument if the variable is not of type FLOAT
         *
         * @param var the variable to be bound
         * @param floatVal the value to bind
         */
        BoundVariable(const Variable& var, float floatVal);

        /**
         * Constructs a BoundVariable from an existing Variable
         * 
         * @throws std::invalid_argument if the variable is not of type UINT8
         *
         * @param var the variable to be bound
         * @param uint8Val the value to bind
         */
        BoundVariable(const Variable& var, uint8_t uint8Val);

        /**
         * Constructs a BoundVariable from an existing Variable
         * 
         * @throws std::invalid_argument if the variable is not of type INT8
         *
         * @param var the variable to be bound
         * @param int8Val the value to bind
         */
        BoundVariable(const Variable& var, int8_t int8Val);

        /**
         * Constructs a BoundVariable from an existing Variable
         * 
         * @throws std::invalid_argument if the variable is not of type UINT16
         *
         * @param var the variable to be bound
         * @param uint16Val the value to bind
         */
        BoundVariable(const Variable& var, uint16_t uint16Val);

        /**
         * Constructs a BoundVariable from an existing Variable
         * 
         * @throws std::invalid_argument if the variable is not of type INT16
         *
         * @param var the variable to be bound
         * @param int16Val the value to bind
         */
        BoundVariable(const Variable& var, int16_t int16Val);

        /**
         * Constructs a BoundVariable from an array of values
         *
         * @param var the variable to be bound
         * @param arrVal the array of values, in little endian order. Depending on type type
         * of the variable, some values may be ignored
         */
        BoundVariable(const Variable& var, uint8_t arrVal[4]);

        /**
         * Returns the value of an integral-type variable
         *
         * @throws std::invalid_argument if the variable is not an integral type
         *
         * @return the value bound to this variable
         */
        int getInt() const;

        /**
         * Returns the value of a floating point variable
         *
         * @throws std::invalid_argument if the variable is not a floating point type
         *
         * @return the value bound to this variable
         */
        float getFloat() const;

        /**
         * Binds all of this variable's registers into a register set
         *
         * @param set the set to bind values into
         */
        void bindInSet(BoundRegisterSet& set) const;

        /**
         * Binds all of this variable's registers into a register range
         *
         * @param range the range to bind values into
         */
        void bindInRange(BoundRegisterRange& range) const;

    private:
        /// The value bound to the variable
        union {
            float f;
            uint8_t u8;
            int8_t i8;
            uint16_t u16;
            int16_t i16;
            uint8_t arr[4];
        } m_val;
};

} // end namespace serial
} // end namespace cuauv

namespace std
{
    /// @brief Specialization of std::hash for Variable
    template<> struct hash<cuauv::serial::Variable>
    {
        /// The type being hashed
        typedef cuauv::serial::Variable argument_type;
        /// The result of the hash
        typedef std::size_t result_type;
        /// Computes the hash function
        result_type operator()(argument_type const& var) const
        {
            result_type seed = 0;
            cuauv::serial::hash_combine(seed, var.name());
            cuauv::serial::hash_combine(seed, (std::underlying_type<cuauv::serial::Variable::Type>::type) var.type()); // lol
            cuauv::serial::hash_combine(seed, var.startReg());
            cuauv::serial::hash_combine(seed, var.isReadOnly());
            return seed;
        }
    };

    /// @brief Specialization of std::hash for BoundVariable
    template<> struct hash<cuauv::serial::BoundVariable>
    {
        /// The type being hashed
        typedef cuauv::serial::BoundVariable argument_type;
        /// The result of the hash
        typedef std::size_t result_type;
        /// Computes the hash function
        result_type operator()(argument_type const& var) const
        {
            result_type seed = 0;
            cuauv::serial::hash_combine(seed, var.name());
            cuauv::serial::hash_combine(seed, (std::underlying_type<cuauv::serial::Variable::Type>::type) var.type()); // lol
            cuauv::serial::hash_combine(seed, var.startReg());
            cuauv::serial::hash_combine(seed, var.isReadOnly());
            // intentionally don't include the value of the variable in the hash
            // since all use cases assume that the container only holds one of each
            // variable
            return seed;
        }
    };
}
