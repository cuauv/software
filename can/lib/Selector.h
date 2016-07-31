#pragma once
#include <cstdint>
#include <string>


class Selector {
    public:
        enum class Type {
            BROADCAST = 0x0,
            DEVID = 0x1,
            GROUP = 0x2,
            HWTYPE = 0x3,
            FWVER = 0x4
        };

        Selector(Type type, uint8_t value);
        Selector(uint16_t field);

        uint16_t getField() const;
        Type getType() const;
        uint8_t getValue() const;
        bool operator==(const Selector &other) const;

        static Selector Broadcast();
        static Selector DevID(uint8_t id);
        static Selector Group(uint8_t group);
        static Selector HWType(uint8_t type);
        static Selector FWVer(uint8_t ver);

        static std::string typeStr(Type type);
        std::string str() const;

    private:
        Type m_type;
        uint8_t m_value;
};
