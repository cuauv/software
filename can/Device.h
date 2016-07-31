#pragma once
#include <cstdint>
#include <mutex>
#include <deque>
#include <chrono>

#include "lib/Message.h"
#include "lib/CANSocket.h"
#include "Command.h"

class Device {
    public:
        Device(uint8_t devid, uint8_t desiredGroup, uint8_t desiredOffset);
        ~Device();

        uint8_t getDevID() const;
        uint8_t getHWType() const;
        uint8_t getFWVer() const;
        uint8_t getGroup() const;
        uint8_t getOffset() const;
        bool getHardEnable() const;

        int16_t getFeedback(uint8_t id) const;
        uint8_t getFlags() const;

        void processMessage(const Message& msg, const CANSocket& sock, std::shared_ptr<MessageBuilder> builder);

        void pushCommand(std::shared_ptr<Command> cmd);
        void clearCommands();

        bool isDead();

        virtual void onDeviceAnnounced();
        virtual void onStatusChanged();
        virtual void onDeviceCulled();

    private:

        const uint8_t m_devid;
        uint8_t m_hwtype;
        uint8_t m_fwver;
        uint8_t m_group;
        uint8_t m_offset;
        bool m_hardEnable;

        int16_t m_feedback[4];
        uint8_t m_flags;

        std::mutex m_cmdMutex;
        std::shared_ptr<Command> m_curCommand;
        std::deque<std::shared_ptr<Command>> m_cmdQueue;

        std::chrono::steady_clock::time_point m_lastMsgTime;
        std::chrono::steady_clock::time_point m_cmdStartTime;
};
