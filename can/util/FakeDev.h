#pragma once
#include <memory>

#include <can/lib/Message.h>

#define NUM_PARAM 32

class FakeDev {
    public:
        FakeDev(uint8_t devid, uint8_t hwtype, uint8_t fwver);
        void setGroup(uint8_t gid);
        void setHEnable(bool en);

        uint8_t getGroup() const;
        uint8_t getDevID() const;
        uint8_t getHWType() const;
        uint8_t getFWVer() const;
        
        int16_t getValue() const;
        uint8_t getOffset() const;
        bool getSEnable() const;
        bool getHEnable() const;

        int16_t getParam(uint8_t id) const;

        std::unique_ptr<Message> handleMessage(Message& msg);
        bool wantsMessage(Message& msg) const;

    private:
        const uint8_t m_devid;
        uint8_t m_group;
        const uint8_t m_hwtype;
        const uint8_t m_fwver;

        int16_t m_value;
        uint8_t m_offset;

        bool m_sEnable;
        bool m_hEnable;

        int16_t m_params[NUM_PARAM];

        std::vector<Selector> m_selectors;

        std::unique_ptr<Message> valueMessage() const;
        std::unique_ptr<Message> offsetMessage() const;
        std::unique_ptr<Message> paramMessage(uint8_t id) const;
        std::unique_ptr<Message> commitMessage(uint8_t id) const;
        std::unique_ptr<Message> groupMessage() const;
        std::unique_ptr<Message> announceMessage() const;
};
