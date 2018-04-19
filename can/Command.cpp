#include "Command.h"
#include <stdexcept>

#include "Manager.h"

class PayloadCommand : public Command {
    private:
        uint8_t m_devid;
        Message::Type m_msgType;
        uint8_t m_payload;

    public:
        PayloadCommand(uint8_t devid, Message::Type msgType, uint8_t payload) : 
                                            m_devid(devid),
                                            m_msgType(msgType),
                                            m_payload(payload)
        {
            if (devid > 63 || devid == 0) throw std::invalid_argument("Invalid DevID");
        }

        virtual bool isValidResponse(const Message& msg) const {
            if (msg.getSource() != m_devid) return false;
            if (msg.getType() != m_msgType) return false;
            if (msg.getPayload() != m_payload) return false;
            
            return true;
        }

        virtual Message getMessage(std::shared_ptr<MessageBuilder> builder) const {
            return builder->buildMsg(m_msgType, Selector::DevID(m_devid), m_payload, false, {});
        }
};

class ValueCommand : public Command {
  private:
      uint8_t m_devid;
      Message::Type m_msgType;
      uint8_t m_payload;
      int16_t m_value;
  
  public:
      ValueCommand(uint8_t devid, Message::Type msgType, uint8_t payload, int16_t value) : m_devid(devid), m_msgType(msgType), m_payload(payload), m_value(value)
      { 
          if (devid > 63 || devid == 0) throw std::invalid_argument("Invalid DevID");
      }

      virtual bool isValidResponse(const Message& msg) const {
          if (msg.getSource() != m_devid) return false;
          if (msg.getType() != m_msgType) return false;
          if (msg.getPayload() != m_payload) return false;
          return true;
      }
      
      virtual Message getMessage(std::shared_ptr<MessageBuilder> builder) const {
          MessageData data;
          data.int16Push(m_value);
          return builder->buildMsg(m_msgType, Selector::DevID(m_devid), m_payload, false, data);
      }
};

std::shared_ptr<Command> Command::setGroup(uint8_t devid, uint8_t gid) {
    if (gid > 63) throw std::invalid_argument("Invalid gid");
    return std::make_shared<PayloadCommand>(devid, Message::Type::GROUP, gid);
}

std::shared_ptr<Command> Command::setOffset(uint8_t devid, uint8_t offset) {
    if (offset > 3) throw std::invalid_argument("Invalid offset");
    return std::make_shared<PayloadCommand>(devid, Message::Type::OFFSET, offset);
}

std::shared_ptr<Command> Command::setParam(uint8_t devid, uint8_t payload, uint16_t value) {
    return std::make_shared<ValueCommand>(devid, Message::Type::PARAM, payload, value);
}
