#include <string>
#include <memory>
#include <list>
#include <map>
#include <cstdint>

class ShmDevice {
    public:
        virtual std::string getName() const = 0;
        virtual uint8_t getDevID() const = 0;
        virtual void setSetpoint(int16_t setpoint) = 0;
        virtual int16_t getSetpoint() const = 0;
        virtual int16_t getStatus() const = 0;
        virtual std::string getFeedbackName(uint8_t id) const = 0;
        virtual int16_t getFeedback(uint8_t id) const = 0;
};

class ShmDeviceList {
    public:
        ShmDeviceList();

        const std::list<std::shared_ptr<ShmDevice>> &getThrusters() const;
        const std::list<std::shared_ptr<ShmDevice>> &getVectors() const;

        std::shared_ptr<ShmDevice> getDevice(uint8_t id) const;
        void setSetpoint(uint8_t id, int16_t setpoint);

    private:
        std::map<uint8_t, std::shared_ptr<ShmDevice>> m_devices;
        std::list<std::shared_ptr<ShmDevice>> m_thrusters;
        std::list<std::shared_ptr<ShmDevice>> m_vectors;
};
