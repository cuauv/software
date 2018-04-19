#include "../ShmDevices.h"
#include <libshm/c/shm.h>

<!--(macro shm_set)-->
shm_set(@!var[0]!@, @!var[1]!@, @!val!@)#!
<!--(end)-->

<!--(macro shm_get)-->
shm_get(@!var[0]!@, @!var[1]!@, @!val!@)#!
<!--(end)-->

<!--(macro maybe_set)-->
    <!--(if var in dev)-->
@!shm_set(var=dev[var], val=val)!@ #!
    <!--(end)-->
<!--(end)-->

<!--(for dev in devs)-->
class ShmDevice@!dev['id']!@ : public ShmDevice {
    public:
    std::string getName() const {
        return "@!dev['name']!@";
    }

    uint8_t getDevID() const {
        return @!dev['id']!@;
    }

    void setSetpoint(int16_t setpoint) {
        @!shm_set(var=dev['setpoint'], val='setpoint')!@;
    }

    int16_t getSetpoint() const {
        int16_t result;
        @!shm_get(var=dev['setpoint'], val='result')!@;
        return result;
    }

    int16_t getStatus() const {
        int16_t result;
        @!shm_get(var=dev['status'], val='result')!@;
        return result;
    }

    std::string getFeedbackName(uint8_t id) const {
    <!--(macro maybe_feedbackName)-->
        <!--(if var in dev)-->
    case @!id!@:
        return "@!dev[var]!@";
        <!--(end)-->
    <!--(end)-->
        switch(id) {
            @!maybe_feedbackName(var='feedback_0_name', dev=dev, id=0)!@
            @!maybe_feedbackName(var='feedback_1_name', dev=dev, id=1)!@
            @!maybe_feedbackName(var='feedback_2_name', dev=dev, id=2)!@
            @!maybe_feedbackName(var='feedback_3_name', dev=dev, id=3)!@
            default:
                return "";
        }
    }

    int16_t getFeedback(uint8_t id) const {
    <!--(macro maybe_feedback)-->
        <!--(if var in dev)-->
    case @!id!@:
        @!shm_get(var=dev[var], val=val)!@;
        break;
        <!--(end)-->
    <!--(end)-->
        int16_t result = 0;
        switch(id) {
            @!maybe_feedback(var='feedback_0', dev=dev, val='result', id=0)!@
            @!maybe_feedback(var='feedback_1', dev=dev, val='result', id=1)!@
            @!maybe_feedback(var='feedback_2', dev=dev, val='result', id=2)!@
            @!maybe_feedback(var='feedback_3', dev=dev, val='result', id=3)!@
            default:
                break;
        }
        return result;
    }
};
<!--(end)-->

ShmDeviceList::ShmDeviceList() {
    std::shared_ptr<ShmDevice> dev;
<!--(for dev in sorted(devs, key=lambda x: x['id']))-->
    dev = std::make_shared<ShmDevice@!dev['id']!@>();
    m_devices[@!dev['id']!@] = dev;
    <!--(if dev['type'] == "thruster")-->
    m_thrusters.push_back(dev);
    <!--(elif dev['type'] == "stepper")-->
    m_vectors.push_back(dev);
    <!--(end)-->
<!--(end)-->
}

const std::list<std::shared_ptr<ShmDevice>>& ShmDeviceList::getThrusters() const {
    return m_thrusters;
}

const std::list<std::shared_ptr<ShmDevice>>& ShmDeviceList::getVectors() const {
    return m_vectors;
}

std::shared_ptr<ShmDevice> ShmDeviceList::getDevice(uint8_t id) const {
    auto it = m_devices.find(id);
    if (it == m_devices.end()) return nullptr;
    return it->second;
}

void ShmDeviceList::setSetpoint(uint8_t id, int16_t setpoint) {
    auto dev = getDevice(id);
    if (dev && dev->getStatus() == 1) {
        dev->setSetpoint(setpoint);
    }
}
