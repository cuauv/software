#include "../ShmRegistry.h"
#include <iostream>
#include <libshm/c/shm.h>
#include "../lib/Log.h"

#define DEVICE_DEAD 0
#define DEVICE_LIVE 1
#define DEVICE_BADHW 2

<!--(macro shm_set)-->
shm_set(@!var[0]!@, @!var[1]!@, @!val!@); #!
<!--(end)-->

<!--(macro maybe_set)-->
    <!--(if var in dev)-->
@!shm_set(var=dev[var], val=val)!@ #!
    <!--(end)-->
<!--(end)-->

<!--(for dev in devs)-->
class Device@!dev['id']!@ : public Device {
    <!--(for var in dev['other_vars'])-->
    int $!var['group'] + '_' + var['var']!$;
    <!--(end)-->
    public:
        Device@!dev['id']!@() : Device(@!dev['id']!@, @!dev['group']!@, @!dev['offset']!@) {
            @!shm_set(var=dev['status'], val='DEVICE_LIVE')!@
        }

        virtual void onDeviceAnnounced() {
            if (@!dev['hwid']!@ != getHWType()) {
                Log::log("Hardware type mismatch for devid @!dev['id']!@");
                @!shm_set(var=dev['status'], val='DEVICE_BADHW')!@
                clearCommands();
                pushCommand(Command::setGroup(@!dev['id']!@, 1));
                <!--(for var in dev['other_vars'])-->
                {
                  int val;
                  shm_get($!var['group']!$, $!var['var']!$, val);
                  $!var['group'] + '_' + var['var']!$ = val;
                  pushCommand(Command::setParam(@!dev['id']!@, $!var['payload']!$, static_cast<int16_t>(val)));
                }
                <!--(end)-->
            }
        }

        virtual void onStatusChanged() {
            @!maybe_set(var='flags', dev=dev, val='getFlags()')!@
            @!maybe_set(var='feedback_0', dev=dev, val='getFeedback(0)')!@
            @!maybe_set(var='feedback_1', dev=dev, val='getFeedback(1)')!@
            @!maybe_set(var='feedback_2', dev=dev, val='getFeedback(2)')!@
            @!maybe_set(var='feedback_3', dev=dev, val='getFeedback(3)')!@
            <!--(for var in dev['other_vars'])-->
            {
              int val;
              shm_get($!var['group']!$, $!var['var']!$, val);
              if (val != $!var['group'] + '_' + var['var']!$) {
                $!var['group'] + '_' + var['var']!$ = val;
                pushCommand(Command::setParam(@!dev['id']!@, $!var['payload']!$, static_cast<int16_t>(val)));
              }
            }
            <!--(end)-->
        }

        virtual void onDeviceCulled() {
            @!shm_set(var=dev['status'], val='DEVICE_DEAD')!@
        }
};
<!--(end)-->

<!--(for group in groups)-->
class Group@!group['id']!@ : public Group {
    public:
        Group@!group['id']!@() : Group(@!group['id']!@) {}

    virtual void setSetpoint(uint8_t offset, int16_t setpoint) {
        switch(offset) {
            <!--(for off, var in group['varnames'])-->
            case @!off!@:
                shm_set(@!group['shmgroup']!@, @!var!@, setpoint);
                break;
            <!--(end)-->
            default:
                break;
        }
    }

    virtual int16_t getSetpoint(uint8_t offset) const {
        int16_t result;
        switch(offset) {
            <!--(for off, var in group['varnames'])-->
            case @!off!@:
                shm_get(@!group['shmgroup']!@, @!var!@, result);
                break;
            <!--(end)-->
            default:
                result = 0;
                break;
        }
        return result;
    }

    virtual MessageData buildMessageData() const {
        MessageData data;
        struct @!group['shmgroup']!@ @!group['shmgroup']!@;
        shm_getg(@!group['shmgroup']!@, @!group['shmgroup']!@);

        $!setvar("cnt", "0")!$
        <!--(for off, var in sorted(group['varnames'], key=lambda v: v[0]))-->
        data.int16Push(@!group['shmgroup']!@.@!var!@);
        $!setvar("cnt", "cnt+1")!$
        <!--(end)-->
        <!--(for i in range(cnt, 4))-->
        data.int16Push(0);
        <!--(end)-->
        return data;
    }
};
<!--(end)-->

ShmRegistry::ShmRegistry() {
    // Reset all device status variables to DEVICE_DEAD
<!--(for dev in devs)-->
    @!shm_set(var=dev['status'], val='DEVICE_DEAD')!@
<!--(end)-->
}

std::shared_ptr<Device> ShmRegistry::createDevice(uint8_t devid) {
    Log::log("Looking up devid " + std::to_string(devid));
    std::shared_ptr<Device> dev;
    switch (devid) {
<!--(for dev in devs)-->
        case @!dev['id']!@:
            Log::log("Matched devid @!dev['id']!@");
            if (!getGroup(@!dev['group']!@)) {
                Log::log("Creating group @!dev['group']!@");
                addGroup(std::make_shared<Group@!dev['group']!@>());
            }
            dev = std::make_shared<Device@!dev['id']!@>();
            break;
<!--(end)-->
        default:
            Log::log("Unknown devid " + std::to_string(devid));
            dev = std::make_shared<Device>(devid, 1, 0);
            break;
    }
    addDevice(dev);
    return dev;
}
