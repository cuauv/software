#include "ShmMessageBuilder.h"

#include <libshm/c/shm.h>

ShmMessageBuilder::ShmMessageBuilder(uint8_t devid) : MessageBuilder(devid) {}

void ShmMessageBuilder::setEnable(bool enable) {
    shm_set(switches, soft_kill, !enable);
}

bool ShmMessageBuilder::getEnable() const {
    int kill;
    shm_get(switches, soft_kill, kill);
    return !kill;
}
