/*!
 * \file device.h
 * \author Peter Tseng
 */

#ifndef _USD_DEVICES_H_
#define _USD_DEVICES_H_

#include <list>
#include <map>

#include "../device.h"

void initDevices(std::list<Device*>& devices, std::map<int, Device*>& autoDetectDevices);

#endif
