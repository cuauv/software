#pragma once

#include <unordered_set>

#include "Variable.h"

namespace cuauv
{
namespace serial
{

/**
 * @brief Interface class for clients of libserial to receive callbacks
 */
class DeviceCallbacks {
    public:
        // Required for subclass destruction to behave correctly
        virtual ~DeviceCallbacks() {}

        /**
         * Called on the completion of any read operation
         *
         * Note that calls to this callback may not necessarily correspond to
         * reads which were requested. libserial may choose to collapse multiple
         * read requests into one, or may merge them with periodic polls.
         * Implementations of this function should be able to handle any
         * combination of variables being passed in.
         *
         * @param vars the variables which were read, with their values
         */
        virtual void onRead(const std::unordered_set<BoundVariable>& vars) = 0;

        /**
         * Called on the completion of any write operation
         *
         * Note that calls to this callback may not necessarily correspond to
         * writes which were requested. libserial may choose to collapse multiple
         * write requests into one.
         * Implementations of this function should be able to handle any
         * combination of variables being passed in.
         *
         * @param vars the variables which were written, with their values
         */
        virtual void onWrite(const std::unordered_set<BoundVariable>& vars) = 0;

        /**
         * Called to request the current value of soft kill
         *
         * @return true if the vehicle is currently soft killed
         */
        virtual bool getSoftKill() = 0;

        /**
         * Called when the device updates its hard kill status
         * 
         * @param canHardKill true if the device is capable of reporting hard kill
         * @param hardKill true if the device is hard killed. Only valid if canHardKill is true
         */
        virtual void postHardKill(bool canHardKill, bool hardKill) = 0;

        /**
         * Called when the device transitions to the disconnected state
         *
         * No further calls will be made on this object after this callback occurs
         */
        virtual void onDisconnect() = 0;
};

} // end namespace serial
} // end namespace cuauv
