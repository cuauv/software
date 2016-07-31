#include <libshm/c/shm.h>

#include "@!dev.name!@.h"
#include "../filters.h"
#include "../pollblock.h"

@!dev.name.capitalize()!@::@!dev.name.capitalize()!@(int id, const char* name, const char* path, bool debugSend, bool debugReceive)
    : Device(id, name, path, @!dev.baud_rate!@, debugSend, debugReceive) {

<!--(for addr, size, interval in blocks)-->
    <!--(if interval > 0)-->
    addPollBlock(new PollBlock(@!interval!@, @!addr!@, @!size!@));
    <!--(end)-->
<!--(end)-->
<!--(for group in dev.get_groups())-->
    shm_getg(@!group!@, @!group!@);
<!--(end)-->
}

void @!dev.name.capitalize()!@::handleReadResponse(uint16_t address, uint8_t count, uint8_t* buffer) {
    int i = 0;
<!--(for group in dev.get_groups())-->
    bool @!group!@_changed = false;
<!--(end)-->
    union {
        double vDouble;
        int vInt;
        bool vBool;
    };
    (void)vDouble;
    (void)vInt;
    (void)vBool;

    while (i < count) {
        switch(address + i) {
<!--(for var in dev.vars)-->
        case @!var.address!@:
    <!--(if var.ctype == 'string')-->
            if (memcmp(buffer, @!var.group!@.@!var.name!@, @!var.size * 2!@) != 0) {
                // If first change on this group, resync
                if (!@!var.group!@_changed) {
                    shm_getg(@!var.group!@, @!var.group!@);
                }
                @!var.group!@_changed = true;
                memcpy(@!var.group!@.@!var.name!@, buffer, @!var.size * 2!@);
            }
    <!--(else)-->
            v@!var.ctype.capitalize()!@ = @!var.filter!@(@!var.vartype!@FromBuffer(buffer));
            if (v@!var.ctype.capitalize()!@ != @!var.group!@.@!var.name!@) {
                // If first change on this group, resync
                if (!@!var.group!@_changed) {
                    shm_getg(@!var.group!@, @!var.group!@);
                }
                @!var.group!@_changed = true;
                @!var.group!@.@!var.name!@ = v@!var.ctype.capitalize()!@;
            }
    <!--(end)-->
            i += @!var.size!@;
            buffer += @!var.size * 2!@;
            break;
<!--(end)-->
        default:
            ++i;
            buffer += 2;
        }
    }

<!--(for group in dev.get_groups())-->
    <!--(if not dev.is_writeonly_group(group))-->
    if (@!group!@_changed) {
        shm_setg(@!group!@, @!group!@);
    }
    <!--(end)-->
<!--(end)-->
}

void @!dev.name.capitalize()!@::watch() {
    watcher_t w = create_watcher();
<!--(for group in dev.get_groups())-->
    shm_watch(@!group!@, w);
<!--(end)-->

    // Initial write:
    // TODO: resync before this?
<!--(for var in dev.vars)-->
    <!--(if not var.readonly)-->
        <!--(if var.iwrite)-->
            <!--(if var.ctype == 'string')-->
    makeWriteRequest(@!var.size!@, @!var.address!@, reinterpret_cast<uint8_t*>(@!var.group!@.@!var.name!@));
            <!--(else)-->
    send@!var.vartype.capitalize()!@(@!var.address!@, @!var.group!@.@!var.name!@);
            <!--(end)-->
        <!--(end)-->
    <!--(end)-->
<!--(end)-->

    while (isActive()) {
        wait_watcher(w, false); // Don't block if there has been an update
                                // we want to make sure it gets through
<!--(for group in dev.get_groups())-->
        shm_getg(@!group!@, @!group!@_new);
<!--(end)-->

<!--(for var in dev.vars)-->
    <!--(if not var.readonly)-->
        <!--(if var.ctype == 'string')-->
        if (strncmp(@!var.group!@_new.@!var.name!@, @!var.group!@.@!var.name!@, @!var.size * 2!@) != 0) {
            makeWriteRequest(@!var.size!@, @!var.address!@, reinterpret_cast<uint8_t*>(@!var.group!@.@!var.name!@));
            strncpy(@!var.group!@.@!var.name!@, @!var.group!@_new.@!var.name!@, @!var.size * 2!@);
        }
        <!--(else)-->
        if (@!var.group!@_new.@!var.name!@ != @!var.group!@.@!var.name!@) {
            send@!var.vartype.capitalize()!@(@!var.address!@, @!var.filter!@(@!var.group!@_new.@!var.name!@));
            @!var.group!@.@!var.name!@ = @!var.group!@_new.@!var.name!@;
        }
        <!--(end)-->
    <!--(end)-->
<!--(end)-->
    }

    destroy_watcher(w);
}
