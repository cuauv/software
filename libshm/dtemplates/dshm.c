#include <stdbool.h>

#include "dshm.h"
#include "shm.h"
#include "watcher.h"

// NOTE: I *think*, but am very uncertain, that we should use 0 instead of NULL
// or nullptr, because we want this to work in both C and C++. I suppose there
// is some other fancy way to define it as well.
// "Seriously, have any actual machines really used nonzero null pointers, or
//  different representations for pointers to different types?"
// http://c-faq.com/null/machexamp.html

// See the definitions of tvmap and tbmap in libshm/generate_shmd.py.

bool dshm_is_string(dshm_var_id id)
{
    return id >= $!tbmap["string"][0]!$ && id < $!tbmap["string"][1]!$;
}

bool dshm_get_string(dshm_var_id id, char* dest)
{
    switch (id) {
        <!--(for v in tvmap["string"])-->
    case $!v[3]!$: shm_getstr($!v[1]!$, $!v[2]!$, dest); break;
        <!--(end)-->
    default:
        return false;
    }
    
    return true;
}

bool dshm_set_string(dshm_var_id id, char* src)
{
    switch (id) {
        <!--(for v in tvmap["string"])-->
    case $!v[3]!$: shm_setstr($!v[1]!$, $!v[2]!$, src); break;
        <!--(end)-->
    default:
        return false;
    }
    
    return true;
}

<!--(for tn, t in sorted(tvmap.items()))-->
    <!--(if tn != "string")-->
bool dshm_is_$!tn!$(dshm_var_id id)
{
    return id >= $!tbmap[tn][0]!$ && id < $!tbmap[tn][1]!$;
}
bool dshm_get_$!tn!$(dshm_var_id id, $!tn!$* dest)
{
    switch (id) {
        <!--(for v in t)-->
    case $!v[3]!$: shm_get($!v[1]!$, $!v[2]!$, *dest); break;
        <!--(end)-->
    default:
        return false;
    }
    return true;
}
bool dshm_set_$!tn!$(dshm_var_id id, $!tn!$ x)
{
    switch (id) {
        <!--(for v in t)-->
    case $!v[3]!$: shm_set($!v[1]!$, $!v[2]!$, x); break;
        <!--(end)-->
    default:
        return false;
    }
    return true;
}
    <!--(end)-->
<!--(end)-->

bool dshm_watch(dshm_var_id id, watcher_t w)
{
    switch (id) {
<!--(for tn, t in sorted(tvmap.items()))-->
    <!--(for v in t)-->
    case $!v[3]!$: {
        shm_watch($!v[1]!$, w);
        break;
    }
    <!--(end)-->
<!--(end)-->
    default:
        return false;
    }
    return true;
}

bool dshm_unwatch(dshm_var_id id, watcher_t w)
{
    switch (id) {
<!--(for tn, t in sorted(tvmap.items()))-->
    <!--(for v in t)-->
    case $!v[3]!$: {
        shm_watch($!v[1]!$, w);
        break;
    }
    <!--(end)-->
<!--(end)-->
    default:
        return false;
    }
    return true;
}
