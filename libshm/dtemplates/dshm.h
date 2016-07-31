#ifndef AUV_DSHM_H
#define AUV_DSHM_H

#include <stdbool.h>

#include "watcher.h"

typedef int dshm_var_id;

bool dshm_is_string(dshm_var_id id);
bool dshm_get_string(dshm_var_id id, char* dest);
bool dshm_set_string(dshm_var_id id, char* src);
    
<!--(for tn, t in sorted(tvmap.items()))-->
    <!--(if tn != "string")-->
bool dshm_is_$!tn!$(dshm_var_id id);
bool dshm_get_$!tn!$(dshm_var_id id, $!tn!$* dest);;
bool dshm_set_$!tn!$(dshm_var_id id, $!tn!$ x);
    <!--(end)-->
<!--(end)-->

// Note: watchers have group resolution, so while you provide a dshm_var_id,
// you're actually having the watcher watch the group that that variable
// belongs to.

bool dshm_watch(dshm_var_id id, watcher_t w);
bool dshm_unwatch(dshm_var_id id, watcher_t w);

<!--(for tn, t in sorted(tvmap.items()))-->
    <!--(for v in t)-->
#define DSHM_$!v[0]!$ $!v[3]!$
    <!--(end)-->
<!--(end)-->

#endif // AUV_DSHM_H
