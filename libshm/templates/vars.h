#ifndef SHM_VARS_H
#define SHM_VARS_H

#include "shm.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * shm_watch_* returns 0 on success.
 * All other functions continue in spite of failures.
 */

<!--(for g in groups)-->

void shm_watch_$!g['groupname']!$(watcher_t watcher);
void shm_unwatch_$!g['groupname']!$(watcher_t watcher);

    <!--(for k in g['varnames'])-->
        <!--(if g['vars'][k]['type'] == 'string')-->
void shm_get_$!g['groupname']!$_$!k!$(char* dst);
        <!--(else)-->
$!g['vars'][k]['ctype']!$ shm_get_$!g['groupname']!$_$!k!$();
$!g['vars'][k]['ctype']!$ shm_get_nl_$!g['groupname']!$_$!k!$();
        <!--(end)-->
void shm_set_$!g['groupname']!$_$!k!$($!g['vars'][k]['ctype']!$ val);
void shm_set_nl_$!g['groupname']!$_$!k!$($!g['vars'][k]['ctype']!$ val);
    <!--(end)-->

struct $!g['groupname']!$ shm_get_$!g['groupname']!$();
void shm_set_$!g['groupname']!$(struct $!g['groupname']!$ val);
void shm_lock_$!g['groupname']!$();
void shm_unlock_$!g['groupname']!$();
void shm_finish_$!g['groupname']!$();
void shm_zero_$!g['groupname']!$();
<!--(end)-->

#ifdef __cplusplus
}
#endif

#endif  // SHM_VARS_H
