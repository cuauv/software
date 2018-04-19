#include "shm.h"
#include "vars.h"

<!--(for g in groups)-->

void shm_watch_$!g['groupname']!$(watcher_t watcher) {
    shm_watch($!g['groupname']!$, watcher);
}

void shm_unwatch_$!g['groupname']!$(watcher_t watcher) {
    shm_unwatch($!g['groupname']!$, watcher);
}

    <!--(for k in g['varnames'])-->
        <!--(if g['vars'][k]['type'] == 'string')-->
void shm_get_$!g['groupname']!$_$!k!$(char* dst) {
    shm_getstr($!g['groupname']!$, $!k!$, dst);
}
        <!--(else)-->
$!g['vars'][k]['ctype']!$ shm_get_$!g['groupname']!$_$!k!$() {
    $!g['vars'][k]['ctype']!$ v;
    shm_get($!g['groupname']!$, $!k!$, v);
    return v;
}
$!g['vars'][k]['ctype']!$ shm_get_nl_$!g['groupname']!$_$!k!$() {
    $!g['vars'][k]['ctype']!$ v;
    shm_get_nl($!g['groupname']!$, $!k!$, v);
    return v;
}
        <!--(end)-->

void shm_set_$!g['groupname']!$_$!k!$($!g['vars'][k]['ctype']!$ val) {
        <!--(if g['vars'][k]['type'] == 'string')-->
    shm_setstr($!g['groupname']!$, $!k!$, val);
        <!--(else)-->
    shm_set($!g['groupname']!$, $!k!$, val);
        <!--(end)-->
}
        <!--(if g['vars'][k]['type'] != 'string')-->
void shm_set_nl_$!g['groupname']!$_$!k!$($!g['vars'][k]['ctype']!$ val) {
    shm_set_nl($!g['groupname']!$, $!k!$, val);
}
        <!--(end)-->
    <!--(end)-->

struct $!g['groupname']!$ shm_get_$!g['groupname']!$() {
    struct $!g['groupname']!$ ret;
    shm_getg($!g['groupname']!$, ret);
    return ret;
}

void shm_set_$!g['groupname']!$(struct $!g['groupname']!$ val) {
    shm_setg($!g['groupname']!$, val);
}

void shm_lock_$!g['groupname']!$() {
    shm_lock($!g['groupname']!$);
}
void shm_unlock_$!g['groupname']!$() {
    shm_unlock($!g['groupname']!$);
}
void shm_finish_$!g['groupname']!$() {
    shm->$!g['groupname']!$.m.f = 1; 
    shm->$!g['groupname']!$.m.stream = 1;
    shm->$!g['groupname']!$.m.last_client = 0;
    shm_notify(shm->$!g['groupname']!$.m.w);
}
void shm_zero_$!g['groupname']!$() {
    shm_zerog($!g['groupname']!$);
}
<!--(end)-->
