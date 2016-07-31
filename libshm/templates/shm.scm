(module cuauv-shm
  (shm-init!
  <!--(for g in groups)-->
   $!srenameg(g['groupname'])!$-zero!
   $!srenameg(g['groupname'])!$-set!
   $!srenameg(g['groupname'])!$-ref
    <!--(for k in g['varnames'])-->
   $!srename(g['groupname'], k)!$-set!
   $!srename(g['groupname'], k)!$-ref
    <!--(end)-->
  <!--(end)-->
  )
  (import scheme chicken)
  (import foreign)

  (foreign-declare "#include <libshm/c/shm.h>")
  (foreign-declare "#include <libshm/c/vars.h>")

  (define shm-init!
    (foreign-lambda void shm_init))

  <!--(for g in groups)-->
  (define $!srenameg(g['groupname'])!$-zero!
    (foreign-lambda* void ()
      "shm_zero_$!g['groupname']!$();"))

#! Originally this directly accessed the shared memory struct. But then I ran into all sorts of bizarre problems.
#! I ended up with some sort of duplicate shared memory that I could write to and read from that would persist, and
#! changes from the actual shared memory would *sometimes* carry over. I gave up and wrote the hacky *_nl functions
#! and *_finish* functions because it seems like functions work fine although the macros wouldn't. I really don't
#! care anymore.
#!     -- jyc57 7/3/15
  (define $!srenameg(g['groupname'])!$-set!
    (foreign-lambda* void ($!" ".join(["(%s %s)" % (g['vars'][k]['scmtype'], k) for k in g['varnames']])!$)
      "shm_lock_$!g['groupname']!$();"
    <!--(for k in g['varnames'])-->
      <!--(if g['vars'][k]['type'] == 'string')-->
      ; String retrieval in bulk not yet supported, just ignore.
      <!--(else)-->
      "shm_set_nl_$!g['groupname']!$_$!k!$($!k!$);"
      <!--(end)-->
    <!--(end)-->
      "shm_finish_$!g['groupname']!$();"
      "shm_unlock_$!g['groupname']!$();"
    ))

  (define ($!srenameg(g['groupname'])!$-ref)
    ((foreign-lambda* void () "shm_lock_$!g['groupname']!$();"))
    (let ((x (list
    <!--(for k in g['varnames'])-->
      <!--(if g['vars'][k]['type'] == 'string')-->
               ; String retrieval in bulk not yet supported.
               (void)
      <!--(else)-->
               ((foreign-lambda* $!g['vars'][k]['scmtype']!$ ()
                 "C_return(shm_get_nl_$!g['groupname']!$_$!k!$());"))
      <!--(end)-->
    <!--(end)-->
            )))
      ((foreign-lambda* void () "shm_unlock_$!g['groupname']!$();")) 
      x))

    <!--(for k in g['varnames'])-->
      <!--(if g['vars'][k]['type'] == 'string')-->
  (define $!srename(g['groupname'], k)!$-set!
    (foreign-lambda void shm_set_$!g['groupname']!$_$!k!$ c-string))

  (define $!srename(g['groupname'], k)!$-ref
    (foreign-lambda* c-string* ()
      "char* res = malloc($!g['vars'][k]['length']!$ + 1);
       if (res == NULL) {
         C_return(NULL);
       }
       res[$!g['vars'][k]['length']!$] = 0;
       shm_get_$!g['groupname']!$_$!k!$(res);
       C_return(res);"))
      <!--(else)-->
  (define $!srename(g['groupname'], k)!$-set!
    (foreign-lambda void shm_set_$!g['groupname']!$_$!k!$ $!g['vars'][k]['scmtype']!$))

  (define $!srename(g['groupname'], k)!$-ref
    (foreign-lambda $!g['vars'][k]['scmtype']!$ shm_get_$!g['groupname']!$_$!k!$))
      <!--(end)-->
    <!--(end)-->
  <!--(end)-->
)

(include "dshm")
