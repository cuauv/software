open Ctypes
open Foreign
open Lwt

let libshm_so = Filename.concat (Sys.getenv "CUAUV_SOFTWARE" ^ Filename.dir_sep ^ "link-stage") "libshm.so"
let _ = Dl.dlopen ~filename:libshm_so ~flags:[Dl.RTLD_LAZY; Dl.RTLD_GLOBAL]

module Watcher = struct

  type t = unit ptr
  let t : t typ = ptr void

  let create =
    foreign "create_watcher" (void @-> returning t)

  let destroy =
    foreign "destroy_watcher" (t @-> returning int)

  let wait =
    #! wait_watcher will block until an update occurs.
    #! We release the runtime lock so OCaml code can run concurrently.
    foreign ~release_runtime_lock:true "wait_watcher" (t @-> bool @-> returning int)

  exception Wait_failure of float

  let waiter ?(new_update=false) watcher =
    let waiter, wakener = Lwt.wait () in
    let _ = Thread.create (fun () ->
        let res = wait watcher new_update in
        let now = Sys.time () in
        if res <> 0 then Lwt.wakeup_exn wakener (Wait_failure now)
        else Lwt.wakeup_later wakener now 
      ) ()
    in waiter

end

let init =
  foreign "shm_init" (void @-> returning void)

let () = init ()

<!--(for g in groups)-->
module $!renameg(g)!$ = struct

  type t = {
  <!--(for k in g['varnames'])-->
    $!k[:1].lower() + k[1:]!$ : $!g['vars'][k]['ocamltype']!$;
  <!--(end)-->
  }

  type cstruct
  let cstruct : cstruct structure typ = structure "$!g['groupname']!$"
  <!--(for k in g['varnames'])-->
  let field_$!k!$ = field cstruct "$!k!$" $!g['vars'][k]['ocamlctype']!$
  <!--(end)-->
  let () = seal cstruct

  let watch =
    foreign "shm_watch_$!g['groupname']!$" (Watcher.t @-> returning void)

  let set' =
    foreign "shm_set_$!g['groupname']!$" (cstruct @-> returning void)

  let set g =
    let s = make cstruct in
  <!--(for k in g['varnames'])-->
    let () = setf s field_$!k!$ g.$!k[:1].lower() + k[1:]!$ in
  <!--(end)-->
    set' s

  let get' =
    foreign "shm_get_$!g['groupname']!$" (void @-> returning cstruct)

  let get ()  =
    let s = get' () in
    { 
  <!--(for k in g['varnames'])-->
      $!k[:1].lower() + k[1:]!$ = getf s field_$!k!$;
  <!--(end)-->
    }

  let lock =
    foreign "shm_lock_$!g['groupname']!$" (void @-> returning void)

  let unlock =
    foreign "shm_unlock_$!g['groupname']!$" (void @-> returning void)

  <!--(for k in g['varnames'])-->
  let set_$!k!$ =
    foreign "shm_set_$!g['groupname']!$_$!k!$" ($!g['vars'][k]['ocamlctype']!$ @-> returning void)

    <!--(if g['vars'][k]['type'] == 'string')-->
  let get_$!k!$' =
    foreign "shm_get_$!g['groupname']!$_$!k!$" (ocaml_string @-> returning void)

  let get_$!k!$ () =
    let s = String.make $!g['vars'][k]['length']!$ ' ' in
    let () = get_$!k!$' (ocaml_string_start s) in
    let n = String.index s '\000' in
    String.sub s 0 n
    
    <!--(else)-->
  let get_$!k!$ =
    foreign "shm_get_$!g['groupname']!$_$!k!$" (void @-> returning $!g['vars'][k]['ocamlctype']!$)
    <!--(end)-->

  <!--(end)-->
end
<!--(end)-->
