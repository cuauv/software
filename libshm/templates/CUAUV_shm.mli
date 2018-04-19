open Ctypes
open Foreign
open Lwt

module Watcher : sig

  type t

  val create : unit -> t
  val destroy : t -> int
  val wait : t -> bool -> int

  exception Wait_failure of float
  val waiter : ?new_update:bool -> t -> float Lwt.t

end

<!--(for g in groups)-->
module $!renameg(g)!$ : sig

  type t = {
  <!--(for k in g['varnames'])-->
    #! Lowercase the first character (most already are) because only records
    #! and modules can have uppercase first characters in OCaml.
    $!k[:1].lower() + k[1:]!$ : $!g['vars'][k]['ocamltype']!$;
  <!--(end)-->
  }

  val set : t -> unit
  val get : unit -> t

  <!--(for k in g['varnames'])-->
  val get_$!k!$ : unit -> $!g['vars'][k]['ocamltype']!$
  val set_$!k!$ : $!g['vars'][k]['ocamltype']!$ -> unit
  <!--(end)-->

  val watch : Watcher.t -> unit

end

<!--(end)-->
