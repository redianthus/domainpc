(* SPDX-License-Identifier: AGPL-3.0-or-later *)

val wait_on_unavailable : unit -> unit
(** [wait_on_unavailable ()] when all cores are used and more are requested,
    wait for cores to be freed instead of crashing. *)

val isolate_current : unit -> unit
(** [isolate_current ()] ensures that the current domain (usually the main one)
    is restricted to run on a core where other isolated domains can't run. *)

val spawn : ?isolated:bool -> (unit -> 'a) -> 'a Domain.t
(** [spawn ?isolated f] if [isolated] is [true] spawns a domain that runs on a
    given core, otherwise behaves as [Domain.spawn].

    [isolated] is [true] by default.

    @raise Failure
      if [spawn] is called multiple times (or both [spawn] and [spawn_n] are
      called) with different values of [isolated]. *)

val spawn_n : ?isolated:bool -> ?n:int -> (unit -> 'a) -> 'a Domain.t array
(** [spawn_n ?isolated ?n f] if [isolated] is [true], [n] is provided and is
    less than or equal to the number of free physical cores, spawns [n] new
    domains. If [n] is not provided and there is at least one free physical
    core, spawns as many domains as there are physical cores.

    If [isolated] is [false], the spawned domains are not guaranteed to run on
    separate cores.

    [isolated] is [true] by default.

    @raise Failure if there aren't enough free physical cores.
    @raise Failure
      if [spawn_n] is called multiple times (or both [spawn] and [spawn_n] are
      called) with different values of [isolated]. *)
