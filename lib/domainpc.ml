(* SPDX-License-Identifier: AGPL-3.0-or-later *)

include Domain
module IntMap = Map.Make (Int)

let cores_condition = Condition.create ()
let cores_mutex = Mutex.create ()
let crash = Atomic.make true
let wait_on_unavailable () = Atomic.set crash false

(** List of sets of cpus, where each distinct set of CPUs is linked to a unique
    core. *)
let cpus_per_core =
  let queue = Queue.create () in
  let m =
    List.fold_left
      (fun acc Processor.Cpu.{ id; core; _ } ->
        IntMap.update core
          (function None -> Some [ id ] | Some l -> Some (id :: l))
          acc)
      IntMap.empty Processor.Topology.t
  in
  match IntMap.to_list m with
  | [] -> failwith "unexpected: no CPUS/Cores found"
  | l ->
      List.iter (fun (_, cpus) -> Queue.add cpus queue) l;
      queue

(** fetches a set of cpus, if all sets of cpus are used by other domains, either
    waits for a set to be freed or crashes if [set_crash_on_unavailable ()] was
    called. *)
let get_cpus () =
  let rec aux () =
    if Queue.is_empty cpus_per_core then
      if Atomic.get crash then failwith "No free cores"
      else (
        Condition.wait cores_condition cores_mutex;
        aux ())
    else
      let cpus = Queue.pop cpus_per_core in
      cpus
  in
  Mutex.protect cores_mutex aux

(** When a domain is **properly** stopped (i.e. its `at_exit`) function
    executes, release the core (set of cpus) it uses, and signals to any waiting
    domain that a core has been free (and there are cpus that can be used). *)
let free_cpus cpus =
  Mutex.protect cores_mutex (fun () -> Queue.push cpus cpus_per_core);
  Condition.signal cores_condition

let spawn_aux f cpus =
  Processor.Affinity.set_ids cpus;
  Domain.at_exit (fun () -> free_cpus cpus);
  f ()

let spawn f =
  spawn (fun () ->
      let cpus = get_cpus () in
      spawn_aux f cpus)

let spawn_n ?n f =
  let cpul =
    Mutex.protect cores_mutex (fun () ->
        let ncores = Queue.length cpus_per_core in
        match n with
        | None ->
            if ncores > 0 then
              List.init ncores (fun _ -> Queue.pop cpus_per_core)
            else failwith "spawn_n: no free cores"
        | Some n ->
            if n <= ncores then List.init n (fun _ -> Queue.pop cpus_per_core)
            else
              failwith
                (Format.sprintf
                   "spawn_n: requested %d cores, but only %n are available" n
                   ncores))
  in
  Array.of_list
    (List.map (fun cpus -> Domain.spawn (fun () -> spawn_aux f cpus)) cpul)
