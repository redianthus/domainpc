(* SPDX-License-Identifier: AGPL-3.0-or-later *)

include Domain
module IntMap = Map.Make (Int)

let cores_condition = Condition.create ()
let cores_mutex = Mutex.create ()

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
  | (_, cpus) :: tl ->
      (* set the cpus on which the parent thread can run, the other cpus
         are put in a queue to be used by child domains. *)
      Processor.Affinity.set_ids cpus;
      List.iter (fun (_, cpus) -> Queue.add cpus queue) tl;
      queue

(* fetches a set of cpus, waits for a set to be freed if all sets are used by
   other domains *)
let get_cpus () =
  let rec aux () =
    if Queue.is_empty cpus_per_core then (
      Condition.wait cores_condition cores_mutex;
      aux ())
    else
      let cpus = Queue.pop cpus_per_core in
      Mutex.unlock cores_mutex;
      cpus
  in
  Mutex.lock cores_mutex;
  aux ()

(** When a domain is **properly** stopped (i.e. its `at_exit`) function
    executes, release the core (set of cpus) it uses, and signals to any waiting
    domain that a core has been free (and there are cpus that can be used). *)
let free_cpus cpus =
  Mutex.lock cores_mutex;
  Queue.push cpus cpus_per_core;
  Mutex.unlock cores_mutex;
  Condition.signal cores_condition

let spawn f =
  spawn (fun () ->
      let cpus = get_cpus () in
      Processor.Affinity.set_ids cpus;
      Domain.at_exit (fun () -> free_cpus cpus);
      f ())
