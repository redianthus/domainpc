(* SPDX-License-Identifier: AGPL-3.0-or-later *)

module IntMap = Map.Make (Int)

let cores_condition = Condition.create ()
let cores_mutex = Mutex.create ()
let crash = Atomic.make true
let wait_on_unavailable () = Atomic.set crash false

(** List of sets of cpus, where each distinct set of CPUs is linked to a unique
    core. *)
let cpus_per_core = Queue.create ()

(** Whether or not the queue of cpus_per_core was initialized. *)
let initialized = ref false

(** The mode in which the library is used. It is dermined from the first call to
    `spawn` or `spawn_n` *)
type mode = Unintialized | Isolated | NotIsolated

let check_mode =
  let mode = ref Unintialized in
  fun isolated ->
    match !mode with
    | Unintialized when isolated -> mode := Isolated
    | Unintialized -> mode := NotIsolated
    | Isolated when not isolated ->
        failwith
          "trying to run an unisolated domain when you previously ran an \
           isolated domain"
    | NotIsolated when isolated ->
        failwith
          "trying to run an isolated domain when you previously ran an \
           unisolated domain"
    | Isolated | NotIsolated -> ()

(** initalizes the queue of cores represented by their lists of cpus. *)
let init_cpus () =
  initialized := true;
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
  | l -> List.iter (fun (_, cpus) -> Queue.add cpus cpus_per_core) l

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

(** Ensure that the current domain runs a specific core. *)
let isolate_current () =
  if not !initialized then init_cpus ();
  check_mode true;
  let cpus = get_cpus () in
  Processor.Affinity.set_ids cpus

let spawn ?(isolated = true) f =
  if not !initialized then init_cpus ();
  check_mode isolated;
  if isolated then
    Domain.spawn (fun () ->
        let cpus = get_cpus () in
        spawn_aux f cpus)
  else Domain.spawn f

let spawn_n ?(isolated = true) ?n f =
  if not !initialized then init_cpus ();
  check_mode isolated;
  match n with
  | Some n ->
      if n <= 0 then
        failwith (Format.sprintf "spawn_n: expected n > 0, got n = %d" n)
      else if isolated then
        Mutex.protect cores_mutex (fun () ->
            let nb_free_cores = Queue.length cpus_per_core in
            let cpul =
              if n <= nb_free_cores then
                List.init n (fun _ -> Queue.pop cpus_per_core)
              else
                failwith
                  (Format.sprintf
                     "spawn_n: requested %d cores, but only %n are available" n
                     nb_free_cores)
            in
            Array.of_list
              (List.map
                 (fun cpus -> Domain.spawn (fun () -> spawn_aux f cpus))
                 cpul))
      else Array.of_list (List.init n (fun _ -> Domain.spawn f))
  | None ->
      if isolated then
        Mutex.protect cores_mutex (fun () ->
            let nb_free_cores = Queue.length cpus_per_core in
            let cpul =
              if nb_free_cores > 0 then
                List.init nb_free_cores (fun _ -> Queue.pop cpus_per_core)
              else failwith "spawn_n: no free cores"
            in
            Array.of_list
              (List.map
                 (fun cpus -> Domain.spawn (fun () -> spawn_aux f cpus))
                 cpul))
      else
        Array.of_list
          (List.init Processor.Query.core_count (fun _ -> Domain.spawn f))
