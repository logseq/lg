type detected = {
  generation : int;
  paths : string list;
  content_hash : string;
}

type completed = {
  generation : int;
  paths : string list;
  content_hash : string;
  elapsed_ms : int;
}

type rejected = {
  generation : int;
  paths : string list;
  error : Lg.Compiler.compile_error;
  elapsed_ms : int;
}

type event =
  | Change_detected of detected
  | Reload_committed of completed
  | Reload_rejected of rejected

type snapshot = (string * string) list

type pending = {
  generation : int;
  snapshot : snapshot;
  paths : string list;
  detected_at : float;
}

type t = {
  reload :
    generation:int ->
    string list ->
    (unit, Lg.Compiler.compile_error) result;
  paths : string list;
  settle_seconds : float;
  now : unit -> float;
  mutable baseline : snapshot;
  mutable pending : pending option;
  mutable requested_generation : int;
}

let infrastructure_error message =
  {
    Lg.Compiler.code = "LG9000";
    phase = `Infrastructure;
    message;
    location = None;
  }

let read_hash path =
  match In_channel.with_open_bin path In_channel.input_all with
  | source -> Ok (Digest.string source |> Digest.to_hex)
  | exception Sys_error message -> Error (infrastructure_error message)

let snapshot paths =
  let rec collect values = function
    | [] -> Ok (List.rev values)
    | path :: rest -> (
        match read_hash path with
        | Ok hash -> collect ((path, hash) :: values) rest
        | Error _ as error -> error)
  in
  collect [] paths

let aggregate_hash snapshot =
  snapshot
  |> List.map (fun (path, hash) -> path ^ "\000" ^ hash)
  |> String.concat "\000"
  |> Digest.string |> Digest.to_hex

let changed_paths baseline candidate =
  candidate
  |> List.filter_map (fun (path, hash) ->
         match List.assoc_opt path baseline with
         | Some previous when String.equal previous hash -> None
         | Some _ | None -> Some path)

let elapsed_ms started finished =
  Float.to_int ((finished -. started) *. 1000.)

let create_with_reload ~paths ~settle_seconds ~now ~reload =
  if settle_seconds < 0. then
    Error (infrastructure_error "watch settle time cannot be negative")
  else
    match snapshot paths with
    | Error _ as error -> error
    | Ok baseline ->
        Ok
          {
            reload;
            paths;
            settle_seconds;
            now;
            baseline;
            pending = None;
            requested_generation = 0;
          }

let create ~session ~paths ~settle_seconds ~now =
  create_with_reload ~paths ~settle_seconds ~now
    ~reload:(fun ~generation:_ paths ->
      Session.eval_files session paths |> Result.map (fun _ -> ()))

let detect_change watcher candidate =
  let paths = changed_paths watcher.baseline candidate in
  if paths = [] then (
    watcher.pending <- None;
    None)
  else (
    watcher.requested_generation <- watcher.requested_generation + 1;
    let generation = watcher.requested_generation in
    let pending =
      {
        generation;
        snapshot = candidate;
        paths;
        detected_at = watcher.now ();
      }
    in
    watcher.pending <- Some pending;
    Some
      (Change_detected
         {
           generation;
           paths;
           content_hash = aggregate_hash candidate;
         }))

let settle watcher pending =
  let started = watcher.now () in
  if started -. pending.detected_at < watcher.settle_seconds then None
  else
    let result = watcher.reload ~generation:pending.generation pending.paths in
    let finished = watcher.now () in
    watcher.baseline <- pending.snapshot;
    watcher.pending <- None;
    let content_hash = aggregate_hash pending.snapshot in
    let elapsed_ms = elapsed_ms started finished in
    match result with
    | Ok _ ->
        Some
          (Reload_committed
             {
               generation = pending.generation;
               paths = pending.paths;
               content_hash;
               elapsed_ms;
             })
    | Error error ->
        Some
          (Reload_rejected
             {
               generation = pending.generation;
               paths = pending.paths;
               error;
               elapsed_ms;
             })

let poll watcher =
  match snapshot watcher.paths with
  | Error error ->
      watcher.requested_generation <- watcher.requested_generation + 1;
      Some
        (Reload_rejected
           {
             generation = watcher.requested_generation;
             paths = watcher.paths;
             error;
             elapsed_ms = 0;
           })
  | Ok candidate -> (
      match watcher.pending with
      | Some pending when pending.snapshot = candidate -> settle watcher pending
      | Some _ | None -> detect_change watcher candidate)
