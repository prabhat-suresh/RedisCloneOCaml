open Base

type value_entry = { value : string; expires_at : float option }

let store : (string, value_entry) Hashtbl.t =
  Hashtbl.create (module String) ~size:1024

let set key value ~ttl =
  let expires_at =
    match ttl with Some t -> Some (Unix.gettimeofday () +. t) | None -> None
  in
  Hashtbl.set store ~key ~data:{ value; expires_at }

let get key =
  match Hashtbl.find store key with
  | None -> None
  | Some entry -> (
      match entry.expires_at with
      | Some expires when Float.(Unix.gettimeofday () > expires) ->
          Hashtbl.remove store key;
          None
      | _ -> Some entry.value)
