open Base

type t =
  | SimpleString of string
  | Err of string
  | Integer of int
  | BulkString of string option
  | Arr of t list
[@@deriving compare, equal, sexp_of]

let rec to_string =
  let open Printf in
  function
  | SimpleString s -> sprintf "+%s\r\n" s
  | Err s -> sprintf "-%s\r\n" s
  | Integer i -> sprintf ":%d\r\n" i
  | BulkString None -> sprintf "$-1\r\n"
  | BulkString (Some s) -> sprintf "$%d\r\n%s\r\n" (String.length s) s
  | Arr l ->
      sprintf "*%d\r\n%s" (List.length l)
        (List.map l ~f:to_string |> String.concat)

let parse_until_crlf buffer =
  let s = Eio.Buf_read.take_while (fun c -> Char.(c <> '\r')) buffer in
  let crlf = Eio.Buf_read.take 2 buffer in
  assert (String.(crlf = "\r\n"));
  s

let rec parse_command buffer =
  match Eio.Buf_read.any_char buffer with
  | '+' -> SimpleString (parse_until_crlf buffer)
  | '-' -> Err (parse_until_crlf buffer)
  | ':' -> Integer (Int.of_string @@ parse_until_crlf buffer)
  | '$' ->
      let len = Int.of_string @@ parse_until_crlf buffer in
      if len = -1 then BulkString None
      else
        let s = Eio.Buf_read.take len buffer in
        let crlf = Eio.Buf_read.take 2 buffer in
        assert (String.(crlf = "\r\n"));
        BulkString (Some s)
  | '*' ->
      let count = Int.of_string @@ parse_until_crlf buffer in
      Arr (List.rev @@ List.init count ~f:(fun _ -> parse_command buffer))
  | c -> failwith (Printf.sprintf "Unknown RESP type prefix: %C" c)

let parse_ttl = function
  | BulkString (Some "PX") :: BulkString (Some ms) :: _ ->
      Some (Float.of_int (Int.of_string ms) /. 1000.0)
  | BulkString (Some "EX") :: BulkString (Some s) :: _ ->
      Some (Float.of_int (Int.of_string s))
  | _ -> None

let reply = function
  | Arr [ BulkString (Some "PING") ] -> SimpleString "PONG"
  | Arr [ BulkString (Some "ECHO"); (BulkString _ as msg) ] -> msg
  | Arr
      (BulkString (Some "SET")
      :: BulkString (Some key)
      :: BulkString (Some value)
      :: rest) ->
      let ttl = parse_ttl rest in
      Store.set key value ~ttl;
      SimpleString "OK"
  | Arr [ BulkString (Some "GET"); BulkString (Some key) ] -> (
      match Store.get key with
      | Some v -> BulkString (Some v)
      | None -> BulkString None)
  | _ -> SimpleString "PONG"
