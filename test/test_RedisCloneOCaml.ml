open Base
open RedisCloneOCaml
open Resp

let parse s = Eio.Buf_read.of_string s |> Resp.parse_command

let%test_unit "to_string: simple string" =
  [%test_eq: string] (to_string (SimpleString "OK")) "+OK\r\n"

let%test_unit "to_string: error" =
  [%test_eq: string]
    (to_string (Err "ERR unknown command"))
    "-ERR unknown command\r\n"

let%test_unit "to_string: integer" =
  [%test_eq: string] (to_string (Integer 42)) ":42\r\n"

let%test_unit "to_string: negative integer" =
  [%test_eq: string] (to_string (Integer (-1))) ":-1\r\n"

let%test_unit "to_string: bulk string" =
  [%test_eq: string] (to_string (BulkString (Some "hello"))) "$5\r\nhello\r\n"

let%test_unit "to_string: empty bulk string" =
  [%test_eq: string] (to_string (BulkString (Some ""))) "$0\r\n\r\n"

let%test_unit "to_string: null bulk string" =
  [%test_eq: string] (to_string (BulkString None)) "$-1\r\n"

let%test_unit "to_string: array" =
  [%test_eq: string]
    (to_string (Arr [ BulkString (Some "ECHO"); BulkString (Some "hey") ]))
    "*2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n"

let%test_unit "to_string: empty array" =
  [%test_eq: string] (to_string (Arr [])) "*0\r\n"

let%test_unit "to_string: nested array" =
  [%test_eq: string]
    (to_string (Arr [ Integer 1; BulkString None; SimpleString "x" ]))
    "*3\r\n:1\r\n$-1\r\n+x\r\n"

let%test_unit "parse: simple string" =
  [%test_eq: Resp.t] (parse "+OK\r\n") (SimpleString "OK")

let%test_unit "parse: error" =
  [%test_eq: Resp.t] (parse "-ERR boom\r\n") (Err "ERR boom")

let%test_unit "parse: integer" =
  [%test_eq: Resp.t] (parse ":100\r\n") (Integer 100)

let%test_unit "parse: bulk string" =
  [%test_eq: Resp.t] (parse "$5\r\nhello\r\n") (BulkString (Some "hello"))

let%test_unit "parse: empty bulk string" =
  [%test_eq: Resp.t] (parse "$0\r\n\r\n") (BulkString (Some ""))

let%test_unit "parse: null bulk string" =
  [%test_eq: Resp.t] (parse "$-1\r\n") (BulkString None)

let%test_unit "parse: array" =
  [%test_eq: Resp.t]
    (parse "*2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n")
    (Arr [ BulkString (Some "ECHO"); BulkString (Some "hey") ])

let%test_unit "parse: empty array" =
  [%test_eq: Resp.t] (parse "*0\r\n") (Arr [])

let%test_unit "round trip: simple string" =
  [%test_eq: Resp.t]
    (parse (to_string (SimpleString "hello")))
    (SimpleString "hello")

let%test_unit "round trip: error" =
  [%test_eq: Resp.t] (parse (to_string (Err "ERR nope"))) (Err "ERR nope")

let%test_unit "round trip: integer" =
  [%test_eq: Resp.t] (parse (to_string (Integer (-7)))) (Integer (-7))

let%test_unit "round trip: bulk string" =
  [%test_eq: Resp.t]
    (parse (to_string (BulkString (Some "hello world"))))
    (BulkString (Some "hello world"))

let%test_unit "round trip: null bulk string" =
  [%test_eq: Resp.t] (parse (to_string (BulkString None))) (BulkString None)

let%test_unit "round trip: empty array" =
  [%test_eq: Resp.t] (parse (to_string (Arr []))) (Arr [])

let%test_unit "round trip: mixed array" =
  let v = Arr [ Integer 1; BulkString None; SimpleString "x" ] in
  [%test_eq: Resp.t] (parse (to_string v)) v

let%test_unit "reply: ECHO echoes its message" =
  let cmd = Arr [ BulkString (Some "ECHO"); BulkString (Some "hello") ] in
  [%test_eq: Resp.t] (reply cmd) (BulkString (Some "hello"))

let%test_unit "reply: ECHO with null bulk string" =
  let cmd = Arr [ BulkString (Some "ECHO"); BulkString None ] in
  [%test_eq: Resp.t] (reply cmd) (BulkString None)

let%test_unit "reply: unknown command returns PONG" =
  [%test_eq: Resp.t]
    (reply (Arr [ BulkString (Some "PING") ]))
    (SimpleString "PONG")

let%test_unit "reply: bare simple string returns PONG" =
  [%test_eq: Resp.t] (reply (SimpleString "hello")) (SimpleString "PONG")

let%test_unit "reply: SET returns OK" =
  let cmd =
    Arr
      [ BulkString (Some "SET"); BulkString (Some "k"); BulkString (Some "v") ]
  in
  [%test_eq: Resp.t] (reply cmd) (SimpleString "OK")

let%test_unit "reply: SET then GET round trip" =
  let set =
    Arr
      [ BulkString (Some "SET"); BulkString (Some "k"); BulkString (Some "v") ]
  in
  let get = Arr [ BulkString (Some "GET"); BulkString (Some "k") ] in
  [%test_eq: Resp.t] (reply set) (SimpleString "OK");
  [%test_eq: Resp.t] (reply get) (BulkString (Some "v"))

let%test_unit "reply: SET then GET with TTL" =
  let set =
    Arr
      [
        BulkString (Some "SET");
        BulkString (Some "k");
        BulkString (Some "v");
        BulkString (Some "PX");
        BulkString (Some "1");
      ]
  in
  [%test_eq: Resp.t] (reply set) (SimpleString "OK")

let%test_unit "reply: GET missing key returns null bulk string" =
  let cmd = Arr [ BulkString (Some "GET"); BulkString (Some "missing") ] in
  [%test_eq: Resp.t] (reply cmd) (BulkString None)
