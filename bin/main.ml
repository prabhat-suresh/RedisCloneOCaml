open Base
open RedisCloneOCaml

let handle_client flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:1000 in
  try
    while true do
      let cmd = Resp.parse_command reader in
      Eio.traceln "received command";
      Eio.Flow.copy_string (Resp.to_string @@ Resp.reply cmd) flow
    done
  with End_of_file -> ()

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, 6379) in
  Eio.Switch.run (fun sw ->
      let server = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
      Eio.traceln "Redis server running on port 6379...";
      Eio.Net.run_server
        ~on_error:(fun ex -> Eio.traceln "%s" (Exn.to_string ex))
        server
        (fun flow _addr -> handle_client flow))
