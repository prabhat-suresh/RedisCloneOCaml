let handle_client flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:1000 in
  try
    while true do
      let line = Eio.Buf_read.line reader in
      Eio.traceln "received %s" line;
      Eio.Flow.copy_string "+PONG\r\n" flow
    done
  with End_of_file -> ()

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.any, 6379) in
  Eio.Switch.run (fun sw ->
      let server = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
      print_endline "Redis server running on port 6379...";
      Eio.Net.run_server
        ~on_error:(fun ex -> prerr_endline (Printexc.to_string ex))
        server
        (fun flow _addr -> handle_client flow))
