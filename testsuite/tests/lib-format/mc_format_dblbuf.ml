(* TEST *)

let format_sync_via_double_buffering () =
  let buf_key = Domain.DLS.new_key (fun () -> Buffer.create 80) in
  let out_funs = Format.get_formatter_out_functions () in
  let out_string str ofs len =
    Buffer.add_substring (Domain.DLS.get buf_key) str ofs len
  in
  let out_flush () =
    let buf = Domain.DLS.get buf_key in
    let len = Buffer.length buf in
    let str = Buffer.contents buf in
    out_funs.out_string str 0 len ;
    Buffer.clear buf
  in
  Format.set_formatter_out_functions {out_funs with out_string; out_flush}

let () =
  let domains = Array.init 7 (fun i ->
    Domain.spawn (fun () ->
          format_sync_via_double_buffering ();
          for j = 1 to 10000000 do () done;
          for j = 1 to 100 do
            Format.printf "The quick brown fox@\n";
            Format.printf "jumped over the lazy dog@.";
          done
      )
    ) in
  Array.iter Domain.join domains
