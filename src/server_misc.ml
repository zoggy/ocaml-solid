(** *)

let split_string ?(keep_empty=false) s chars =
  let re =
    Re.(
     let re = alt (List.map char chars) in
     let re = if keep_empty then re else rep1 re in
     compile re
    )
  in
  Re.split re s