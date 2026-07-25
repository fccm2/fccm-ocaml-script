#directory "+unix"
#load "unix.cma"

let input_line ic =
  try let line = input_line ic in Some(line)
  with End_of_file -> close_in ic; None

let load_lines f =
  let ic = open_in f in
  let rec aux acc =
    match input_line ic with
    | Some line -> aux (line::acc)
    | None -> List.rev (acc)
  in
  aux []

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n; close_in ic;
  (Bytes.to_string s)

let rem_ext s =
  try Filename.chop_extension s
  with Invalid_argument _ -> s

let scan_line_1 s =
  let year, month, day, annot =
    Scanf.sscanf s "%d-%d-%d: %s" (fun y m d annot -> y, m, d, annot)
  in
  (year, month, day, annot)

let scan_line_2 s =
  let year, month, day, annot =
    Scanf.sscanf s "%d-%d-%d:%s" (fun y m d annot -> y, m, d, annot)
  in
  (year, month, day, annot)

let scan_line s =
  try scan_line_1 s with _ ->
  try scan_line_2 s with _ ->
  (0, 0, 0, "")

let read_days () =
  let days_file = Sys.argv.(1) in
  let ds_list = load_lines days_file in
  let f d_line =
    let y, m, d, annot = scan_line d_line in
    ((y, m, d), (String.trim annot))
  in
  List.filter (fun e -> e <> ((0, 0, 0), "")
  ) (List.map f ds_list)

let read_days () =
  try read_days ()
  with _ -> []

let print_days ds =
  List.iter (fun ((y, m, d), (annot)) ->
    Printf.printf " %d %d %d -- (%s)\n" y m d annot;
  ) ds

let polymorph_color = function
  | "default"   -> `default
  | "reset"     -> `reset
  | "bold"      -> `bold
  | "red"       -> `red
  | "green"     -> `green
  | "yellow"    -> `yellow
  | "blue"      -> `blue
  | "magenta"   -> `magenta
  | "cyan"      -> `cyan
  | "bold_red"      -> `bold_red
  | "bold_green"    -> `bold_green
  | "bold_yellow"   -> `bold_yellow
  | "bold_blue"     -> `bold_blue
  | "bold_magenta"  -> `bold_magenta
  | "bold_cyan"     -> `bold_cyan
  | "bg_red"      -> `bg_red
  | "bg_green"    -> `bg_green
  | "bg_yellow"   -> `bg_yellow
  | "bg_blue"     -> `bg_blue
  | "bg_magenta"  -> `bg_magenta
  | "bg_cyan"     -> `bg_cyan
  | "black_on_white"    -> `black_on_white
  | "black_on_red"      -> `black_on_red
  | "black_on_green"    -> `black_on_green
  | "black_on_yellow"   -> `black_on_yellow
  | "black_on_blue"     -> `black_on_blue
  | "black_on_magenta"  -> `black_on_magenta
  | "black_on_cyan"     -> `black_on_cyan
  | "black_on_bold_red"      -> `black_on_bold_red
  | "black_on_bold_green"    -> `black_on_bold_green
  | "black_on_bold_yellow"   -> `black_on_bold_yellow
  | "black_on_bold_blue"     -> `black_on_bold_blue
  | "black_on_bold_magenta"  -> `black_on_bold_magenta
  | "black_on_bold_cyan"     -> `black_on_bold_cyan
  | _ -> invalid_arg "polymorph_color"


let trm_color plm_color s =
  let r = "\027[00m" in  (* reset *)
  match plm_color with
  | `default        -> s
  | `reset          -> "\027[m" ^ s ^ r
  | `bold           -> "\027[1m" ^ s ^ r
  | `red            -> "\027[31m" ^ s ^ r
  | `green          -> "\027[32m" ^ s ^ r
  | `yellow         -> "\027[33m" ^ s ^ r
  | `blue           -> "\027[34m" ^ s ^ r
  | `magenta        -> "\027[35m" ^ s ^ r
  | `cyan           -> "\027[36m" ^ s ^ r
  | `bold_red       -> "\027[1;31m" ^ s ^ r
  | `bold_green     -> "\027[1;32m" ^ s ^ r
  | `bold_yellow    -> "\027[1;33m" ^ s ^ r
  | `bold_blue      -> "\027[1;34m" ^ s ^ r
  | `bold_magenta   -> "\027[1;35m" ^ s ^ r
  | `bold_cyan      -> "\027[1;36m" ^ s ^ r
  | `bg_red         -> "\027[41m" ^ s ^ r
  | `bg_green       -> "\027[42m" ^ s ^ r
  | `bg_yellow      -> "\027[43m" ^ s ^ r
  | `bg_blue        -> "\027[44m" ^ s ^ r
  | `bg_magenta     -> "\027[45m" ^ s ^ r
  | `bg_cyan        -> "\027[46m" ^ s ^ r

  | `black_on_white     -> "\027[30;47m" ^ s ^ r
  | `black_on_red       -> "\027[30;41m" ^ s ^ r
  | `black_on_green     -> "\027[30;42m" ^ s ^ r
  | `black_on_yellow    -> "\027[30;43m" ^ s ^ r
  | `black_on_blue      -> "\027[30;44m" ^ s ^ r
  | `black_on_magenta   -> "\027[30;45m" ^ s ^ r
  | `black_on_cyan      -> "\027[30;46m" ^ s ^ r

  | `black_on_bold_red       -> "\027[30;101m" ^ s ^ r
  | `black_on_bold_green     -> "\027[30;102m" ^ s ^ r
  | `black_on_bold_yellow    -> "\027[30;103m" ^ s ^ r
  | `black_on_bold_blue      -> "\027[30;104m" ^ s ^ r
  | `black_on_bold_magenta   -> "\027[30;105m" ^ s ^ r
  | `black_on_bold_cyan      -> "\027[30;106m" ^ s ^ r

let s_color color_name s =
  trm_color (polymorph_color color_name) s

let next_day t =
  let t2 = { t with Unix.tm_mday = succ t.Unix.tm_mday } in
  let _, t3 = Unix.mktime t2 in
  if t.Unix.tm_mon = t3.Unix.tm_mon
  then Some(t3)
  else None

let t_same t1 t2 =
  ( t1.Unix.tm_year = t2.Unix.tm_year &&
    t1.Unix.tm_mon  = t2.Unix.tm_mon &&
    t1.Unix.tm_mday = t2.Unix.tm_mday )

let today () =
  let t = Unix.localtime (Unix.time ()) in
  (t)

let is_today t1 =
  let t2 = today () in
  (t_same t1 t2)


(* sunday-last *)
let day_index t =
  match t.Unix.tm_wday with
  | 0 -> 6
  | i -> (i - 1)

let week_day t = [|
   "L ";
   "M ";
   "M ";
   "J ";
   "V ";
   "S ";
   "D ";
  |].(day_index t)

let is_sunday t =
 (t.Unix.tm_wday = 0)

let months_a = [|
    (" Jan   ");
    (" Fev   ");
    (" Mar   ");
    (" Apr   ");
    (" Mai   ");
    (" Jui   ");
  |]

let months_b = [|
    (" Jul   ");
    (" Aug   ");
    (" Sep   ");
    (" Oct   ");
    (" Nov   ");
    (" Dec   ");
  |]

let year =
  try int_of_string Sys.argv.(2)
  with _ ->
    let t1 = Unix.time () in
    let tm = Unix.localtime t1 in
    (1900 + tm.Unix.tm_year)

let md_a = 0
let md_b = 6

let md, months =
  let args = Array.to_list Sys.argv in
  match
    ( List.mem "a" (args),
      List.mem "b" (args) )
  with
  | true, false -> (md_a, months_a)
  | false, true -> (md_b, months_b)
  | _, _ -> (md_a, months_a)


let fit _ s =
  let n = String.length s in
  if n > 7 then String.sub s 0 7 else
  if n < 7 then s ^ (String.make (7 - n) ' ') else s

let main () =
  let ys = (Printf.sprintf "   %4d     " year) in

  Printf.printf " \n";
  Printf.printf " %s" (trm_color `black_on_bold_blue (ys ^ (String.make 76 ' ')));
  Printf.printf " %s\n" (trm_color `default " ");
  Printf.printf " %s\n" (trm_color `default " ");

  Array.iter (fun mn ->
    Printf.printf " %s " ((trm_color `black_on_bold_blue (mn ^ (String.make 6 ' ') )));
  ) months;
  Printf.printf " %s\n" (trm_color `default " ");

  let ds = read_days () in
  (*
  print_days ds;
  *)

  let tm month year =
    let t0 = {
      (Unix.gmtime 0.0) with
       Unix.tm_year = year - 1900;
       Unix.tm_mon = month - 1;
       Unix.tm_mday = 1;
    } in
    let _, t1 = Unix.mktime t0 in
    (t1)
  in

  let rec aux acc t =
    let n = Printf.sprintf "%02d" t.Unix.tm_mday in
    let d = (week_day t) in
    let s = (Printf.sprintf "%s%s" d n) in
    let td = if (is_today t) then ">" else " " in
    let this =
      (*  " L 01 "  *)
      (*  " M 02 "  *)
      Printf.sprintf "%s%s" td (trm_color `blue s)
    in
    let sep = if is_sunday t then "_" else " " in
    let this = sep ^ this in
    match (next_day t) with
    | Some nd -> aux (this::acc) (nd)
    | None -> List.rev (this::acc)
  in
  let ds1 = aux [] (tm (1 + md) year) in
  let ds2 = aux [] (tm (2 + md) year) in
  let ds3 = aux [] (tm (3 + md) year) in
  let ds4 = aux [] (tm (4 + md) year) in
  let ds5 = aux [] (tm (5 + md) year) in
  let ds6 = aux [] (tm (6 + md) year) in

  let m1 = Array.of_list ds1 in
  let m2 = Array.of_list ds2 in
  let m3 = Array.of_list ds3 in
  let m4 = Array.of_list ds4 in
  let m5 = Array.of_list ds5 in
  let m6 = Array.of_list ds6 in

  let ms = [ m1; m2; m3; m4; m5; m6; ] in

  for day = 1 to 31 do
    let month = ref (1 + md) in
    List.iter (fun m ->
      try
        let d = m.(day - 1) in
        let ymd = (year, !month, day) in
        let annot = try List.assoc ymd ds with Not_found -> (String.make 5 ' ') in
        Printf.printf "%s %s " d (fit 7 annot);
        incr month;
      with Invalid_argument _ ->
        Printf.printf "%s" ("   " ^ (String.make 12 ' '));
    ) ms;
    Printf.printf "\n";
  done;

  Printf.printf "\n";
;;
 
let () =
  let print_help () =
    prerr_endline begin
      Printf.sprintf {|
        %s <date-file> <year> <a|b>

        a: first half of the year
        b: second half of the year

        example date-file:
          2057-07-05: StAnt
      |} Sys.argv.(0)
    end
  in
  let args = Array.to_list Sys.argv in
  match
    ( List.mem "-h" args ||
      List.mem "--help" args )
  with
  | true -> print_help (); exit 1
  | _ -> ()

let () = main ()
