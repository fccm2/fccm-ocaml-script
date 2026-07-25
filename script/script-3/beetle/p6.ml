let dump_image img =
  let h = Array.length img in
  let w = Array.length img.(0) in
  let n = Array.length img.(0).(0) in
  Printf.printf "P6\n";
  Printf.printf "# %d\n" n;
  Printf.printf "%d %d\n" w h;
  Printf.printf "255\n";
  for y = 0 to pred h do
    let row = img.(y) in
    for x = 0 to pred w do
      let cl = row.(x) in
      print_char (char_of_int cl.(0));
      print_char (char_of_int cl.(1));
      print_char (char_of_int cl.(2));
    done;
  done;
;;

let new_img (_w, _h) (r, g, b) () =
  let img =
    Array.init _h (fun y ->
      Array.init _w (fun x ->
        Array.copy [| r; g; b |]
      )
    )
  in
  (img)
;;

let put_px img (x, y) (r, g, b) =
  let h = Array.length img in
  let w = Array.length img.(0) in
  if x < 0 then () else
  if y < 0 then () else
  if x >= w then () else
  if y >= h then () else
  begin
    img.(y).(x).(0) <- r;
    img.(y).(x).(1) <- g;
    img.(y).(x).(2) <- b;
  end;
;;

let fill_rect img (_x, _y) (_w, _h) (r, g, b) =
  for y = _y to pred (_y + _h) do
    for x = _x to pred (_x + _w) do
      put_px img (x, y) (r, g, b) ;
    done;
  done;
;;

let new_img_0 (_w, _h) () =
  let img =
    Array.init _h (fun y ->
      Array.init _w (fun x ->
        Array.init 3 (fun c -> 0)
      )
    )
  in
  (img)
;;

let new_img_1 (_w, _h) () =
  let img =
    Array.init _h (fun y ->
      Array.init _w (fun x ->
        Array.init 3 (fun c -> 255)
      )
    )
  in
  (img)
;;

let draw_circ img (_x, _y) (_r) (r, g, b) =
  let _w = Array.length img.(0) in
  let _h = Array.length img in
  for i = (_x - _r) to (_x + _r) do
    for j = (_y - _r) to (_y + _r) do
      if i < 0 then () else
      if j < 0 then () else
      if i >= _w then () else
      if j >= _h then () else
      let dx = i - _x in
      let dy = j - _y in
      let sq_dist = (dx * dx) + (dy * dy) in
      let sq_r = (_r * _r) in
      if (sq_dist < sq_r) then
      begin
        let x, y = i, j in
        img.(y).(x).(0) <- r;
        img.(y).(x).(1) <- g;
        img.(y).(x).(2) <- b;
      end
    done
  done
;;

type color = int array
type t = color array array

