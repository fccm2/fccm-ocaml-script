val dump_image : int array array array -> unit
val new_img : int * int -> 'a * 'a * 'a -> unit -> 'a array array array
val put_px : 'a array array array -> int * int -> 'a * 'a * 'a -> unit
val fill_rect :
  'a array array array -> int * int -> int * int -> 'a * 'a * 'a -> unit
val new_img_0 : int * int -> unit -> int array array array
val new_img_1 : int * int -> unit -> int array array array
val draw_circ :
  'a array array array -> int * int -> int -> 'a * 'a * 'a -> unit
type t
