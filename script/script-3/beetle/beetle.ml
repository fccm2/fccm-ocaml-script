let dist (x1, y1) (x2, y2) =
  let dx = (x2 - x1) in
  let dy = (y2 - y1) in
  let sq_dist = (dx * dx) + (dy * dy) in
  int_of_float (sqrt (float sq_dist))

let () =
  Random.self_init ();
  let img = P6.new_img_1 (90, 80) () in
  let _r = (10) in
  let _rh = (_r / 2) in
  let _r2 = (_r + _rh) in
  let (cx, cy) = (45, 40) in
  P6.draw_circ img (cx, cy) (_r) (0, 0, 0) ;
  for i = (45 - _r2) to (45) do
    for j = (40 - _r2) to (40 + _r2) do
      if Random.int 10 < 2 then
      begin
        let dx = 45 - i in
        let dy = 40 - j in
        let sq_dist = (dx * dx) + (dy * dy) in
        let sq_r = (_r * _r) in
        let c =
          if (sq_dist < sq_r)
          then (255, 255, 255)
          else (0, 0, 0)
        in
        let d = dist (i, j) (cx, cy) in
        let c1 = ((_r - d) < 4) && (d < _r) in (*ins*)
        let c2 = ((d - _r) < 2) && (d > _r) in (*ext*)
        if c1 || c2 then
        begin
          let i2 = cx + (cx - i) in
          P6.put_px img (i, j) c ;
          P6.put_px img (i2, j) c ;
        end;
      end;
    done;
  done;
  P6.dump_image img ;
;;
