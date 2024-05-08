include Vec3

module ColorUtil = struct
  let hsv_to_rgb hue s v =
    let h = hue /. 60.0 in
    let fraction = h -. float_of_int (int_of_float h) in
    let p = v *. (1.0 -. s) in
    let q = v *. (1.0 -. (s *. fraction)) in
    let t = v *. (1.0 -. (s *. (1.0 -. fraction))) in
    let r, g, b =
      if 0.0 <= h && h < 1.0 then (v, t, p)
      else if 1.0 <= h && h < 2.0 then (q, v, p)
      else if 2.0 <= h && h < 3.0 then (p, v, t)
      else if 3.0 <= h && h < 4.0 then (p, q, v)
      else if 4.0 <= h && h < 5.0 then (t, p, v)
      else if 5.0 <= h && h < 6.0 then (v, p, q)
      else (0.0, 0.0, 0.0)
    in
    { Vec3.x = r; y = g; z = b }

  let random_color () = hsv_to_rgb (Random.float 360.0) 0.75 0.45
end
