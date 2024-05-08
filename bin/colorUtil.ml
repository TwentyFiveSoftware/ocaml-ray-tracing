include Vec3

module ColorUtil = struct
  let random_color _ =
    let hsv_to_rgb h s v =
      let c = s *. v in
      let x = c *. (1.0 -. abs_float ((mod_float (h /. 60.0) 2.0) -. 1.0)) in
      let m = v -. c in
      let (r, g, b) = 
        if h >= 0.0 && h < 60.0 then
          (c,x,0.0)
        else if h >= 60.0 && h < 120.0 then
          (x, c, 0.0)
        else if h >= 120.0 && h < 180.0 then
          (0.0, c, x)
        else if h >= 180.0 && h < 240.0 then
          (0.0, x, c)
        else if h >= 240.0 && h < 300.0 then
          (x, 0.0, c)
        else
          (c, 0.0, x) in
      {Vec3.x = r +. m; y = g +. m; z = b +. m}
    in
    hsv_to_rgb (Random.float 360.0) 0.75 0.45;;
    
end