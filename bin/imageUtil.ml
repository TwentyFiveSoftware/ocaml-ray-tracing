include Vec3

module ImageUtil = struct
  let rec write_pixel_to_image img pixels i width height =
    match pixels with
    | [] -> ()
    | (pixel : Vec3.t) :: pixels ->
        let color = Vec3.vec_mul_scalar pixel 255.0 in
        let r, g, b =
          (int_of_float color.x, int_of_float color.y, int_of_float color.z)
        in
        let x, y = (i mod width, i / width) in
        let _ = Image.write_rgb img x y r g b in
        write_pixel_to_image img pixels (i + 1) width height

  let write_pixels_to_image img pixels width height =
    write_pixel_to_image img pixels 0 width height
end
