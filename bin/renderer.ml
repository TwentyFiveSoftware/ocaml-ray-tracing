include Vec3
include Material
include Scene
include Camera

module Renderer = struct
  type t = {
    width : int;
    height : int;
    samples_per_pixel : int;
    max_ray_recursive_depth : int;
    render_threads : int;
    scene : Scene.t;
    camera : Camera.t;
  }

  let rec ray_color renderer ?(depth = 0) ray =
    if depth >= renderer.max_ray_recursive_depth then Vec3.zero
    else
      let hit_record = Scene.calculate_ray_collision renderer.scene ray in
      match hit_record with
      | Some hit_record -> (
          let scatter_record =
            Material.scatter hit_record.material ray hit_record.point
              hit_record.normal hit_record.front_face
          in
          match scatter_record with
          | Some scatter_record ->
              Vec3.vec_mul scatter_record.attenuation
                (ray_color renderer scatter_record.scattered_ray
                   ~depth:(depth + 1))
          | None -> Vec3.zero)
      | None ->
          let t = ((Vec3.normalize ray.direction).y +. 1.0) *. 0.5 in
          Vec3.lerp
            { Vec3.x = 1.0; y = 1.0; z = 1.0 }
            { Vec3.x = 0.5; y = 0.7; z = 1.0 }
            t

  let rec calculate_pixel_color renderer ?(sample = 0) (x, y) =
    if sample >= renderer.samples_per_pixel then Vec3.zero
    else
      let u, v =
        ( (float_of_int x +. Random.float 1.0)
          /. float_of_int (renderer.width - 1),
          (float_of_int y +. Random.float 1.0)
          /. float_of_int (renderer.height - 1) )
      in
      let color =
        ray_color renderer (Camera.get_camera_ray renderer.camera u v)
      in
      Vec3.vec_add color
        (calculate_pixel_color renderer (x, y) ~sample:(sample + 1))

  let rec render_row renderer img y x =
    if x >= renderer.width then []
    else
      let raw_color = calculate_pixel_color renderer (x, y) in
      let color =
        Vec3.vec_mul_scalar raw_color
          (1.0 /. float_of_int renderer.samples_per_pixel)
      in
      let color = Vec3.vec_sqrt color in
      let color = Vec3.vec_mul_scalar color 255.0 in
      let r, g, b =
        (int_of_float color.x, int_of_float color.y, int_of_float color.z)
      in
      let _ = Image.write_rgb img x y r g b in
      color :: render_row renderer img y (x + 1)

  let render renderer img =
    let next_row = Atomic.make 0 in
    let render_worker () =
      let rec loop () =
        let y = Atomic.fetch_and_add next_row 1 in
        if y >= renderer.height then ()
        else
          let _ =
            Printf.printf "%d / %d (%.2f%%)\n%!" (y + 1) renderer.height
              (float_of_int (y + 1) /. float_of_int renderer.height *. 100.0)
          in
          let _ = render_row renderer img y 0 in
          loop ()
      in
      loop ()
    in
    let render_workers =
      List.init renderer.render_threads (fun _ -> Domain.spawn render_worker)
    in
    List.iter Domain.join render_workers
end
