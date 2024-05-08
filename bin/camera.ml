include Vec3
include Ray

module Camera = struct
  type t = {
    look_from : Vec3.t;
    upper_left_corner : Vec3.t;
    horizontal_direction : Vec3.t;
    vertical_direction : Vec3.t;
  }

  let new_camera look_from look_at fov width height =
    let aspect_ratio = float_of_int width /. float_of_int height in
    let viewport_height = tan (fov /. 360.0 *. acos (-1.0)) *. 2.0 in
    let viewport_width = viewport_height *. aspect_ratio in
    let forward = Vec3.normalize (Vec3.vec_sub look_at look_from) in
    let right =
      Vec3.normalize (Vec3.vec_cross { x = 0.0; y = 1.0; z = 0.0 } forward)
    in
    let up = Vec3.normalize (Vec3.vec_cross forward right) in
    let horizontal_direction = Vec3.vec_mul_scalar right viewport_width in
    let vertical_direction = Vec3.vec_mul_scalar up viewport_height in
    let upper_left_corner =
      Vec3.vec_add
        (Vec3.vec_add
           (Vec3.vec_sub look_from
              (Vec3.vec_mul_scalar horizontal_direction 0.5))
           (Vec3.vec_mul_scalar vertical_direction 0.5))
        forward
    in
    { look_from; upper_left_corner; horizontal_direction; vertical_direction }

  let get_camera_ray camera u v =
    let target =
      Vec3.vec_sub
        (Vec3.vec_add camera.upper_left_corner
           (Vec3.vec_mul_scalar camera.horizontal_direction u))
        (Vec3.vec_mul_scalar camera.vertical_direction v)
    in
    {
      Ray.origin = camera.look_from;
      direction = Vec3.vec_sub target camera.look_from;
    }
end
