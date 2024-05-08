module Vec3 = struct
  type t = { x : float; y : float; z : float }

  let zero = { x = 0.0; y = 0.0; z = 0.0 }
  let vec_neg vec = { x = -.vec.x; y = -.vec.y; z = -.vec.z }
  let vec_add a b = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z }
  let vec_sub a b = { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z }

  let vec_mul_scalar vec scalar =
    { x = vec.x *. scalar; y = vec.y *. scalar; z = vec.z *. scalar }

  let vec_mul a b = { x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z }
  let vec_dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

  let vec_cross a b =
    {
      x = (a.y *. b.z) -. (a.z *. b.y);
      y = (a.z *. b.x) -. (a.x *. b.z);
      z = (a.x *. b.y) -. (a.y *. b.x);
    }

  let vec_sqrt vec = { x = sqrt vec.x; y = sqrt vec.y; z = sqrt vec.z }
  let vec_length_squared vec = vec_dot vec vec
  let vec_length vec = sqrt (vec_length_squared vec)

  let normalize vec =
    let length = vec_length vec in
    { x = vec.x /. length; y = vec.y /. length; z = vec.z /. length }

  let vec_is_near_zero vec =
    let epsilon = 1e-8 in
    abs_float vec.x < epsilon
    && abs_float vec.y < epsilon
    && abs_float vec.z < epsilon

  let vec_reflect vec normal =
    vec_sub vec (vec_mul_scalar normal (2.0 *. vec_dot vec normal))

  let vec_refract vec normal refraction_ratio =
    let cos_theta = min (vec_dot (vec_neg vec) normal) 1.0 in
    let sin_theta = sqrt (1.0 -. (cos_theta *. cos_theta)) in
    let r0 = (1.0 -. refraction_ratio) /. (1.0 +. refraction_ratio) in
    let reflectance =
      (r0 *. r0) +. ((1.0 -. (r0 *. r0)) *. ((1.0 -. cos_theta) ** 5.0))
    in
    if refraction_ratio *. sin_theta > 1.0 || reflectance > Random.float 1.0
    then vec_reflect vec normal
    else
      let r_out_perpendicular =
        vec_mul_scalar
          (vec_add vec (vec_mul_scalar normal cos_theta))
          refraction_ratio
      in
      let r_out_parallel =
        vec_mul_scalar normal
          (-.sqrt (1.0 -. vec_dot r_out_perpendicular r_out_perpendicular))
      in
      vec_add r_out_perpendicular r_out_parallel

  let lerp a b t =
    {
      x = (a.x *. (1.0 -. t)) +. (b.x *. t);
      y = (a.y *. (1.0 -. t)) +. (b.y *. t);
      z = (a.z *. (1.0 -. t)) +. (b.z *. t);
    }

  let rec random_unit_vector _ =
    let vector =
      {
        x = Random.float 2.0 -. 1.0;
        y = Random.float 2.0 -. 1.0;
        z = Random.float 2.0 -. 1.0;
      }
    in
    if vec_length_squared vector < 1.0 then normalize vector
    else random_unit_vector ()
end
