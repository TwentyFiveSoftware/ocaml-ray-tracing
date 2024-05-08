include Vec3
include Material
include HitRecord

module Sphere = struct
  type t = { center : Vec3.t; radius : float; material : Material.t }

  let ray_hits_sphere (sphere : t) (ray : Ray.t) (t_min : float) =
    let oc = Vec3.vec_sub ray.origin sphere.center in
    let a = Vec3.vec_length_squared ray.direction in
    let half_b = Vec3.vec_dot oc ray.direction in
    let c = Vec3.vec_length_squared oc -. (sphere.radius *. sphere.radius) in
    let discriminant = (half_b *. half_b) -. (a *. c) in
    if discriminant < 0. then None
    else
      let sqrt_d = sqrt discriminant in
      let root_1 = (-.half_b -. sqrt_d) /. a in
      let root_2 = (-.half_b +. sqrt_d) /. a in
      let t =
        if root_1 > t_min && root_1 < root_2 then root_1
        else if root_2 > t_min then root_2
        else -1.0
      in
      if t < 0.0 then None
      else
        let point = Ray.at ray t in
        let outward_normal =
          Vec3.vec_mul_scalar
            (Vec3.vec_sub point sphere.center)
            (1.0 /. sphere.radius)
        in
        let front_face = Vec3.vec_dot ray.direction outward_normal < 0.0 in
        let normal =
          if front_face then outward_normal else Vec3.vec_neg outward_normal
        in
        Some
          { HitRecord.t; point; normal; front_face; material = sphere.material }
end
