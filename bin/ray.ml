include Vec3

module Ray = struct
  type t = { origin : Vec3.t; direction : Vec3.t }

  let at ray t = Vec3.vec_add ray.origin (Vec3.vec_mul_scalar ray.direction t)
end
