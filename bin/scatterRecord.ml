include Vec3
include Ray

module ScatterRecord = struct
  type t = { attenuation : Vec3.t; scattered_ray : Ray.t }
end
