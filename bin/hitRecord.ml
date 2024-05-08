include Vec3
include Material

module HitRecord = struct
  type t = {
    t : float;
    point : Vec3.t;
    normal : Vec3.t;
    front_face : bool;
    material : Material.t;
  }
end
