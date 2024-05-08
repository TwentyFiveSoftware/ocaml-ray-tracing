include Vec3

module Texture = struct
  type texture_type = Solid | Checkered

  type t = {
    texture_type: texture_type;
    albedo: Vec3.t list;
  }

  let solid albedo = {texture_type = Solid; albedo = [albedo]}

  let checkered albedo1 albedo2 = {texture_type = Checkered; albedo = [albedo1; albedo2]}

  let get_color (texture: t) (point: Vec3.t) =
    match texture.texture_type with
    | Solid -> (
        match texture.albedo with
        | [] -> Vec3.zero
        | albedo :: _ -> albedo)
    | Checkered -> match texture.albedo with
      | [] -> Vec3.zero
      | [albedo] -> albedo
      | albedo1 :: albedo2 :: _ ->
        let size = 6.0 in
        let sines = sin (size *. point.x) *. sin (size *. point.y) *. sin (size *. point.z) in
        if sines < 0.0 then
          albedo1
        else
          albedo2
end
