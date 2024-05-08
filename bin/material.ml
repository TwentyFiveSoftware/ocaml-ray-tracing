include Vec3
include Ray
include Texture
include ScatterRecord

module Material = struct
  type material_type = Diffuse | Metal | Dielectric

  type t = {
    material_type : material_type;
    texture : Texture.t;
    refraction_index : float;
  }

  let diffuse texture =
    { material_type = Diffuse; texture; refraction_index = 0.0 }

  let metal texture = { material_type = Metal; texture; refraction_index = 0.0 }

  let dielectric refraction_index =
    {
      material_type = Dielectric;
      texture = Texture.solid Vec3.zero;
      refraction_index;
    }

  let scatter_diffuse (texture : Texture.t) hit_point hit_normal =
    let scatter_direction =
      Vec3.vec_add hit_normal (Vec3.random_unit_vector ())
    in
    let scatter_direction =
      if Vec3.vec_is_near_zero scatter_direction then hit_normal
      else scatter_direction
    in
    let scattered_ray =
      { Ray.origin = hit_point; direction = scatter_direction }
    in
    Some
      {
        ScatterRecord.attenuation = Texture.get_color texture hit_point;
        scattered_ray;
      }

  let scatter_metal (texture : Texture.t) (ray : Ray.t) hit_point hit_normal =
    let scatter_direction =
      Vec3.vec_reflect (Vec3.normalize ray.direction) hit_normal
    in
    let scattered_ray =
      { Ray.origin = hit_point; direction = scatter_direction }
    in
    if Vec3.vec_dot scatter_direction hit_normal <= 0.0 then None
    else
      Some
        {
          ScatterRecord.attenuation = Texture.get_color texture hit_point;
          scattered_ray;
        }

  let scatter_dielectric (refraction_index : float) (ray : Ray.t) hit_point
      hit_normal hit_is_front_face =
    let refraction_ratio =
      if hit_is_front_face then 1.0 /. refraction_index else refraction_index
    in
    let scatter_direction =
      Vec3.vec_refract
        (Vec3.normalize ray.direction)
        hit_normal refraction_ratio
    in
    let scattered_ray =
      { Ray.origin = hit_point; direction = scatter_direction }
    in
    Some
      {
        ScatterRecord.attenuation = { x = 1.0; y = 1.0; z = 1.0 };
        scattered_ray;
      }

  let scatter (material : t) (ray : Ray.t) hit_point hit_normal
      hit_is_front_face =
    match material.material_type with
    | Diffuse -> scatter_diffuse material.texture hit_point hit_normal
    | Metal -> scatter_metal material.texture ray hit_point hit_normal
    | Dielectric ->
        scatter_dielectric material.refraction_index ray hit_point hit_normal
          hit_is_front_face
end
