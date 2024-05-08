include Sphere
include Material
include Texture
include ColorUtil

module Scene = struct
  type t = { spheres : Sphere.t list }

  let generate_scene () =
    let rec generate_small_spheres i =
      if i >= 21 * 21 then []
      else
        let x = (i mod 22) - 11 in
        let z = (i / 22) - 11 in
        let center =
          {
            Vec3.x = float_of_int x +. Random.float 0.7;
            y = 0.2;
            z = float_of_int z +. Random.float 0.7;
          }
        in
        let material_random = Random.float 1.0 in
        let sphere =
          if material_random < 0.8 then
            {
              Sphere.center;
              radius = 0.2;
              material =
                Material.diffuse (Texture.solid (ColorUtil.random_color ()));
            }
          else if material_random < 0.95 then
            {
              Sphere.center;
              radius = 0.2;
              material =
                Material.metal (Texture.solid (ColorUtil.random_color ()));
            }
          else
            { Sphere.center; radius = 0.2; material = Material.dielectric 1.5 }
        in
        sphere :: generate_small_spheres (i + 1)
    in
    let spheres = generate_small_spheres 0 in
    let ground_sphere =
      {
        Sphere.center = { x = 0.0; y = -1000.0; z = 1.0 };
        radius = 1000.0;
        material =
          Material.diffuse
            (Texture.checkered
               { x = 0.05; y = 0.05; z = 0.05 }
               { x = 0.95; y = 0.95; z = 0.95 });
      }
    in
    let center_sphere =
      {
        Sphere.center = { x = 0.0; y = 1.0; z = 0.0 };
        radius = 1.0;
        material = Material.dielectric 1.5;
      }
    in
    let left_sphere =
      {
        Sphere.center = { x = -4.0; y = 1.0; z = 0.0 };
        radius = 1.0;
        material =
          Material.diffuse (Texture.solid { x = 0.6; y = 0.3; z = 0.1 });
      }
    in
    let right_sphere =
      {
        Sphere.center = { x = 4.0; y = 1.0; z = 0.0 };
        radius = 1.0;
        material = Material.metal (Texture.solid { x = 0.7; y = 0.6; z = 0.5 });
      }
    in
    let spheres =
      ground_sphere :: center_sphere :: left_sphere :: right_sphere :: spheres
    in
    { spheres }

  let calculate_ray_collision scene ray =
    let hit_records =
      List.filter_map
        (fun (sphere : Sphere.t) -> Sphere.ray_hits_sphere sphere ray 0.001)
        scene.spheres
    in
    let colliding_hit_records =
      List.filter
        (fun (hit_record : HitRecord.t) -> hit_record.t > 0.001)
        hit_records
    in
    let sorted_records =
      List.sort
        (fun (a : HitRecord.t) (b : HitRecord.t) -> if a.t > b.t then 1 else -1)
        colliding_hit_records
    in
    match sorted_records with [] -> None | first :: _ -> Some first
end
