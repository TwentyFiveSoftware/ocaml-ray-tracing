type vec3 = {
  x: float;
  y: float;
  z: float;
};;

type ray = {
  origin: vec3;
  direction: vec3;
};;

type material_type = Diffuse | Metal | Dielectric;;

type material = {
  material_type: material_type;
  albedo: vec3;
  refraction_index: float;
};;

type sphere = {
  center: vec3;
  radius: float;
  material: material;
};;

type camera = {
  look_from: vec3;
  upper_left_corner: vec3;
  horizontal_direction: vec3;
  vertical_direction: vec3;
};;

type hit_record = {
  t: float;
  point: vec3;
  normal: vec3;
  front_face: bool;
  material: material;
};;

type scatter_record = {
  does_scatter: bool;
  attenuation: vec3;
  scattered_ray: ray;
};;

type scene = {
  spheres: sphere list;
};;



let (width, height) = (800, 450);;
let max_ray_recursive_depth = 50;;
let samples_per_pixel = 1;;



let vec_zero = {x = 0.0; y = 0.0; z = 0.0};;

let vec_length_squared vec =
  vec.x *. vec.x +. vec.y *. vec.y +. vec.z *. vec.z;;

let vec_length vec = sqrt(vec_length_squared(vec));;

let normalize vec =
  let length = vec_length vec in
    {x = vec.x /. length; y = vec.y /. length; z = vec.z /. length};;

let vec_neg vec =
  {x = -.vec.x; y = -.vec.y; z = -.vec.z};;
      
let vec_add a b =
  {x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z};;

let vec_sub a b =
  {x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z};;

let vec_mul_scalar vec scalar =
  {x = vec.x *. scalar; y = vec.y *. scalar; z = vec.z *. scalar};;

let vec_mul a b =
  {x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z};;
  
let vec_dot a b =
  a.x *. b.x +. a.y *. b.y +. a.z *. b.z;;

let vec_cross a b =
  {
    x = a.y *. b.z -. a.z *. b.y; 
    y = a.z *. b.x -. a.x *. b.z; 
    z = a.x *. b.y -. a.y *. b.x; 
  };;

let rec random_unit_vector _ =
  let vector = {x = (Random.float 2.0) -. 1.0; y = (Random.float 2.0) -. 1.0; z = (Random.float 2.0) -. 1.0} in
  if vec_length_squared vector < 1.0 then
    normalize vector
  else
    random_unit_vector ()

let vec_is_near_zero vec =
  let epsilon = 1e-8 in
  (abs_float vec.x) < epsilon && (abs_float vec.y) < epsilon && (abs_float vec.z) < epsilon

let vec_reflect vec normal =
  vec_sub vec (vec_mul_scalar normal (2.0 *. vec_dot vec normal))

let vec_refract vec normal refraction_ratio = 
  let cos_theta = min (vec_dot (vec_neg vec) normal) 1.0 in
  let sin_theta = sqrt (1.0 -. cos_theta *. cos_theta) in
  let r0 = (1.0 -. refraction_ratio) /. (1.0 +. refraction_ratio) in
  let reflectance = r0 *. r0 +. (1.0 -. r0 *. r0) *. ((1.0 -. cos_theta) ** 5.0) in
  if refraction_ratio *. sin_theta > 1.0 || reflectance > Random.float 1.0 then
    vec_reflect vec normal
  else
    let r_out_perpendicular = vec_mul_scalar (vec_add vec (vec_mul_scalar normal cos_theta)) refraction_ratio in
    let r_out_parallel = vec_mul_scalar normal (-.sqrt (1.0 -. vec_dot r_out_perpendicular r_out_perpendicular)) in
    vec_add r_out_perpendicular r_out_parallel;;

let ray_at ray t =
  vec_add ray.origin (vec_mul_scalar ray.direction t);;

let random_color _ =
  let hsv_to_rgb h s v =
    let c = s *. v in
    let x = c *. (1.0 -. abs_float ((mod_float (h /. 60.0) 2.0) -. 1.0)) in
    let m = v -. c in
    let (r, g, b) = 
      if h >= 0.0 && h < 60.0 then
        (c,x,0.0)
      else if h >= 60.0 && h < 120.0 then
        (x, c, 0.0)
      else if h >= 120.0 && h < 180.0 then
        (0.0, c, x)
      else if h >= 180.0 && h < 240.0 then
        (0.0, x, c)
      else if h >= 240.0 && h < 300.0 then
        (x, 0.0, c)
      else
        (c, 0.0, x) in
    {x = r +. m; y = g +. m; z = b +. m}
  in
  hsv_to_rgb (Random.float 360.0) 0.75 0.45;;


let new_camera look_from look_at fov =
  let aspect_ratio = float_of_int width /. float_of_int height in
  let viewport_height = tan (fov /. 360.0 *. acos (-.1.0)) *. 2.0 in
  let viewport_width = viewport_height *. aspect_ratio in
  let forward = normalize (vec_sub look_at look_from) in
  let right = normalize (vec_cross {x = 0.0; y = 1.0; z = 0.0} forward) in
  let up = normalize (vec_cross forward right) in
  let horizontal_direction = vec_mul_scalar right viewport_width in
  let vertical_direction = vec_mul_scalar up viewport_height in
  let upper_left_corner = vec_add 
    (vec_add 
      (vec_sub look_from (vec_mul_scalar horizontal_direction 0.5)) 
      (vec_mul_scalar vertical_direction 0.5)) 
    forward in
  {look_from; upper_left_corner; horizontal_direction; vertical_direction};;


let generate_scene _ =
  let rec generate_small_spheres i =
    if i >= 21 * 21 then
      []
    else 
      let x = i mod 22 - 11 in
      let z = i / 22 - 11 in
      let center = {x = float_of_int x +. Random.float 0.7; y = 0.2; z = float_of_int z +. Random.float 0.7} in
      let material_random = Random.float 1.0 in
      let sphere = (if material_random < 0.8 then
          {center; radius = 0.2; material = {material_type = Diffuse; albedo = random_color (); refraction_index = 0.0}}
        else if material_random < 0.95 then
          {center; radius = 0.2; material = {material_type = Metal; albedo = random_color (); refraction_index = 0.0}}
        else
          {center; radius = 0.2; material = {material_type = Dielectric; albedo = vec_zero; refraction_index = 1.5}}) in
      sphere::generate_small_spheres (i + 1)
  in
  let spheres = generate_small_spheres 0 in
  let ground_sphere = {center = {x = 0.0; y = -1000.0; z = 1.0}; radius = 1000.0; material = 
                      {material_type = Diffuse; albedo = {x = 0.95; y = 0.95; z = 0.95}; refraction_index = 0.0}} in
  let center_sphere = {center = {x = 0.0; y = 1.0; z = 0.0}; radius = 1.0; material = 
                      {material_type = Dielectric; albedo = vec_zero; refraction_index = 1.5}} in
  let left_sphere = {center = {x = -4.0; y = 1.0; z = 0.0}; radius = 1.0; material = 
                      {material_type = Diffuse; albedo = {x = 0.6; y = 0.3; z = 0.1}; refraction_index = 0.0}} in
  let right_sphere = {center = {x = 4.0; y = 1.0; z = 0.0}; radius = 1.0; material = 
                      {material_type = Metal; albedo = {x = 0.7; y = 0.6; z = 0.5}; refraction_index = 0.0}} in
  let spheres = ground_sphere :: center_sphere :: left_sphere :: right_sphere :: spheres in
  {spheres};;


let camera = new_camera {x = 12.0; y = 2.0; z = -3.0} vec_zero 25.0;;
let scene = generate_scene ();;

let get_camera_ray u v =
  let target = vec_sub (vec_add camera.upper_left_corner (vec_mul_scalar camera.horizontal_direction u)) 
                       (vec_mul_scalar camera.vertical_direction v) in
  {origin = camera.look_from; direction = vec_sub target camera.look_from};;

let scatter_material_diffuse hit_record =
  let scatter_direction = vec_add hit_record.normal (random_unit_vector ()) in
  let scatter_direction = if vec_is_near_zero scatter_direction then hit_record.normal else scatter_direction in
  let scattered_ray = {origin = hit_record.point; direction = scatter_direction} in
  {does_scatter = true; attenuation = hit_record.material.albedo; scattered_ray};;

let scatter_material_metal hit_record ray =
  let scatter_direction = vec_reflect (normalize ray.direction) hit_record.normal in
  let scattered_ray = {origin = hit_record.point; direction = scatter_direction} in
  {does_scatter = vec_dot scatter_direction hit_record.normal > 0.0; attenuation = hit_record.material.albedo; scattered_ray};;

let scatter_material_dielectric hit_record ray =
  let refraction_ratio = if hit_record.front_face then 
      1.0 /. hit_record.material.refraction_index
    else 
      hit_record.material.refraction_index in
  let scatter_direction = vec_refract (normalize ray.direction) hit_record.normal refraction_ratio in
  let scattered_ray = {origin = hit_record.point; direction = scatter_direction} in
  {does_scatter = true; attenuation = {x = 1.0; y = 1.0; z = 1.0}; scattered_ray};;

let scatter hit_record ray =
  match hit_record.material.material_type with
  | Diffuse -> scatter_material_diffuse hit_record
  | Metal -> scatter_material_metal hit_record ray
  | Dielectric -> scatter_material_dielectric hit_record ray;;

let ray_hits_sphere ray sphere t_min =
  let oc = vec_sub ray.origin sphere.center in
  let a = vec_length_squared ray.direction in
  let half_b = vec_dot oc ray.direction in
  let c = vec_length_squared oc -. sphere.radius *. sphere.radius in
  let discriminant = half_b *. half_b -. a *. c in
  
  if discriminant < 0. then
    None
  else 
    let sqrt_d = sqrt discriminant in
    let root_1 = (-.half_b -. sqrt_d) /. a in
    let root_2 = (-.half_b +. sqrt_d) /. a in
    let t = if root_1 > t_min && root_1 < root_2 then root_1 else if root_2 > t_min then root_2 else -1.0 in
    if t < 0.0 then
      None
    else
      let point = ray_at ray t in
      let outward_normal = vec_mul_scalar (vec_sub point sphere.center) (1.0 /. sphere.radius) in
      let front_face = vec_dot ray.direction outward_normal < 0.0 in
      let normal = if front_face then outward_normal else vec_neg outward_normal in
      Some {t; point; normal; front_face; material = sphere.material};;


let calculate_ray_collision ray =
  let hit_records = List.filter_map (fun sphere -> ray_hits_sphere ray sphere 0.001) scene.spheres in
  let colliding_hit_records = List.filter (fun hit_record -> hit_record.t > 0.001) hit_records in
  let sorted_records = List.sort (fun a b -> if a.t > b.t then 1 else -1) colliding_hit_records in
  match sorted_records with
  | [] -> None
  | first::_ -> Some first;;

let lerp a b t =
  {x = a.x *. (1.0 -. t) +. b.x *. t;
   y = a.y *. (1.0 -. t) +. b.y *. t;
   z = a.z *. (1.0 -. t) +. b.z *. t}

let rec ray_color ?(depth=0) ray =
  if depth >= max_ray_recursive_depth then
    vec_zero
  else
    let hit_record = calculate_ray_collision ray in
    match hit_record with
    | Some hit_record ->
      let scatter_record = scatter hit_record ray in
      if scatter_record.does_scatter then
        vec_mul scatter_record.attenuation (ray_color scatter_record.scattered_ray ~depth: (depth + 1))
      else
        vec_zero
    | None -> 
      let t = ((normalize ray.direction).y +. 1.0) *. 0.5 in
      lerp {x = 1.0; y = 1.0; z = 1.0} {x = 0.5; y = 0.7; z = 1.0} t;;

let rec calculate_pixel_color ?(sample=0) (x, y) =
  if sample >= samples_per_pixel then
    vec_zero
  else
    let (u, v) = (
      (float_of_int x +. Random.float 1.0) /. float_of_int (width - 1), 
      (float_of_int y +. Random.float 1.0) /. float_of_int (height - 1)
    ) in 
    let color = ray_color (get_camera_ray u v) in
    vec_add color (calculate_pixel_color (x, y) ~sample: (sample + 1));;

let rec ray_trace ?(i=0) img =
  if i >= width * height then ()
  else
    let (x, y) = (i mod width, i / width) in
    let raw_color = calculate_pixel_color (x, y) in
    let color = vec_mul_scalar raw_color (255.0 /. float_of_int samples_per_pixel) in
    let (r, g, b) = (int_of_float color.x, int_of_float color.y, int_of_float color.z) in
    let _ = Image.write_rgb img x y r g b in
    ray_trace img ~i: (i + 1);;

    
let img: Image.image = Image.create_rgb width height ~alpha: false ~max_val: 255;;
ray_trace img;;
ImageLib.PNG.write (ImageUtil_unix.chunk_writer_of_path "render.png") img;;
