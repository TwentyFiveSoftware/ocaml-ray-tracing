type vec3 = {
  x: float;
  y: float;
  z: float;
};;

type ray = {
  origin: vec3;
  direction: vec3;
};;

(* type material_type = Diffuse | Metal;; *)
type material_type = Diffuse;;

type material = {
  material_type: material_type;
  albedo: vec3;
};;

type sphere = {
  center: vec3;
  radius: float;
  material: material;
};;

type camera = {
  look_from: vec3;
  upper_left_corner: vec3;
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


let (width, height) = (800, 450);;
let max_ray_recursive_depth = 50;;
let samples_per_pixel = 10;;

let spheres: sphere list = [
  {center = {x = 0.; y = 0.; z = 1.}; radius = 0.5; material = {material_type = Diffuse; albedo = {x = 0.9; y = 0.9; z = 0.9}}}
];;


let aspect_ratio = float_of_int width /. float_of_int height;;
let viewport_height = 2.0;;
let viewport_width = viewport_height *. aspect_ratio;;
let focal_length = 1.0;;
let origin = {x = 0.; y = 0.; z = 0.};;
let upper_left_corner = {
  x = viewport_width *. -0.5 -. origin.x;
  y = viewport_height *. 0.5 -. origin.y; 
  z = focal_length -. origin.z
};;

let camera: camera = {look_from = origin; upper_left_corner};;


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

let rec random_unit_vector _ =
  let vector = {x = (Random.float 2.0) -. 1.0; y = (Random.float 2.0) -. 1.0; z = (Random.float 2.0) -. 1.0} in
  if vec_length_squared vector < 1.0 then
    normalize vector
  else
    random_unit_vector ()

let vec_is_near_zero vec =
  let epsilon = 1e-8 in
  (abs_float vec.x) < epsilon && (abs_float vec.y) < epsilon && (abs_float vec.z) < epsilon

let ray_at ray t =
  vec_add ray.origin (vec_mul_scalar ray.direction t);;

let scatter_material_diffuse hit_record =
  let scatter_direction = vec_add hit_record.normal (random_unit_vector ()) in
  let scatter_direction = if vec_is_near_zero scatter_direction then hit_record.normal else scatter_direction in
  let scattered_ray = {origin = hit_record.point; direction = scatter_direction} in
  {does_scatter = true; attenuation = hit_record.material.albedo; scattered_ray};;

let scatter hit_record =
  match hit_record.material.material_type with
  | Diffuse -> scatter_material_diffuse hit_record
  (* | Metal -> empty_scatter_record () *)

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
  let hit_records = List.filter_map (fun sphere -> ray_hits_sphere ray sphere 0.001) spheres in
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
      let scatter_record = scatter hit_record in
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
    let color = ray_color {
      origin = camera.look_from; 
      direction = {
        x = camera.upper_left_corner.x +. viewport_width *. u;
        y = camera.upper_left_corner.y -. viewport_height *. v;
        z = camera.upper_left_corner.z;
      }
    } in
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
