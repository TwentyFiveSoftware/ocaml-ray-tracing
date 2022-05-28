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
  hit: bool;
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


let (width, height) = (300, 200);;
let max_ray_recursive_depth = 50;;

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



let vec_length_squared = fun vec ->
  vec.x *. vec.x +. vec.y *. vec.y +. vec.z *. vec.z;;

let vec_length = fun vec -> sqrt(vec_length_squared(vec));;

let normalize = fun vec ->
  let length = vec_length vec in
    {x = vec.x /. length; y = vec.y /. length; z = vec.z /. length};;

let vec_neg = fun vec ->
  {x = -.vec.x; y = -.vec.y; z = -.vec.z};;
      
let vec_add = fun a b ->
  {x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z};;

let vec_sub = fun a b ->
  {x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z};;

let vec_mul_scalar = fun vec scalar ->
  {x = vec.x *. scalar; y = vec.y *. scalar; z = vec.z *. scalar};;

let vec_mul = fun a b ->
  {x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z};;
  
let vec_dot = fun a b ->
  a.x *. b.x +. a.y *. b.y +. a.z *. b.z;;

let vec_zero = fun _ -> {x = 0.0; y = 0.0; z = 0.0};;

let ray_at = fun ray t ->
  vec_add ray.origin (vec_mul_scalar ray.direction t);;

(* let empty_ray = fun _ -> {origin = vec_zero (); direction = vec_zero ()};; *)

let empty_hit_record = fun _ -> 
  {hit = false; t = 0.0; point = vec_zero (); normal = vec_zero (); front_face = true;
  material = {material_type = Diffuse; albedo = vec_zero ()}};;

(* let empty_scatter_record = fun _ ->
  {does_scatter = false; attenuation = vec_zero (); scattered_ray = empty_ray ()};; *)

let scatter_material_diffuse = fun hit_record ->
  let scattered_ray = {origin = hit_record.point; direction = hit_record.normal} in
  {does_scatter = true; attenuation = hit_record.material.albedo; scattered_ray};;

(* let scatter = fun ray hit_record -> *)
let scatter = fun hit_record ->
  match hit_record.material.material_type with
  | Diffuse -> scatter_material_diffuse hit_record
  (* | Metal -> empty_scatter_record () *)

let ray_hits_sphere = fun ray sphere t_min ->
  let oc = vec_sub ray.origin sphere.center in
  let a = vec_length_squared ray.direction in
  let half_b = vec_dot oc ray.direction in
  let c = vec_length_squared oc -. sphere.radius *. sphere.radius in
  let discriminant = half_b *. half_b -. a *. c in
  
  if discriminant < 0. then
    empty_hit_record ()
  else 
    let sqrt_d = sqrt discriminant in
    let root_1 = (-.half_b -. sqrt_d) /. a in
    let root_2 = (-.half_b +. sqrt_d) /. a in
    let t = if root_1 > t_min && root_1 < root_2 then root_1 else if root_2 > t_min then root_2 else -1.0 in
    if t < 0.0 then
      empty_hit_record ()
    else
      let point = ray_at ray t in
      let outward_normal = vec_mul_scalar (vec_sub point sphere.center) (1.0 /. sphere.radius) in
      let front_face = vec_dot ray.direction outward_normal < 0.0 in
      let normal = if front_face then outward_normal else vec_neg outward_normal in
      {hit = true; t; point; normal; front_face; material = sphere.material};;


let calculate_ray_collision = fun ray ->
  let hit_records = List.map (fun sphere -> ray_hits_sphere ray sphere 0.001) spheres in
  let colliding_hit_records = List.filter (fun hit_record -> hit_record.hit && hit_record.t > 0.001) hit_records in
  let sorted_records = List.sort (fun a b -> if a.t > b.t then 1 else -1) colliding_hit_records in
  match sorted_records with
  | [] -> empty_hit_record ()
  | first::_ -> first;;

let lerp = fun a b t ->
  {x = a.x *. (1.0 -. t) +. b.x *. t;
   y = a.y *. (1.0 -. t) +. b.y *. t;
   z = a.z *. (1.0 -. t) +. b.z *. t}

let rec ray_color = fun ray depth -> 
  if depth >= max_ray_recursive_depth then
    vec_zero ()
  else
    let hit_record = calculate_ray_collision ray in
    if hit_record.hit then
      (* let scatter_record = scatter ray hit_record in *)
      let scatter_record = scatter hit_record in
      if scatter_record.does_scatter then
        vec_mul scatter_record.attenuation (ray_color scatter_record.scattered_ray (depth + 1))
      else
        vec_zero ()
    else 
      let t = ((normalize ray.direction).y +. 1.0) *. 0.5 in
      lerp {x = 1.0; y = 1.0; z = 1.0} {x = 0.5; y = 0.7; z = 1.0} t;;

let calculate_pixel_color = fun (x, y) -> 
  let (u, v) = (float_of_int x /. float_of_int (width - 1), float_of_int y /. float_of_int (height - 1)) in 
  ray_color {
    origin = camera.look_from; 
    direction = {
      x = camera.upper_left_corner.x +. viewport_width *. u;
      y = camera.upper_left_corner.y -. viewport_height *. v;
      z = camera.upper_left_corner.z;
    }
  } 0;;

let rec ray_trace = fun ?(i=0) img -> 
  if i >= width * height then ()
  else
    let (x, y) = (i mod width, i / width) in
    let raw_color = calculate_pixel_color (x, y) in
    let color = vec_mul_scalar raw_color 255.0 in
    let (r, g, b) = (int_of_float color.x, int_of_float color.y, int_of_float color.z) in
    let _ = Image.write_rgb img x y r g b in
    ray_trace img ~i: (i + 1);;

    
let img: Image.image = Image.create_rgb width height ~alpha: false ~max_val: 255;;
ray_trace img;;
ImageLib.PNG.write (ImageUtil_unix.chunk_writer_of_path "render.png") img;;
