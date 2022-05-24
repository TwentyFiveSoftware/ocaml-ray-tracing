type vec3 = {
  x: float;
  y: float;
  z: float;
};;

type ray = {
  origin: vec3;
  direction: vec3;
};;

type sphere = {
  center: vec3;
  radius: float;
};;

type camera = {
  origin: vec3;
  upper_left_corner: vec3;
};;

type hit_record = {
  hit: bool;
  t: float;
  point: vec3;
  normal: vec3;
  front_facing: bool;
};;


let (width, height) = (300, 200);;

let spheres: sphere list = [{center = {x = 0.; y = 0.; z = 1.}; radius = 0.5}];;


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

let camera: camera = {origin; upper_left_corner};;



let normalize = fun vec ->
  let length = sqrt(vec.x *. vec.x +. vec.y *. vec.y +. vec.z *. vec.z) in
    {x = vec.x /. length; y = vec.y /. length; z = vec.z /. length};;

let empty_hit_record = fun _ -> 
  {hit = false; t = 0.0; point = {x = 0.0; y = 0.0; z = 0.0}; normal = {x = 0.0; y = 0.0; z = 0.0}; front_facing = true};;

let ray_hits_sphere = fun ray sphere ->
  (* {hit = true; t = 0.0; point = {x = 0.0; y = 0.0; z = 0.0}; normal = {x = 0.0; y = 0.0; z = 0.0}; front_facing = true};; *)
  empty_hit_record ();;

let calculate_ray_collision = fun ray ->
  let hit_records = List.map (fun sphere -> ray_hits_sphere ray sphere) spheres in
  let colliding_hit_records = List.filter (fun hit_record -> hit_record.hit) hit_records in
  let sorted_records = List.sort (fun a b -> if a.t > b.t then 1 else -1) colliding_hit_records in
  match sorted_records with
  | [] -> empty_hit_record ()
  | first::_ -> first;;

let lerp = fun a b t ->
  {x = a.x *. (1.0 -. t) +. b.x *. t;
   y = a.y *. (1.0 -. t) +. b.y *. t;
   z = a.z *. (1.0 -. t) +. b.z *. t}

let ray_color = fun ray -> 
  let hit_record = calculate_ray_collision ray in
  if hit_record.hit then
    hit_record.normal
  else 
    let t = ((normalize ray.direction).y +. 1.0) *. 0.5 in
    lerp {x = 1.0; y = 1.0; z = 1.0} {x = 0.3; y = 0.5; z = 0.8} t;;

let calculate_pixel_color = fun (x, y) -> 
  let (u, v) = (float_of_int x /. float_of_int (width - 1), float_of_int y /. float_of_int (height - 1)) in 
  ray_color {
    origin = camera.origin; 
    direction = {
      x = camera.upper_left_corner.x +. viewport_width *. u;
      y = camera.upper_left_corner.y -. viewport_height *. v;
      z = camera.upper_left_corner.z;
    }
  };;

let rec ray_trace = fun i -> 
  if i >= width * height then []
  else
    calculate_pixel_color (i mod width, i / width) :: ray_trace (i + 1);;

ray_trace 0;;
