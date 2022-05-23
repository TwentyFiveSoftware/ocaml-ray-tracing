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


let ray_color = fun ray -> ((normalize ray.direction).y +. 1.0) *. 0.5;;

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
