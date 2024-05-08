include Vec3
include Renderer
include Scene
include Camera
include ImageUtil

let width, height = (1920, 1080)
let max_ray_recursive_depth = 50
let samples_per_pixel = 1

let camera =
  Camera.new_camera
    { Vec3.x = 12.0; y = 2.0; z = -3.0 }
    Vec3.zero 25.0 width height

let scene = Scene.generate_scene ()

let renderer =
  {
    Renderer.width;
    height;
    samples_per_pixel;
    max_ray_recursive_depth;
    camera;
    scene;
  }

let img : Image.image = Image.create_rgb width height ~alpha:false ~max_val:255
let start_time_ms = int_of_float (Unix.gettimeofday () *. 1000.0)
let pixels = Renderer.render renderer
let end_time_ms = int_of_float (Unix.gettimeofday () *. 1000.0);;

Printf.printf "Rendered %d samples/pixel with %d threads in %d ms\n"
  samples_per_pixel 1
  (end_time_ms - start_time_ms)
;;

ImageUtil.write_pixels_to_image img pixels width height;;
ImageLib.PNG.write (ImageUtil_unix.chunk_writer_of_path "render.png") img
