# Ray Tracing

<img src="https://github.com/TwentyFiveSoftware/ray-tracing-gpu/blob/master/sceneRender.png">

## Overview

This is my take on [Peter Shirley's Ray Tracing in One Weekend](https://github.com/RayTracing/raytracing.github.io) book.

This project uses the [OCaml](https://ocaml.org/) programming language, which is a general-purpose, functional programming language.

## Build & Run this project

1. [Install OCaml (including the opam package manager)](https://ocaml.org/install)
2. Switch to a OCaml version that has the flambda compiler enabled (the `ocaml-option-flambda` option)
    Check the [releases page](https://ocaml.org/releases) for the newest verson
    ```sh
    opam switch create flambda ocaml-variants.5.1.0+options ocaml-option-flambda
    ```
3. Clone the repository
4. Optional: Change sample and thread count in `bin/ray_tracing.ml`
5. Install dependencies
   ```sh
   opam install . --deps-only
   ```
6. Build the project
   ```sh
   dune build --release
   ```
7. Run the executable
   ```sh
	./bin/ray_tracing.exe
   ```


## Performance

I've already implemented Peter Shirley's ray tracing in various programming languages running on CPU & GPU and compared their performance.

The performance was measured on the same scene (see image above) with the same amount of objects, the same recursive
depth, the same resolution (1920 x 1080). The measured times are averaged over multiple runs.

*Reference system: AMD Ryzen 9 5900X (12 Cores / 24 Threads) | AMD Radeon RX 6800 XT*

|                                                                                                    | 1 sample / pixel | 100 samples / pixel | 
|----------------------------------------------------------------------------------------------------|-----------------:|--------------------:|
| [Elixir](https://github.com/TwentyFiveSoftware/elixir-ray-tracing)                                 |        67,200 ms |                 N/A |
| [JavaScript - Node.js](https://github.com/TwentyFiveSoftware/javascript-ray-tracing)               |         4,870 ms |               308 s |
| [Go](https://github.com/TwentyFiveSoftware/go-ray-tracing)                                         |         1,410 ms |               142 s |
| [OCaml](https://github.com/TwentyFiveSoftware/ocaml-ray-tracing)                                   |           795 ms |                75 s |
| [Java](https://github.com/TwentyFiveSoftware/java-ray-tracing)                                     |           770 ms |                59 s |
| [C++](https://github.com/TwentyFiveSoftware/ray-tracing)                                           |           685 ms |                70 s |
| [Rust](https://github.com/TwentyFiveSoftware/rust-ray-tracing)                                     |           362 ms |                36 s |
| [C](https://github.com/TwentyFiveSoftware/c-ray-tracing)                                           |           329 ms |                33 s |
| [GPU - Compute Shader](https://github.com/TwentyFiveSoftware/ray-tracing-gpu)                      |            21 ms |                 2 s |
| [GPU - Vulkan Ray Tracing Extension](https://github.com/TwentyFiveSoftware/ray-tracing-gpu-vulkan) |             1 ms |               0.1 s |
