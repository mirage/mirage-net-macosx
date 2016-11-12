#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mirage-net-macosx" @@ fun c ->
  Ok [
    Pkg.mllib "lib/mirage-net-macosx.mllib";
  ]
