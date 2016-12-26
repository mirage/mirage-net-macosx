#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let opam =
  let nolint = ["mirage-net-lwt"] in
  Pkg.opam_file ~lint_deps_excluding:(Some nolint) "opam"

let () =
  Pkg.describe ~opams:[opam] "mirage-net-macosx" @@ fun c ->
  Ok [
    Pkg.mllib "src/mirage-net-macosx.mllib";
  ]
