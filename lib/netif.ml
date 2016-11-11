(*
 * Copyright (c) 2010-2014 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix
open Printf

type +'a io = 'a Lwt.t
type id = string

(** IO operation errors *)
type error = [
  | `Unknown of string (** an undiagnosed error *)
  | `Unimplemented     (** operation not yet implemented in the code *)
  | `Disconnected      (** the device has been previously disconnected *)
]

type stats = {
  mutable rx_bytes : int64;
  mutable rx_pkts : int32;
  mutable tx_bytes : int64;
  mutable tx_pkts : int32;
}

type t = {
  id: id;
  buf_sz: int;
  mutable active: bool;
  mac: Macaddr.t;
  stats : stats;
  dev: Lwt_vmnet.t;
}

let devices = Hashtbl.create 1

let connect _ =
  Lwt_vmnet.init () >|= fun dev ->
  let devname = "unknown" in (* TODO fix *)
  let mac = Lwt_vmnet.mac dev in
  let active = true in
  let buf_sz = 4096 in (* TODO get from vmnet *)
  let t = {
    id=devname; dev; active; mac; buf_sz;
    stats= { rx_bytes=0L;rx_pkts=0l;
             tx_bytes=0L; tx_pkts=0l }; } in
  Hashtbl.add devices devname t;
  printf "Netif: connect %s\n%!" devname;
  t

let disconnect t =
  printf "Netif: disconnect %s\n%!" t.id;
  (* TODO *)
  Lwt.return_unit

type macaddr = Macaddr.t
type page_aligned_buffer = Io_page.t
type buffer = Cstruct.t

(* Input a frame, and block if nothing is available *)
let read t page =
  Lwt.catch (fun () -> Lwt_vmnet.read t.dev page >|= fun c -> `Ok c)
    (function
      | Lwt_vmnet.Error e ->
        Printf.printf "read: %s\n%!"
          (Sexplib.Sexp.to_string_hum (Lwt_vmnet.sexp_of_error e));
        Lwt.return (`Error `Disconnected)
      | e -> Lwt.fail e)

(* Loop and listen for packets permanently *)
let rec listen t fn =
  match t.active with
  | true ->
    Lwt.catch (fun () ->
        let page = Io_page.get_buf () in
        read t page >>= function
        | `Error _ ->
          printf "Netif: error, terminating listen loop\n%!";
          Lwt.return ()
        | `Ok buf ->
          Lwt.ignore_result (
            Lwt.catch (fun () -> fn buf)
              (fun exn ->
                 Lwt.return
                   (printf "EXN: %s bt: %s\n%!" (Printexc.to_string exn)
                      (Printexc.get_backtrace())))
          );
          listen t fn
      ) (function exn ->
        eprintf "[netif-input] error : %s\n%!" (Printexc.to_string exn);
        listen t fn)
  | false -> Lwt.return_unit

(* Transmit a packet from an Io_page *)
let write t page =
  Lwt_vmnet.write t.dev page >|= fun () ->
  t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
  t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int page.Cstruct.len)

(* TODO use writev: but do a copy for now *)
let writev t pages =
  match pages with
  | []     -> Lwt.return_unit
  | [page] -> write t page
  | pages  ->
    let page = Io_page.(to_cstruct (get 1)) in
    let off = ref 0 in
    List.iter (fun p ->
        let len = Cstruct.len p in
        Cstruct.blit p 0 page !off len;
        off := !off + len;
      ) pages;
    let v = Cstruct.sub page 0 !off in
    write t v

let mac t = t.mac

let get_stats_counters t = t.stats

let reset_stats_counters t =
  t.stats.rx_bytes <- 0L;
  t.stats.rx_pkts  <- 0l;
  t.stats.tx_bytes <- 0L;
  t.stats.tx_pkts  <- 0l
