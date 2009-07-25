(* Code generation *)

open Printf
open ExtList
open ExtString
open Operators
open Stmt

type subst_mode = | Named | Unnamed | Oracle

(** defines substitution function for parameter literals *)
let params_mode = ref None

let (inc_indent,dec_indent,make_indent) =
  let v = ref 0 in
  (fun () -> v := !v + 2),
  (fun () -> v := !v - 2),
  (fun () -> String.make !v ' ')

let print_indent () = print_string (make_indent ())
let indent s = print_indent (); print_string s
let indent_endline s = print_indent (); print_endline s
let output fmt = kprintf indent_endline fmt
let output_l = List.iter indent_endline
let print fmt = kprintf print_endline fmt
let indented k = inc_indent (); k (); dec_indent ()

let name_of attr index =
  match attr.RA.name with
  | "" -> sprintf "_%u" index
  | s -> s

let param_name_to_string ((name,_):param_id) index =
  match name with
  | None -> sprintf "_%u" index
  | Some s -> s

let make_name props default = Option.default default (Props.get props "name")
let default_name str index = sprintf "%s_%u" str index

let choose_name props kind index =
  let name = match kind with
  | Create t -> sprintf "create_%s" t
  | CreateIndex t -> sprintf "create_index_%s" t
  | Update (Some t) -> sprintf "update_%s_%u" t index
  | Update None -> sprintf "update_%u" index
  | Insert (_,t) -> sprintf "insert_%s_%u" t index
  | Delete t -> sprintf "delete_%s_%u" t index
  | Alter t -> sprintf "alter_%s_%u" t index
  | Drop t -> sprintf "drop_%s" t
  | Select _  -> sprintf "select_%u" index
  | Other -> sprintf "statement_%u" index
  in
  make_name props name

let substitute_params s params f =
  let index = ref 0 in
  let b = Buffer.create (String.length s) in
  let last = List.fold_left (fun i ((_,(i1,i2)),_ as param) ->
    let prefix = String.slice ~first:i ~last:i1 s in
    Buffer.add_string b prefix;
    Buffer.add_string b (f !index param);
    incr index;
    i2) 0 params in
  Buffer.add_string b (String.slice ~first:last s);
  Buffer.contents b

let subst_named index (id,_) = "@" ^ (param_name_to_string id index)
let subst_oracle index (id,_) = ":" ^ (param_name_to_string id index)
let subst_unnamed _ _ = "?"

let get_sql stmt =
  let sql = Props.get stmt.props "sql" >> Option.get in
  match !params_mode with
  | None -> sql
  | Some subst ->
    let f = match subst with 
    | Named -> subst_named
    | Unnamed -> subst_unnamed
    | Oracle -> subst_oracle 
    in
    substitute_params sql stmt.params f

let time_string () = 
  let module U = Unix in
  let t = U.time () >> U.gmtime in
  sprintf "%04u-%02u-%02uT%02u:%02uZ" (1900 + t.U.tm_year) (t.U.tm_mon+1) t.U.tm_mday t.U.tm_hour t.U.tm_min

module type LangTypes = sig

val as_api_type : Sql.Type.t -> string
val as_lang_type : Sql.Type.t -> string

end

module Translate(T : LangTypes) = struct

let param_type_to_string = T.as_api_type
let schema_to_values = List.mapi (fun i attr -> name_of attr i, T.as_lang_type attr.RA.domain)
(* let schema_to_string = G.Values.to_string & schema_to_values  *)
let all_params_to_values = List.mapi (fun i (n,t) -> param_name_to_string n i, T.as_lang_type t)
let params_to_values = List.unique & all_params_to_values

end

module type Generator = sig
  type t
  val generate : t -> string -> Stmt.t Enum.t -> unit
  val start : unit -> t
  val comment : t -> ('a,unit,string,unit) format4 -> 'a
  val empty_line : t -> unit
end

module Make(S : Generator) = struct

let generate_header out =
  S.comment out "DO NOT EDIT MANUALLY";
  S.comment out "";
  S.comment out "generated by sqlgg %s on %s" Config.version (time_string ());
  S.comment out "visit http://ygrek.org.ua/p/sqlgg/";
  S.empty_line out

let process name stmts =
  let out = S.start () in
  generate_header out;
  S.generate out name stmts

end

