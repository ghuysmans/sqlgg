(** JS code generation *)

open ExtLib
open Sqlgg

open Gen
open Sql

module G = Gen_cxx

let comment = G.comment
let empty_line = G.empty_line

let quote = Gen_java.quote

let set_param index param =
  let name = make_param_name index param.id in
  output "if (%s != null) _params.%s = %s;" name name name

let output_params_binder _ params = List.iteri set_param params

type t = unit

let start () = ()

module T = Translate(struct
  let as_lang_type _ = ""
  let as_api_type _ = ""
end)
open T

let generate_code index stmt =
   let params = params_only stmt.vars in
   let values = G.Values.inject @@ values_of_params params in
   let name = choose_name stmt.props stmt.kind index in
   G.func "function" name values (fun () ->
      output "var _params = {};";
      output_params_binder index params;
      output "_params._name = %s;" (quote name);
      output "return my_fetch(_params);"
      (* FIXME handle errors *)
   );
   empty_line ()

let generate () _name stmts =
  params_mode := Some Unnamed; (* allow only unnamed params *)
  G.func "function" "my_fetch" ["o", ""] (fun () ->
    output "return fetch(endpoint + '?' + new URLSearchParams(o).toString()).then(x => x.json());"
  );
  empty_line ();
  List.iteri generate_code stmts
