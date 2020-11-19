(** PHP code generation *)

open Printf
open ExtLib
open Sqlgg

open Gen
open Sql

module G = Gen_cxx

let comment = G.comment
let empty_line = G.empty_line

let quote = String.replace_chars (function '\n' -> "\" .\n\"" | '\r' -> "" | '"' -> "\\\"" | c -> String.make 1 c)
let quote s = "\"" ^ quote s ^ "\""

let start_stm name =
  output "else if ($_GET['_name'] == %S)" name;
  G.open_curly ()

let end_stm name =
  G.close_curly " // %s" name

let as_pdo_type = function
  | Type.Int -> "INT"
  | Float -> "FLOAT"
  | Blob -> "LOB" (* I guess *)
  | Bool -> "BOOL"
  | Decimal -> "FLOAT"
  | Datetime -> "STR"
  | Unit _ -> assert false
  | _ -> "STR"

let as_filter = function
  | Type.Int -> "INT"
  | Float -> "FLOAT"
  | Blob -> failwith "unsupported blob type"
  | Bool -> "BOOLEAN"
  | Decimal -> "FLOAT"
  | Datetime -> ""
  | Unit _ -> assert false
  | _ -> ""

module L = struct

(* FIXME? *)
let as_lang_type = function
  | Type.Int -> "(int)"
  | Float -> "(float)"
  | Blob -> failwith "unsupported blob type"
  | Bool -> "(bool)"
  | Decimal -> "(float)"
  | Datetime -> ""
  | Unit _ -> assert false
  | _ -> ""

let as_api_type = as_lang_type (* unused *)

end

module T = Translate(L)

open L
open T

let get_column index attr =
  sprintf "%s => is_null($row[%u]) ? null : %s$row[%u]"
    (attr.name |> quote)
    index
    (attr.domain |> as_api_type)
    index

let set_param inp _ index param =
  output "$stm->bindValue(%u, isset($_%s[%s]) ? %s$_%s[%s] : null);"
    (index+1)
    inp (quote (make_param_name index param.id))
    (param.typ |> as_lang_type) inp (quote (make_param_name index param.id))

let output_params_binder inp name _ params =
  List.iteri (set_param inp name) params;
  output "$stm->execute();"

type t = unit

let start () =
  output "<?php"

let generate_code index stmt =
   let params = params_only stmt.vars in
   let _values = G.Values.inject @@ values_of_params params in
   let name = choose_name stmt.props stmt.kind index in
   let sql = quote (get_sql_string_only stmt) in
   let inp =
      match stmt.schema with
      | [] -> "POST"
      | _ -> "GET"
   in
   start_stm name;
   output "$stm = $db->prepare(%s);" sql;
   output_params_binder inp name index params;
   begin match stmt.schema with
   | [] ->
      output "echo '[]';"
   | _ ->
      let args = List.mapi get_column stmt.schema in
      let args = String.concat ", " args in
      output "echo '[';";
      output "$first = true;";
      output "while ($row = $stm->fetch())";
      G.open_curly ();
      output "if (!$first) echo ',';";
      output "echo json_encode(array(%s));" args;
      output "$first = false;";
      G.close_curly "";
      output "echo ']';"
   end;
   end_stm name

let generate () _ stmts =
  params_mode := Some Unnamed; (* allow only unnamed params *)
  output "require('db.php');";
  output "$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);";
  output "$db->setAttribute(PDO::ATTR_DEFAULT_FETCH_MODE, PDO::FETCH_NUM);";
  empty_line ();
  output "header('Content-type', 'application/json');";
  output "if (!isset($_GET['_name'])) die('{\"error\": \"missing parameter\"}');";
  List.iteri generate_code stmts;
  output "else die('{\"error\": \"invalid parameter\"}');"
