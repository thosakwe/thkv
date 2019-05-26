module StringMap = Map.Make(String)

type value =
  | Number of float
  | String of string
  | Bool of bool
  | Dict of value StringMap.t
  | Array of value list
  | Null

type command =
  | Get of int
  | Set of int * value
  | Put of value

let rec json_of_value value =
  let string_of_pair pair =
    let (k, v) = pair in
    k ^ ":" ^ (json_of_value v)
  in
  match value with
    | Number x -> "\"" ^ (string_of_float x) ^ "\""
    | String x -> "\"" ^ x ^ "\""
    | Bool x -> string_of_bool x
    | Null -> "null"
    | Array items -> "[" ^ (String.concat "," (List.map json_of_value items)) ^ "]"
    | Dict map -> begin
        let pairs = StringMap.bindings map in
        let str_pairs = List.map string_of_pair pairs in
        "{" ^ (String.concat "," str_pairs) ^ "}"
      end
