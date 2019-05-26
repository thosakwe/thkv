%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token GET SET PUT
%token NULL
%token EOF

%start <Data.command option> command
%%

command:
  | EOF { None }
  | GET; id = FLOAT { Some (Data.Get (int_of_float id)) }
  | SET; id = FLOAT; v = value { Some (Data.Set (int_of_float id, v)) }
  | PUT; v = value { Some (Data.Put v) }

value:
  | NULL { Data.Null }
  | x = FLOAT { Data.Number x }
  | x = BOOL { Data.Bool x }
  | x = STRING { Data.String x }
