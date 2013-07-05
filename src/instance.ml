type typesig = string * typeparam list
and typeparam =
  | NameParam of typesig
;;


type funsig = string * typesig list
;;


type typetable = (string, typeparam) Hashtbl.t
;;


type vartype =
  | StaticVar of typeparam
  | Var of typesig
;;


type symtable = (string, vartype) Hashtbl.t
;;


type scope =
  | FunScope of symtable
  | ClauseScope of scope * symtable
  | CaseScope of scope * symtable
  | LambdaScope of scope * symtable
;;


type record = string * typesig * Template.position
;;


type enum = string * typesig list * Template.position
;;


type typedef =
  | AliasType of typesig * Template.position
  | RecordType of record list * Template.position
  | EnumType of enum list * Template.position
;;


type expression = typesig * Template.position * exp
and exp =
  | IgnoreExp
  | StringExp of string
  | VarExp of string
  | EnumExp of string * expression list
  | NewRecordExp of (string * expression) list
  | DeriveRecordExp of expression * (string * expression) list
  | AttribExp of expression * string
  | CallExp of expression * expression list
  | IfExp of (expression list * expression list) list
  | CaseExp of expression * (expression * expression list * expression list) list
  | FunExp of string option * string list * typesig list * typesig * expression list * clause list
  | FunRefExp of string * typesig list
and clause = expression list * expression list * expression list * Template.position
;;


type typeform = typesig * typedef * string * Template.position
;;


type funform = funsig * typesig * clause list * string * Template.position
;;


type typeformtable = (typesig, typeform) Hashtbl.t
;;


type funformtable = (funsig, funform) Hashtbl.t
;;


type formtable = typeformtable * funformtable
;;
