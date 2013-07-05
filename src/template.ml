type position = {
  lineno: int;
  column: int;
}
;;


type binop =
  | Match
  | ExactEqual
  | NotExactEqual
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | Assign
  | Plus
  | Minus
  | Times
  | Divide
;;


type typename = string * typeparam list
and typeparam =
  | IgnoreParam of position
  | VarParam of string * position
  | NameParam of typename * position
;;


type record = string * typeparam * position
;;


type enum = string * typeparam list * position
;;


type typedef =
  | AliasType of typename * position
  | RecordType of record list * position
  | EnumType of enum list * position
;;


type expression =
  | IgnoreExpr of position
  | StringExpr of string * position
  | VarExpr of string * position
  | EnumExpr of string * typeparam option * expression list * position
  | NewRecordExpr of typeparam option * (string * expression) list * position
  | DeriveRecordExpr of expression * (string * expression) list * position
  | AttribExpr of expression * string * position
  | BinOpExpr of binop * expression * expression * position
  | CallNameExpr of string * expression list * position
  | CallValueExpr of expression * expression list * position
  | IfExpr of (expression list * expression list) list * position
  | CaseExpr of expression * (expression * expression list * expression list) list * position
  | FunExpr of string option * typeparam list * typeparam * expression list * clause list * position
  | FunRefExpr of string * typeparam list * position
and clause = expression list * expression list * expression list * position
;;


type form =
  | UseForm of string * position
  | TypeForm of typename * typedef * position
  | FunForm of string * typeparam list * typeparam * expression list * clause list * position
;;


type typeform = string * typeparam list * typedef * string * position
;;


type funform = string * typeparam list * typeparam * expression list * clause list * string * position
;;


type typeformtable = (string, typeform) Hashtbl.t
;;


type funformtable = (string, funform) Hashtbl.t
;;


type formtable = typeformtable * funformtable
;;
