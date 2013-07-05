type typedef =
  | CType of string
  | UnionType of int list list
  | TupleType of int list
;;


type typeform =
  | SimpleTypeForm of int
  | EnumTypeForm of (string, int) Hashtbl.t
  | RecordTypeForm of int * (string, int) Hashtbl.t
  | UnionTypeForm of int * (string, int) Hashtbl.t
;;


type typedeftable = (int, typedef) Hashtbl.t
;;


type ctypetable = (string, int) Hashtbl.t
;;


type typetable = (Instance.typesig, typeform) Hashtbl.t
;;


type typenumtable = (Instance.typesig, int) Hashtbl.t
;;


type fundef = unit
;;


type fundeftable = (int, fundef) Hashtbl.t
;;


type funnumtable = (Instance.funsig, int) Hashtbl.t
;;
