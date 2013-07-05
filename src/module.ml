open Template;;


type ctx = {
  mutable loaded: string list;
  types: (string, typeform) Hashtbl.t;
  funs: (string, funform) Hashtbl.t;
};;



let rec find_module paths name =
  match paths with
    | [] -> None
    | head :: tail ->
      let filename = head ^ "/" ^ name in
      match Sys.file_exists filename with
        | true ->
          Some(filename)
        | false ->
          find_module tail name
;;



let parse_module find name =
  match find name with
    | None -> raise (Failure "not found")
    | Some(filename) ->
      let chan = open_in filename in
      try
        let forms =
          Parser.program
            Lexer.token
            (Lexing.from_channel chan)
        in
        close_in chan;
        forms
      with e ->
        close_in chan;
        raise e
;;



let rec load_module parse ctx name =
  ctx.loaded <- ctx.loaded @ [name];
  let forms = parse name in
  List.iter
    (
      fun form ->
        match form with
          | UseForm(modname, pos) ->
            load_module parse ctx modname
          | TypeForm((typename, args), body, pos) ->
            Hashtbl.add
              ctx.types
              (typename ^ "/" ^ (string_of_int (List.length args)))
              (typename, args, body, name, pos)
          | FunForm(funname, args, res, guards, clauses, pos) ->
            Hashtbl.add
              ctx.funs
              (funname ^ "/" ^ (string_of_int (List.length args)))
              (funname, args, res, guards, clauses, name, pos)
    )
    forms
;;



let load_modules paths name =
  let find = find_module paths in
  let parse = parse_module find in
  let load = load_module parse in
  let ctx =
    {
      loaded = [];
      types = Hashtbl.create 0;
      funs = Hashtbl.create 0;
    }
  in
  load ctx name;
  (ctx.types, ctx.funs)
;;
