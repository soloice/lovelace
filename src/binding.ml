type ctx = {
  typeforminsts: Instance.typeformtable;

  mutable typecount: int;
  typedeftable: Core.typedeftable;
  ctypetable: Core.ctypetable;
  typetable: Core.typetable;
  typenumtable: Core.typenumtable;

  mutable funcount: int;
  funnumtable: Core.funnumtable;
}
;;



let is_enum enums =
  List.for_all
    (
      fun (_, typesigs, _) ->
        match typesigs with
          | [] -> true
          | _ -> false
    )
    enums
;;



let make_enum_table enums =
  let table = Hashtbl.create 0 in
  List.iteri
    (
      fun i (name, _, _) ->
        Hashtbl.add table name i
    )
    enums;
  table
;;



let number_type ctx typesig typeform =
  let (_, typedef, _, _) = typeform in
  match typedef with
    | Instance.AliasType(_, _) ->
      ()
    | Instance.RecordType(_, _) ->
      Hashtbl.add ctx.typenumtable typesig ctx.typecount;
      ctx.typecount <- ctx.typecount + 1;
    | Instance.EnumType(enums, _) ->
      match is_enum enums with
        | true ->
          ()
        | false ->
          Hashtbl.add ctx.typenumtable typesig ctx.typecount;
          ctx.typecount <- ctx.typecount + 1
;;



let rec lookup_typenum ctx typesig =
  match Hashtbl.mem ctx.typenumtable typesig with
    | true ->
      Hashtbl.find ctx.typenumtable typesig
    | false ->
      let (_, typedef, _, _) = Hashtbl.find ctx.typeforminsts typesig in
      match typedef with
        | Instance.AliasType(typesig1, _) ->
          lookup_typenum ctx typesig1
        | _ ->
          assert false
;;



let number_alias ctx typesig typeform =
  let (_, typedef, _, _) = typeform in
  match typedef with
    | Instance.AliasType(typesig1, _) ->
      let num = lookup_typenum ctx typesig1 in
      Hashtbl.add ctx.typenumtable typesig num
    | _ ->
      ()
;;



let make_type ctx typesig typeform =
  let (_, typedef, _, _) = typeform in
  match typedef with
    | Instance.AliasType(_, _) ->
      ()
    | Instance.RecordType(_, _) ->
      ()
    | Instance.EnumType(enums, _) ->
      (
        match is_enum enums with
          | true ->
            let nametable = make_enum_table enums in
            Hashtbl.add ctx.typetable typesig (Core.EnumTypeForm nametable)
          | false ->
            let typenum = Hashtbl.find ctx.typenumtable typesig in
            let nametable = make_enum_table enums in
            let typenumlist =
              List.map
                (
                  fun (_, typesigs, _) ->
                    List.map
                      (Hashtbl.find ctx.typenumtable)
                      typesigs
                )
                enums
            in
            Hashtbl.add ctx.typedeftable typenum (Core.UnionType typenumlist);
            Hashtbl.add ctx.typetable typesig (Core.RecordTypeForm(typenum, nametable))
      )
;;



let number_fun ctx funsig funform =
  Hashtbl.add ctx.funnumtable funsig ctx.funcount;
  ctx.funcount <- ctx.funcount + 1
;;


let make_fun ctx funsig funform =
  let (_, _, _, _, _) = funform in
  ()
;;



let bind instancetable =
  let (typeforminsts, funforminsts) = instancetable in
  let ctx =
    {
      typeforminsts = typeforminsts;

      typecount = 0;
      typedeftable = Hashtbl.create 0;
      ctypetable = Hashtbl.create 0;
      typetable = Hashtbl.create 0;
      typenumtable = Hashtbl.create 0;

      funcount = 0;
      funnumtable = Hashtbl.create 0;
    }
  in

  Hashtbl.add ctx.typedeftable ctx.typecount (Core.CType "int");
  Hashtbl.add ctx.ctypetable "int" ctx.typecount;
  ctx.typecount <- ctx.typecount + 1;

  Hashtbl.iter (number_type ctx) typeforminsts;
  Hashtbl.iter (number_alias ctx) typeforminsts;
  Hashtbl.iter (make_type ctx) typeforminsts;

  Hashtbl.iter (number_fun ctx) funforminsts;
  Hashtbl.iter (make_fun ctx) funforminsts;
  ()
;;
