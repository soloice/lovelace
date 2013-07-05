type ctx = {
  typeformtpls_by_name: Template.typeformtable;
  funformtpls_by_name: Template.funformtable;

  typeformtpls_by_sig: (Instance.typesig, Template.typeform * Instance.typetable) Hashtbl.t;
  funformtpls_by_sig: (Instance.funsig, Template.funform * Instance.typesig * Instance.typetable) Hashtbl.t;

  typeforminsts: Instance.typeformtable;
  funforminsts: Instance.funformtable;
};;



let rec make_typeparam table param =
  match param with
    | Template.IgnoreParam(_) ->
      None
    | Template.VarParam(name, _) ->
      (
        match Hashtbl.mem table name with
          | false ->
            None
          | true ->
            Some(Hashtbl.find table name)
      )
    | Template.NameParam((name, params), _) ->
      match make_typeparams table params with
        | None ->
          None
        | Some(types) ->
          Some(Instance.NameParam((name, types)))
and make_typeparams table params =
  match params with
    | [] ->
      Some([])
    | h :: t ->
      match make_typeparam table h with
        | None ->
          None
        | Some(param) ->
          match make_typeparams table t with
            | None ->
              None
            | Some(params) ->
              Some(param :: params)
;;



let make_typesig_from_param table param =
  match make_typeparam table param with
    | None ->
      None
    | Some(t) ->
      match t with
        | Instance.NameParam(typesig) ->
          Some(typesig)
;;



let make_typesig table typename =
  let (name, params) = typename in
  match make_typeparams table params with
    | None -> assert false
    | Some(types) ->
      (name, types)
;;



let rec match_typeparam table t1 t2 =
  match t2 with
    | Template.IgnoreParam(_) ->
      true
    | Template.VarParam(name, _) ->
      (
        match Hashtbl.mem table name with
          | false ->
            Hashtbl.add table name t1;
            true
          | true ->
            let t0 = Hashtbl.find table name in
            t0 = t1
      )
    | Template.NameParam((name2, params2), _) ->
      (
        match t1 with
          | Instance.NameParam((name1, params1)) ->
            (
              match name1 = name2 with
                | false ->
                  false
                | true ->
                  match_typeparams table params1 params2
            )
      )
and match_typeparams table l1 l2 =
  match (l1, l2) with
    | ([], []) ->
      true
    | (h1 :: t1, h2 :: t2) ->
      (
        match match_typeparam table h1 h2 with
          | false ->
            false
          | true ->
            match_typeparams table t1 t2
      )
    | _ ->
      false
;;



let match_typeform typesig typeform =
  let (name1, params1) = typesig in
  let (name2, params2, _, _, _) = typeform in
  let table = Hashtbl.create 0 in
  match name1 = name2 with
    | false ->
      None
    | true ->
      match match_typeparams table params1 params2 with
        | false ->
          None
        | true ->
          Some((typeform, table))
;;



let match_funform argtypes1 funform =
  let argtypes2 = List.map (fun x -> Instance.NameParam(x)) argtypes1 in
  let (name, argtypes3, restype, _, _, _, _) = funform in
  let table = Hashtbl.create 0 in
  match match_typeparams table argtypes2 argtypes3 with
    | false ->
      None
    | true ->
      match make_typesig_from_param table restype with
        | None ->
          None
        | Some(typesig) ->
          Some((funform, typesig, table))
;;



let find_typeform typeforms typesig =
  let (name, params) = typesig in
  let fullname = name ^ "/" ^ (string_of_int (List.length params)) in
  let typeforms = Hashtbl.find_all typeforms fullname in
  let found =
    List.concat
      (
        List.map
          (
            fun x ->
              match x with
                | None -> []
                | Some((a,b)) -> [(a,b)]
          )
          (List.map (match_typeform typesig) typeforms)
      )
  in
  match found with
    | [] -> 
      assert false
    | [x] ->
      x
    | _ ->
      assert false
;;



let find_funform funforms funsig =
  let (name, argtypes) = funsig in
  let fullname = name ^ "/" ^ (string_of_int (List.length argtypes)) in
  let funforms = Hashtbl.find_all funforms fullname in
  let found =
    List.concat
      (
        List.map
          (
            fun x ->
              match x with
                | None -> []
                | Some(node) -> [node]
          )
          (List.map (match_funform argtypes) funforms)
      )
  in
  match found with
    | [] -> assert false
    | [x] ->
      x
    | _ -> assert false
;;



let make_symtable typetable =
  let symtable = Hashtbl.create 0 in
  Hashtbl.iter
    (
      fun v t ->
        Hashtbl.add symtable v (Instance.StaticVar(t))
    )
    typetable;
  symtable
;;



let shallow_lookup scope name =
  let table =
    match scope with
      | Instance.FunScope(table) -> table
      | Instance.ClauseScope(_, table) -> table
      | Instance.CaseScope(_, table) -> table
      | Instance.LambdaScope(_, table) -> table
  in
  match Hashtbl.mem table name with
    | false -> None
    | true->
      Some(Hashtbl.find table name)
;;



let rec lookup scope name =
  match shallow_lookup scope name with
    | Some(x) ->
      Some(x)
    | None ->
      match scope with
        | Instance.FunScope(table) ->
          None
        | Instance.ClauseScope(parent, _) ->
          lookup parent name
        | Instance.CaseScope(parent, _) ->
          lookup parent name
        | _ ->
          assert false
;;



let rec lookup_typeparam scope typeparam =
  match typeparam with
    | Template.IgnoreParam(_) ->
      assert false
    | Template.NameParam((name, params), _) ->
      Instance.NameParam((name, (List.map (lookup_typeparam scope) params)))
    | Template.VarParam(name, _) ->
      match lookup scope name with
        | Some(Instance.StaticVar(param)) ->
          param
        | _ ->
          assert false
and lookup_typesig scope typeparam =
  match lookup_typeparam scope typeparam with
    | Instance.NameParam(typesig) ->
      typesig
;;



let check_typesig typereq typesigopt =
  match typereq with
    | None ->
      (
        match typesigopt with
          | None -> assert false
          | Some(typesig) -> typesig
      )
    | Some(typesigreq) ->
      match typesigopt with
        | None ->
          typesigreq
        | Some(typesig) when typesig = typesigreq ->
          typesig
        | _ ->
          assert false
;;



let lookup_typesigopt scope typeparamopt =
  match typeparamopt with
    | None -> None
    | Some(typeparam) ->
      Some(lookup_typesig scope typeparam)
;;



let rec find_enum enums name =
  match enums with
    | [] -> assert false
    | (name1, typesigs, _) :: _ when name = name1 ->
      typesigs
    | _ :: tail ->
      find_enum tail name
;;



let rec instantiate_record ctx typetable record =
  let (name, param, pos) = record in
  match make_typesig_from_param typetable param with
    | None -> assert false
    | Some(typesig) ->
      check_typeform ctx typesig;
      (name, typesig, pos)
and instantiate_enum ctx typetable enum =
  let (name, typeparams, pos) = enum in
  let typesigs =
    List.map
      (
        fun x ->
          match make_typesig_from_param typetable x with
            | None -> assert false
            | Some(typesig) ->
              check_typeform ctx typesig;
              typesig
      )
      typeparams
  in
  (name, typesigs, pos)
and instantiate_typedef ctx typesig =
  let (typeformtpl, typetable) = Hashtbl.find ctx.typeformtpls_by_sig typesig in
  let (_, _, typedeftpl, filename, position) = typeformtpl in
  let typedefinst =
    (
      match typedeftpl with
        | Template.AliasType(param, pos) ->
          let typesig1 = make_typesig typetable param in
          check_typeform ctx typesig1;
          Instance.AliasType(typesig1, pos)
        | Template.RecordType(records, pos) ->
          Instance.RecordType(
            (List.map (instantiate_record ctx typetable) records),
            pos
          )
        | Template.EnumType(enums, pos) ->
          Instance.EnumType(
            (List.map (instantiate_enum ctx typetable) enums),
            pos
          )
    )
  in
  (typesig, typedefinst, filename, position)
and instantiate_typeform ctx typesig =
  let (name, params) = typesig in
  let typeformtpl = find_typeform ctx.typeformtpls_by_name typesig in
  Hashtbl.add ctx.typeformtpls_by_sig typesig typeformtpl;
  List.iter
    (
      fun param ->
        match param with
          | Instance.NameParam(typesig1) ->
            check_typeform ctx typesig1
    )
    params;
  let typeforminst = instantiate_typedef ctx typesig in
  Hashtbl.add ctx.typeforminsts typesig typeforminst
and check_typeform ctx typesig =
  match Hashtbl.mem ctx.typeformtpls_by_sig typesig with
    | true ->
      ()
    | false ->
      instantiate_typeform ctx typesig
and get_typeform ctx typesig =
  (
    match Hashtbl.mem ctx.typeforminsts typesig with
      | true ->
        ()
      | false ->
        instantiate_typeform ctx typesig
  );
  Hashtbl.find ctx.typeforminsts typesig
and get_typedef ctx typesig =
  let (_, typedef, _, _) = get_typeform ctx typesig in
  typedef
;;



let rec instantiate_exp ctx scope typereq exp =
  match exp with
    | Template.EnumExpr(name, typeparamopt, exptpls, pos) ->
      let typesigopt = lookup_typesigopt scope typeparamopt in
      let typesig = check_typesig typereq typesigopt in
      let enums =
        match get_typedef ctx typesig with
          | Instance.AliasType(_, _) -> assert false
          | Instance.RecordType(_, _) -> assert false
          | Instance.EnumType(enums, _) -> enums
      in
      let typesigs = find_enum enums name in
      let _ =
        match (List.length exptpls) = (List.length typesigs) with
          | false -> assert false
          | true ->
            ()
      in
      let typereqs = List.map (fun x -> Some(x)) typesigs in
      let expressions = List.map2 (instantiate_exp ctx scope) typereqs exptpls in
      (typesig, pos, Instance.EnumExp(name, expressions))
    | _ ->
      assert false
and instantiate_clause ctx scope argtypesigs rettypesig clause =
  let (argtpls, guardtpls, exptpls, position) = clause in
  let clause_scope = Instance.ClauseScope(scope, Hashtbl.create 0) in
  let typereqs = (List.tl (List.map (fun x -> None) exptpls)) @ [Some(rettypesig)] in
  let expressions = List.map2 (instantiate_exp ctx clause_scope) typereqs exptpls in
  ([], [], expressions, position)
and instantiate_funform ctx funsig =
  let (name, argtypesigs) = funsig in
  let (funformtpl, rettypesig, typetable) = Hashtbl.find ctx.funformtpls_by_sig funsig in
  let (_, _, _, _, clausetpls, filename, pos) = funformtpl in
  let scope = Instance.FunScope (make_symtable typetable) in
  let clauses = List.map (instantiate_clause ctx scope argtypesigs rettypesig) clausetpls in
  ((name, argtypesigs), rettypesig, clauses, filename, pos)
and get_fun_rettypesig ctx funsig =
  let (name, argtypesigs) = funsig in
  match Hashtbl.mem ctx.funformtpls_by_sig funsig with
    | true ->
      let (_, rettypesig, _) = Hashtbl.find ctx.funformtpls_by_sig funsig in
      rettypesig
    | false ->
      let funformtpl = find_funform ctx.funformtpls_by_name funsig in
      let (_, rettypesig, _) = funformtpl in
      Hashtbl.add ctx.funformtpls_by_sig funsig funformtpl;
      List.iter (check_typeform ctx) argtypesigs;
      check_typeform ctx rettypesig;
      let funforminst = instantiate_funform ctx funsig in
      Hashtbl.add ctx.funforminsts funsig funforminst;
      rettypesig
;;



let instantiate tpltable =
  let (typeformtpls, funformtpls) = tpltable in
  let ctx =
    {
      typeformtpls_by_name = typeformtpls;
      funformtpls_by_name = funformtpls;
      typeformtpls_by_sig = Hashtbl.create 0;
      funformtpls_by_sig = Hashtbl.create 0;
      typeforminsts = Hashtbl.create 0;
      funforminsts = Hashtbl.create 0;
    }
  in
  let rettypesig = get_fun_rettypesig ctx ("main", []) in
  match rettypesig with
    | ("void", []) ->
      (* TODO: check type loop *)
      (ctx.typeforminsts, ctx.funforminsts)
    | _ ->
      assert false
;;
