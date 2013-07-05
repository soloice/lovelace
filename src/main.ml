let compile filename =
  let templatetable = Module.load_modules ["./"; "./lib/"] filename in
  let instancetable = TypeCheck.instantiate templatetable in
  let _ = Binding.bind instancetable in
  ()
;;

let () = compile "hello.lol";;
