ML_OBJS=                \
  src/template.cmo      \
  src/parser.cmo        \
  src/lexer.cmo         \
  src/module.cmo        \
  src/instance.cmo      \
  src/typeCheck.cmo     \
  src/core.cmo          \
  src/binding.cmo       \
  src/main.cmo          \

ML_FLAGS=-I src

.PHONY: all clean run

all: bin/lol

src/lexer.cmo:     src/parser.cmo
src/parser.cmo:    src/parser.cmi
src/parser.cmi:    src/template.cmo
src/parser.mli:    src/parser.ml
src/typeCheck.cmo: src/template.cmo src/instance.cmo
src/core.cmo:      src/template.cmo src/instance.cmo
src/binding.cmo:   src/template.cmo src/instance.cmo src/core.cmo
src/main.cmo:      src/module.cmo src/typeCheck.cmo src/binding.cmo


run: $(ML_OBJS)
	ocaml $(ML_FLAGS) $(ML_OBJS)

bin/lol: $(ML_OBJS) | bin
	ocamlc $(ML_FLAGS) -o $@ $^

src/%.cmo: src/%.ml
	ocamlc $(ML_FLAGS) -c "$<"

src/%.cmi: src/%.mli
	ocamlc $(ML_FLAGS) -c "$<"

src/%.ml: src/%.mll
	ocamllex "$<"

src/%.ml: src/%.mly
	ocamlyacc -v "$<"

bin:
	mkdir bin

clean:
	rm -f src/*.cmo src/*cmi src/*.mli src/lexer.ml src/parser.ml bin/lol
