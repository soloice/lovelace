XRLS=$(wildcard *.xrl)
YRLS=$(wildcard *.yrl)
XERLS=$(XRLS:%.xrl=%.erl)
YERLS=$(YRLS:%.yrl=%.erl)
ERLS=$(filter-out $(XERLS) $(YERLS),$(wildcard *.erl))
BEAMS=$(ERLS:%.erl=%.beam) $(XERLS:%.erl=%.beam) $(YERLS:%.erl=%.beam)

all: $(BEAMS)

%.erl: %.xrl
	erl -run leex file "$<" -run init stop -noshell

%.erl: %.yrl
	erl -run yecc file "$<" -run init stop -noshell

%.beam: %.erl
	erlc "$<"

clean:
	rm -rf $(BEAMS) $(XERLS) $(YERLS)
