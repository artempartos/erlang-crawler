REBAR="rebar"

all: get-deps compile

clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

run: cleanapp compileapp
	erl -pa ebin deps/*/ebin -s crawler

test:
	$(REBAR) compile eunit skip_deps=true
 
get-deps:
	$(REBAR) get-deps

cleanapp:
	$(REBAR) clean skip_deps=true

compileapp:
	$(REBAR) compile skip_deps=true	

.PHONY: test
