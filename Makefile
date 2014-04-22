REBAR="rebar"

all: get-deps compile

clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

test: compileapp
	ERL_FLAGS="-config $(CURDIR)/sys" $(REBAR) eu skip_deps=true

run: cleanapp compileapp
	erl -config $(CURDIR)/sys -pa ebin deps/*/ebin -s crawler
 
get-deps:
	$(REBAR) get-deps

cleanapp:
	$(REBAR) clean skip_deps=true

compileapp:
	$(REBAR) compile skip_deps=true	

.PHONY: test
