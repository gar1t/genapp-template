rebar = ./rebar

compile: deps
	$(rebar) compile

quick:
	$(rebar) compile skip_deps=true

deps:
	$(rebar) get-deps

update-deps:
	$(rebar) update-deps

.PHONY: doc
doc:
	cd doc && make html

clean:
	$(rebar) clean

CONFIG=dev
shell: compile
	erl -pa ebin $(wildcard deps/*/ebin) -s genapp_reloader

