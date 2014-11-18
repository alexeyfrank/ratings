PROJECT = ratings

REBAR = ./rebar

all: clean get-deps compile

update-deps:
	@$(REBAR) update-deps

get-deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

cleanrel:
	@$(RM) -rf _rel/

compile:
	@$(REBAR) compile

cleanapp:
	@($(REBAR) clean skip_deps=true)

compileapp: cleanapp
	@($(REBAR) compile skip_deps=true)

run: compileapp
	@(erl -config $(CURDIR)/sys -pa $(CURDIR)/apps/*/ebin $(CURDIR)/deps/*/ebin -boot start_sasl -s $(PROJECT))

repl: compileapp
	@(erl -config $(CURDIR)/sys -pa $(CURDIR)/apps/*/ebin $(CURDIR)/deps/*/ebin -boot start_sasl)

test: compileapp
	ERL_FLAGS="-config $(CURDIR)/sys" $(REBAR) eu skip_deps=true

add_user_score:
	curl -X PUT -H "Content-Type: application/json" http://127.0.0.1:8008/api/users/user1/100
add_user_score2:
	curl -X PUT -H "Content-Type: application/json" http://127.0.0.1:8008/api/users/user2/20
get_rating_weekly:
	curl -X GET -H "Content-Type: application/json" http://127.0.0.1:8008/api/ratings/weekly

get_stat_weekly:
	curl -X GET -H "Content-Type: application/json" http://127.0.0.1:8008/api/stat/weekly
.PHONY: rel
