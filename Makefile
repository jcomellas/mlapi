APPLICATION := mlapi

ERL := erl
EPATH := -pa ebin -pz deps/*/ebin
TEST_EPATH := -pa .eunit -pz deps/*/ebin

.PHONY: all doc clean test

all: compile

compile:
	@rebar compile

doc:
	@rebar doc

clean:
	@rebar clean

distclean:
	@rebar delete-deps

build-plt: compile
	@rebar build-plt

check-plt: compile
	@rebar check-plt

dialyze: compile
	@rebar dialyze

test:
	@rebar eunit

console: compile
	$(ERL) -sname $(APPLICATION) $(EPATH) -s mlapi_deps ensure

test-console: test
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH)

