# ex: set ts=8 noet:

all:
	(rebar get-deps compile)

local:
	(rebar compile)

clean:
	(rm -rf apps/*/ebin)
	(rm -rf deps/*/ebin)

