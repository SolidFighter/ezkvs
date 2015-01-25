all:
	test -d deps || rebar get-deps
	rebar compile
	#erl -noshell -pa "./ebin" -pa "./deps/bitcask/ebin" -s bertie start
