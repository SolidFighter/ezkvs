all:
	test -d deps || rebar get-deps
	rebar compile
	erl -pa "./ebin" -pa "./deps/bitcask/ebin"  

clean:
	rebar clean	
