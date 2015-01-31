-module(kvs).
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% client method
-export([put/2, get/1, delete/1]).
-define(SERVER, ?MODULE).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) -> 
  %% Note we must set trap_exit = true if we 
  %% want terminate/2 to be called when the application
  %% is stopped
  process_flag(trap_exit, true),
  Handle = bitcask:open("test_db", [read_write]),
  {ok, Handle}.

handle_call({put, Key, Value}, _From, Handle) -> 
  bitcask:put(Handle, term_to_binary(Key) , term_to_binary(Value)),
  {reply, ok, Handle};
handle_call({get, Key}, _From, Handle) -> 
  case bitcask:get(Handle, term_to_binary(Key)) of
    not_found -> 
      Reply = not_found;
  {ok, Bin} -> 
      Reply = {ok, binary_to_term(Bin)}
  end,
  {reply, Reply, Handle};
handle_call({delete, Key}, _From, Handle) -> 
  bitcask:delete(Handle, term_to_binary(Key)),
  {reply, ok, Handle}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, Handle) -> 
  io:format("stop~n"),
  bitcask:close(Handle),
  ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%client method
put(Key, Value) -> gen_server:call(?MODULE, {put, Key, Value}).
get(Key) -> gen_server:call(?MODULE, {get, Key}).
delete(Key) -> gen_server:call(?MODULE, {delete, Key}).