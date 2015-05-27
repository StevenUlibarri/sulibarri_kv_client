-module(sulibarri_dht_client).

%%%-------------------------------------------
%%% @author 
%%% @copyright
%%% @doc 
%%% @end
%%%-------------------------------------------

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
        start_link/0,
        new_cluster/1,
        join_cluster/1,
        put/2,
        get/1,
        delete/1,
        colors/0,
        connect/1
        ]).

-define(CACHE_NAME, client_cachce).
-include("dht_object.hrl").

-record(state, {connected_node}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_cluster(Nodes) ->
	gen_server:cast(?SERVER, {new_cluster, Nodes}).

join_cluster(Node) ->
	gen_server:cast(?SERVER, {join_cluster, Node}).

put(Key, Value) ->
	gen_server:cast(?SERVER, {put, Key, Value}).

get(Key) ->
	gen_server:cast(?SERVER, {get, Key}).

delete(Key) ->
	gen_server:cast(?SERVER, {delete, Key}).

colors() ->
	lager:debug("test"),
	lager:info("test"),
	lager:notice("test"),
	lager:warning("test"),
	lager:error("test"),
	lager:critical("test"),
	lager:alert("test"),
	lager:emergency("test").

connect(Node) ->
	gen_server:cast(?SERVER, {connect, Node}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	ets:new(?CACHE_NAME, [named_table, {keypos, 2}]),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({new_cluster, Nodes}, State) ->
	gen_server:cast({sulibarri_dht_node, State#state.connected_node},
					{new_cluster, Nodes, node()}),
	% sulibarri_dht_node:new_cluster(Nodes),
    {noreply, State};

handle_cast({join_cluster, Node}, State) ->
	sulibarri_dht_node:join_cluster(Node),
	{noreply, State};

handle_cast({put, Key, Value}, State) ->
	Obj = case ets:lookup(?CACHE_NAME, Key) of
		[] -> #object{key = Key, values=[Value]};
		[Res] -> Res#object{values=[Value], deleted=false}
	end,
	sulibarri_dht_node:put(Obj, node()),
	{noreply, State};

handle_cast({get, Key}, State) ->
	sulibarri_dht_node:get(Key, node()),
	{noreply, State};

handle_cast({delete, Key}, State) ->
	sulibarri_dht_node:delete(Key, node()),
	{noreply, State};

handle_cast({reply, Content}, State) ->
	case Content of
		{ok, Obj} ->
			lager:notice("recieved ~p", [Obj]),
			ets:insert(?CACHE_NAME, Obj);
		{not_found, Key} ->
			lager:notice("Key ~p not found", [Key]);
		{info, Message} ->
			lager:notice("~p", [Message]);
		{error, Context} ->
			lager:error("~p", [Context])
	end,
	{noreply, State};

handle_cast({connect, Node}, State) ->
	New_State = case net_adm:ping(Node) of
		pang -> 
			lager:error("Node ~p uncreachable", [Node]),
			State;
		pong -> 
			lager:notice("Client connected to node ~p", [Node]),
			#state{connected_node = Node}
	end,
	{noreply, New_State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------






