%%%-------------------------------------------------------------------
%%% @author Naor Dahan
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. אוג׳ 2019 09:36
%%%-------------------------------------------------------------------
-module(garden).
-author("Naor Dahan").

-behaviour(gen_server).
-include("../globalVariables.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2]).
-export([start_link/2]).
-record(state, {}).

handle_call(Request, From, State) ->
  {reply, ok, State}.

handle_cast(Request, State) ->
  erlang:error(not_implemented).

%%Creates a gen_server process as part of a supervision tree.
%%start_link(ServerName, Module, Args, Options) -> Result
start_link(GlobalName, MainNode) ->
  gen_server:start_link({global,GlobalName}, ?MODULE, [MainNode], []).

%%A set or ordered_set table can only have one object associated with each key
%%When the process terminates, the table is automatically destroyed
%%Notice that there is no automatic garbage collection for tables
%% To destroy a table explicitly, use function delete/1.
%%The table is a set table: one key, one object, no order among objects
init([MainNode]) ->
  put(server,{mainServer,MainNode}),
  ets:new(flowers,[set, public, named_table]),
  ets:new(gardeners,[set, public, named_table]),
  Status = gen_server:call(get(server),{connect,node()}),
  io:fwrite("garden: init: Status = ~p ~n",[Status]), %TODO for test
  {ok, #state{}}.

handle_cast({addFlower,Flower}, State) ->
  add_flower(Flower),
  {noreply, State};

handle_cast({flowerUpdate,Flower}, State) ->
  gen_server:cast(get(server),{flower_update_from_slave,Flower}),
  {noreply, State};

handle_cast({flowerDie,_Flower=#flowers{name=Name,gardener_name = none}}, State) ->
  %delete from ets
  ets:delete(flowers,Name),
  gen_server:cast(get(server),{delete_flower,Name}),
  {noreply, State};

handle_cast({flowerDie,_Flower=#flowers{name=Name,gardener_name = GardenerName}}, State) ->
  %delete from ets
  [{_,GardenerPid}]=ets:lookup(gardeners,GardenerName),
  GardenerPid ! abort,
  ets:delete(flowers,Name),
  gen_server:cast(get(server),{delete_flower,Name}),
  {noreply, State};

handle_cast({gardener_assign_from_master,Gardener,Flower_cord}, State) ->
  gardener_assign(Gardener,Flower_cord),
  {noreply, State};

handle_cast({gardener_finished_mission_from_gardener,Gardener}, State) ->
  ets:delete(gardeners,Gardener#gardeners.name),
  gen_server:cast(get(server),{gardener_free,Gardener}),
  {noreply, State};


%%update gardener ets
handle_cast({backup,Gardeners,Flowers},State) ->
  io:format("debug got back up ~n"),
  io:format("debug flower list: ~p~n",[Flowers]),
  io:format("debug gardeners  list: ~p~n",[Gardeners]),
  lists:foreach(fun(Flower) ->
    NewFlower=Flower#flowers{owner = self()},
    add_flower(NewFlower) end,Flowers),
  lists:foreach(fun(Gardener) ->
    FlowerRec =  lists:keyfind(Gardener#gardeners.flower_name,2,Flowers),
    % check if the flower has been deleted
    if FlowerRec =/= false ->
      FlowerCord = FlowerRec#flowers.cord;
      true -> FlowerCord = {0,0}
    end,
    NewGardener = Gardener#gardeners{owner = self()},
    gardener_assign(NewGardener,FlowerCord) end,Gardeners),
  {noreply, State};


handle_cast(Message, State) ->
  gen_server:cast(get(server),Message),
  {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_flower(Flower) ->
  Pid=self(),
  FlowerPid=spawn_link(flower,flower_loop,[Flower,0,Pid]), % init location_updater process
  ets:insert(flowers, { Flower#flowers.name,FlowerPid}).


gardener_assign(Gardener,Flower_cord) ->
  [{_,Flower_pid}]=ets:lookup(flowers,Gardener#gardeners.flower_name),
  SlavePid=self(),
  Flower_pid ! {gardner_on_the_way,Gardener#gardeners.name},
  GardenerPid=spawn_link(gardener,gardener_loop,[Gardener,Flower_cord,Flower_pid,SlavePid]),
  ets:insert(gardeners,{Gardener#gardeners.name,GardenerPid}).