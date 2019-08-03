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
-include("globalVariables.hrl").

%% API
-export([init/1, handle_cast/2]).
-export([start_link/2,terminate/2]).
-record(state, {}).

%%Creates a gen_server process as part of a supervision tree.
%%start_link(ServerName, Module, Args, Options) -> Result
start_link(GlobalName, MainServerGlobalName) ->
  gen_server:start_link({global,GlobalName}, ?MODULE, [MainServerGlobalName], []).

%%A set or ordered_set table can only have one object associated with each key
%%When the process terminates, the table is automatically destroyed
%%Notice that there is no automatic garbage collection for tables
%% To destroy a table explicitly, use function delete/1.
%%The table is a set table: one key, one object, no order among objects
init([MainServerGlobalName]) ->
  put(server,{global,MainServerGlobalName}),
  ets:new(flowers,[set, public, named_table]),
  ets:new(gardeners,[set, public, named_table]),
  Status = gen_server:call(get(server),{connect,node()}),
  io:fwrite("garden: init: Status = ~p ~n",[Status]), %TODO for test
  {ok, #state{}}.

handle_cast({addFlower,Flower}, NewState) ->
  addFlower(Flower),
  {noreply, NewState};

handle_cast({updateFlowerStatus,Flower}, NewState) -> %TODO change in flower module
  gen_server:cast(get(server),{updateFlowerStatus,Flower}, NewState),%Send to main server updateFlowerStatus
  {noreply, NewState};

%TODO update flower record
handle_cast({flowerDie,Flower=#flower{id = Id, gardenerId = none}}, NewState) -> %TODO flower need owner in case garden on his way.
  ets:delete(flowers,Id),%delete from ets
  gen_server:cast(get(server), {deleteFlower,Id}),%Send to main server delete flower
  {noreply, NewState};

handle_cast({flowerDie,Flower=#flower{id = Id, gardenerId = GardenerId}}, NewState) ->
  %[{_,GardenerPid}] = ets:lookup(gardeners, GardenerId),%delete from ets
  gen_server:cast({global,GardenerId},{delete_flower,Id}),%GardenerPid ! abort,
  ets:delete(flowers,Id),%delete from ets
  gen_server:cast(get(server), {deleteFlower,Id}),%Send to main server delete flower
  {noreply, NewState};

handle_cast({gardenerWalkToFlower, Gardener, FlowerLocation, FlowerId}, NewState) ->
  gardenerWalkToFlower(Gardener,FlowerLocation,FlowerId),
  {noreply, NewState};

handle_cast({gardenerResting,Gardener}, NewState) ->
  gen_server:cast(get(server),{gardenerResting,Gardener}),
  {noreply, NewState}.

terminate(Reason, State) ->
  ok.

% Send msg to gardner and flower
gardenerWalkToFlower(Gardener, FlowerLocation, FlowerId) ->
  [{_,FlowerPid}] = ets:lookup(flowers,FlowerId),
  gen_server:cast({global,Gardener#gardener.id},{walkToFlower, FlowerId, FlowerLocation}).%TODO change function in gardener.
  %TODO FlowerPid ! {gardner_on_the_way,Gardener#gardener.id}.

addFlower(Flower) ->
  Pid = self(),
  FlowerPid = spawn_link(flower,flowerAsStateMachine,[Flower,0,Pid]), % init location_updater process
  ets:insert(flowers, { Flower#flowers.name,FlowerPid}).