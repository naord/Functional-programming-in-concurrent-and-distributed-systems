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

%TODO check if need more msg from/to flower and main server
%TODO who create the flowers?

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

%From MainServer
handle_cast({addFlower,Flower}, NewState) ->
  addFlower(Flower),
  {noreply, NewState};

%From MainServer
handle_cast({sendGardenerToFlower, Gardener, FlowerLocation, FlowerId}, NewState) ->
  sendGardenerToFlower(Gardener,FlowerLocation,FlowerId),
  {noreply, NewState};

%From Flower
handle_cast({updateFlowerStatus,Flower}, NewState) -> %TODO one msg to all status changes?
  gen_server:cast(get(server),{updateFlowerStatus,Flower}),%Send to main server updateFlowerStatus
  {noreply, NewState};

%From Flower
handle_cast({flowerDie,Flower=#flower{id = Id, gardenerID = none}}, NewState) ->
  ets:delete(flowers,Id),%delete from ets %TODO need ets?
  gen_server:cast(get(server), {deleteFlower,Flower}),%Send to main server delete flower
  {noreply, NewState};

%From Flower
handle_cast({flowerDie,Flower=#flower{id = Id, gardenerID = GardenerId}}, NewState) ->
  gen_server:cast({global,GardenerId},cancelWalk),
  ets:delete(flowers,Id),%delete from ets %TODO need ets?
  gen_server:cast(get(server), {deleteFlower,Flower}),%Send to main server delete flower
  {noreply, NewState};

%From gardener
handle_cast({gardenerHandleFlower,Gardener}, NewState) ->
  gen_server:cast(get(server),{gardenerHandleFlower,Gardener}),
  {noreply, NewState};

%From gardener
handle_cast({gardenerWalkToFlower,Gardener}, NewState) ->
  gen_server:cast(get(server),{gardenerWalkToFlower,Gardener}),
  {noreply, NewState};

%From gardener
handle_cast({changeGardenerGarden,Gardener}, NewState) ->
  gen_server:cast(get(server),{changeGardenerGarden,Gardener}),
  {noreply, NewState};

%From gardener
handle_cast({gardenerResting,Gardener}, NewState) ->
  gen_server:cast(get(server),{gardenerResting,Gardener}),
  {noreply, NewState};

%From gardener
handle_cast({changeGardenerLocation,Gardener,OldX,OldY}, NewState) ->
  gen_server:cast(get(server),{changeGardenerLocation,{OldX, OldY, Gardener}}),
  {noreply, NewState}.

terminate(Reason, State) -> %TODO complete
  ok.

% Send msg to gardner and flower
sendGardenerToFlower(Gardener, FlowerLocation, FlowerId) ->
  [{_,FlowerPid}] = ets:lookup(flowers,FlowerId),
  gen_server:cast({global,Gardener#gardener.id},{walkToFlower, FlowerId, FlowerLocation}), %send to gardener
  FlowerPid ! {setGardenerID,Gardener#gardener.id}. %send to flower

addFlower(Flower) ->
  Pid = self(),
  FlowerPid = spawn_link(flower,flowerAsStateMachine,[Flower,0,Pid]), %TODO update here & flower file to have the fields
  ets:insert(flowers, {Flower#flower.id, FlowerPid}).