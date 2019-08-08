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
-export([init/1, handle_cast/2, handle_call/3]).
-export([start_link/2,terminate/2, createFlowers/2]).

-record(state, {number}).

%%Creates a gen_server process as part of a supervision tree.
%%start_link(ServerName, Module, Args, Options) -> Result
start_link(Number, MainServerGlobalName) ->
  MyNameAtom = list_to_atom("garden" ++ integer_to_list(Number)),
  %io:fwrite("garden: start_link: Number = ~p EtsName= ~p ~n",[MyNameAtom,EtsName]),
  %put(gardenName, MyNameAtom),
  gen_server:start_link({global, MyNameAtom}, ?MODULE, [MainServerGlobalName,Number], []).

%% A set or ordered_set table can only have one object associated with each key
%% When the process terminates, the table is automatically destroyed
%% Notice that there is no automatic garbage collection for tables
%%  To destroy a table explicitly, use function delete/1.
%% The table is a set table: one key, one object, no order among objects
init([MainServerGlobalName, Number]) ->
  EtsName = list_to_atom("flowers" ++ integer_to_list(Number)),
  put(etsName, EtsName),
  put(server,{masterServer,MainServerGlobalName}),
  GraphicServerPid = graphicServer:start(),
  gen_server:call({masterServer,MainServerGlobalName},{connectGarden,Number,node(),GraphicServerPid}),
  ets:new(get(etsName),[set,named_table,private]),
  {ok, #state{number = Number}}.

%From MainServer
%garden start creating flowers after all gardener created.
handle_cast({newGardener, Gardener}, NewState) ->
  createFlowers(10, Gardener#gardener.gardenNumber), % create flowers
  {noreply, NewState};

%From MainServer
%cast to gardener message to start walking to flower
handle_cast({sendGardenerToFlower, Gardener, Flower}, NewState) ->
  sendGardenerToFlower(Gardener, Flower,NewState),
  {noreply, NewState};

%From Flower
%cast to master server to update flower state
handle_cast({updateFlower,Flower}, NewState) ->
  gen_server:cast(get(server),{updateFlower,Flower}),%Send to main server updateFlowerStatus
  {noreply, NewState};

%From Flower
%cast to master server to update flower status
handle_cast({changeFlowerStatus,Flower}, NewState) ->
  gen_server:cast(get(server),{changeFlowerStatus,Flower}),%Send to main server updateFlowerStatus
  {noreply, NewState};


%From Flower
%cast to master server to update flower die
handle_cast({flowerDie,Flower=#flower{id = _, gardenerID = none}}, NewState) ->
  %ets:delete(get(etsName),Id),
  gen_server:cast(get(server), {deleteFlower, Flower}),%Send to main server delete flower
  {noreply, NewState};

%From Flower
%cast to master server to update flower die
%cast to gardener that walking to the flower to stop
handle_cast({flowerDie,Flower=#flower{id = _, gardenerID = GardenerPid}}, NewState) ->
  %ets:delete(get(etsName),Id),
  gen_server:cast(GardenerPid,cancelWalk),
  gen_server:cast(get(server), {deleteFlower,Flower}),%Send to main server delete flower
  {noreply, NewState};

%From gardener
%cast to master server to update gardener record
handle_cast({gardenerHandleFlower,Gardener}, NewState) ->
  gen_server:cast(get(server),{gardenerHandleFlower,Gardener}),
  {noreply, NewState};

%From gardener
%cast to master server to update gardener record
handle_cast({gardenerWalkToFlower,Gardener}, NewState) ->
  gen_server:cast(get(server),{gardenerWalkToFlower,Gardener}),
  {noreply, NewState};

%From gardener
%cast to master server to update gardener record
handle_cast({changeGardenerGarden,{OldGarden, OldX, OldY, Gardener}}, NewState) ->
  gen_server:cast(get(server),{changeGardenerGarden,{OldGarden, OldX, OldY, Gardener}}),
  {noreply, NewState};

%From gardener
%cast to master server to update gardener record
handle_cast({gardenerResting,Gardener}, NewState) ->
  gen_server:cast(get(server),{gardenerResting,Gardener}),
  {noreply, NewState};

%From gardener
%cast to master server to update gardener record
handle_cast({changeGardenerLocation, {OldX, OldY, Gardener}}, NewState) ->
  gen_server:cast(get(server),{changeGardenerLocation,{OldX, OldY, Gardener}}),
  {noreply, NewState}.

%From gardener
%reply to gardener if flower is alive in the garden.
handle_call({isFlowerAlive, FlowerId},_,State)->
  Reply = ets:lookup(get(etsName),FlowerId),
  {reply,Reply,State}.

terminate(Reason, State) ->
  {Reason, State}.

% Send msg to gardner and flower
sendGardenerToFlower(Gardener, Flower,State) ->
  FlowerId = Flower#flower.id,
  FlowerLocation = {Flower#flower.x, Flower#flower.y},
  [{_,FlowerPid}] = ets:lookup(get(etsName),FlowerId),
  gen_server:cast(Gardener#gardener.id,{walkToFlower, FlowerId, FlowerPid, State#state.number, FlowerLocation, self()}),
  FlowerPid ! {setGardenerID,Gardener#gardener.id}. %send to flower

getRandomFlower()->
  RandomFlower = getRandomNumber(40),
  if
    RandomFlower < 10 -> iris_l;
    RandomFlower < 20 -> iris_r;
    RandomFlower < 30 -> red_l;
    true -> red_r
  end.

getRandomNumber(Gap)->
  {T1,T2,T3} = now(),
  random:seed(T1, T2, T3),
  random:uniform(Gap).

%create NumberOfFlowers flowers
createFlowers(NumberOfFlowers, GardenNumber)->
  GardenName = list_to_atom("garden" ++ integer_to_list(GardenNumber)),
  % Range of flower id
  NumbersList = lists:seq(GardenNumber * ?screen_width, (GardenNumber * ?screen_width) + NumberOfFlowers),

  % Make them atoms
  RegisterIDs = [intToAtom(Number)|| Number <- NumbersList],

  % All aveilable coordinate in garden
  AvailableCoordinate =[{X * ?squareSize, Y * ?squareSize} || X <- lists:seq(0, 16), Y <- lists:seq(0, 11)],

  Fun = fun(RegisterID) ->
    % get random coordinate
    RandomCoordinate = getRandomNumber(lists:flatlength(AvailableCoordinate)),
    {X, Y} =lists:nth(RandomCoordinate, AvailableCoordinate),

    % Delete it from aveilable list
    lists:delete({X, Y}, AvailableCoordinate),

    % Create flower
    Flower = #flower{id = RegisterID, type = getRandomFlower(), status = normal,
      timeSinceProblem = 0, gardenerID = none, gardenID = GardenNumber, x = X, y = Y },
    gen_server:cast(get(server), {newFlower, Flower}),
    Pid = spawn(flower, flowerAsStateMachine, [GardenName, Flower]),
    timer:sleep(100),
    % save in ets
    ets:insert(get(etsName), {Flower#flower.id, Pid})
        end,

  lists:foreach(Fun, RegisterIDs).

intToAtom(Name)->list_to_atom(integer_to_list(Name)).