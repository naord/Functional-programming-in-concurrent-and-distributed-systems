%%%-------------------------------------------------------------------
%%% @author nirkov
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2019 10:20
%%%-------------------------------------------------------------------
-module(masterServer).
-author("nirkov").

-export([start/0, init/1, handle_cast/2, recovery/1,handle_call/3]).
-include("globalVariables.hrl").
%% API
-behaviour(gen_server).
-record(state,{numOfGardens = 1}).

start()->
  %GraphicServer1Pid = graphicServer:start(),
  %GraphicServer2Pid = graphicServer:start(),
  %GraphicServer3Pid = graphicServer:start(),
  %GraphicServer4Pid = graphicServer:start(),
  %io:fwrite("masterServer: init: GraphicServerPid = ~p ~p ~n",[GraphicServer1Pid,GraphicServer2Pid]), %TODO for test
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([])->
%%  put({1,garden}, {global, ?garden1Name}),
%%  put({2,garden}, {global, ?garden2Name}),
%%  put({3,garden}, {global, ?garden3Name}),
%%  put({4,garden}, {global, ?garden4Name}),
%%
%%  put({1,graphic}, GraphicServer1Pid),
%%  put({2,graphic}, GraphicServer2Pid),
%%  put({3,graphic}, GraphicServer1Pid), %TODO: change GraphicServer3Pid
%%  put({4,graphic}, GraphicServer1Pid), %TODO: change GraphicServer4Pid

  databaseUtils:startDatabase(),
  {ok, #state{}}.

handle_call({connectGarden,Number,Node,GraphicPid},From,State=#state{numOfGardens = NumOfGardens}) ->
  {Pid,_} = From,
  monitor_node(Node,true),%TODO check if work
  %wx_object:cast(UIServerPid,{numOfServers,length(ServersList)+1}), TODO for gui
  put({Number,garden},Pid), %TODO need node?
  put({Number,graphic}, GraphicPid), %TODO need node?
  case NumOfGardens =:= 1 of
    true ->
      createGardeners();
    false -> ok
  end,
  {reply,ok,State#state{numOfGardens = NumOfGardens + 1}}.


handle_cast({newFlower, Flower}, NewState) ->
  databaseUtils:updateFlowerRecord(Flower),
  %io:fwrite("masterServer: newFlower: Flower = ~p ~n",[Flower]), %TODO for test
  % Send to specific graphic server to sit down the gardener.
  wx_object:cast(get({Flower#flower.gardenID,graphic}),{newFlower,Flower}),

  {noreply, NewState};

handle_cast({newGardener, Gardener}, NewState) ->
  %io:fwrite("masterServer: newGardener: Gardener = ~p ~n",[Gardener]), %TODO for test
  databaseUtils:updateGardenerRecord(Gardener),
  % Send to specific graphic server to sit down the gardener.
  wx_object:cast(get({Gardener#gardener.gardenNumber,graphic}),{rest, Gardener}),
  gen_server:cast(get({Gardener#gardener.gardenNumber,garden}),{newGardener, Gardener}),
  {noreply, NewState};

handle_cast({changeFlowerStatus,Flower}, NewState) -> %TODO one msg to all status changes?
  % Draw the updated status flower in graphicServer of this garden
  wx_object:cast(get({Flower#flower.gardenID,graphic}),{update, Flower}),
  %io:fwrite("masterServer: changeFlowerStatus++ ~n"), %TODO for test
  databaseUtils:updateFlowerRecord(Flower),
  FlowerStatus = Flower#flower.status,
  if
    FlowerStatus =/= normal ->
      Gardeners = databaseUtils:getRestingGardener(),
      Length =  lists:flatlength(Gardeners),
      %io:fwrite("masterServer: changeFlowerStatus: Gardeners= ~p ~n",[Gardeners]), %TODO for test
      if
        Length > 0 ->
          [Gardener | _] = Gardeners,
          %io:fwrite("masterServer: changeFlowerStatus: cast= ~p ~n",[get({Flower#flower.gardenID,garden})]), %TODO for test
          gen_server:cast(get({Flower#flower.gardenID,garden}), {sendGardenerToFlower, Gardener, Flower});
        true ->
          ok
      end;
    true ->
      ok
  end,
  {noreply, NewState};

handle_cast({updateFlower, Flower}, NewState) ->
  % Update the database.
  databaseUtils:updateFlowerRecord(Flower),
  {noreply, NewState};

handle_cast({deleteFlower, Flower}, NewState) ->
  % Delete the flower from the map in specific graphicServer.
  wx_object:cast(get({Flower#flower.gardenID,graphic}), {update, Flower}),

  % Delete the flower from the database.
  databaseUtils:deleteFlower(Flower#flower.id),
  {noreply, NewState};

handle_cast({gardenerWalkToFlower, Gardener}, NewState)->
  %io:fwrite("masterServer: gardenerWalkToFlower: Gardener = ~p ~n",[Gardener]), %TODO for test
  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  {noreply, NewState};


handle_cast({changeGardenerLocation, {OldX, OldY, Gardener}}, NewState)->
  % Send to specific graphic server to move the gardener.
  %io:fwrite("masterServer: changeGardenerLocation =~p ~p ~p ~p ~n",[get({Gardener#gardener.gardenNumber,graphic}),OldX,OldY, Gardener]), %TODO for test
  wx_object:cast(get({Gardener#gardener.gardenNumber,graphic}), {makeSteps, {OldX, OldY, Gardener}}),

  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  {noreply, NewState};

handle_cast({changeGardenerGarden, {OldGarden, OldX, OldY, Gardener}}, NewState)->
  % Send to specific graphic server to move the gardener.
  %io:fwrite("masterServer: changeGardenerGarden =~p ~p ~p ~p ~n",[get({Gardener#gardener.gardenNumber,graphic}),OldX,OldY, Gardener]), %TODO for test
  wx_object:cast(get({OldGarden, graphic}), {deleteGardenerFromGarden, {OldX, OldY}}),

  % Draw garden in new garden
  {X,Y} = Gardener#gardener.location,
  case (X > OldX) of
    true ->
      NewX = 0;
    false ->
      NewX = ?screen_width
  end,
  G = Gardener#gardener{location = {NewX, Y}},
  %io:fwrite("masterServer: changeGardenerGarden =~p ~p ~p ~p ~n",[NewX,OldX,OldY, G]), %TODO for test
  wx_object:cast(get({Gardener#gardener.gardenNumber, graphic}), {addGardener, G}),

  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  {noreply, NewState};

handle_cast({gardenerResting, Gardener}, NewState) ->
  % Send to specific graphic server to sit down the gardener.
  %io:fwrite("masterServer: gardenerResting =~p ~n",[Gardener]), %TODO for test
  wx_object:cast(get({Gardener#gardener.gardenNumber,graphic}), {rest, Gardener}),

  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  ListFlowerInDanger = databaseUtils:flowerListSortedByDangerousLevel(),
  Length = lists:flatlength(ListFlowerInDanger),
  if
    Length > 0 ->
      [Flower|_] = ListFlowerInDanger,
      gen_server:cast(get({Flower#flower.gardenID,garden}), {sendGardenerToFlower, Gardener, Flower});
    true ->
      ok
  end,
  {noreply, NewState}.


createGardeners() ->
  %GardenNumber = NewState#state.number,
  %X = ((GardenNumber - 1) * ?screen_width) + ?squareSize,

  %gardener:start_link(get(server), GlobalParams, GardenNumber, nir, {X+?squareSize,0}),
  %gardener:start_link(get(server), GlobalParams, GardenNumber, nir, {X+2*?squareSize,0}),


  Garden1Pid = get({1,garden}),
  gardener:start_link(self(), Garden1Pid, 1, nir, {0,0}).


%gen_server:cast(Garden1Pid, {createGardeners, Garden1Pid}).
%%  Garden1Pid = get({1,garden}),
%%  Garden2Pid = get({2,garden}),
%%  Garden3Pid = get({3,garden}),
%%  Garden4Pid = get({4,garden}),
%%  GlobalParams = {Garden1Pid,Garden2Pid,Garden3Pid,Garden4Pid},%GardenNumber,Type, Location
%%  gen_server:cast(Garden1Pid, {createGardeners, GlobalParams}),
%%  gen_server:cast(Garden2Pid, {createGardeners, GlobalParams}),
%%  gen_server:cast(Garden3Pid, {createGardeners, GlobalParams}),
%%  gen_server:cast(Garden4Pid, {createGardeners, GlobalParams}).

recovery(GardenID)->
  FlowerInGardenID   = databaseUtils:listsRecordOfFlowerInGarden(GardenID),
  GardenerInGardenID = databaseUtils:listsRecordOfGardenerInGarden(GardenID),
  SortedFlowerList   = databaseUtils:flowerListSortedByDangerousLevel(),

  % Send obejcts to graphic server to recover
  %TODO: NEED TO INIT THE GRAPHIC SERVER HERE AND HANDLE RECOVERY. SHOULD BE HANDLE_CALL TO GRAPHICSERVER?
  wx_object:cast(get({GardenID,garden}), {recovery, {FlowerInGardenID, GardenerInGardenID}}),%TODO check get function

  % Send the flowers sorted list by dangerous level to garden
  gen_server:cast(get({GardenID,garden}), {recovery, SortedFlowerList}).%TODO check get function


%%connectUIServerToGarden(GardenNumber)->
%%  case GardenNumber of
%%    1 -> get(?graphic1Name);
%%    2 -> get(graphic2Pid);
%%    3 -> get(graphic3Pid);
%%    4 -> get(graphic4Pid)
%%  end.

%%connectUIServerToGarden(GardenID)->
%%  case GardenID of
%%    garden1 -> get(graphic1);%graphic1; %TODO
%%    garden2 -> get(graphic1);%graphic2;
%%    garden3 -> get(graphic1);%graphic3;
%%    garden4 -> get(graphic1)%graphic4
%%end.

%%getGardenName(Number) ->
%%  case Number of
%%    1 -> {global,?garden1Name};
%%    2 -> {global,?garden2Name};
%%    3 -> {global,?garden3Name};
%%    _ -> {global,?garden4Name}
%%  end.


