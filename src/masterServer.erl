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
-record(state,{numOfGardens = 0 , gardensPids = []}).

%Start the master server(call the init function).
start()->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([])->
  databaseUtils:startDatabase(),
  {ok, #state{}}.

%%connect garden and its graphic server to master server.
%% send from garden.
%% if all gardens connected start creating the gardeners.
handle_call({connectGarden,Number,Node,GraphicPid},From,State=#state{numOfGardens = NumOfGardens, gardensPids = GardenPids}) ->
  {Pid,_} = From,
  monitor_node(Node,true),
  put({Number,garden},Pid),
  put({Number,graphic}, GraphicPid),
  NewState = State#state{numOfGardens = NumOfGardens + 1, gardensPids = GardenPids ++ [Pid]},
  case NewState#state.numOfGardens =:= ?numOfGardens of %if all gardens connected start creating the gardeners.
    true ->
      createGardeners(NewState);
    false -> ok
  end,
  {reply,ok,NewState}.

%handle new flower request
%update the data base with the flower record.
handle_cast({newFlower, Flower}, NewState) ->
  databaseUtils:updateFlowerRecord(Flower),
  % Send to specific graphic server to sit down the gardener.
  wx_object:cast(get({Flower#flower.gardenID,graphic}),{newFlower,Flower}),
  {noreply, NewState};

%%handle new gardener request.
%%update data base with the gardener record.
%%send cast to the gardeners graphic server and garden.
handle_cast({newGardener, Gardener}, NewState) ->
  databaseUtils:updateGardenerRecord(Gardener),
  % Send to specific graphic server to sit down the gardener.
  wx_object:cast(get({Gardener#gardener.gardenNumber,graphic}),{rest, Gardener}),
  gen_server:cast(get({Gardener#gardener.gardenNumber,garden}),{newGardener, Gardener}),
  {noreply, NewState};

%%handle flower change status update.
%% update data base with the new record.
%%send cast to the flowers graphic server.
%% check if the status is normal if so look for free gardener and send him to needed flower.
handle_cast({changeFlowerStatus,Flower}, NewState) ->
  % Draw the updated status flower in graphicServer of this garden
  wx_object:cast(get({Flower#flower.gardenID,graphic}),{update, Flower}),
  databaseUtils:updateFlowerRecord(Flower),
  FlowerStatus = Flower#flower.status,
  if
    FlowerStatus =/= normal ->
      Gardeners = databaseUtils:getRestingGardener(),
      Length =  lists:flatlength(Gardeners),
      if
        Length > 0 ->
          [Gardener | _] = Gardeners,
          gen_server:cast(get({Flower#flower.gardenID,garden}), {sendGardenerToFlower, Gardener, Flower});
        true ->
          ok
      end;
    true ->
      ok
  end,
  {noreply, NewState};

%%update data base record for changes that are not status change.
handle_cast({updateFlower, Flower}, NewState) ->
  % Update the database.
  databaseUtils:updateFlowerRecord(Flower),
  {noreply, NewState};

%%delete flower from record and send delete request to his graphic server.
handle_cast({deleteFlower, Flower}, NewState) ->
  % Delete the flower from the map in specific graphicServer.
  wx_object:cast(get({Flower#flower.gardenID,graphic}), {update, Flower}),

  % Delete the flower from the database.
  databaseUtils:deleteFlower(Flower#flower.id),
  {noreply, NewState};

%update gardener record to gardenerWalkToFlower
handle_cast({gardenerWalkToFlower, Gardener}, NewState)->
  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  {noreply, NewState};

%update data base with gardener new location
%send change location request to his graphic server.
handle_cast({changeGardenerLocation, {OldX, OldY, Gardener}}, NewState)->
  % Send to specific graphic server to move the gardener.
  wx_object:cast(get({Gardener#gardener.gardenNumber,graphic}), {makeSteps, {OldX, OldY, Gardener}}),

  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  {noreply, NewState};

%update data base with gardener new garden
%send delete gardener request to old graphic server and create gardener to new graphic server.
handle_cast({changeGardenerGarden, {OldGarden, OldX, OldY, Gardener}}, NewState)->
  % Send to specific graphic server to move the gardener.
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
  wx_object:cast(get({Gardener#gardener.gardenNumber, graphic}), {addGardener, G}),

  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  {noreply, NewState};

%update data base and graphic server with gardener new record.
%check if there are flower who need handeling if so send the to the garden the gardener.
handle_cast({gardenerResting, Gardener}, NewState) ->
  % Send to specific graphic server to sit down the gardener.
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

%create 4 gardeners to each garden
createGardeners(NewState)  ->
  GardensPid = NewState#state.gardensPids,

  gardener:start_link(self(), GardensPid, 1, nir, {0,0},1),
  gardener:start_link(self(), GardensPid, 1, nir, {?squareSize,0},2),
  gardener:start_link(self(), GardensPid, 1, nir, {2*?squareSize,0},3),
  gardener:start_link(self(), GardensPid, 1, nir, {3*?squareSize,0},4),

  gardener:start_link(self(), GardensPid, 2, nir, {?squareSize + ?screen_width,0},5),
  gardener:start_link(self(), GardensPid, 2, nir, {2*?squareSize + ?screen_width,0},6),
  gardener:start_link(self(), GardensPid, 2, nir, {3*?squareSize + ?screen_width,0},7),
  gardener:start_link(self(), GardensPid, 2, nir, {4*?squareSize + ?screen_width,0},8),

  gardener:start_link(self(), GardensPid, 3, nir, {?squareSize + ?screen_width*2,0},9),
  gardener:start_link(self(), GardensPid, 3, nir, {2*?squareSize + ?screen_width*2,0},10),
  gardener:start_link(self(), GardensPid, 3, nir, {3*?squareSize + ?screen_width*2,0},11),
  gardener:start_link(self(), GardensPid, 3, nir, {4*?squareSize + ?screen_width*2,0},12).

recovery(GardenID)->
  FlowerInGardenID   = databaseUtils:listsRecordOfFlowerInGarden(GardenID),
  GardenerInGardenID = databaseUtils:listsRecordOfGardenerInGarden(GardenID),
  SortedFlowerList   = databaseUtils:flowerListSortedByDangerousLevel(),

  % Send obejcts to graphic server to recover
  wx_object:cast(get({GardenID,garden}), {recovery, {FlowerInGardenID, GardenerInGardenID}}),

  % Send the flowers sorted list by dangerous level to garden
  gen_server:cast(get({GardenID,garden}), {recovery, SortedFlowerList}).

