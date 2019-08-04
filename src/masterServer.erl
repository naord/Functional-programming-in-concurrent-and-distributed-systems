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

-export([init/0, handle_cast/2, handle_call/2, recovery/2]).

%% API
-behaviour(gen_server).
-include("globalVariables.hrl").
-include("databaseUtils.erl").

init()->
  put(garden1, {global, ?garden1Name}),
  put(garden2, {global, ?garden2Name}),
  put(garden3, {global, ?garden3Name}),
  put(garden4, {global, ?garden4Name}),

  put(graphic1, {global, ?graphic1Name}),
  put(graphic2, {global, ?graphic2Name}),
  put(graphic3, {global, ?graphic3Name}),
  put(graphic4, {global, ?graphic4Name}).


handle_cast({updateFlowerStatus, Flower}, NewState) ->
  % Draw the updated status flower in graphicServer of this garden
  gen_server:cast(get(connectUIServerToGarden(Flower#flower.gardenID)), {update, Flower}, NewState),

  % Update the database.
  databaseUtils:updateFlowerRecord(Flower),
  {noreply, NewState};

handle_cast({deleteFlower, Flower}, NewState) ->
  % Delete the flower from the map in specific graphicServer.
  gen_server:cast(get(connectUIServerToGarden(Flower#flower.gardenID)), {update, Flower}, NewState),

  % Delete the flower from the database.
  databaseUtils:deleteFlower(Flower#flower.id),
  {noreply, NewState};

handle_cast({changeGardenerLocation, {OldX, OldY, Gardener}}, NewState)->
  % Send to specific graphic server to move the gardener.
  gen_server:cast(get(connectUIServerToGarden(Gardener#gardener.gardenNumber)), {makeSteps, {OldX, OldY, Gardener}}, NewState),

  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  {noreply, NewState};

handle_cast({gardenerResting, Gardener}, NewState) ->
  % Send to specific graphic server to sit down the gardener.
  gen_server:cast(get(connectUIServerToGarden(Gardener#gardener.gardenNumber)), {rest, Gardener}, NewState),

  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  {noreply, NewState}.


handle_call({sortedFlowerList, GardenName}, NewState)->
  SortedList = databaseUtils:flowerListSortedByDangerousLevel(GardenName),
  gen_server:cast(get(GardenName), {listFlowerInDanger, SortedList}, NewState).


recovery(GardenID, NewState)->
  FlowerInGardenID   = databaseUtils:listsRecordOfFlowerInGarden(GardenID),
  GardenerInGardenID = databaseUtils:listsRecordOfGardenerInGarden(GardenID),
  SortedFlowerList   = flowerListSortedByDangerousLevel(GardenID),

  % Send obejcts to graphic server to recover
  gen_server:cast(get(connectUIServerToGarden(GardenID)), {recover, {FlowerInGardenID, GardenerInGardenID}}, NewState),

  % Send the flowers sorted list by dangerous level to garden
  gen_server:cast(get(GardenID), {recover, SortedFlowerList}, NewState).



connectUIServerToGarden(GardenID)->
  case GardenID of
    garden1 -> graphic1;
    garden2 -> graphic2;
    garden3 -> graphic3;
    garden4 -> graphic4
end.


