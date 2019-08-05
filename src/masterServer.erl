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

-export([start/0, init/1, handle_cast/2, recovery/1]).
-include("globalVariables.hrl").
%% API
-behaviour(gen_server).
-record(state,{}).

start()->
  gen_server:start({global, ?masterServerName}, ?MODULE, [], []).

init(_)->
  put(garden1, {global, ?garden1Name}),
  put(garden2, {global, ?garden2Name}),
  put(garden3, {global, ?garden3Name}),
  put(garden4, {global, ?garden4Name}),

  put(graphic1, {global, ?graphic1Name}),
  put(graphic2, {global, ?graphic2Name}),
  put(graphic3, {global, ?graphic3Name}),
  put(graphic4, {global, ?graphic4Name}),
  databaseUtils:startDatabase(),
  {ok, #state{}}.

handle_cast({newGardener, Gardener}, NewState) ->
  databaseUtils:updateGardenerRecord(Gardener),

  % Send to specific graphic server to sit down the gardener.
  gen_server:cast(get(connectUIServerToGarden(Gardener#gardener.gardenNumber)), {rest, Gardener}, NewState);


handle_cast({updateFlowerStatus, Flower}, NewState) ->
  % Draw the updated status flower in graphicServer of this garden
  gen_server:cast(get(connectUIServerToGarden(Flower#flower.gardenID)), {update, Flower}), %TODO need to delete NewState from cast

  % Update the database.
  databaseUtils:updateFlowerRecord(Flower),
  FlowerStatus = Flower#flower.status,
  if
    FlowerStatus =/= normal ->
      Gardeners = databaseUtils:getRestingGardener(),
      Length =  lists:flatlength(Gardeners),
      if
        Length > 0 ->
          [Gardener | _] = Gardeners,
          gen_server:cast(getGardenName(Flower#flower.gardenID), {sendGardenerToFlower, Gardener, Flower})
      end
  end,
  {noreply, NewState};

handle_cast({deleteFlower, Flower}, NewState) ->
  % Delete the flower from the map in specific graphicServer.
  gen_server:cast(get(connectUIServerToGarden(Flower#flower.gardenID)), {update, Flower}),

  % Delete the flower from the database.
  databaseUtils:deleteFlower(Flower#flower.id),
  {noreply, NewState};

handle_cast({changeGardenerLocation, {OldX, OldY, Gardener}}, NewState)->
  % Send to specific graphic server to move the gardener.
  gen_server:cast(get(connectUIServerToGarden(Gardener#gardener.gardenNumber)), {makeSteps, {OldX, OldY, Gardener}}),

  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  {noreply, NewState};

handle_cast({gardenerResting, Gardener}, NewState) ->
  % Send to specific graphic server to sit down the gardener.
  gen_server:cast(get(connectUIServerToGarden(Gardener#gardener.gardenNumber)), {rest, Gardener}),

  % Update the database.
  databaseUtils:updateGardenerRecord(Gardener),
  ListFlowerInDanger = databaseUtils:flowerListSortedByDangerousLevel(),
  Length = lists:flatlength(ListFlowerInDanger),
  if
    Length > 0 ->
      [Flower|_] = ListFlowerInDanger,
      gen_server:cast(getGardenName(Flower#flower.gardenID), {sendGardenerToFlower, Gardener, Flower})
  end,
  {noreply, NewState}.



recovery(GardenID)->
  FlowerInGardenID   = databaseUtils:listsRecordOfFlowerInGarden(GardenID),
  GardenerInGardenID = databaseUtils:listsRecordOfGardenerInGarden(GardenID),
  SortedFlowerList   = databaseUtils:flowerListSortedByDangerousLevel(),

  % Send obejcts to graphic server to recover
  %TODO: NEED TO INIT THE GRAPHIC SERVER HERE AND HANDLE RECOVERY. SHOULD BE HANDLE_CALL TO GRAPHICSERVER?
  gen_server:cast(get(connectUIServerToGarden(GardenID)), {recovery, {FlowerInGardenID, GardenerInGardenID}}),

  % Send the flowers sorted list by dangerous level to garden
  gen_server:cast(get(GardenID), {recovery, SortedFlowerList}).



connectUIServerToGarden(GardenID)->
  case GardenID of
    garden1 -> graphic1;
    garden2 -> graphic2;
    garden3 -> graphic3;
    garden4 -> graphic4
end.

getGardenName(Number) ->
  case Number of
    1 -> {global,?garden1Name};
    2 -> {global,?garden2Name};
    3 -> {global,?garden3Name};
    _ -> {global,?garden4Name}
  end.

