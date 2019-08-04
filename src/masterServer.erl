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


handle_cast({updateFlowerStatus, Flower}, State) ->
  % Draw the updated status flower in graphicServer of this garden
  gen_server:cast(get(Flower#flower.gardenID), {update, Flower}),

  % Update the database.
  databaseUtils:update_flower(Flower),
  {noreply, State};

handle_cast({deleteFlower, Flower}, State) ->
  % Delete the flower from the map in specific graphicServer.
  gen_server:cast(get(Flower#flower.gardenID), {update, Flower}),

  % Delete the flower from the database.
  databaseUtils:update_flower(Flower#flower.id),
  {noreply, State}.

יhandle_cast({changeGardenerLocation, Gardener}, State)->

  {noreply, State}.


handle_call({sortedFlowerList, GardenName}, State)->
  SortedList = databaseUtils:flowerListSortedByDangerousLevel(GardenName),
  gen_server:cast(get(GardenName), {listFlowerInDanger, SortedList}).





