%%%-------------------------------------------------------------------
%%% @author nirkov
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2019 17:09
%%%-------------------------------------------------------------------
-module(databaseUtils).
-author("nirkov").

%% API
-export([test/0]).
-include_lib("stdlib/include/qlc.hrl").
-include("globalVariables.hrl").


startDatabase()->
  try
    % Initialize new empty DB.
    mnesia:delete_table(gardens),
    mnesia:delete_table(gardeners),
    mnesia:delete_table(flowers),

    mnesia:create_schema([node()]),
    mnesia:start(),

    % Create tableS
    mnesia:create_table(gardens,[{type,bag}, {record_name, garden}, {attributes,record_info(fields, garden)}]),
    mnesia:create_table(gardeners,[{type,bag}, {record_name, gardener}, {attributes,record_info(fields, gardener)}]),
    mnesia:create_table(flowers,[{type,bag}, {record_name, flower}, {attributes,record_info(fields, flower)}]),
    mnesia:create_table(graphic,[{type,bag}, {record_name, graphic_server}, {attributes,record_info(fields, graphic_server)}])

  catch
    error :_  -> io:format("FAIL TO INIT MNESIA DATA BASE");
    exit  :_  -> io:format("FAIL TO INIT MNESIA DATA BASE")
  end.


listsRecordOfFlowerInGarden(GardenID)->
  F = fun() ->
    Quary = qlc:q([Flower || Flower <- mnesia:table(flowers), Flower#flower.gardenerId =:= GardenID]),
    qlc:e(Quary)
      end,

  {atomic, Ans} = mnesia:transaction(F),
  Ans.

listsRecordOfGardenerInGarden(GardenID)->
  F = fun() ->
    Quary = qlc:q([Gardener || Gardener <- mnesia:table(gardeners), Gardener#gardener.gardenId =:= GardenID]),
    qlc:e(Quary)
      end,
  {atomic, Ans} = mnesia:transaction(F),
  Ans.

%%listsRecordOfGardenerInGarden(GardenID)->
%%  F = fun() ->
%%    Quary = qlc:q([Gardener || Gardener <- mnesia:table(gardeners), Gardener#gardener.gardenId =:= GardenID]),
%%    qlc:e(Quary)
%%      end,
%%  {atomic, Ans} = mnesia:transaction(F),
%%  Ans.

getGraphicServerObjectsMatrix()->
  F = fun() ->
    Quary = qlc:q([ObjectsMatrix || UI <- mnesia:table(graphic), ObjectsMatrix <- UI#graphic_server.objectsMatrix]),
    qlc:e(Quary)
      end,
  {atomic, Ans} = mnesia:transaction(F),
  Ans.

updateFlowerRecord(Flower)->
  F = fun() ->
    mnesia:write(Flower)
      end,
  mnesia:transaction(F).


test()->
  startDatabase(),
  updateFlowerRecord(#flower{id = 1, type = a, status = water,  gardenerId = 1, gardenId = 2, x = 10, y = 10 }),
  updateFlowerRecord(#flower{id = 1, type = a, status = water,  gardenerId = 1, gardenId = 2, x = 10, y = 10 }),
  updateFlowerRecord(#flower{id = 2, type = a, status = water,  gardenerId = 1, gardenId = 2, x = 10, y = 10 }),
  L = listsRecordOfFlowerInGarden(2),
  X =3.

