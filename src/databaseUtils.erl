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
-export([startDatabase/0,
  getRestingGardener/0,
  flowerListSortedByDangerousLevel/0,
  listsRecordOfFlowerInGarden/1,
  listsRecordOfGardenerInGarden/1,
  getAllObjectsOf/1,
  getGardenerWithID/1,
  updateGardenerRecord/1,
  deleteFlower/1,
  deleteGardener/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("globalVariables.hrl").


startDatabase()->
  try
    % Initialize new empty DB.
    mnesia:delete_table(garden),
    mnesia:delete_table(gardener),
    mnesia:delete_table(flower),

    mnesia:create_schema([node()]),
    mnesia:start(),

    % Create tableS
    mnesia:create_table(gardener ,[{attributes, record_info(fields, gardener)} , {type,set}]),
    mnesia:create_table(flower   ,[{attributes, record_info(fields, flower)}   , {type,set}])

  catch
    error :_  -> io:format("FAIL TO INIT MNESIA DATA BASE");
    exit  :_  -> io:format("FAIL TO INIT MNESIA DATA BASE")
  end.

%===========================================================================
%                           QUERY FROM DATA BASE
%===========================================================================

%-----------------------------------------
% listsRecordOfFlowerInGarden
% Return all the flower in garden number GardenID
%-----------------------------------------
listsRecordOfFlowerInGarden(GardenID)->
  F = fun() ->
    Quary = qlc:q([Flower || Flower <- mnesia:table(flower), Flower#flower.gardenID =:= GardenID]),
    qlc:e(Quary)
      end,

  {atomic, Ans} = mnesia:transaction(F),
  Ans.

%-----------------------------------------
% listsRecordOfGardenerInGarden-

% Return all the gardener in garden number GardenID
%-----------------------------------------
listsRecordOfGardenerInGarden(GardenID)->
  F = fun() ->
    Quary = qlc:q([Gardener || Gardener <- mnesia:table(gardener), Gardener#gardener.gardenNumber =:= GardenID]),
    qlc:e(Quary)
      end,
  {atomic, Ans} = mnesia:transaction(F),
  Ans.

getRestingGardener()->
  F = fun() ->
    Quary = qlc:q([Gardener || Gardener <- mnesia:table(gardener), Gardener#gardener.state =:= resting]),
    qlc:e(Quary)
      end,
  {atomic, Ans} = mnesia:transaction(F),
  Ans.

%-----------------------------------------
% getAllKeysOf-

% Return all the objects in all the garden
% which store in MnesiaBlock (flower,
% gardener or garden).
%-----------------------------------------
getAllObjectsOf(MnesiaBlock) ->
  try
  Pattern = mnesia:table_info(MnesiaBlock, wild_pattern),
  mnesia:dirty_match_object(Pattern)
  catch
    exit  :_  ->  io:format("EXIT: FAILED TO GET ALL OBJECT FROM TYPE: " ++ atom_to_list(MnesiaBlock));
    error :_  ->  io:format("ERROR: FAILED TO GET ALL OBJECT FROM TYPE: " ++ atom_to_list(MnesiaBlock))
  end.

%-----------------------------------------
% flowerListSortedByDangerousLevel -

% Return sorted list of all the flower
% which need handle their problem such
% that the first in the list is the
% most urgent case.
%-----------------------------------------
flowerListSortedByDangerousLevel()->
  AllFLowerInDanger =
    fun() ->
      Quary = qlc:q([Flower || Flower <- mnesia:table(flower), Flower#flower.status =/= normal]),
      qlc:e(Quary)
    end,
  {atomic, AllFLowerInDangerList} = mnesia:transaction(AllFLowerInDanger),

  ComperTo =
    fun(A, B) ->
      AtolarableTime = getTolarableTime(A#flower.status),
      BtolarableTime = getTolarableTime(B#flower.status),
      AsinceProblem = A#flower.timeSinceProblem,
      BsinceProblem = B#flower.timeSinceProblem,
      if
        (AtolarableTime - AsinceProblem) > (BtolarableTime - BsinceProblem) -> false ;
        (AtolarableTime - AsinceProblem) =< (BtolarableTime - BsinceProblem) -> true
      end
    end,
  lists:sort(ComperTo, AllFLowerInDangerList).

%-----------------------------------------
% getFlowerWithID -

% Return the flower record correspond to
% FlowerID.
%-----------------------------------------
getFlowerWithID(FlowerID) ->
  F = fun () -> mnesia:read(flower, FlowerID) end,
  try
    {_, Ans} = mnesia:transaction(F),
    Ans
  catch
    error :_  -> io:format("ERROR: CAN RETURN FLOWER WITH ID:" ++ FlowerID);
    exit  :_  ->  io:format("EXIT: CAN RETURN FLOWER WITH ID:" ++ FlowerID)
  end.

%-----------------------------------------
% getGardenerWithID -

% Return the gardener record correspond to
% GardenerID.
%-----------------------------------------
getGardenerWithID(GardenerID) ->
  F = fun () -> mnesia:read(gardener, GardenerID) end,
  try
    {_, Ans} = mnesia:transaction(F),
    Ans
  catch
    exit  :_  -> io:format("EXIT: CAN RETURN FLOWER WITH ID:" ++ GardenerID);
    error :_  -> io:format("ERROR: CAN RETURN FLOWER WITH ID:" ++ GardenerID)
end.

%TODO : sortedGardenerDistanceFromFlower()
%===========================================================================
%                            UPDATE OR ADD STATUS
%===========================================================================

updateFlowerRecord(Flower)->
  F = fun() ->
    mnesia:write(Flower)
      end,
  mnesia:transaction(F).

updateGardenerRecord(Gardener)->
  F = fun() ->
    mnesia:write(Gardener)
      end,
  mnesia:transaction(F).

%===========================================================================
%                          DELETE FROM DATA BASE
%===========================================================================
deleteFlower(FlowerID) ->
  F = fun() ->
    mnesia:delete(flower, FlowerID, write)
      end,
  try
    mnesia:transaction(F)
  catch
    error :_  -> io:format("FAILED TO DELETE FLOWER WITH ID: " ++ FlowerID);
    exit :_  ->  io:format("FAILED TO DELETE FLOWER WITH ID: " ++ FlowerID)
  end.

deleteGardener(GardenerID) ->
  F = fun() ->
    mnesia:delete(flower, GardenerID, write)
      end,
  try
    mnesia:transaction(F)
  catch
    exit :_  ->  io:format("FAILED TO DELETE GARDENER WITH ID: " ++ GardenerID);
    error :_  -> io:format("FAILED TO DELETE GARDENER WITH ID: " ++ GardenerID)
  end.

getTolarableTime(Status)->
  case Status of
    pests_ant    -> ?pestsTime;
    pests_purple -> ?pestsTime;
    pests_green  -> ?pestsTime;
    wilted       -> ?waterTime
  end.



%%test()->
%%  startDatabase(),
%%  A = #flower{id = 1, type = a, status = pests_ant, timeSinceProblem = 50,  gardenerID = 1, gardenID = 2, x = 10, y = 10 },
%%  updateFlowerRecord(A),
%%  updateFlowerRecord(#flower{id = 4, type = a, status = pests_ant, timeSinceProblem = 60,  gardenerID = 1, gardenID = 2, x = 10, y = 10 }),
%%  updateFlowerRecord(#flower{id = 3, type = b, status = wilted, timeSinceProblem = 100, gardenerID = 1, gardenID = 2, x = 22, y = 10 }),
%%  updateFlowerRecord(#flower{id = 2, type = a, status = wilted, timeSinceProblem = 190, gardenerID = 1, gardenID = 2, x = 10, y = 10 }),
%%  L = getFlowerWithID(4),
%%
%%
%%
%%  flowerListSortedByDangerousLevel(1), ok.

