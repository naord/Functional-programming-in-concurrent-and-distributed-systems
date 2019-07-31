%%%-------------------------------------------------------------------
%%% @author Naor Dahan
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. יול׳ 2019 19:46
%%%-------------------------------------------------------------------
-module(gardener).
-author("Naor Dahan").

%% API
-export([]).

-record(gardener,{id,state,location}).
-define(watering, 10).
-define(fertilizing, 10).
-define(pest_control, 10).
-define(uprooting, 10).
-define(walkTime, 10).

run(Gardener, Flower, Destenation, Self) ->
  State = Gardener#gardener.state,
  case State of
    walkRandom -> ok;%%TODO implement.
      %move_random
    walkToFlower -> ok;%%TODO implement.
      %move_to_flower
    waterPlant ->
      Time = ?watering,
      action(Gardener, State, Time, Flower, Self);
    fertilizePlant ->
      Time = ?fertilizing,
      action(Gardener, State, Time, Flower, Self);
    removePests ->
      Time = ?pest_control,
      action(Gardener, State, Time, Flower, Self);
    uprootingWeeds ->
      Time = ?uprooting,
      action(Gardener, State, Time, Flower, Self)
  end.

action(Gardener, Action, Time, Flower, Self) ->
  Flower ! Action, %TODO check flower module
  receive
  after Time -> %TODO need define file
    gen_server:cast(Self,{changeGardenerState, Gardener#gardener{state = walkRandom}}),%TODO: change after write gen_server
    timer:sleep(2000),
    run(Gardener#gardener{state = walkRandom}, Flower, _, Self)
  end.

walk(Gardener, Self, Flower, {DestX,DestY}, {AddX,AddY})->
  receive
    cancel ->
      gen_server:cast(Self,{changeGardenerState, Gardener#gardener{state = walkRandom}}),%TODO: change after write gen_server
      run(Gardener#gardener{state = walkRandom}, _, _, Self)
  after ?walkTime ->
    {MyX,MyY} = Gardener#gardener.location,
    Arrive = math:sqrt(((math:pow(MyY - DestY,2) + math:pow(MyX - DestX,2)))) < 30,
    State =  Gardener#gardener.state,
    case Arrive of
      false ->
        NewX = MyX + AddX,
        NewY = MyY + AddY,
        gen_server:cast(Self,{changeGardenerState, Gardener#gardener{location = {NewX,NewY}}}),
        move(Gardener#gardener{location = {NewX,NewY}}, Self, Flower, {DestX,DestY}, {AddX,AddY});
      true ->
        case State of
          to_flower->
            NewSelf = Self#gardener{status = water},
            gardener_loop(NewSelf,?BaseDest,Flower,Self);
          to_base ->
            NewSelf = Self#gardener{status = base},
            gen_server:cast(Self,{gardener_finished_mission_from_gardener,NewSelf})
        end
    end
  end.