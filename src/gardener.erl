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
-behavior(gen_server).

%% API
-export([init/1, handle_cast/2]). %, handle_call/3
-export([start/1]).

-record(gardener,{id, state = walkRandom, location = {0,0}}).
-define(handle, 10). %TODO: need to create one define file
-define(progressSize,30).
-define(walkTime, 10).
%%-define(watering, 10).
%%-define(fertilizing, 10).
%%-define(pest_control, 10).
%%-define(uprooting, 10).

start(Id) ->
  gen_server:start({global, Id}, ?MODULE, Id, []).

init(Args) ->
  #gardener{id = Args},
  {ok, #gardener{}}.

handle_cast(walkRandom, State, Flower) -> %%TODO implement.
  erlang:error(not_implemented).

handle_cast(walkToFlower, Action, State, Flower, {DestX,DestY}, Sender) ->
  Location = State#gardener.location,
  {AddX,AddY} = calculateProgress(Location, {DestX,DestY}),
  walking(State, Action, Flower, {DestX,DestY}, {AddX,AddY}, Sender).

handle_cast(handleFlower, State) ->
  erlang:error(not_implemented).

walking(State, Action, Flower, {DestX,DestY}, {AddX,AddY}, Sender) ->
  timer:sleep(?walkTime),
  {MyX,MyY} = State#gardener.location,
  Arrive = isArrive(MyX, MyY, DestX, DestY), %math:sqrt(((math:pow(MyY - DestY,2) + math:pow(MyX - DestX,2)))) < 30,
  case Arrive of
    false ->
      NewX = MyX + AddX,
      NewY = MyY + AddY,
      gen_server:cast(Sender,{changeGardenerState, State#gardener{location = {NewX,NewY}}}),
      walking(State#gardener{location = {NewX,NewY}}, Action, Flower, {DestX,DestY}, {AddX,AddY}, Sender);
    true ->
      gen_server:cast(Sender,{changeGardenerState, State#gardener{state = {handleFlower, Action}}}),
      handleFlower(State#gardener{state = {handleFlower, Action}}, Action, Flower, Sender)
  end.

isArrive(MyX, MyY, DestX, DestY) ->
  math:sqrt(((math:pow(MyY - DestY,2) + math:pow(MyX - DestX,2)))) < ?progressSize.

handleFlower(State, Action, Flower, Sender) ->
  Flower ! handleProblem, %TODO check flower module
  timer:sleep(?handle),
  gen_server:cast(Sender,{changeGardenerState, State#gardener{state = walkRandom}}),%TODO: change after write gen_server
  timer:sleep(2000),
  walkRandom(State#gardener{state = walkRandom}, Sender).

walkRandom(State,Sender) ->
  erlang:error(not_implemented).

calculateProgress({MyX,MyY},{DestX,DestY}) ->
  Dx = (MyX - DestX),
  Dy = (MyY - DestY),
  Alpha = math:atan2(Dy,Dx) + math:atan(1) * 4,
  {round(math:cos(Alpha)*?progressSize),round(math:sin(Alpha)*?progressSize)}.



%%run(Gardener, Flower, Destination, Self) ->
%%  State = Gardener#gardener.state,
%%  case State of
%%    walkRandom -> ok;%%TODO implement.
%%      %move_random
%%    {walkToFlower, Action} ->
%%      Location = Gardener#gardener.location,
%%      Progress = calculateProgress(Location, Destination),
%%      walk(Gardener, Self, Flower, Destination, Progress);
%%    {handleFlower, Action} ->
%%      action(Gardener, ?handle, Flower, Self)
%%%%    waterPlant ->
%%%%      Time = ?watering,
%%%%      action(Gardener, Time, Flower, Self);
%%%%    fertilizePlant ->
%%%%      Time = ?fertilizing,
%%%%      action(Gardener, Time, Flower, Self);
%%%%    removePests ->
%%%%      Time = ?pest_control,
%%%%      action(Gardener, Time, Flower, Self);
%%%%    uprootingWeeds ->
%%%%      Time = ?uprooting,
%%%%      action(Gardener, Time, Flower, Self)
%%  end.
%%
%%action(Gardener, Time, Flower, Self) ->
%%  Flower ! handleProblem, %TODO check flower module
%%  receive
%%  after Time -> %TODO need define file
%%    gen_server:cast(Self,{changeGardenerState, Gardener#gardener{state = walkRandom}}),%TODO: change after write gen_server
%%    timer:sleep(2000),
%%    run(Gardener#gardener{state = walkRandom}, Flower, _, Self)
%%  end.
%%
%%walk(Gardener, Self, Flower, {DestX,DestY}, {AddX,AddY})->
%%  receive
%%    cancel ->
%%      gen_server:cast(Self,{changeGardenerState, Gardener#gardener{state = walkRandom}}),%TODO: change after write gen_server
%%      run(Gardener#gardener{state = walkRandom}, _, _, Self)
%%  after ?walkTime ->
%%    {MyX,MyY} = Gardener#gardener.location,
%%    Arrive = math:sqrt(((math:pow(MyY - DestY,2) + math:pow(MyX - DestX,2)))) < 30,
%%    {WalkToFlower, Action} =  Gardener#gardener.state,
%%    case WalkToFlower of
%%      walkToFlower ->
%%        case Arrive of
%%          false ->
%%            NewX = MyX + AddX,
%%            NewY = MyY + AddY,
%%            gen_server:cast(Self,{changeGardenerState, Gardener#gardener{location = {NewX,NewY}}}),
%%            run(Gardener#gardener{location = {NewX,NewY}}, Self, Flower, {DestX,DestY}, {AddX,AddY});
%%          true ->
%%            gen_server:cast(Self,{changeGardenerState, Gardener#gardener{state = {handleFlower, Action}}}),
%%            run(Gardener#gardener{state = {handleFlower, Action}}, Flower, _, Self)
%%        end;
%%      _ ->
%%          print("error gardener state")
%%    end
%%  end.
%%
%%
%%
%%
%%
%%handle_call(Request, From, State) ->
%%  erlang:error(not_implemented).
%%
%%handle_cast(Request, State) ->
%%  erlang:error(not_implemented).