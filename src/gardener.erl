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
-export([init/1, handle_cast/6, handle_cast/2, handle_call/3]).
-export([start/1,newGardener/4, isArrive/4]).

%%----------------------------------------------------
%%  Gardener states are atoms:
%%                          walkToFlower
%%                          handleFlower
%%                          walkRandom
%%                          resting
%%  Movement: move one square at a time-
%%            left
%%            right
%%            up
%%            down
%%            diagonally(up/down ,left/right)
%%           *location is multiple of 80
%%----------------------------------------------------

-include("globalVariables.hrl").

newGardener(Id, Type, State, Location)->
  #gardener{id = Id, type = Type, state = State, location = Location}.

start([Id, Type]) ->
  gen_server:start({global, Id}, ?MODULE, [Id, Type], []).

init([Id, Type]) ->
  #gardener{id = Id, type = Type},
  {ok, #gardener{}}.

handle_cast(walkToFlower, Action, State, Flower, {DestX,DestY}, Sender) ->
  InputCheck = abs((DestX rem 80) + (DestY rem 80)) == 0,
  case InputCheck of
     false ->
       io:fwrite("Wrong input to handle_cast(walkToFlower), input ={~p,~p} ~n",[DestX, DestY]);
     true ->
       Location = State#gardener.location,
       {AddX,AddY} = calculateProgress(Location, {DestX,DestY}),
       io:fwrite("walkToFlower: Location = ~p, calc = {~p, ~p} ~n",[Location, AddX,AddY]),
       walking(State#gardener{state = walkToFlower}, Action, Flower, {DestX,DestY}, {AddX,AddY}, Sender)
  end.

rest(State,Sender) ->
  %TODO gen_server:cast(Sender,{changeGardenerState, State#gardener{state = resting}}}),
  io:fwrite("rest: State = ~p ~n",[State]).

walking(State, Action, Flower, {DestX,DestY}, {StepX,StepY}, Sender) ->
  timer:sleep(?walkTime),
  {MyX,MyY} = State#gardener.location,
  Arrive = isArrive(MyX, MyY, DestX, DestY),
  io:fwrite("walking: State= ~p Location = {~p, ~p}, Dest = {~p, ~p}, Arrive = ~p ~n",[State, MyX,MyY,DestX, DestY, Arrive]),
  case Arrive of
    false ->
      NewX = updateLocation(MyX, DestX),
      NewY = updateLocation(MyY, DestY),
      %TODO gen_server:cast(Sender,{changeGardenerState, State#gardener{location = {NewX,NewY}}}),
      walking(State#gardener{location = {NewX,NewY}}, Action, Flower, {DestX,DestY}, {StepX,StepY}, Sender);
    true ->
      Status = State#gardener.state,
      case Status of
        walkRandom ->
          walkRandom;%randWalk(State,Sender);
        walkToFlower ->
          %TODO gen_server:cast(Sender,{changeGardenerState, State#gardener{state = {handleFlower, Action}}}),
          handleFlower(State#gardener{state = {handleFlower, Action}}, Action, Flower, Sender)
      end
  end.

updateLocation(Location, Dest) ->
  Delta = Dest - Location,
  if
    Delta == 0 ->
      Location;
    Delta > 0 ->
      Location + ?squareSize;
    Delta < 0 ->
      Location - ?squareSize
  end.

isArrive(MyX, MyY, DestX, DestY) ->
   ((MyX == DestX) and (MyY == DestY)).

handleFlower(State, Action, Flower, Sender) ->
  io:fwrite("handleFlower1, State= ~p ~n", [State]),
  %TODO Flower ! handleProblem, %TODO check flower module
  timer:sleep(?handle),
  %TODO gen_server:cast(Sender,{changeGardenerState, State#gardener{state = walkRandom}}),%TODO: change after write gen_server
  timer:sleep(2000),
  rest(State#gardener{state = resting}, Sender).
  %randWalk(State#gardener{state = walkRandom}, Sender).

calculateProgress({MyX,MyY},{DestX,DestY}) ->
  Dx = (DestX - MyX),
  Dy = (DestY - MyY),
  StepX = Dx/?squareSize,
  StepY = Dy/?squareSize,
  Steps = {StepX, StepY},
  io:fwrite("calculateProgress: Steps = ~p ~n",[Steps]),
  Steps.

handle_cast(handleFlower, State) ->
  io:fwrite("handleFlower ~n").
%erlang:error(not_implemented).

%%handle_cast(walkRandom, State, Flower) -> %%TODO implement.
%%  io:fwrite("walkRandom ~n").
%%  %erlang:error(not_implemented).

%%randWalk(State,Sender) -> %TODO need to update to new walking
%%  Xrand = getRandomPoint(),
%%  Yrand = getRandomPoint(),
%%  Location = State#gardener.location,
%%  {AddX,AddY} = calculateProgress(Location, {Xrand,Yrand}),
%%  io:fwrite("randWalk: State= ~p, Location = ~p, Dest = {~p,~p} calc = {~p, ~p} ~n",[State,Location, Xrand,Yrand, AddX,AddY]),
%%  walking(State, walkRandom, walkRandom, {Xrand,Yrand}, {AddX,AddY}, Sender).

%%getRandomPoint() ->
%%  rand:uniform(?gardenSize).

%%walking(State, Action, Flower, {DestX,DestY}, {AddX,AddY}, Sender) ->
%%  timer:sleep(?walkTime),
%%  {MyX,MyY} = State#gardener.location,
%%  Arrive = isArrive(MyX, MyY, DestX, DestY), %math:sqrt(((math:pow(MyY - DestY,2) + math:pow(MyX - DestX,2)))) < 30,
%%  io:fwrite("walking: State= ~p Location = {~p, ~p}, Arrive = ~p ~n",[State, MyX,MyY, Arrive]),
%%  case Arrive of
%%    false ->
%%      NewX = MyX + AddX,
%%      NewY = MyY + AddY,
%%      %TODO gen_server:cast(Sender,{changeGardenerState, State#gardener{location = {NewX,NewY}}}),
%%      walking(State#gardener{location = {NewX,NewY}}, Action, Flower, {DestX,DestY}, {AddX,AddY}, Sender);
%%    true ->
%%      Status = State#gardener.state,
%%      case Status of
%%        walkRandom ->
%%          walkRandom;%randWalk(State,Sender);
%%        walkToFlower ->
%%          %TODO gen_server:cast(Sender,{changeGardenerState, State#gardener{state = {handleFlower, Action}}}),
%%          handleFlower(State#gardener{state = {handleFlower, Action}}, Action, Flower, Sender)
%%      end
%%  end.

%%calculateProgress({MyX,MyY},{DestX,DestY}) ->
%%  Dx = (MyX - DestX),
%%  Dy = (MyY - DestY),
%%  Alpha = math:atan2(Dy,Dx) + math:atan(1) * 4,
%%  Step = {round(math:cos(Alpha)*?squareSize),round(math:sin(Alpha)*?squareSize)},
%%  io:fwrite("calculateProgress: Alpha = ~p, Step = ~p ~n",[Alpha,Step]),
%%  Step.

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
%%          io:fwrite("error gardener state")
%%    end
%%  end.
%%
%%
%%
%%
%%
%%handle_cast(Request, State) ->
%%  erlang:error(not_implemented).
handle_call(Request, From, State) ->
  erlang:error(not_implemented).

