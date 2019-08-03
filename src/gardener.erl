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
-include("globalVariables.hrl").

%% API
-export([init/1, handle_cast/2, handle_call/3]).
-export([start_link/3,newGardener/4, isArrive/4]).

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

%%TODO  change all cast msg

newGardener(Id, Type, State, Location)->
  #gardener{id = Id, type = Type, state = State, location = Location}.

%%start([Id, Type]) ->
%%  gen_server:start({global, Id}, ?MODULE, [Id, Type], []).
%%
%%init([Id, Type]) ->
%%  #gardener{id = Id, type = Type},
%%  {ok, #gardener{}}.


%%Creates a gen_server process as part of a supervision tree.
%%start_link(ServerName, Module, Args, Options) -> Result
start_link(GlobalName, Type, MainServerGlobalName) ->
  gen_server:start_link({global,GlobalName}, ?MODULE, [GlobalName, Type, MainServerGlobalName], []).

init([GlobalName, Type, MainServerGlobalName]) ->
  put(server,{global,MainServerGlobalName}),
  put(1,{global,?garden1Name}),
  put(2,{global,?garden2Name}),
  put(3,{global,?garden3Name}),
  put(4,{global,?garden4Name}),
  Status = gen_server:call(get(server),{connect,node()}),
  #gardener{id = GlobalName, type = Type , gardenNumber = 1},
  io:fwrite("gardener: init: Status = ~p ~n",[Status]), %TODO for test
  {ok, #gardener{}}.

%for case flower die while gardner on his way.
handle_cast({cancelWalk, State}, Sender) ->
  rest(State#gardener{state = resting}, Sender).

handle_cast({walkToFlower, Action, State, Flower, {DestX,DestY}}, Sender) ->
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

rest(State, Sender) ->

  %TODO gen_server:cast(Sender,{changeGardenerState, State#gardener{state = resting}}}),
  io:fwrite("rest: State = ~p ~n",[State]).

%TODO add check for changing garden
walking(State, Action, Flower, {DestX,DestY}, {StepX,StepY}, Sender) ->
  CancelWalk = isCanceledWalk(State), %for case flower die while gardner on his way.
  case CancelWalk of
    true -> %need to stop
      io:fwrite("Stopped walking: State= ~p ~n",[State]);
    false -> %keep walking
      timer:sleep(?walkTime),
      {MyX,MyY} = State#gardener.location,
      Arrive = isArrive(MyX, MyY, DestX, DestY),
      io:fwrite("walking: State= ~p Location = {~p, ~p}, Dest = {~p, ~p}, Arrive = ~p ~n",[State, MyX,MyY,DestX, DestY, Arrive]),
      case Arrive of
        false ->
          NewX = updateLocation(MyX, DestX),
          NewY = updateLocation(MyY, DestY),
          GardenNumber = moveGarden(NewX, State#gardener.gardenNumber),
          %TODO gen_server:cast(Sender,{changeGardenerState, State#gardener{location = {NewX,NewY}, gardenNumber = GardenNumber}}),
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
      end
  end.

moveGarden(NewX, GardenNumber) ->
  MaxRange = GardenNumber * ?screen_width,
  MinRange = MaxRange - ?screen_width - 1,
  if
    NewX > MaxRange ->
      GardenNumber + 1;
    NewX < MinRange ->
      GardenNumber - 1;
    true ->
      GardenNumber
  end.


isCanceledWalk(State) ->
  Status = State#gardener.state,
  case Status of
    walkToFlower ->
      false;
    true ->
      true
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

checkCoordinate(X, Y) ->
  if
    (((X >= 0) and (X =< 1280)) and ((X >= 0) and (X =< 880)))
      and ((X rem 80 =:= 0) and (Y rem 80 =:= 0))-> ok;
    true -> error
  end.

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

