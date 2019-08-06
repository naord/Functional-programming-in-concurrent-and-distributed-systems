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
-export([init/1, start_link/2, handle_cast/2, handle_call/3]).
-export([test/0]). %TODO for test

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

%%Creates a gen_server process as part of a supervision tree.
%%start_link(ServerName, Module, Args, Options) -> Result
start_link(GlobalName, Type) ->
  gen_server:start_link({global,GlobalName}, ?MODULE, [GlobalName, Type], []).

init([GlobalName, Type]) ->
  put(server,{global,?masterServerName}),
  put(1,{global,?garden1Name}),
  put(2,{global,?garden2Name}),
  put(3,{global,?garden3Name}),
  put(4,{global,?garden4Name}),
  %TODO Status = gen_server:call(get(server),{connect,node()}),
  Gardener = #gardener{id = GlobalName, type = Type}, %TODO ask nir about starting garden
  gen_server:cast(get(server),{newGardener,Gardener}),
  {ok, Gardener}.

%for case flower die while gardner on his way.
handle_cast(cancelWalk, State) ->
  rest(State#gardener{state = resting});

%handle flower request.
handle_cast({walkToFlower, FlowerId, FlowerPid, GardenNumber, {DestX,DestY}}, State) ->
  InputCheck = abs((DestX rem 80) + (DestY rem 80)) == 0, %TODO delete?
  case InputCheck of
     false ->
       io:fwrite("Wrong input to handle_cast(walkToFlower), input ={~p,~p} ~n",[DestX, DestY]);
     true ->
       {MyX, MyY} = State#gardener.location,
       Dest = calcNewDest(GardenNumber, DestX, DestY, MyX),% need to get near the flower.
       io:fwrite("walkToFlower:get(gardenNumber)=~p Location = ~p, Dest1 = ~p newDest = ~p ~n",[get(State#gardener.gardenNumber),{MyX, MyY},{DestX,DestY},Dest]),%TODO for debug
       gen_server:cast(get(State#gardener.gardenNumber),{gardenerWalkToFlower, State#gardener{state = walkToFlower, flowerId = FlowerId}}),
       walking(State#gardener{state = walkToFlower, flowerId = FlowerId}, Dest, FlowerPid)
  end;

handle_cast(Request, State) -> %TODO for debug
  io:fwrite("handle_cast gardener: wrong request. Request = ~p State = ~p ~n",[Request, State]),
  ok.

handle_call(Request,From,State)->
  {Request,From,State}.

rest(State) ->
  gen_server:cast(get(State#gardener.gardenNumber),{gardenerResting, State#gardener{state = resting}}),
  io:fwrite("rest: State = ~p ~n",[State]),%TODO for debug
  {noreply, State}.

walking(State, {DestX,DestY}, FlowerPid) ->
  CancelWalk = isCanceledWalk(State),
  case CancelWalk of %for case flower die while gardner on his way.
    true -> %need to stop (status changed by garden)
      io:fwrite("Stopped walking: State= ~p ~n",[State]);
    false -> %keep walking
      timer:sleep(?walkTime),
      {MyX,MyY} = State#gardener.location,
      Arrive = isArrive(MyX, MyY, DestX, DestY),
      io:fwrite("walking: State= ~p Location = {~p, ~p}, Dest = {~p, ~p}, Arrive = ~p ~n",[State, MyX,MyY,DestX, DestY, Arrive]),
      case Arrive of
        false -> %keep walking
          NewX = updateLocation(MyX, DestX),
          NewY = updateLocation(MyY, DestY),
          CurrGarden = State#gardener.gardenNumber,
          NewGarden = moveGarden(NewX, State#gardener.gardenNumber),
          if  %for case gardener move to different garden.
            CurrGarden =:= NewGarden -> %stay in the same garden
              gen_server:cast(get(State#gardener.gardenNumber),{changeGardenerLocation,
                {MyX,MyY,State#gardener{location = {NewX,NewY}}}}),
              walking(State#gardener{location = {NewX,NewY}}, {DestX,DestY},FlowerPid);
            true -> %move to new garden
              gen_server:cast(get(State#gardener.gardenNumber),{changeGardenerGarden,
                {MyX,MyY,State#gardener{location = {NewX,NewY},gardenNumber = NewGarden}}}),
              walking(State#gardener{location = {NewX,NewY},gardenNumber = NewGarden}, {DestX,DestY},FlowerPid)
          end;
        true -> %stop walking
          Status = State#gardener.state,
          case Status of
%%            walkRandom ->
%%              walkRandom;%randWalk(State,Sender);
            walkToFlower ->
              %gen_server:cast(get(State#gardener.gardenNumber),{gardenerHandleFlower,State#gardener{state = handleFlower}}),
              handleFlower(State#gardener{state = handleFlower}, FlowerPid);
            _->
              io:fwrite("walking: bad state after arriveing. State= ~p ~n",[State]) %TODO for debug
          end
      end
  end.

calcNewDest(GardenNumber, FlowerX, FLowerY, MyX)->
  NewFlowerX = ((GardenNumber - 1) * ?screen_width) + FlowerX,
  io:fwrite("GardenNumber = ~p,FlowerX=~p , FLowerY =~p ,MyX = ~p, NewFlowerX =~p ~n",[GardenNumber,FlowerX,FLowerY,MyX,NewFlowerX]), %TODO for debug
  if
    NewFlowerX >= MyX -> if
                           FlowerX =:= 0 -> {NewFlowerX + 2*?squareSize, FLowerY};
                           true -> {NewFlowerX - ?squareSize, FLowerY}
                         end;

    NewFlowerX < MyX -> if
                          FlowerX =:= ?screen_width -> {NewFlowerX - 2*?squareSize, FLowerY};
                          true ->  {NewFlowerX + ?squareSize, FLowerY}
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

handleFlower(State, FlowerPid) ->
  io:fwrite("handleFlower, State= ~p ~p ~n", [State, FlowerPid]),
  FlowerPid ! handleProblem,
  timer:sleep(?handle),
  rest(State#gardener{state = resting}).
  %randWalk(State#gardener{state = walkRandom}, Sender).


test() ->
  gen_server:cast({global,gardener1},{walkToFlower, 123, 2, {0,?screen_height}}), %send to gardener
  gen_server:cast({global,gardener1},cancelWalk).

%%calculateProgress({MyX,MyY},{DestX,DestY}) ->
%%  Dx = (DestX - MyX),
%%  Dy = (DestY - MyY),
%%  StepX = Dx/?squareSize,
%%  StepY = Dy/?squareSize,
%%  Steps = {StepX, StepY},
%%  io:fwrite("calculateProgress: Steps = ~p ~n",[Steps]),
%%  Steps.


%%checkCoordinate(X, Y) ->
%%  if
%%    (((X >= 0) and (X =< 1280)) and ((X >= 0) and (X =< 880)))
%%      and ((X rem 80 =:= 0) and (Y rem 80 =:= 0))-> ok;
%%    true -> error
%%  end.

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

