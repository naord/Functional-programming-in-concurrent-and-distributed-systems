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
-export([init/1, start_link/5, handle_cast/2, handle_call/3]).
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
start_link(MainServerGlobalName,{Garden1Pid,Garden2Pid,Garden3Pid,Garden4Pid},GardenNumber,Type, Location) ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [MainServerGlobalName,Garden1Pid,Garden2Pid,Garden3Pid,Garden4Pid,GardenNumber,Type, Location], []).

init([MainServerGlobalName,{Garden1Pid,Garden2Pid,Garden3Pid,Garden4Pid},GardenNumber,Type, Location]) ->
  put(server,MainServerGlobalName),
  put(1,{garden,Garden1Pid}),
  put(2,{garden,Garden2Pid}),
  put(3,{garden,Garden3Pid}),
  put(4,{garden,Garden4Pid}),
  Gardener = #gardener{id = self(), type = Type, location = Location, gardenNumber = GardenNumber},
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
  timer:sleep(?walkTime),
  CancelWalk = isCanceledWalk(State),
  case CancelWalk of %for case flower die while gardner on his way.
    true -> %need to stop (status changed by garden)
      io:fwrite("Stopped walking: State= ~p ~n",[State]);
    false -> %keep walking
      timer:sleep(?walkTime),
      {MyX,MyY} = State#gardener.location,
      Arrive = isArrive(MyX, MyY, DestX, DestY),
      %io:fwrite("walking: State= ~p Location = {~p, ~p}, Dest = {~p, ~p}, Arrive = ~p ~n",[State, MyX,MyY,DestX, DestY, Arrive]),
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
                {CurrGarden, MyX,MyY,State#gardener{location = {NewX,NewY},gardenNumber = NewGarden}}}),
              timer:sleep(2000),
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
  %io:fwrite("GardenNumber = ~p,FlowerX=~p , FLowerY =~p ,MyX = ~p, NewFlowerX =~p ~n",[GardenNumber,FlowerX,FLowerY,MyX,NewFlowerX]), %TODO for debug
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
  io:fwrite(" Gardener: handleFlower, State= ~p ~p ~n", [State, FlowerPid]),
  FlowerPid ! handleProblem,
  timer:sleep(?handle),
  rest(State#gardener{state = resting}).
  %randWalk(State#gardener{state = walkRandom}, Sender).


test() ->
  gen_server:cast({global,gardener1},{walkToFlower, 123, 2, {0,?screen_height}}), %send to gardener
  gen_server:cast({global,gardener1},cancelWalk).


