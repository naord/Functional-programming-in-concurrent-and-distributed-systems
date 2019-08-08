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
-export([init/1, start_link/6, handle_cast/2, handle_call/3]).

%%----------------------------------------------------
%%  Gardener states are atoms:
%%                          walkToFlower
%%                          handleFlower
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
start_link(MainServerGlobalName,GardensPids,GardenNumber,Type, Location,Num) ->
  MyNameAtom = list_to_atom("gardener" ++ integer_to_list(Num)),
  gen_server:start_link({local,MyNameAtom}, ?MODULE, [MainServerGlobalName,GardensPids,GardenNumber,Type, Location], []).

init([MainServerGlobalName,GardensPids,GardenNumber,Type, Location]) ->
  put(server,MainServerGlobalName),
  setGardenPids(1,GardensPids),
  Gardener = #gardener{id = self(), type = Type, location = Location, gardenNumber = GardenNumber},
  gen_server:cast(MainServerGlobalName,{newGardener,Gardener}),
  {ok, Gardener}.

%for case flower die while gardner on his way.
handle_cast(cancelWalk, State) ->
  rest(State#gardener{state = resting});

%handle flower request.
handle_cast({walkToFlower, FlowerId, FlowerPid, GardenNumber, {DestX,DestY},GardenPid}, State) ->
  InputCheck = abs((DestX rem 80) + (DestY rem 80)) == 0,
  case InputCheck of
    false ->
      io:fwrite("Wrong input to handle_cast(walkToFlower), input ={~p,~p} ~n",[DestX, DestY]);
    true ->
      {MyX, _} = State#gardener.location,
      Dest = calcNewDest(GardenNumber, DestX, DestY, MyX),% need to get near the flower.
      gen_server:cast(get(State#gardener.gardenNumber),{gardenerWalkToFlower, State#gardener{state = walkToFlower, flowerId = FlowerId}}),
      walking(State#gardener{state = walkToFlower, flowerId = FlowerId}, Dest, FlowerPid, GardenPid)
  end;

handle_cast(Request, State) ->
  io:fwrite("handle_cast gardener: wrong request. Request = ~p State = ~p ~n",[Request, State]),
  ok.

handle_call(Request,From,State)->
  {Request,From,State}.

%%send message to garden to change gardener state to resting
rest(State) ->
  gen_server:cast(get(State#gardener.gardenNumber),{gardenerResting, State#gardener{state = resting}}),
  {noreply, State}.

%%change gardener location and cast to garden
walking(State, {DestX,DestY}, FlowerPid, GardenPid) ->
  timer:sleep(?walkTime),
  CancelWalk = isCanceledWalk(State,GardenPid),
  case CancelWalk of %for case flower die while gardner on his way.
    true -> %need to stop (status changed by garden)
      rest(State);
    false -> %keep walking
      timer:sleep(?walkTime),
      {MyX,MyY} = State#gardener.location,
      Arrive = isArrive(MyX, MyY, DestX, DestY),
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
              walking(State#gardener{location = {NewX,NewY}}, {DestX,DestY},FlowerPid,GardenPid);
            true -> %move to new garden
              gen_server:cast(get(State#gardener.gardenNumber),{changeGardenerGarden,
                {CurrGarden, MyX,MyY,State#gardener{location = {NewX,NewY},gardenNumber = NewGarden}}}),
              timer:sleep(1000),
              walking(State#gardener{location = {NewX,NewY},gardenNumber = NewGarden}, {DestX,DestY},FlowerPid,GardenPid)
          end;
        true -> %stop walking
          Status = State#gardener.state,
          case Status of
            walkToFlower ->
              handleFlower(State#gardener{state = handleFlower}, FlowerPid);
            _->
              io:fwrite("walking: bad state after arriveing. State= ~p ~n",[State]) %for debug
          end
      end
  end.

%%calculate new destination
calcNewDest(GardenNumber, FlowerX, FLowerY, MyX)->
  NewFlowerX = ((GardenNumber - 1) * ?screen_width) + FlowerX,
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

isCanceledWalk(State,GardenPid) ->
  try
    Reply = gen_server:call(GardenPid,{isFlowerAlive, State#gardener.flowerId}),
    Length =  lists:flatlength(Reply),
    case Length > 0 of
      true ->
        false;
      false ->
        true
    end
  catch
    exit  :_  -> true;
    error :_  -> true
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
  FlowerPid ! handleProblem,
  timer:sleep(?handle),
  rest(State#gardener{state = resting}).

setGardenPids(_,[])-> ok;
setGardenPids(GardenNumber,[H|T])->
  put(GardenNumber,H),
  setGardenPids(GardenNumber + 1,T).

