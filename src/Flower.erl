%%%-------------------------------------------------------------------
%%% @author nirkov
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jul 2019 19:43
%%%-------------------------------------------------------------------
-module('Flower').
-author("nirkov").

%% API
-export([flowerAsStateMachine/3, getTolarableTime/1, getRandomStatus/0]).

-record(flower,{id, status, pointsLifeTime}).
newFlower(ID, Status, PointsLifeTime)->
  #flower{id = ID, status = Status, pointsLifeTime = PointsLifeTime}.

-define(pestsTime, 10).
-define(waterTime, 10).
-define(weedTime, 10).
-define(fertalizeTime, 10).
-define(pointLifeDelay, 20).


flowerAsStateMachine(Flower=#flower{id=ID, status=Status, pointsLifeTime = PointsLifeTime},
    StartTimeProblem,
    MyGardenServerPID)->

  receive
    updateStatus ->
      % Choosing new mode randomly
      NewStatus      = getRandomStatus(),

      % Create new record
      NewStateFlower = newFlower(ID, NewStatus, PointsLifeTime),

      % Send report to the server about status changing.
      gardenGenServer:cast(MyGardenServerPID, {updateFlowerStatus, NewStateFlower}),

      flowerAsStateMachine(NewStateFlower, 0, MyGardenServerPID);

    handleProblem ->
      % Time the gardener handle the problem
      timer:sleep(3),

      % New record flower with normal state.
      NewStateFlower = newFlower(ID, normal, PointsLifeTime),

      % Change the status of the flower in the server to normal.
      gardenGenServer:cast(MyGardenServerPID, {updateFlowerStatus, NewStateFlower}),
      flowerAsStateMachine(NewStateFlower, 0, MyGardenServerPID)

    after 10 ->
    if
      % Send trigger to himself to change the status if it status is nurmal and
      % do it only one time when the startTimeProblem is 0 (from the handleProblem state).
      Status =:= normal and StartTimeProblem =:= 0 ->
        ID ! updateStatus,
        flowerAsStateMachine(Flower, -1, MyGardenServerPID);

      true ->
        ToleranceTime = getTolarableTime(Status),
        if
          % Check if the The tolerable time for the given problem is less than the time
          % it took the gardener to handle the problem and in this case kill the flower.
          StartTimeProblem > ToleranceTime ->  gardenGenServer:cast(MyGardenServerPID, {kill, Flower});

          % Otherwise the gardener still has not addressed the problem and we are increment
          % the counter of time by 1.
          true -> flowerAsStateMachine(Flower, StartTimeProblem + 1, MyGardenServerPID)
        end
    end


  end.


getTolarableTime(Status)->
  case Status of
    pests -> ?pestsTime;
    water -> ?waterTime;
    weed  -> ?weedTime;
    fertalize -> ?fertalizeTime
  end.

getRandomStatus()->
  {T1,T2,T3} = now(),
  random:seed(T1, T2, T3),
  RandomStatus = random:uniform(40),
  if
    RandomStatus > 0  and RandomStatus < 10 -> pests;
    RandomStatus > 10 and RandomStatus < 20 -> water;
    RandomStatus > 20 and RandomStatus < 30 -> weed;
    RandomStatus > 30 and RandomStatus < 10 -> fertalize
  end.

