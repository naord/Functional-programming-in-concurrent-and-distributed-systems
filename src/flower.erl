%%%-------------------------------------------------------------------
%%% @author nirkov
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jul 2019 19:43
%%%-------------------------------------------------------------------
-module(flower).
-author("nirkov").

%% API
-export([flowerAsStateMachine/3, getTolarableTime/1, getRandomStatus/0, tests/0, print/1, print/2]).
-include("globalVariables.hrl").

newFlower(ID, Type, Status, GardenerID, GardenID, X, Y)->
  #flower{id = ID, type = Type, status = Status,  gardenerID = GardenerID, gardenID = GardenID, x = X, y = Y }.

-define(pestsTime, 100).
-define(waterTime, 200).
-define(weedTime, 300).
-define(fertalizeTime, 400).

flowerAsStateMachine(Flower=#flower{id=ID, type =Type , status=Status, gardenerID = GardenerID, gardenID = GardenID, x = X, y = Y },
    StartTimeProblem,
    MyGardenServerPID)->

  receive
    updateStatus ->
      print("Entered updateState"),
      % Choosing new mode randomly
      NewStatus      = getRandomStatus(),
      print("New Status = ", NewStatus),

      % Create new record
      NewStateFlower = newFlower(ID, Type, NewStatus, GardenerID, GardenID, X, Y),

      % Send report to the server about status changing.
      %TODO gardenGenServer:cast(MyGardenServerPID, {updateFlowerStatus, NewStateFlower}),

      flowerAsStateMachine(NewStateFlower, 0, MyGardenServerPID);

    {setGardenerID, NewGardenerID} ->
      NewStateFlower = newFlower(ID, Type, Status, NewGardenerID, GardenID, X, Y),
      flowerAsStateMachine(NewStateFlower, StartTimeProblem, MyGardenServerPID);


    handleProblem ->
      % Time the gardener handle the problem
      timer:sleep(2000),

      % New record flower with normal state.
      NewStateFlower = newFlower(ID, Type, normal, GardenerID, GardenID, X, Y),
      print("Handle the problem after = ", StartTimeProblem),
      % Change the status of the flower in the server to normal.
      %TODO gardenGenServer:cast(MyGardenServerPID, {updateFlowerStatus, NewStateFlower}),
      flowerAsStateMachine(NewStateFlower, 0, MyGardenServerPID)


  after 50 ->
    print("Entered after state"),
    print("Status = ", Status),
    print("Start Time Problem = ", StartTimeProblem),
    if
    % Send trigger to himself to change the status if it status is nurmal and
    % do it only one time when the startTimeProblem is 0 (from the handleProblem state).
      (Status =:= normal) and (StartTimeProblem =:= 0) ->
        print("Entered after and send trigger to updateState"),
        ID ! updateStatus,
        flowerAsStateMachine(Flower, -1, MyGardenServerPID);

      (Status =:= normal) and (StartTimeProblem =/= 0)->
        flowerAsStateMachine(Flower, StartTimeProblem, MyGardenServerPID);

      true ->
        ToleranceTime = getTolarableTime(Status),
        print("Tolerance Time = ", ToleranceTime),
        if
        % Check if the The tolerable time for the given problem is less than the time
        % it took the gardener to handle the problem and in this case kill the flower.
          StartTimeProblem > ToleranceTime ->
            print("Start Time Problem = ", StartTimeProblem),
            print("kill"); %TODO gardenGenServer:cast(MyGardenServerPID, {kill, Flower});

        % Otherwise the gardener still has not addressed the problem and we are increment
        % the counter of time by 1.
          true ->
            flowerAsStateMachine(Flower, StartTimeProblem + 1, MyGardenServerPID)
        end
    end


  end.


getTolarableTime(Status)->
  case Status of
    pests_ant    -> ?pestsTime;
    pests_purple -> ?pestsTime;
    pests_green  -> ?pestsTime;
    wilted       -> ?waterTime
%%    weed   -> ?weedTime;
%%    fertalize -> ?fertalizeTime
  end.

getRandomStatus()->
  {T1,T2,T3} = now(),
  random:seed(T1, T2, T3),
  RandomStatus = random:uniform(120 - ?level),
  if
    RandomStatus < 10 -> pests_ant;
    RandomStatus < 20 -> pests_purple;
    RandomStatus < 30 -> pests_green;
    RandomStatus < 60 -> wilted;
    RandomStatus < 120 - ?level -> normal
  end.

tests()->
%%  print("Start tests"),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%  print("Status : ", getRandomStatus()),
%%
%%  print("Tolerance time of pests : ", getTolarableTime(pests)),
%%  print("Tolerance time of water : ", getTolarableTime(water)),
%%  print("Tolerance time of weed : ", getTolarableTime(weed)),
%%  print("Tolerance time of fertalize : ", getTolarableTime(fertalize)),


  NewFlower = newFlower(a, normal, 0),
  register(a, spawn(flower, flowerAsStateMachine, [NewFlower, 0, 1])),
  timer:sleep(3),
  a ! handleProblem,
  timer:sleep(3000),
  a ! handleProblem,
  timer:sleep(4000),
  a ! handleProblem,
  timer:sleep(70000),
  a ! handleProblem

.


print(Msg, Object)-> io:fwrite(Msg ++ "~p~n", [Object]).
print(Msg)-> io:fwrite(Msg ++ "~n").