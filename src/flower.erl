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
-export([flowerAsStateMachine/1]).
-include("globalVariables.hrl").

newFlower(ID, Type, Status, TimeSinceProblem, GardenerID, GardenID, X, Y)->
  #flower{id = ID, type = Type, status = Status, timeSinceProblem = TimeSinceProblem, gardenerID = GardenerID, gardenID = GardenID, x = X, y = Y }.



flowerAsStateMachine(Flower=#flower{id=ID, type =Type , status=Status, timeSinceProblem = TimeSinceProblem, gardenerID = GardenerID, gardenID = GardenID, x = X, y = Y })->

  receive
    updateStatus ->
      % Choosing new mode randomly
      NewStatus = getRandomStatus(),

      % Create new record
      NewStateFlower = newFlower(ID, Type, NewStatus, 0, GardenerID, GardenID, X, Y),

      % Send report to the garden about status changing.
      gen_server:cast(getGardenName(GardenID), {updateFlowerStatus, NewStateFlower}),

      flowerAsStateMachine(NewStateFlower);

    {setGardenerID, NewGardenerID} ->
      NewStateFlower = newFlower(ID, Type, Status, TimeSinceProblem, NewGardenerID, GardenID, X, Y),
      gen_server:cast(getGardenName(GardenID), {updateFlowerStatus, NewStateFlower}),
      flowerAsStateMachine(NewStateFlower);


    handleProblem ->
      % Time the gardener handle the problem
      timer:sleep(?handle),

      % New record flower with normal state.
      NewStateFlower = newFlower(ID, Type, normal, 0, GardenerID, GardenID, X, Y),

      % Change the status of the flower in the server to normal.
      gen_server:cast(getGardenName(GardenID), {updateFlowerStatus, NewStateFlower}),
      flowerAsStateMachine(NewStateFlower)


  after 5000 ->
    if
    % Send trigger to himself to change the status if it status is nurmal and
    % do it only one time when the startTimeProblem is 0 (from the handleProblem state).
      (Status =:= normal) and (TimeSinceProblem =:= 0) ->
        ID ! updateStatus,
        flowerAsStateMachine(Flower#flower{timeSinceProblem = -1});

      (Status =:= normal) and (TimeSinceProblem =/= 0)->
        flowerAsStateMachine(Flower);

      true ->
        ToleranceTime = getTolarableTime(Status),
        if
        % Check if the The tolerable time for the given problem is less than the time
        % it took the gardener to handle the problem and in this case kill the flower.
          TimeSinceProblem > ToleranceTime ->
            gen_server:cast(getGardenName(GardenID), {flowerDie, Flower#flower{status = kill}});

        % Otherwise the gardener still has not addressed the problem and we are increment
        % the counter of time by 1.
          true ->
            NewStateFlower = newFlower(ID, Type, Status, TimeSinceProblem + 1, GardenerID, GardenID, X, Y),
            gen_server:cast(getGardenName(GardenID), {updateFlowerStatus, NewStateFlower}),
            flowerAsStateMachine(NewStateFlower)
        end
    end
  end.

getGardenName(Number) ->
  case Number of
    1 ->
      {global,?garden1Name};
    2 ->
      {global,?garden2Name};
    3 ->
      {global,?garden3Name};
    _ ->
      {global,?garden4Name}
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

getTolarableTime(Status)->
  case Status of
    pests_ant    -> ?pestsTime;
    pests_purple -> ?pestsTime;
    pests_green  -> ?pestsTime;
    wilted       -> ?waterTime
  end.

print(Msg, Object)-> io:fwrite(Msg ++ "~p~n", [Object]).
print(Msg)-> io:fwrite(Msg ++ "~n").

tests()->ok.
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


%%  NewFlower = newFlower(a, normal, 0),
%%  register(a, spawn(flower, flowerAsStateMachine, [NewFlower, 0, 1])),
%%  timer:sleep(3),
%%  a ! handleProblem,
%%  timer:sleep(3000),
%%  a ! handleProblem,
%%  timer:sleep(4000),
%%  a ! handleProblem,
%%  timer:sleep(70000),
%%  a ! handleProblem.
