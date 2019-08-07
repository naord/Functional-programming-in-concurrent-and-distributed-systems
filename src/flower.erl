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
-export([flowerAsStateMachine/2]).
-include("globalVariables.hrl").

newFlower(ID, Type, Status, TimeSinceProblem, GardenerID, GardenID, X, Y)->
  #flower{id = ID, type = Type, status = Status, timeSinceProblem = TimeSinceProblem, gardenerID = GardenerID, gardenID = GardenID, x = X, y = Y }.


flowerAsStateMachine(GardenName,Flower=#flower{id=ID, type =Type , status=Status, timeSinceProblem = TimeSinceProblem, gardenerID = GardenerID, gardenID = GardenID, x = X, y = Y })->

  receive
    updateStatus ->

      % Choosing new mode randomly
      NewStatus = getRandomStatus(),

      % Create new record
      NewStateFlower = newFlower(ID, Type, NewStatus, 0, GardenerID, GardenID, X, Y),
      %print("update status", NewStateFlower),

      % Send report to the garden about status changing.
      gen_server:cast({global,GardenName}, {changeFlowerStatus, NewStateFlower}),

      flowerAsStateMachine(GardenName, NewStateFlower);

    {setGardenerID, NewGardenerID} ->
      NewStateFlower = newFlower(ID, Type, Status, TimeSinceProblem, NewGardenerID, GardenID, X, Y),
      %print("setGardenerID", NewGardenerID),

      gen_server:cast({global,GardenName}, {updateFlower, NewStateFlower}),
      flowerAsStateMachine(GardenName, NewStateFlower);


    handleProblem ->
      %print("flower handle problem", Flower),
      % Time the gardener handle the problem
      timer:sleep(?handle),
      %io:fwrite("flower handle problem"),

      % New record flower with normal state.
      NewStateFlower = newFlower(ID, Type, normal, 0, none, GardenID, X, Y),

      % Change the status of the flower in the server to normal.
      gen_server:cast({global,GardenName}, {changeFlowerStatus, NewStateFlower}),
      flowerAsStateMachine(GardenName, NewStateFlower);

    kill->
      exit(flowerDie),
      io:fwrite("flower: nothing")

  after 3000 ->
    if
    % Send trigger to himself to change the status if it status is nurmal and
    % do it only one time when the startTimeProblem is 0 (from the handleProblem state).
      (Status =:= normal) and (TimeSinceProblem =:= 0) ->
        %io:fwrite("flower: after.self()=~p, Folwer = ~p ~n",[self(),Flower]),
        self() ! updateStatus ,
        flowerAsStateMachine(GardenName, Flower#flower{timeSinceProblem = -1});

      (Status =:= normal) and (TimeSinceProblem =/= 0)->
        flowerAsStateMachine(GardenName, Flower);

      true ->
        ToleranceTime = getTolarableTime(Status),
        if
        % Check if the The tolerable time for the given problem is less than the time
        % it took the gardener to handle the problem and in this case kill the flower.
          TimeSinceProblem > ToleranceTime ->

            gen_server:cast({global,GardenName}, {flowerDie, Flower#flower{status = kill}}),
            % io:fwrite("flower: kill"),
            self() ! kill,
            exit(flowerDie);

        % Otherwise the gardener still has not addressed the problem and we are increment
        % the counter of time by 1.
          true ->
            NewStateFlower = newFlower(ID, Type, Status, TimeSinceProblem + 1, GardenerID, GardenID, X, Y),
            gen_server:cast({global,GardenName}, {updateFlower, NewStateFlower}),
            flowerAsStateMachine(GardenName, NewStateFlower)
        end
    end
  end.

getGardenName(Number) ->
  case Number of
    1 -> {global,?garden1Name};
    2 -> {global,?garden2Name};
    3 -> {global,?garden3Name};
    4 -> {global,?garden4Name}
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
    RandomStatus < 120 - ?level -> normal;
    true -> normal
  end.

getTolarableTime(Status)->
  case Status of
    pests_ant    -> ?pestsTime;
    pests_purple -> ?pestsTime;
    pests_green  -> ?pestsTime;
    wilted       -> ?waterTime
  end.
