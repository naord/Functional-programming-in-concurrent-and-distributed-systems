%%%-------------------------------------------------------------------
%%% @author nirkov
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jul 2019 18:07
%%%-------------------------------------------------------------------

-module(graphicServer).
-author("nirkov").

-behaviour(wx_object).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("wx/include/wx.hrl").

-include("globalVariables.hrl").

%% API
-export([start_link/0]).

-export([ start/0,
  init/1,
  handle_info/2,
  handle_call/1,
  handle_cast/4,
  handle_cast/2,
  terminate/0,
  handle_event/2,
  drawing_timer/1]).


start() ->
  wx_object:start(?MODULE, [wx:new()], []).


init([WxObject]) ->
  initializeGraphicWindow(WxObject).


handle_call(state) ->
  get(my_state).


handle_cast(update, #flower{id = _, type = Type, status = Status, gardenerID = _, x = X, y = Y}) ->
  updateFlowerStatus(Type, Status, {X, Y});

handle_cast(newFlower, #flower{id = _, type = Type, status = _, gardenerID = _, x = X, y = Y}) ->
  drawNewFLower(Type , X, Y);

handle_cast(rest, #gardener{id = _, type = Type, state = _, location = {X, Y}}) ->
  sitDownTheGardener(Type, X, Y).

handle_cast(makeSteps, NewX, NewY, #gardener{id = _, type = Type, state = _, location = {X, Y}}) ->
  makeSteps(Type, NewX, NewY, {X, Y}).


handle_info(_Info, State) ->
  {noreply, State}.


handle_event(A,B)-> ok.

terminate() -> ok.

start_link()-> ok.



initializeGraphicWindow(Wx)->

  GardenFrame = wxFrame:new(Wx, -1, "The Nightmare Garden"),
  wxFrame:createStatusBar(GardenFrame),

  % Create enter window with grass img background - on a GardenFrame.
  wxWindow:setSize(GardenFrame, 0, 0, ?screen_width, ?screen_height),
  wxFrame:show(GardenFrame),
  wxFrame:connect(GardenFrame, enter_window),

  % Upload background images.
  BackgroundImgs = #{
    lawn_background => wxBitmap:new(wxImage:scale(wxImage:new(?lawn_background), ?screen_width, ?screen_height)),
    lawn_peice      => wxBitmap:new(wxImage:scale(wxImage:new(?lawn_piece), 80, 80))
  },

  % Draw the background images - laws and gardens storehouse.
  GardenPainter    = wxClientDC:new(GardenFrame),
  wxDC:drawBitmap(GardenPainter, maps:get(lawn_background, BackgroundImgs), {0,0}),


  % upload Flower images - buds, flowering and Wilted flower
  AllImages = #{
    % Upload background images.
    lawn_background => wxBitmap:new(wxImage:scale(wxImage:new(?lawn_background), ?screen_width, ?screen_height)),
    lawn_peice      => wxBitmap:new(wxImage:scale(wxImage:new(?lawn_piece), 80, 80)),

    %iris
    iris_r_wilted  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_r_wilted), 80, 80)),
    iris_l_wilted  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_l_wilted), 80, 80)),

    iris_r_pests_ant  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_r_pests_ant), 80, 80)),
    iris_l_pests_ant  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_l_pests_ant), 80, 80)),

    iris_r_pests_purple  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_r_pests_purple), 80, 80)),
    iris_l_pests_purple  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_l_pests_purple), 80, 80)),

    iris_r_pests_green  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_r_pests_green), 80, 80)),
    iris_l_pests_green  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_l_pests_green), 80, 80)),

    iris_r_normal  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_r), 80, 80)),
    iris_l_normal   => wxBitmap:new(wxImage:scale(wxImage:new(?iris_l), 80, 80)),

    % red
    red_r_wilted  => wxBitmap:new(wxImage:scale(wxImage:new(?red_r_wilted), 80, 80)),
    red_l_wilted  => wxBitmap:new(wxImage:scale(wxImage:new(?red_l_wilted), 80, 80)),

    red_r_pests_ant  => wxBitmap:new(wxImage:scale(wxImage:new(?red_r_pests_ant), 80, 80)),
    red_l_pests_ant  => wxBitmap:new(wxImage:scale(wxImage:new(?red_l_pests_ant), 80, 80)),

    red_r_pests_purple  => wxBitmap:new(wxImage:scale(wxImage:new(?red_r_pests_purple), 80, 80)),
    red_l_pests_purple  => wxBitmap:new(wxImage:scale(wxImage:new(?red_l_pests_purple), 80, 80)),

    red_r_pests_green  => wxBitmap:new(wxImage:scale(wxImage:new(?red_r_pests_green), 80, 80)),
    red_l_pests_green  => wxBitmap:new(wxImage:scale(wxImage:new(?red_l_pests_green), 80, 80)),

    red_r_normal   => wxBitmap:new(wxImage:scale(wxImage:new(?red_r), 80, 80)),
    red_l_normal   => wxBitmap:new(wxImage:scale(wxImage:new(?red_l), 80, 80)),

    % upload gardener images - nir and naor images
    nir_left_first    => wxBitmap:new(wxImage:scale(wxImage:new(?nir_left_first), 80, 80)),
    nir_left_second   => wxBitmap:new(wxImage:scale(wxImage:new(?nir_left_second), 80, 80)),
    nir_right_first   => wxBitmap:new(wxImage:scale(wxImage:new(?nir_right_first), 80, 80)),
    nir_right_second  => wxBitmap:new(wxImage:scale(wxImage:new(?nir_right_second), 80, 80)),
    nir_sit           => wxBitmap:new(wxImage:scale(wxImage:new(?nir_sit), 80, 80))
  },


  % upload gardener images - nir and naor images
  GardenerImgs = #{
    nir_left_first    => wxBitmap:new(wxImage:scale(wxImage:new(?nir_left_first), 80, 80)),
    nir_left_second   => wxBitmap:new(wxImage:scale(wxImage:new(?nir_left_second), 80, 80)),
    nir_right_first   => wxBitmap:new(wxImage:scale(wxImage:new(?nir_right_first), 80, 80)),
    nir_right_second  => wxBitmap:new(wxImage:scale(wxImage:new(?nir_right_second), 80, 80)),
    nir_sit           => wxBitmap:new(wxImage:scale(wxImage:new(?nir_sit), 80, 80))
  },

  MatrixObjectsPosition = initObjectMatrixAsMap(lawn_peice, fillAvailableCoordinateList(), maps:new()),

  % Initialize statistic
  wxFrame:setStatusText(GardenFrame,
    "Number of flowers:  " ++ integer_to_list(3) ++ "    " ++
      "Number of Gardener:  " ++ integer_to_list(3) ++ "    " ++
      "Connected servers:  " ++ integer_to_list(2)),




  put(my_state, #graphic_server{
    objectsMatrix  = MatrixObjectsPosition,
    gardenFrame    = GardenFrame,
    gardenPainter  = GardenPainter,
    numOfServers   = 0,
    numberOfFlower = 0,
    allImages      = AllImages,
    lawnImgs       = BackgroundImgs}).
%%
%%A=updateObjectStatusInObjectsMatrix(480, 480, red_r_pests_green, MatrixObjectsPosition),
%%B=updateObjectStatusInObjectsMatrix(800, 320, iris_r_normal, A),
%%C=updateObjectStatusInObjectsMatrix(640, 560, red_l_wilted, B),
%%D=updateObjectStatusInObjectsMatrix(560, 160, iris_r_wilted, C),
%%E=updateObjectStatusInObjectsMatrix(320, 320, red_r_normal, D),
%%F=updateObjectStatusInObjectsMatrix(720, 480, iris_l_pests_green, E),
%%
%%wxDC:drawBitmap(GardenPainter, maps:get(red_r_pests_green, AllImages), {480, 480}),
%%wxDC:drawBitmap(GardenPainter, maps:get(iris_r_normal, AllImages), {800, 320}),
%%wxDC:drawBitmap(GardenPainter, maps:get(red_l_wilted, AllImages), {640, 560}),
%%wxDC:drawBitmap(GardenPainter, maps:get(iris_r_wilted, AllImages), {560, 160}),
%%wxDC:drawBitmap(GardenPainter, maps:get(red_r_normal, AllImages), {320, 320}),
%%wxDC:drawBitmap(GardenPainter, maps:get(iris_l_pests_green, AllImages), {720, 480}),

%%makeSteps(nir,240, 320, {160, 240}),
%%makeSteps(nir,320, 400, {240, 320}),
%%makeSteps(nir,400, 480, {320, 400}),
%%makeSteps(nir,480, 480, {400, 480}),
%%makeSteps(nir,560, 480, {480, 480}),
%%makeSteps(nir,640, 560, {560, 480}),
%%makeSteps(nir,720, 480, {640, 560}),
%%makeSteps(nir,800, 400, {720, 480}),
%%makeSteps(nir,800, 320, {800, 400}),
%%makeSteps(nir,800, 240, {800, 320}),
%%makeSteps(nir,800, 160, {800, 240}),
%%makeSteps(nir,720, 160, {800, 160}),
%%makeSteps(nir,640, 160, {720, 160}),
%%makeSteps(nir,560, 160, {640, 160}),
%%makeSteps(nir,480, 160, {560, 160}),
%%makeSteps(nir,400, 240, {480, 160}),
%%makeSteps(nir,320, 320, {400, 240}),
%%makeSteps(nir,240, 400, {320, 320}),
%%makeSteps(nir,240, 480, {240, 400}),
%%makeSteps(nir,240, 560, {240, 480})



drawing_timer(PID)->
  receive
    stop->0
  after 10 ->
    wx_object:cast(PID, {update}) ,
    drawing_timer(PID)
  end.


updateFlowerStatus(Type, NewStatus, {X, Y}) ->
  if
    NewStatus =:= kill -> NewFlowerToDraw = lawn_peice;
    true               -> NewFlowerToDraw = list_to_atom(atom_to_list(Type) ++ atom_to_list(NewStatus))
  end,

  MyState = get(my_state),
  NewMap = updateObjectStatusInObjectsMatrix(X, Y, NewFlowerToDraw, (MyState#graphic_server.objectsMatrix)),
  wxDC:drawBitmap(MyState#graphic_server.gardenFrame, maps:get(NewFlowerToDraw, MyState#graphic_server.allImages), {X, Y}),
  erase(my_state),
  put(my_state, MyState#graphic_server{objectsMatrix = NewMap}).


drawNewFLower(Type , X, Y)->
  MyState   = get(my_state),
  erase(my_state),

  % Draw new random flower.
  wxDC:drawBitmap(MyState#graphic_server.gardenFrame, Type, MyState#graphic_server.allImages, {X, Y}),

  % Update the state record about the new number of the flowers and save in upon the previous record.
  NewNumberOfFLowers = MyState#graphic_server.numberOfFlower + 1,

  % replace the square lawn in this coordinates to be the selected type flower
  NewObjectsMatrix = updateObjectStatusInObjectsMatrix(X, Y, Type, MyState#graphic_server.objectsMatrix),
  put(my_state, MyState#graphic_server{numberOfFlower = NewNumberOfFLowers, objectsMatrix = NewObjectsMatrix}).


makeSteps(Type, NewX, NewY, {X, Y})->
  MyState        = get(my_state),
  WxDC           = MyState#graphic_server.gardenPainter,
  AllImages      = MyState#graphic_server.allImages,
  PrevObjectInXY = maps:get({X, Y}, MyState#graphic_server.objectsMatrix),
  if
    NewX < X ->
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_second"), AllImages), {X, Y}),
      timer:sleep(?delay_between_rect),
      wxDC:drawBitmap(WxDC, maps:get(PrevObjectInXY, MyState#graphic_server.allImages), {X, Y}),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_first"), AllImages), {NewX, NewY}),
      timer:sleep(?delay_within_rect),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_second"),AllImages), {NewX, NewY});

    NewX =:= X ->
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_second"), AllImages), {X, Y}),
      timer:sleep(?delay_between_rect),
      wxDC:drawBitmap(WxDC, maps:get(PrevObjectInXY, MyState#graphic_server.allImages), {X, Y}),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_first"), AllImages), {NewX, NewY});

    NewX > X ->
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_right_second"), AllImages), {X, Y}),
      timer:sleep(?delay_between_rect),
      wxDC:drawBitmap(WxDC, maps:get(PrevObjectInXY, MyState#graphic_server.allImages), {X, Y}),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_right_first"), AllImages), {NewX, NewY}),
      timer:sleep(?delay_within_rect),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_right_second"), AllImages), {NewX, NewY})
  end.

sitDownTheGardener(Type, X, Y)->
  ThisTypeSit =  list_to_atom(atom_to_list(Type) ++ "_sit"),
  MyState = get(my_state),
  WxDC = MyState#graphic_server.gardenPainter,
  wxDC:drawBitmap(WxDC, maps:get(ThisTypeSit, MyState#graphic_server.allImages), {X, Y}),
  ok.

%===================================================================
%                      Get Random Objects Functions
%===================================================================

getRandomBug()->
  RandomBugLeft = getRandomNumber(20),
  if
    RandomBugLeft < 10 -> "ant";
    RandomBugLeft < 20 -> "green";
    true -> "purple"
  end.


getRandomFlower()->
  RandomFlower = getRandomNumber(40),
  if
    RandomFlower < 10 -> iris_l;
    RandomFlower < 20 -> iris_r;
    RandomFlower < 30 -> red_l;
    true -> red_r
  end.

getRandomGardener()->
  RandomGardener = getRandomNumber(20),
  if
    RandomGardener < 10 -> nir_right;
    true -> naor_right
  end.

getRandomNumber(Gap)->
  {T1,T2,T3} = now(),
  random:seed(T1, T2, T3),
  random:uniform(Gap).


initObjectMatrixAsMap(_, [], Map) ->
  Map;

initObjectMatrixAsMap(ToInsert,[H|T], Map)->
  NewMergeMap = maps:merge(Map, #{H => ToInsert}),
  initObjectMatrixAsMap(ToInsert, T, NewMergeMap).

fillAvailableCoordinateList()->
  [{X * 80, Y * 80} || X <- lists:seq(0, 16), Y <- lists:seq(0, 11)].

updateObjectStatusInObjectsMatrix(X, Y, NewObject, Map)->
  maps:update({X, Y}, NewObject, Map).

drawFromRecovery() ->
  MyState = get(my_state),
  WxDC = MyState#graphic_server.gardenPainter,

  % Get the map of object and their position as list.
  ObjectsMatrixAsList = maps:to_list(MyState#graphic_server.objectsMatrix),

  % For each coordinate pair we draw the object should be in this square.
  Fun = fun({{X, Y}, Type}) ->
    wxDC:drawBitmap(WxDC, maps:get(Type, MyState#graphic_server.allImages), {X, Y}) end,
  maps:foreach(Fun, ObjectsMatrixAsList).

checkCoordinate(X, Y) ->
  if
    (((X >= 0) and (X =< 1280)) and ((Y >= 0) and (Y =< 880)))
      and ((X rem 80 =:= 0) and (Y rem 80 =:= 0))-> ok;
    true -> error
  end.



%%fillMatrix(Insert, {X, Y}, Map)->
%%  if
%%    X < 1281 ->
%%      NewMergeMap = maps:merge(Map, #{{X, Y} => Insert}),
%%      NewMergeMapWithCal = fillCol(Insert, {X, Y + 80}, NewMergeMap),
%%      fillMatrix(Insert, {X + 80, Y}, NewMergeMapWithCal);
%%    true -> Map
%%  end.
%%
%%fillCol(Insert, {X, Y}, Map) ->
%%  if
%%    Y < 881 ->
%%      NewMergeMap = maps:merge(Map, #{{X, Y} => Insert}),
%%      fillCol(Insert, {X, Y + 80}, NewMergeMap);
%%    true -> Map
%%  end.
