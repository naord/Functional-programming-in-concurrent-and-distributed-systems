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

%%-record(wx,{event = close}).
%% API
-export([start_link/1]).

-export([ start/0,
  init/1,
  handle_info/2,
  handle_call/3,
  handle_cast/2,
  terminate/2,
  handle_event/2,
  drawing_timer/1]).

start_link(GraphicServerName) ->
  gen_server:start_link({global, GraphicServerName}, ?MODULE, [], []).

start() ->
  Wx=wx:new(),
  wx_object:start(?MODULE, [Wx], []).


init([WxObject]) ->
  initializeGraphicWindow(WxObject).

handle_call(_, _, NewState) ->
  {reply, ok, NewState}.


handle_cast({update, Flower}, NewState) ->
  Type = Flower#flower.type,
  Status = Flower#flower.status,
  X = Flower#flower.x,
  Y = Flower#flower.y,
  updateFlowerStatus(Type, Status, {X, Y}),
  {noreply, NewState};

handle_cast({deleteGardenerFromGarden, {OldX, OldY}}, NewState) ->
  MyState        = get(my_state),
  WxDC           = MyState#graphic_server.gardenPainter,
  NewOldX = OldX rem 1360,
  PrevObjectInXY = maps:get({NewOldX, OldY}, MyState#graphic_server.objectsMatrix),
  wxDC:drawBitmap(WxDC, maps:get(PrevObjectInXY, MyState#graphic_server.allImages), {NewOldX, OldY}),
  {noreply, NewState};


handle_cast({makeSteps, {OldX, OldY, Gardener}}, NewState) ->
  GardenNumber = Gardener#gardener.gardenNumber,
  {X,Y} = Gardener#gardener.location,
  makeSteps(Gardener#gardener.type,normalizeX(OldX,GardenNumber), OldY, {normalizeX(X,GardenNumber),Y}),
  {noreply, NewState};

handle_cast({newFlower,Flower}, NewState) ->
  Type = Flower#flower.type,
  X = Flower#flower.x,
  Y = Flower#flower.y,
  drawNewFLower(Type , X, Y),
  {noreply, NewState};

handle_cast({rest, Gardener}, NewState) ->
  Type = Gardener#gardener.type,
  {X,Y} = Gardener#gardener.location,
  sitDownTheGardener(Type, X, Y),
  {noreply, NewState};

handle_cast({addGardener, Gardener}, NewState) ->
  MyState      = get(my_state),
  WxDC         = MyState#graphic_server.gardenPainter,
  GardenerType = Gardener#gardener.type,
  AllImages    = MyState#graphic_server.allImages,
  {X, Y}       = Gardener#gardener.location,

  if
    X =:= ?screen_width ->
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(GardenerType) ++ "_left_first"), AllImages), {X, Y});
    true ->
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(GardenerType) ++  "_right_first"), AllImages), {X, Y})
  end,
  {noreply, NewState}.

handle_event(_, NewState) ->
  {noreply, NewState}.

handle_info(_, NewState) ->
  {noreply, NewState}.


terminate(_, _) -> ok.


%start_link()-> ok.

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

    iris_r  => wxBitmap:new(wxImage:scale(wxImage:new(?iris_r), 80, 80)),
    iris_l   => wxBitmap:new(wxImage:scale(wxImage:new(?iris_l), 80, 80)),

    % red
    red_r_wilted  => wxBitmap:new(wxImage:scale(wxImage:new(?red_r_wilted), 80, 80)),
    red_l_wilted  => wxBitmap:new(wxImage:scale(wxImage:new(?red_l_wilted), 80, 80)),

    red_r_pests_ant  => wxBitmap:new(wxImage:scale(wxImage:new(?red_r_pests_ant), 80, 80)),
    red_l_pests_ant  => wxBitmap:new(wxImage:scale(wxImage:new(?red_l_pests_ant), 80, 80)),

    red_r_pests_purple  => wxBitmap:new(wxImage:scale(wxImage:new(?red_r_pests_purple), 80, 80)),
    red_l_pests_purple  => wxBitmap:new(wxImage:scale(wxImage:new(?red_l_pests_purple), 80, 80)),

    red_r_pests_green  => wxBitmap:new(wxImage:scale(wxImage:new(?red_r_pests_green), 80, 80)),
    red_l_pests_green  => wxBitmap:new(wxImage:scale(wxImage:new(?red_l_pests_green), 80, 80)),

    red_r   => wxBitmap:new(wxImage:scale(wxImage:new(?red_r), 80, 80)),
    red_l   => wxBitmap:new(wxImage:scale(wxImage:new(?red_l), 80, 80)),

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

  GraphicServer = #graphic_server{
    objectsMatrix  = MatrixObjectsPosition,
    gardenFrame    = GardenFrame,
    gardenPainter  = GardenPainter,
    numOfServers   = 0,
    numberOfFlower = 0,
    allImages      = AllImages,
    lawnImgs       = BackgroundImgs},

  put(my_state, GraphicServer),

  {GardenFrame, GraphicServer}.

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
    NewStatus =:= normal -> NewFlowerToDraw = Type;
    true -> NewFlowerToDraw = list_to_atom(atom_to_list(Type) ++ "_" ++ atom_to_list(NewStatus))
  end,
  MyState = get(my_state),
  NewMap = updateObjectStatusInObjectsMatrix(X, Y, NewFlowerToDraw, (MyState#graphic_server.objectsMatrix)),
  wxDC:drawBitmap(MyState#graphic_server.gardenPainter, maps:get(NewFlowerToDraw, MyState#graphic_server.allImages), {X, Y}),
  erase(my_state),
  put(my_state, MyState#graphic_server{objectsMatrix = NewMap}).

drawNewFLower(Type , X, Y)->
  MyState   = get(my_state),
  erase(my_state),

  % Draw new random flower.
  wxDC:drawBitmap(MyState#graphic_server.gardenPainter,  maps:get(Type, MyState#graphic_server.allImages), {X, Y}),

  % Update the state record about the new number of the flowers and save in upon the previous record.
  NewNumberOfFLowers = MyState#graphic_server.numberOfFlower + 1,

  % replace the square lawn in this coordinates to be the selected type flower
  NewObjectsMatrix = updateObjectStatusInObjectsMatrix(X, Y, Type, MyState#graphic_server.objectsMatrix),
  put(my_state, MyState#graphic_server{numberOfFlower = NewNumberOfFLowers, objectsMatrix = NewObjectsMatrix}).


makeSteps(Type, OldX, OldY, {NewX, NewY})->
  MyState        = get(my_state),
  WxDC           = MyState#graphic_server.gardenPainter,
  AllImages      = MyState#graphic_server.allImages,
  PrevObjectInXY = maps:get({OldX, OldY}, MyState#graphic_server.objectsMatrix),
  if
    NewX < OldX ->
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_second"), AllImages), {OldX, OldY}),
      timer:sleep(?delay_between_rect),
      wxDC:drawBitmap(WxDC, maps:get(PrevObjectInXY, MyState#graphic_server.allImages), {OldX, OldY}),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_first"), AllImages), {NewX, NewY}),
      timer:sleep(?delay_within_rect),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_second"),AllImages), {NewX, NewY});

    NewX =:= OldX ->
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_second"), AllImages), {OldX, OldY}),
      timer:sleep(?delay_between_rect),
      wxDC:drawBitmap(WxDC, maps:get(PrevObjectInXY, MyState#graphic_server.allImages), {OldX, OldY}),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_left_first"), AllImages), {NewX, NewY});

    NewX > OldX ->
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_right_second"), AllImages), {OldX, OldY}),
      timer:sleep(?delay_between_rect),
      wxDC:drawBitmap(WxDC, maps:get(PrevObjectInXY, MyState#graphic_server.allImages), {OldX, OldY}),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_right_first"), AllImages), {NewX, NewY}),
      timer:sleep(?delay_within_rect),
      wxDC:drawBitmap(WxDC, maps:get(list_to_atom(atom_to_list(Type) ++ "_right_second"), AllImages), {NewX, NewY});
    true ->
      ok
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

initObjectMatrixAsMap(_, [], Map) ->
  Map;

initObjectMatrixAsMap(ToInsert,[H|T], Map)->
  NewMergeMap = maps:merge(Map, #{H => ToInsert}),
  initObjectMatrixAsMap(ToInsert, T, NewMergeMap).

fillAvailableCoordinateList()->
  [{X * 80, Y * 80} || X <- lists:seq(0, 16), Y <- lists:seq(0, 11)].

updateObjectStatusInObjectsMatrix(X, Y, NewObject, Map)->
  maps:update({X, Y}, NewObject, Map).

normalizeX(X, _)-> X rem (?screen_width + ?squareSize).


