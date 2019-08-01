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

-record(state,{gardenFrame,
  garden,
  numOfServers,
  numberOfFlower,
  flowerImgs,
  pestsImgs,
  gardenerImg,
  lawnImg,
  storeHouseImg}).

-include("wx.hrl").
-include("glovalVariable.hrl").
%% API
-export([start_link/0]).

-export([ start/0,
  init/1,
  handle_info/2,
  handle_call/3,
  handle_cast/2,
  terminate/0,
  handle_event/2,
  drawing_timer/1]).

terminate()->ok.
start_link()->ok.
start() ->
  wx_object:start(?MODULE, [wx:new()], []).



-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([WxObject]) ->
  initializeGraphicWindow(WxObject).


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State) ->

  {noreply, State}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info(_Info, State) ->
  {noreply, State}.



handle_event(A,B)-> {noreply, A}.




initializeGraphicWindow(Wx)->
  GardenFrame = wxFrame:new(Wx, -1, "The Nightmare Garden"),
  wxFrame:createStatusBar(GardenFrame),


  % Create enter window with grass img background - on a GardenFrame.
  wxWindow:setSize(GardenFrame, 0, 0, 1600, 900),
  wxFrame:show(GardenFrame),
  wxFrame:connect(GardenFrame, enter_window),

  % Draw the background images - laws and gardens storehouse.
  Garden           = wxClientDC:new(GardenFrame),
  LawnToDraw       = wxBitmap:new(wxImage:scale(wxImage:new(?grass_background_path),1600,900)),
  StoreHouseToDraw = wxBitmap:new(wxImage:scale(wxImage:new(?store_house_path),300,300)),

  wxDC:drawBitmap(Garden, LawnToDraw      , {0,0}),
  wxDC:drawBitmap(Garden, StoreHouseToDraw, {0,200}),

  % upload Flower images - buds, flowering and Wilted flower
  FlowerImgs = #{
    flowering_iris          => wxBitmap:new(wxImage:scale(wxImage:new(?iris_path),100,100)),
    wilted_iris             => wxBitmap:new(wxImage:scale(wxImage:new(?wilted_iris_path),50,50)),
    flowering_chrysanthemum => wxBitmap:new(wxImage:scale(wxImage:new(?chrysanthemum_path),50,50)),
    wilted_chrysanthemum    => wxBitmap:new(wxImage:scale(wxImage:new(?wilted_chrysanthemum_path),50,50))
  },

  % upload pests images
  PestsImgs = #{
    green_left   => wxBitmap:new(wxImage:scale(wxImage:new(?pests_green_left_path),70,70)),
    green_right  => wxBitmap:new(wxImage:scale(wxImage:new(?pests_green_right_path),70,70)),
    purple_left  => wxBitmap:new(wxImage:scale(wxImage:new(?pests_purple_left_path),70,70)),
    purple_right => wxBitmap:new(wxImage:scale(wxImage:new(?pests_purple_right_path),70,70))
  },

  % upload gardener images - nir and naor images
  GardenerImgs = #{
    nir_left   => wxBitmap:new(wxImage:scale(wxImage:new(?nir_gardener_left),70,70)),
    nir_right  => wxBitmap:new(wxImage:scale(wxImage:new(?nir_gardener_right),70,70)),
    naor_left  => wxBitmap:new(wxImage:scale(wxImage:new(?naor_gardener_left),70,70)),
    naor_right => wxBitmap:new(wxImage:scale(wxImage:new(?naor_gardener_right),70,70))
  },


  % Initialize statistic
  wxFrame:setStatusText(GardenFrame,
    "Number of flowers:  " ++ integer_to_list(3) ++ "    " ++
    "Number of Gardener:  " ++ integer_to_list(3) ++ "    " ++
    "Connected servers:  " ++ integer_to_list(2)),

  {GardenFrame, #state{
    gardenFrame    = GardenFrame,
    garden         = Garden,
    numOfServers   = 0,
    numberOfFlower = 0,
    flowerImgs     = FlowerImgs,
    pestsImgs      = PestsImgs,
    gardenerImg    = GardenerImgs,
    lawnImg        = LawnToDraw,
    storeHouseImg  = StoreHouseToDraw}}.


drawing_timer(PID)->
  receive
    stop->0
  after 10 ->
    wx_object:cast(PID, {update}) ,
    drawing_timer(PID)
  end.

%===================================================================
%                      Draw New Stated Object
%===================================================================






%===================================================================
%                      Get Random Objects Functions
%===================================================================

getRandomBugLeft()->
  RandomBugLeft = getRandomNumber(20),
  if
    RandomBugLeft < 10 -> green_left;
    true -> purple_left
  end.

getRandomBugRight()->
  RandomBugLeft = getRandomNumber(20),
  if
    RandomBugLeft < 10 -> green_right;
    true -> purple_right
  end.

getRandomFlower()->
  RandomFlower = getRandomNumber(20),
  if
    RandomFlower < 10 -> flowering_iris;
    true -> flowering_chrysanthemum
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


