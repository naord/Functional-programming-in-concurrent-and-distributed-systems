%%%-------------------------------------------------------------------
%%% @author nirkov
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jul 2019 21:20
%%%-------------------------------------------------------------------
-module(wxTries).
-author("nirkov").

%% API
-export([test/0]).

test()->
  MainFrame=wxFrame:new(Wx, -1, "Gaerldan"),
  wxWindow:setSize(MainFrame,0,0,1200,800),
  wxFrame:createStatusBar(MainFrame),

  draw_statistics(MainFrame,0,0,0,0),
  Panel = wxPanel:new(MainFrame),

  Garden=wxPanel:new(Panel,250,0,950,800), %draw_grass(Panel),
  GrassBit = wxBitmap:new(wxImage:scale(wxImage:new("pics/grass.jpg"),950,800)),
  HouseBit = wxBitmap:new(wxImage:scale(wxImage:new("pics/house.png"),110,110)),
  draw_background(Garden,GrassBit,HouseBit),
  wxWindow:setBackgroundColour(Panel, {11,11,211}),
  wxFrame:getMenuBar (MainFrame),
  InteractivePanel = wxPanel:new(Panel,0,0,250,800),
  ToolsSizer = wxStaticBoxSizer:new(?wxVERTICAL, InteractivePanel),
  ToolsNotebook = wxNotebook:new(InteractivePanel, 1000, [{style, ?wxBK_DEFAULT}]),
  wxNotebook:setSize(ToolsNotebook,140,800),


  %%%%%%%%%%%%%%%%% building ui Panel %%%%%%%%%%%%%%%%%%%%%%
  Interactive_Panel=wxPanel:new(ToolsNotebook,0,0,250,800),
  wxWindow:setBackgroundColour(Interactive_Panel, {11,1,211}),
  Add_Flower1_Panel=wxPanel:new(Interactive_Panel,27,30,150,30),
  __Add_Flower1_Button = wxButton:new(Add_Flower1_Panel, 10, [{label,"Flower 1"}]),
  Add_Flower2_Panel=wxPanel:new(Interactive_Panel,27,60,150,30),
  _Add_Flower2_Button = wxButton:new(Add_Flower2_Panel,20,[{label,"Flower 2 "}]),
  Add_Flower3_Panel=wxPanel:new(Interactive_Panel,27,90,150,30),
  _Add_Flower3_Button = wxButton:new(Add_Flower3_Panel,30,[{label,"Flower 3 "}]),
  Add_Gardener_Panel=wxPanel:new(Interactive_Panel,27,120,150,30),
  _Add_Gardener_Button = wxButton:new(Add_Gardener_Panel,40,[{label,"Gardener "}]),
  Wow_Panel=wxPanel:new(Interactive_Panel,27,200,200,100),
  _Wow_Button = wxButton:new(Wow_Panel,50,[{label,"Create 101 Flowers"}]),
  WowGardener_Panel=wxPanel:new(Interactive_Panel,27,250,200,100),
  wxButton:new(WowGardener_Panel,60,[{label,"Create 30 Gardeners"}]),
  DrawContext = wxClientDC:new(Interactive_Panel),
  Font = wxFont:new(10,?wxFONTFAMILY_ROMAN,?wxFONTSTYLE_NORMAL,?wxFONTWEIGHT_BOLD),
  wxClientDC:setFont(DrawContext,Font),
  Pen = wxPen:new({0,0,0}, [{width, 2}]),
  Brush = wxBrush:new(),
  wxBrush:setColour(Brush, {0,0,0}),
  wxClientDC:setPen(DrawContext, Pen),
  wxClientDC:setBrush(DrawContext, Brush),
  wxClientDC:drawText(DrawContext,"Build Map",{100,100}),
  wxBrush:destroy(Brush),
  wxClientDC:destroy(DrawContext),






  wxNotebook:addPage(ToolsNotebook, Interactive_Panel, "Hydro Shop", []),
  wxSizer:add(ToolsSizer, ToolsNotebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxPanel:setSizer(InteractivePanel, ToolsSizer),


  Flowers_Imgs = #{
    seed => wxBitmap:new(wxImage:scale(wxImage:new("pics/seed.png"),35,35)),
    {1,1} => wxBitmap:new(wxImage:scale(wxImage:new("pics/bud1.jpg"),35,35)),
    {1,2} => wxBitmap:new(wxImage:scale(wxImage:new("pics/flower1.png"),35,35)),
    {2,1} => wxBitmap:new(wxImage:scale(wxImage:new("pics/bud2.png"),35,35)),
    {2,2} => wxBitmap:new(wxImage:scale(wxImage:new("pics/flower2.png"),35,35)),
    {3,1} => wxBitmap:new(wxImage:scale(wxImage:new("pics/bud3.jpg"),35,35)),
    {3,2} => wxBitmap:new(wxImage:scale(wxImage:new("pics/flower3.png"),35,35))

  },

  Life_Img = wxImage:new("pics/life.png"),
  Gardener_Img = wxImage:scale(wxImage:new("pics/gardener.jpeg"),15,35),
  % event connection
  wxFrame:connect (Interactive_Panel,command_button_clicked),
  wxFrame:connect(Garden,left_down),
  wxFrame:connect (MainFrame, close_window),
  wxWindow:show(MainFrame),
  UiPid=self(),
  spawn_link(fun() -> drawing_timer(UiPid) end),
  {MainFrame,#state{mainFrame=MainFrame,numOfServers=0,panel=Panel,garden=Garden,
    interactivePanel=Interactive_Panel,
    flowerNum =1,gardenersNum = 1 ,pressed_button =default
    ,flowers_Imgs=Flowers_Imgs,grassBit = GrassBit, houseBit = HouseBit,life_img = Life_Img, gardener_Img = Gardener_Img}}.

