
-module(starter).
-author("ER").


-behaviour(wx_object).

-export([start/0]).
%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3,handle_cast/2, 	 
	 handle_event/2]).


-include_lib("wx/include/wx.hrl").

%% starter configs
-define(ant_gui_server, 'ant_gui_server@132.72.104.230'). %% red

%% starter 
-define(ExitIdStarter,109).
-define(hole_id_1,209).
-define(hole_id_2,210).
-define(hole_id_3,211).

-define(Cycle,500).

% Resolution defines
-define(Resolution,24).
-define(XMax,1210).
-define(YMax,658).
-define(Xoffset,5).
-define(Yoffset,1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {players_selected ,frame ,panel, colors_selected,colors}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start function - temporary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    %%
						% TO BE ADDED - an opening menu - it suppose to be a new wx_object.. will do later	
    %%
    wx_object:start_link({global,?MODULE},?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wxServer functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%*****************************************************************
%% Initialize
%%
						% X/YSize - the size of the main game frame 
%%
%%*****************************************************************

init([]) ->
   	Wx=wx:new(),
    Frame = wxFrame:new(Wx, -1, "Ant Madness", [{size, {(?XMax), (?YMax)}}]),
    Panel = wxPanel:new(Frame),
    
    %%connects
    wxPanel:connect(Panel, command_button_clicked),
    
    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
%%     ChoiceBox = wxBoxSizer:new(?wxHORIZONTAL),
	ButtonsBox = wxBoxSizer:new(?wxHORIZONTAL),
    Buttons = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "Start Here"}]),

    %% Create soldiers buttons
    ExitButton = wxButton:new(Panel, ?ExitIdStarter, [{label,"Exit"},{size, {140,40}}]),
	Start_1_hole = wxButton:new(Panel, ?hole_id_1, [{label,"1 Hole"},{size, {140,40}}]),
	Start_2_hole = wxButton:new(Panel, ?hole_id_2, [{label,"2 Holes"},{size, {140,40}}]),
	Start_3_hole = wxButton:new(Panel, ?hole_id_3, [{label,"3 Holes"},{size, {140,40}}]),


	%% Add to sizers
	
%% 	wxSizer:addSpacer(ChoiceBox, 15),
%%     wxSizer:addSpacer(ChoiceBox, 5),

	wxSizer:addSpacer(Buttons, 5),
	wxSizer:add(Buttons, Start_1_hole),
	wxSizer:addSpacer(Buttons, 5),
	wxSizer:add(Buttons, Start_2_hole),
	wxSizer:addSpacer(Buttons, 5),
	wxSizer:add(Buttons, Start_3_hole),
	wxSizer:addSpacer(Buttons, 10),
	wxSizer:add(Buttons, ExitButton),
    wxSizer:addSpacer(Buttons, 5),
    
	wxSizer:addSpacer(ButtonsBox, 15),
    wxSizer:add(ButtonsBox, Buttons),
	
    wxSizer:addSpacer(MainSizer, 265),
%% 	wxSizer:add(MainSizer, ChoiceBox),
    wxSizer:addSpacer(MainSizer, 165),
    wxSizer:add(MainSizer, ButtonsBox),
	
    wxPanel:setSizer(Panel, MainSizer),
    wxFrame:show(Frame),
		
	paint(wxBitmap:new("Ant_on_leaf.png"),{180,3}, Panel),
	paint(wxBitmap:new("bgu.png"),{15,3}, Panel),
	paint(wxBitmap:new("Erlang.png"),{15,220}, Panel),

	{Panel, #state{frame = Frame,
				   panel=Panel,
				   players_selected = [], %% list of playerX atom
										  %% when X gets 1-4
				   colors_selected = [], %% list in format {playerX,Color} 
										 %% when Color is an atom
				   colors = [red,blue,yellow,green
							]}}.

%%*****************************************************************
%% Handle event
%%
%%*****************************************************************
%% Async Events are handled in handle_event as in handle_info

%% Button event
handle_event(#wx{event = #wxCommand{type=command_button_clicked},id=Id}, 
	     State) when Id =:= ?ExitIdStarter -> 
	io:format("exit button pressed~n"),
    {stop,normal,State};

handle_event(#wx{event = #wxCommand{type=command_button_clicked},id=Id}, State) ->
 		 case Id of
			 ?hole_id_1 ->	rpc:call(?ant_gui_server,ant_gui_server,start,[[{2,{600,100}}]]);
			 ?hole_id_2 ->	rpc:call(?ant_gui_server,ant_gui_server,start,[[{1,{100,100}},{3,{1100,100}}]]);
			 ?hole_id_3 ->	rpc:call(?ant_gui_server,ant_gui_server,start,[[{1,{100,100}},{2,{600,100}},{3,{1100,100}}]])
		 end,
    {stop,normal,State};

handle_event(#wx{event = Event}, State) ->
    io:format("wx server received unknown event =~p~n",[Event]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Regular Server functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_info(_, State) ->
    {noreply, State}.

%%*****************************************************************
%% Handle call
%%*****************************************************************

handle_call(_Msg, _From, State) ->
    {reply,{error, nyi}, State}.

%%*****************************************************************
%% Handle cast
%%*****************************************************************

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

%%*****************************************************************
%% Handle code change
%%*****************************************************************
code_change(_, _, State) ->
    {stop, ignore, State}.

%%*****************************************************************
%% Terminate
%%*****************************************************************
terminate(_Reason, #state{panel = Panel , frame = Frame}) ->
	    wxPanel:destroy(Panel),
	    wxFrame:destroy(Frame),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
paint(Object,{X,Y},Panel)	  -> io:format("Object is ~p~n",[Object]),
								Paint = wxPaintDC:new(Panel),
						   	    wxDC:drawBitmap(Paint, Object, {X,Y}),
						     	wxPaintDC:destroy(Paint).
