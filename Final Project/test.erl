-module(test).
-author("ER").

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

start(Holes) ->
  wx_object:start({global, ?MODULE}, ?MODULE, [Holes], []).

init([Holes]) ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Ants Madness", [{pos,{0,0}},{size,?frame_size}]),

