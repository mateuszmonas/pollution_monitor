-module(pollution_gen_server).
-behaviour(gen_server).
-author("mirek").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getHourlyStationMean/3, getDailyMean/2, getDailyAverageDataCount/1]).

start_link() ->
  gen_server:start_link(
    {local,pollution_server},
    ?MODULE,
    pollution:createMonitor(), []).

init(InitialValue) ->
  io:format("~n=================  Server init  ==================~n~n"),
  {ok, InitialValue}.

stop() -> gen_server:cast(var_server, stop).

safe_update(Module, Fun, Params, Monitor) ->
  try apply(Module, Fun, Params) of
    NewMonitor -> {ok, NewMonitor}
  catch
    Throw -> {{error, Throw}, Monitor}
  end.

safe_get(Module, Fun, Params) ->
  try apply(Module, Fun, Params) of
    Value -> Value
  catch
    Throw -> {error, Throw}
  end.

handle_call({addStation, Name, Geo}, From, Monitor) ->
  {Response, NewMonitor} = safe_update(pollution, addStation, [Name, Geo, Monitor], Monitor),
  {reply, Response, NewMonitor};
handle_call({addValue, Identifier, Date, Type, Value}, From, Monitor) ->
  {Response, NewMonitor} = safe_update(pollution, addValue, [Identifier, Date, Type, Value, Monitor], Monitor),
  {reply, Response, NewMonitor};
handle_call({removeValue, Identifier, Date, Type}, From, Monitor) ->
  {Response, NewMonitor} = safe_update(pollution, removeValue, [Identifier, Date, Type, Monitor], Monitor),
  {reply, Response, NewMonitor};
handle_call({getOneValue, Identifier, Date, Type}, From, Monitor) ->
  Response = safe_get(pollution, getOneValue, [Identifier, Date, Type, Monitor]),
  {reply, Response, Monitor};
handle_call({getStationMean, Identifier, Type}, From, Monitor) ->
  Response = safe_get(pollution, getStationMean, [Identifier, Type, Monitor]),
  {reply, Response, Monitor};
handle_call({getHourlyStationMean, Identifier, Hour, Type}, From, Monitor) ->
  Response = safe_get(pollution, getHourlyStationMean, [Identifier, Hour, Type, Monitor]),
  {reply, Response, Monitor};
handle_call({getDailyMean, Date, Type}, From, Monitor) ->
  Response = safe_get(pollution, getDailyMean, [Date, Type, Monitor]),
  {reply, Response, Monitor};
handle_call({getDailyAverageDataCount, Identifier}, From, Monitor) ->
  Response = safe_get(pollution, getDailyAverageDataCount, [Identifier, Monitor]),
  {reply, Response, Monitor}.

handle_cast(stop, Value) ->
  {stop, normal, Value}.

terminate(Reason, Value) ->
  io:format("Server: exit with reason: ~p~n~n", [Reason]),
  Reason.

addStation(Name, Geo) -> gen_server:call(pollution_server, {addStation, Name, Geo}).

addValue(Identifier, Date, Type, Value) -> gen_server:call(pollution_server, {addValue, Identifier, Date, Type, Value}).

removeValue(Identifier, Date, Type) -> gen_server:call(pollution_server, {removeValue, Identifier, Date, Type}).

getOneValue(Identifier, Date, Type) -> gen_server:call(pollution_server, {getOneValue, Identifier, Date, Type}).

getStationMean(Identifier, Type) -> gen_server:call(pollution_server, {getStationMean, Identifier, Type}).

getHourlyStationMean(Identifier, Hour, Type) -> gen_server:call(pollution_server, {getHourlyStationMean, Identifier, Hour, Type}).

getDailyMean(Date, Type) -> gen_server:call(pollution_server, {getDailyMean, Date, Type}).

getDailyAverageDataCount(Identifier) -> gen_server:call(pollution_server, {getDailyAverageDataCount, Identifier}).
