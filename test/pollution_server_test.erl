-module(pollution_server_test).
-author("mirek").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).
startServer_test() ->
  pollution_server:start(),
  ?assert(lists:member(pollution_server, registered())).

addStation_test() ->
  ?assertEqual(ok, pollution_server:addStation("stacja", {1,1})),
  ?assertEqual(ok, pollution_server:addStation("stacja2", {1,2})).

addValue_test() ->
  ?assertEqual(ok, pollution_server:addValue("stacja", {{2020,20,20},{8,10,10}}, "PM10", 10)),
  ?assertEqual(ok, pollution_server:addValue({1,1}, {{2020,20,20},{10,10,10}}, "PM10", 90)),
  ?assertEqual(ok, pollution_server:addValue("stacja2", {{2020,20,20},{8,10,10}}, "ppm", 10)),
  ?assertEqual(ok, pollution_server:addValue("stacja2", {{2020,20,21},{10,10,10}}, "PM10", 100)),
  ?assertEqual(ok, pollution_server:addValue("stacja2", {{2020,20,22},{10,10,10}}, "PM10", 200)).

removeValue_test() ->
  ?assertEqual(ok, pollution_server:removeValue("stacja2", {{2020,20,20},{8,10,10}}, "ppm")).

getOneValue_test() ->
  ?assertEqual(100, pollution_server:getOneValue("stacja2", {{2020,20,21},{10,10,10}}, "PM10")).

getStationMean_test() ->
  ?assertEqual(50.0, pollution_server:getStationMean("stacja", "PM10")).

getHourlyStationMean_test() ->
  ?assertEqual(150.0, pollution_server:getHourlyStationMean("stacja2", 10, "PM10")).

getDailyMean_test() ->
  ?assertEqual(50.0, pollution_server:getDailyMean({2020,20,20}, "PM10")).

getDailyAverageDataCount_test() ->
  ?assertEqual(1.0, pollution_server:getDailyAverageDataCount("stacja2")).

stopServer_test() ->
  pollution_server:stop(),
  ?assert(not lists:member(pollution_server, registered())).
-endif.