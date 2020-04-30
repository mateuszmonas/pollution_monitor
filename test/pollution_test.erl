-module(pollution_test).
-author("mirek").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).
addStation_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("st", {1, 1}, P),
  P2 = pollution:addStation("st1", {1, 2}, P1),
  [ ?assertThrow('name not correct', pollution:addStation(123, {1, 1}, P)),
    ?assertThrow('geographical coordinates not correct', pollution:addStation("st", {"asd", 1}, P)),
    ?assertEqual(1, length(pollution:addStation("st", {1, 1}, P))),
    ?assertThrow('station already exists', pollution:addStation("st", {1, 2}, P1)),
    ?assertEqual(2, length(pollution:addStation("st1", {1, 2}, P1))),
    ?assertThrow('station already exists', pollution:addStation("st2", {1, 2}, P2))
  ].

addValue_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("st", {1, 1}, P),
  P2 = pollution:addStation("st1", {1, 2}, P1),
  P3 = pollution:addValue("st", {{2020, 1, 1},{10, 10, 10}}, "PM10", 60, P2),
  P4 = pollution:addValue("st", {{2020, 2, 1},{10, 10, 10}}, "PM10", 60, P3),
  [
    ?assertThrow('identifier not correct', pollution:addValue(st, {{2020, 1, 3},{10, 10, 10}}, "PM10", 60, P2)),
    ?assertThrow('identifier not correct', pollution:addValue({"st", 10}, {{2020, 1, 3},{10, 10, 10}}, "PM10", 60, P2)),
    ?assertThrow('date not correct', pollution:addValue("st", {{2020, 1, 3},{10, 10}}, "PM10", 60, P2)),
    ?assertThrow('type not correct', pollution:addValue("st", {{2020, 1, 3},{10, 10, 10}}, 'PM10', 60, P2)),
    ?assertThrow('station does not exist', pollution:addValue("st2", {{2020, 1, 3},{10, 10, 10}}, "PM10", 60, P2)),
    ?assertEqual(2, length(maps:get(values, element(2, lists:search(fun (#{name := Name}) -> (Name == "st") end, P4))))),
    ?assertEqual(0, length(maps:get(values, element(2, lists:search(fun (#{name := Name}) -> (Name == "st1") end, P4)))))
  ].


removeValue_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("st", {1, 1}, P),
  P2 = pollution:addValue("st", {{2020, 1, 1},{10, 10, 10}}, "PM10", 60, P1),
  P3 = pollution:addValue("st", {{2020, 1, 2},{10, 10, 10}}, "PM10", 60, P2),
  [
    ?assertEqual(2, length(maps:get(values, element(2, lists:search(fun (#{name := Name}) -> (Name == "st") end, P3))))),
    ?assertEqual(1, length(maps:get(values, element(2, lists:search(fun (#{name := Name}) -> (Name == "st") end, pollution:removeValue("st", {{2020, 1, 1},{10, 10, 10}}, "PM10", P3))))))
  ].

getOneValue_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("st", {1, 1}, P),
  P2 = pollution:addValue("st", {{2020, 1, 1},{10, 10, 10}}, "PM10", 60, P1),
  [
    ?assertEqual(60, pollution:getOneValue("st", {{2020, 1, 1},{10, 10, 10}}, "PM10", P2))
  ].

getStationMean_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("st", {1, 1}, P),
  P2 = pollution:addValue("st", {{2020, 1, 1},{10, 10, 10}}, "PM10", 60, P1),
  P3 = pollution:addValue("st", {{2020, 2, 1},{10, 10, 10}}, "PM10", 40, P2),
  [
    ?assertEqual(50.0, pollution:getStationMean("st", "PM10", P3))
  ].

getHourlyStationMean_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("st", {1, 1}, P),
  P2 = pollution:addValue("st", {{2020, 1, 1},{10, 10, 10}}, "PM10", 60, P1),
  P3 = pollution:addValue("st", {{2020, 2, 1},{10, 10, 10}}, "PM10", 40, P2),
  P4 = pollution:addValue("st", {{2020, 2, 1},{11, 10, 10}}, "PM10", 100, P3),
  [
    ?assertEqual(50.0, pollution:getHourlyStationMean("st", 10, "PM10", P4))
  ].

getDailyMean_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("st", {1, 1}, P),
  P2 = pollution:addStation("st1", {1, 2}, P1),
  P3 = pollution:addValue("st", {{2020, 1, 1},{10, 10, 10}}, "PM10", 40, P2),
  P4 = pollution:addValue("st", {{2020, 1, 1},{11, 10, 10}}, "ppm", 100, P3),
  P5 = pollution:addValue("st1", {{2020, 2, 1},{10, 10, 10}}, "PM10", 40, P4),
  P6 = pollution:addValue("st1", {{2020, 1, 1},{11, 10, 10}}, "PM10", 60, P5),
  [
    ?assertEqual(50.0, pollution:getDailyMean({2020, 1, 1}, "PM10", P6))
  ].

getDailyAverageDataCount_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("st", {1, 1}, P),
  P2 = pollution:addValue("st", {{2020, 1, 1},{9, 10, 10}}, "PM10", 40, P1),
  P3 = pollution:addValue("st", {{2020, 1, 1},{10, 10, 10}}, "PM10", 40, P2),
  P4 = pollution:addValue("st", {{2020, 2, 1},{10, 10, 10}}, "PM10", 40, P3),
  P5 = pollution:addValue("st", {{2020, 1, 1},{11, 10, 10}}, "ppm", 100, P4),
  P6 = pollution:addValue("st", {{2020, 1, 1},{12, 10, 10}}, "P10", 60, P5),
  P7 = pollution:addValue("st", {{2020, 1, 1},{13, 10, 10}}, "P10", 60, P6),
  [
    ?assertEqual(3.0, pollution:getDailyAverageDataCount("st", P7))
  ].
-endif.