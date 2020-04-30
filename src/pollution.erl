-module(pollution).
-author("mirek").

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getHourlyStationMean/4, getDailyAverageDataCount/2, getDailyMean/3]).

-define(IS_GEO(Geo), (is_tuple(Geo) andalso (size(Geo)==2) andalso is_number(element(1, Geo)) andalso is_number(element(2, Geo)))).
-define(IS_DATE(Date), (is_tuple(Date) andalso (size(Date)==3) andalso is_integer(element(1, Date)) andalso is_integer(element(2, Date)) andalso is_integer(element(3, Date)))).
-define(IS_TIME(Time), (is_tuple(Time) andalso (size(Time)==3) andalso is_integer(element(1, Time)) andalso is_integer(element(2, Time)) andalso is_integer(element(3, Time)))).
-define(IS_DATE_TIME(Datetime), (is_tuple(Datetime) andalso (size(Datetime)==2) andalso ?IS_DATE(element(1, Datetime)) andalso ?IS_TIME(element(2, Datetime)))).

createMonitor() -> [].

addStation(Name, _, _) when not is_list(Name) -> throw('name not correct');
addStation(_, Geo, _) when not ?IS_GEO(Geo) -> throw('geographical coordinates not correct');
addStation(Name, {Lat, Lng}, Stations) when is_list(Name) ->
Found = fun (#{name := Name1, geo := {Lat1, Lng1}}) -> (Name1 == Name) or  ({Lat1, Lng1} == {Lat, Lng}) end,
case lists:any(Found, Stations) of
false -> [ #{name => Name, geo => {Lat, Lng}, values => []} | Stations ];
_ -> throw('station already exists')
end.

addValueToStation(Date, Type, Value, Station = #{values := Values}) ->
  Found = fun (#{date := Date1, type := Type1}) -> (Date1 == Date) andalso (Type1 == Type) end,
  case lists:any(Found, Values) of
    false -> Station#{values := [ #{ date => Date, type => Type, value => Value } | Values ]};
    _ -> throw('measurement already exists')
  end.

addValue(Identifier,  _, _, _, _) when not is_list(Identifier) andalso not ?IS_GEO(Identifier) -> throw('identifier not correct');
addValue(_, Date, _, _, _) when not ?IS_DATE_TIME(Date) -> throw('date not correct');
addValue(_, _, Type, _, _) when not is_list(Type) -> throw('type not correct');
addValue(_, _, _, _, []) -> throw('station does not exist');
addValue(Name, Date, Type, Value, [ Station = #{name := Name} | Stations ]) -> [ addValueToStation(Date, Type, Value, Station) | Stations ];
addValue(Geo, Date, Type, Value, [ Station = #{geo := Geo} | Stations ]) -> [ addValueToStation(Date, Type, Value, Station) | Stations ];
addValue(Identifier, Date, Type, Value, [ H | Stations ]) -> [H] ++ addValue(Identifier, Date, Type, Value, Stations).

removeValueFromValues(_, _, []) -> throw('value does not exist');
removeValueFromValues(Date, Type, [ #{ date := Date, type := Type} | Values ]) -> Values;
removeValueFromValues(Date, Type, [ H | Values]) -> [H] ++ removeValueFromValues(Date, Type, Values).

removeValue(Identifier,  _, _, _) when not is_list(Identifier) andalso not ?IS_GEO(Identifier) -> throw('identifier not correct');
removeValue(_, Date, _, _) when not ?IS_DATE_TIME(Date) -> throw('date not correct');
removeValue(_, _, Type, _) when not is_list(Type) -> throw('type is not correct');
removeValue(_, _, _, []) -> throw('station does not exist');
removeValue(Name, Date, Type, [ Station = #{name := Name, values := Values} | Stations ]) -> [ Station#{values := removeValueFromValues(Date, Type, Values)} | Stations ];
removeValue(Geo, Date, Type, [ Station = #{geo := Geo, values := Values} | Stations ]) -> [ Station#{values := removeValueFromValues(Date, Type, Values)} | Stations ];
removeValue(Identifier, Date, Type, [ H | Stations ]) -> [H] ++ removeValue(Identifier, Date, Type, Stations).

getOneValue(Identifier,  _, _, _) when not is_list(Identifier) andalso not ?IS_GEO(Identifier) -> throw('identifier not correct');
getOneValue(_, Date, _, _) when not ?IS_DATE_TIME(Date) -> throw('date not correct');
getOneValue(_, _, Type, _) when not is_list(Type) -> throw('type is not correct');
getOneValue(Identifier, Date, Type, Stations) ->
  case lists:search(fun (#{name := Name, geo := Geo}) -> (Name == Identifier) or (Geo == Identifier) end, Stations) of
    {value, #{values := Values}} ->
      case lists:search(fun (#{date := Date1, type := Type1}) -> (Date1 == Date) andalso (Type1 == Type) end, Values) of
        {value, #{value := Value}} -> Value;
        false -> throw('value does not exist')
      end;
    _ -> throw('station does not exist')
  end.


getStationMean(Identifier,  _, _) when not is_list(Identifier) andalso not ?IS_GEO(Identifier) -> throw('identifier not correct');
getStationMean(_, Type, _) when not is_list(Type) -> throw('type is not correct');
getStationMean(Identifier, Type, Stations) ->
  case lists:search(fun (#{name := Name, geo := Geo}) -> (Name == Identifier) or (Geo == Identifier) end, Stations) of
    {value, #{values := Values}} ->
      case lists:filter(fun (#{type := Type1}) -> Type1 == Type end, Values ) of
        [] -> 0;
        FilteredValues -> lists:sum(lists:map(fun (#{value := Value}) -> Value end, FilteredValues))/length(FilteredValues)
      end;
    _ -> throw('station does not exist')
  end.

getHourlyStationMean(Identifier, _, _, _) when not is_list(Identifier) andalso not ?IS_GEO(Identifier) -> throw('identifier not correct');
getHourlyStationMean(_, Hour, _, _) when not is_integer(Hour) -> throw('hour not correct');
getHourlyStationMean(_, _, Type, _) when not is_list(Type) -> throw('type is not correct');
getHourlyStationMean(Identifier, Hour, Type, Stations) ->
  case lists:search(fun (#{name := Name, geo := Geo}) -> (Name == Identifier) or (Geo == Identifier) end, Stations) of
    {value, #{values := Values}} ->
      case lists:filter(fun (#{type := Type1, date := {{_, _, _}, {Hour1, _, _}}}) -> (Type1 == Type) andalso (Hour1 == Hour) end, Values ) of
        [] -> 0;
        FilteredValues -> lists:sum(lists:map(fun (#{value := Value}) -> Value end, FilteredValues))/length(FilteredValues)
      end;
    _ -> throw('station does not exist')
  end.

getDailyMean(Date, _, _) when not ?IS_DATE(Date) -> throw('date not correct');
getDailyMean(_, Type, _) when not is_list(Type) -> throw('type is not correct');
getDailyMean(Date, Type, Stations) ->
  ValuesListsList = lists:map(fun (#{values := Values}) -> Values end, Stations),
  ValuesList = lists:flatten(ValuesListsList),
  case lists:filter(fun (#{date := {Date1, _}, type := Type1}) -> (Date1 == Date) andalso (Type1 == Type) end, ValuesList ) of
    [] -> 0;
    FilteredValues ->
      MappedValues = lists:map(fun (#{value := Value}) -> Value end, FilteredValues),
      lists:sum(MappedValues)/length(MappedValues)
  end.

getDailyAverageDataCount(Identifier, _) when not is_list(Identifier) andalso not ?IS_GEO(Identifier) -> throw('identifier not correct');
getDailyAverageDataCount(Identifier, Stations) ->
  case lists:search(fun (#{name := Name, geo := Geo}) -> (Name == Identifier) or (Geo == Identifier) end, Stations) of
    {value, #{values := Values}} ->
      ValuesByDay = lists:foldl(fun (#{date := {Date, _}}, Dict) -> dict:update_counter(Date, 1, Dict) end , dict:new(), Values),
      case lists:map(fun ({_, Value}) -> Value end, dict:to_list(ValuesByDay)) of
        [] -> 0;
        DailyCounts -> lists:sum(DailyCounts)/length(DailyCounts)
      end;
    _ -> throw('station does not exist')
  end.