defmodule PollutionData do
  @moduledoc false
  def parseLine(line) do
    [date, time, lng, lat, value] = String.split(line, ",")
    %{:datetime =>
      {
        date
        |> String.split("-")
        |> Enum.reverse
        |> List.to_tuple,
        time
        |> String.split(":")
        |> Enum.reverse
        |> List.to_tuple
    },
      :location =>
      {lng |> Float.parse |> elem(0), lat |> Float.parse |> elem(0)},
      :pollutionLevel => value |> Integer.parse |> elem(0)
    }
  end

  def importLinesFromCSV(path) do
    path |> File.read! |> String.split("\r\n") |> Enum.map(&parseLine/1) |> uniqueStations
  end

  def uniqueStations(measurements) do
    measurements |> Enum.uniq_by(fn %{:location => location} -> location end) |> Enum.map(fn %{:location => location = {lng, lat}} -> {"station_#{lng}_#{lat}", location} end)
  end



end
