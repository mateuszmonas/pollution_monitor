defmodule PollutionData do
  @moduledoc false
  def parseLine(line) do
    [date, time, lng, lat, value] = String.split(line, ",")
    %{:datetime =>
      {
        date
        |> String.split("-")
        |> Enum.map(&String.to_integer/1)
        |> Enum.reverse
        |> List.to_tuple,
        time
        |> String.split(":")
        |> Enum.map(&String.to_integer/1)
        |> List.insert_at(2, 0)
        |> List.to_tuple
    },
      :location =>
      {lng |> Float.parse |> elem(0), lat |> String.to_float},
      :pollutionLevel => value |> String.to_integer
    }
  end

  def importLinesFromCSV(path) do
    path |> File.read! |> String.split("\r\n") |> Enum.map(&parseLine/1)
  end

  def uniqueStations(measurements) do
    measurements |>
    Enum.uniq_by(fn %{:location => location} -> location end) |>
    Enum.map(fn %{:location => location = {lng, lat}} -> {"station_#{lng}_#{lat}", location} end)
  end

  def addStation({name, location}) do
    :pollution_gen_server.addStation(name, location)
  end

  def addMeasurement(%{:datetime => datetime, :location => location, :pollutionLevel => value, :type => type}) do
    :pollution_gen_server.addValue(location, datetime, type, value)
  end

end
