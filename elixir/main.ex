
defmodule Parse do

    def parse_date(str) do
      {:ok, {year, month, day, hour, minute, seconds, _}} = Calendar.ISO.parse_naive_datetime(str)
      { {year, month, day}, {hour, minute, seconds} }
    end

    def parse_coords(str) do
      str |> String.split(",") |> List.to_tuple()
    end

    def parseline(line) do
        [date, type, value, id, name, coords] = line
        %{
            :datetime => parse_date(date),
            :location => parse_coords(coords),
            :stationId => String.to_integer(id),
            :stationName => id <> " " <> name,
            :pollutionType => type,
            :pollutionLevel => String.to_float(value)
        }
    end
end

defmodule Server do
  def add_stations(data) do
    data
    |> Enum.uniq_by(& &1[:stationId])
    |> Enum.map(& (:pollution_gen_server.add_station(&1[:stationName], &1[:location])))
  end

  def load_data(data) do
    data
    |> Enum.map(& (
      :pollution_gen_server.add_value(&1[:stationName], &1[:datetime], &1[:pollutionType], &1[:pollutionLevel])
    ))
  end
end


data = File.read!("in.csv")
|> String.split("\n")
|> Enum.map( & (String.split(&1, ";")) )
|> Enum.filter(& (&1 != [""]) )
|> Enum.map(&Parse.parseline/1)

Code.append_path("pol/_build/default/lib/pol/ebin/")
Application.stop(:pol)

Application.start(:pol)

IO.puts( (fn -> Server.add_stations(data) end |> :timer.tc |> elem(0)) / 1000000.0 )
IO.puts( (fn -> Server.load_data(data) end |> :timer.tc |> elem(0)) / 1000000.0 )
IO.puts(:pollution_gen_server.get_station_min("9910 Polska, Kraków, Studencka", "PM10"))
IO.puts( (fn -> :pollution_gen_server.get_station_min("9910 Polska, Kraków, Studencka", "PM10") end |> :timer.tc |> elem(0)) / 1000000.0 )
IO.puts(:pollution_gen_server.get_daily_mean("PM25", {2024, 2, 10}))
IO.puts( (fn -> :pollution_gen_server.get_daily_mean("PM25", {2024, 2, 10}) end |> :timer.tc |> elem(0)) / 1000000.0 )
