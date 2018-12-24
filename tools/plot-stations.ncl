if (isdefined("netcdf_file")) then
  f = addfile(netcdf_file, "r")

  station_lon = f->station_lon
  station_lat = f->station_lat
else
  if (.not. isdefined("station_lon")) then
    system("echo '[Error]: Argument station_lon should be given as a number or array!'")
    exit
  end if
  if (.not. isdefined("station_lat")) then
    system("echo '[Error]: Argument station_lat should be given as a number or array!'")
    exit
  end if
end if

wks = gsn_open_wks("pdf", "stations")

res_map = True
res_map@gsnDraw = False
res_map@gsnFrame = False
res_map@mpFillOn = False
res_map@mpLimitMode = "LatLon"
res_map@mpMinLonF = min(station_lon) - 0.1 * (max(station_lon) - min(station_lon))
res_map@mpMaxLonF = max(station_lon) + 0.1 * (max(station_lon) - min(station_lon))
res_map@mpMinLatF = min(station_lat) - 0.1 * (max(station_lat) - min(station_lat))
res_map@mpMaxLatF = max(station_lat) + 0.1 * (max(station_lat) - min(station_lat))

map = gsn_csm_map(wks, res_map)

res_station = True
res_station@gsMarkerIndex = 16
res_station@gsMarkerSizeF = 0.0005
res_station@gsMarkerColor = "red"

gsn_polymarker(wks, map, station_lon, station_lat, res_station)

draw(map)
frame(wks)
