if (.not. isdefined("station_lon")) then
  system("echo '[Error]: Argument station_lon should be given as a number or array!'")
  exit
end if
if (.not. isdefined("station_lat")) then
  system("echo '[Error]: Argument station_lat should be given as a number or array!'")
  exit
end if

wks = gsn_open_wks("pdf", "stations")

res_map = True
res_map@gsnDraw = False
res_map@gsnFrame = False
res_map@mpFillOn = False

map = gsn_csm_map(wks, res_map)

res_station = True
res_station@gsMarkerIndex = 16

gsn_polymarker(wks, map, station_lon, station_lat, res_station)

draw(map)
frame(wks)
