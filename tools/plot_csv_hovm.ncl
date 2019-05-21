load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_string.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_inv_string.ncl"

file_name = "hourly.csv"

values_1d = asciiread(file_name, -1, "string")
ncols     = dimsizes(str_split(values_1d(0), ","))
nrows     = dimsizes(values_1d)

values_2d = transpose(onedtond(asciiread(file_name, -1, "float"), (/nrows,ncols/)))
values_2d@_FillValue = 999999.0

time = new(nrows, double)
time_labels = new(nrows, string)
do i = 0, nrows - 1
  tmp = str_split(values_1d(i), ",")
  time(i) = cd_inv_string(str_strip(tmp(0)), "%Y%N%D%H")
  time_labels(i) = cd_string(time(i), "%Y-%N-%D")
end do
data = values_2d(1:,:)
data!1 = "time"
data&time = time

wks = gsn_open_wks("pdf", file_name + ".hovm")

res = True
res@vpWidthF = 0.6
res@vpHeightF = 0.3
res@cnLinesOn = False
res@cnFillOn = True
res@cnFillPalette = "amwg"
res@tiYAxisString = "Level"
res@tiXAxisString = "Time"
res@tmXBMode = "Explicit"
res@tmXBValues = time(::192)
res@tmXBLabels = time_labels(::192)
res@lbOrientation = "Vertical"

plot = gsn_csm_contour(wks, data, res)
