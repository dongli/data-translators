# 简介

**开发当中...**

这个解码工具包是为了将多种来源和格式的气象观测数据转换为多种所需的格式，如XML转PrepBUFR，并且存储到后端数据库做统一管理。
程序实现采用Fortran语言，但是利用了Fortran 2008语言特性，如多态指针做的哈希表数据结构类型等，设计了一套统一的`type`类型，
方便在其它算法模块中调取数据，并按照统一的数据结构使用观测数据，如：

```Fortran
type, extends(obs_static_record_base_type) :: synop_record_type
  type(synop_station_type), pointer :: station
  real :: sfc_temperature       = real_missing_value ! Temperature (degC)
  real :: sfc_dewpoint          = real_missing_value ! Dewpoint temperature (degC)
  real :: sfc_pressure          = real_missing_value ! Surface pressure (Pa)
  real :: sfc_relative_humidity = real_missing_value ! Relative humidity (%)
  real :: sfc_specific_humidity = real_missing_value ! Specific humidity (Mg/Kg)
  real :: sfc_wind_speed        = real_missing_value ! Wind speed (m/s)
  real :: sfc_wind_direction    = real_missing_value ! Wind direction (deg)
  real :: sfc_rain_01h          = real_missing_value ! 1h accumulated total precipitation (mm)
  real :: sfc_rain_03h          = real_missing_value ! 3h accumulated total precipitation (mm)
  real :: sfc_rain_06h          = real_missing_value ! 6h accumulated total precipitation (mm)
  real :: sfc_rain_12h          = real_missing_value ! 12h accumulated total precipitation (mm)
  real :: sfc_rain_24h          = real_missing_value ! 24h accumulated total precipitation (mm)
  real :: sfc_cloud_amount      = real_missing_value ! Cloud amount (???)

  real :: sfc_temperature_stack(max_stack) = real_missing_value
  integer :: sfc_temperature_qc(max_stack) = int_missing_value
  integer :: sfc_temperature_pc(max_stack) = int_missing_value

  real :: sfc_specific_humidity_stack(max_stack) = real_missing_value
  integer :: sfc_specific_humidity_qc(max_stack) = int_missing_value
  integer :: sfc_specific_humidity_pc(max_stack) = int_missing_value

  real :: sfc_pressure_stack(max_stack) = real_missing_value
  integer :: sfc_pressure_qc(max_stack) = int_missing_value
  integer :: sfc_pressure_pc(max_stack) = int_missing_value

  real :: sfc_wind_u_stack(max_stack) = real_missing_value
  real :: sfc_wind_v_stack(max_stack) = real_missing_value
  integer :: sfc_wind_qc(max_stack) = int_missing_value
  integer :: sfc_wind_pc(max_stack) = int_missing_value
end type synop_record_type
```

其中的字段（如`sfc_temperature`）与数据库中的字段保持一致，减少记忆的负担。

需要支持的观测数据格式：

- PrepBUFR
- BUFR
- Z报文
- CIMISS的XML文本

需要支持的输出数据格式：

- PrepBUFR
- BUFR
- CSV
- ODB

同时解码的数据要统一存入后端数据库，目前考虑使用PostgreSQL数据库，并结合TimescaleDB扩展支持长时间序列存储和查询。

# 程序编译

本项目采用了CMake管理，使用流程如下：

```
cd <longrun-decode>/build
FC=gfortran cmake ..
make
```

编译成功后应该会看到`longrun-decode.exe`可执行文件。

# 解码命令使用

只需使用一条命令（`longrun-decode.exe`），输入不同参数：

- `-d`指定需要的解码模块，如`synop_prepbufr`是从PrepBUFR文件中解码地面台站观测数据
- `-f`指定输入文件路径

```
$ ./longrun-decode.exe -d synop_prepbufr -f <PrepBUFR文件路径>
```
