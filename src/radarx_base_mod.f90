module radarx_base_mod

  use radar_mod
  use string_mod
  use netcdf

  implicit none

  integer, parameter :: max_num_radial = 1000
  integer, parameter :: max_num_radial_data = 10000

  type(radar_station_type) radar_station
  type(radar_record_type) radar_record

contains

  subroutine radarx_base_decode(file_path)

    character(*), intent(in) :: file_path

    ! Common header fields
    integer(4) magic_number                       ! 01
    integer(2) major_version                      ! 02
    integer(2) minor_version                      ! 03
    integer(4) generic_type                       ! 04
    integer(4) product_type                       ! 05
    character(16) reserved1                       ! 06
    ! Station fields
    character(8) site_code                        ! 01
    character(32) site_name                       ! 02
    real(4) lat                                   ! 03
    real(4) lon                                   ! 04
    integer(4) antenna_height                     ! 05
    integer(4) ground_height                      ! 06
    real(4) freq                                  ! 07
    real(4) beam_width_h                          ! 08
    real(4) beam_width_v                          ! 09
    integer(4) rda_version                        ! 10
    integer(2) radar_type                         ! 11
    character(54) reserved2                       ! 12
    ! Task fields
    character(32) task_name                       ! 01
    character(128) task_desc                      ! 02
    integer(4) polar_type                         ! 03
    integer(4) scan_type                          ! 04
    integer(4) pulse_width                        ! 05
    integer(4) scan_start_time                    ! 06
    integer(4) cut_number                         ! 07
    real(4) noise_h                               ! 08
    real(4) noise_v                               ! 09
    real(4) calibration_h                         ! 10
    real(4) calibration_v                         ! 11
    real(4) noise_temp_h                          ! 12
    real(4) noise_temp_v                          ! 13
    real(4) zdr_calibration                       ! 14
    real(4) phidp_calibration                     ! 15
    real(4) ldr_calibration                       ! 16
    character(40) reserved3                       ! 17
    ! Scan fields
    integer(4) process_mode                       ! 01
    integer(4) wave_form                          ! 02
    real(4) prf1                                  ! 03 Pulse repeat frequency #1
    real(4) prf2                                  ! 04 Pulse repeat frequency #2
    integer(4) dealias_mode                       ! 05
    real(4) azimuth                               ! 06
    real(4) elevation                             ! 07
    real(4) start_angle                           ! 08
    real(4) end_angle                             ! 09
    real(4) angular_resolution                    ! 10
    real(4) scan_speed                            ! 11
    integer(4) log_resolution                     ! 12
    integer(4) doppler_resolution                 ! 13
    integer(4) max_range1                         ! 14
    integer(4) max_range2                         ! 15
    integer(4) start_range                        ! 16
    integer(4) num_sample1                        ! 17
    integer(4) num_sample2                        ! 18
    integer(4) phase_mode                         ! 19
    real(4) atm_loss                              ! 20
    real(4) nyq_speed                             ! 21
    integer(8) moments_mask                       ! 22
    integer(8) moments_size_mask                  ! 23
    integer(4) misc_filter_mask                   ! 24
    real(4) sqi_threshold                         ! 25
    real(4) sig_threshold                         ! 26
    real(4) csr_threshold                         ! 27
    real(4) log_threshold                         ! 28
    real(4) cpa_threshold                         ! 29
    real(4) pmi_threshold                         ! 30
    real(4) dplog_threshold                       ! 31
    character(4) threshold_r                      ! 32
    integer(4) dbt_mask                           ! 33
    integer(4) dbz_mask                           ! 34
    integer(4) velocity_mask                      ! 35
    integer(4) spectrum_width_mask                ! 36
    integer(4) dp_mask                            ! 37
    character(12) reserved4                       ! 38
    integer(4) scan_sync                          ! 39
    integer(4) direction                          ! 40
    integer(2) ground_clutter_classifier_type     ! 41
    integer(2) ground_clutter_filter_type         ! 42
    integer(2) ground_clutter_filter_notch_width  ! 43
    integer(2) ground_clutter_filter_window       ! 44
    character(72) reserved5                       ! 45
    ! Radial header fields
    integer(4) radial_state                       ! 01
    integer(4) spot_blank                         ! 02
    integer(4) sequence_number                    ! 03
    integer(4) radial_number                      ! 04
    integer(4) elevation_number                   ! 05
    ! real(4) azimuth                             ! 06
    ! real(4) elevation                           ! 07
    integer(4) seconds                            ! 08
    integer(4) microseconds                       ! 09
    integer(4) data_length                        ! 10
    integer(4) moment_number                      ! 11
    character(20) reserved6                       ! 12
    ! Radial data header fields
    integer(4) data_type                          ! 01
    integer(4) scale                              ! 02
    integer(4) offset                             ! 03
    integer(2) bin_length                         ! 04
    integer(2) flags                              ! 05
    integer(4) length                             ! 06
    character(12) reserved7                       ! 07
    ! Radial data fields
    character(1) data(max_num_radial_data)

    integer tilt_idx, moment_idx, radial_idx, i

    open(10, file=file_path, status='old', form='unformatted', access='stream')

    ! Common fields
    read(10) magic_number
    read(10) major_version
    read(10) minor_version
    read(10) generic_type
    read(10) product_type
    read(10) reserved1
    ! Station fields
    read(10) site_code
    read(10) site_name
    read(10) lat
    read(10) lon
    read(10) antenna_height
    read(10) ground_height
    read(10) freq
    read(10) beam_width_h
    read(10) beam_width_v
    read(10) rda_version
    read(10) radar_type
    read(10) reserved2

    radar_station%id = site_code
    radar_station%name = site_name
    radar_station%lon = lon
    radar_station%lat = lat
    radar_station%z = ground_height
    radar_station%freq = freq

    ! Task fields
    read(10) task_name
    read(10) task_desc
    read(10) polar_type
    read(10) scan_type
    read(10) pulse_width
    read(10) scan_start_time
    read(10) cut_number
    read(10) noise_h
    read(10) noise_v
    read(10) calibration_h
    read(10) calibration_v
    read(10) noise_temp_h
    read(10) noise_temp_v
    read(10) zdr_calibration
    read(10) phidp_calibration
    read(10) ldr_calibration
    read(10) reserved3

    radar_record%time = datetime(timestamp=scan_start_time)
    radar_record%num_tilt = cut_number
    allocate(radar_record%tilts(cut_number))

    ! Scan fields
    do tilt_idx = 1, cut_number
      read(10) process_mode
      read(10) wave_form
      read(10) prf1
      read(10) prf2
      read(10) dealias_mode
      read(10) azimuth
      read(10) elevation
      read(10) start_angle
      read(10) end_angle
      read(10) angular_resolution
      read(10) scan_speed
      read(10) log_resolution
      read(10) doppler_resolution
      read(10) max_range1
      read(10) max_range2
      read(10) start_range
      read(10) num_sample1
      read(10) num_sample2
      read(10) phase_mode
      read(10) atm_loss
      read(10) nyq_speed
      read(10) moments_mask
      read(10) moments_size_mask
      read(10) misc_filter_mask
      read(10) sqi_threshold
      read(10) sig_threshold
      read(10) csr_threshold
      read(10) log_threshold
      read(10) cpa_threshold
      read(10) pmi_threshold
      read(10) dplog_threshold
      read(10) threshold_r
      read(10) dbt_mask
      read(10) dbz_mask
      read(10) velocity_mask
      read(10) spectrum_width_mask
      read(10) dp_mask
      read(10) reserved4
      read(10) scan_sync
      read(10) direction
      read(10) ground_clutter_classifier_type
      read(10) ground_clutter_filter_type
      read(10) ground_clutter_filter_notch_width
      read(10) ground_clutter_filter_window
      read(10) reserved5

      radar_record%tilts(tilt_idx)%elevation = elevation
    end do

    tilt_idx = 1
    do while (tilt_idx <= radar_record%num_tilt)
      ! print *, '=> tilt_idx = ', tilt_idx, radar_record%tilts(tilt_idx)%elevation
      do while (.true.)
        ! Radial header fields
        read(10) radial_state
        read(10) spot_blank
        read(10) sequence_number
        read(10) radial_number
        read(10) elevation_number
        read(10) azimuth
        read(10) elevation
        read(10) seconds
        read(10) microseconds
        read(10) data_length
        read(10) moment_number
        read(10) reserved6

        ! print *, '##> radial_state = ', radial_state
        ! Allocate data at scan start or elevation start.
        if (radial_state == 3 .or. radial_state == 0) then
          radar_record%tilts(tilt_idx)%num_moment = moment_number
          allocate(radar_record%tilts(tilt_idx)%moments(moment_number))
          do moment_idx = 1, radar_record%tilts(tilt_idx)%num_moment
            ! We do not know number of radial data in advance, so allocate enough memory space for them.
            allocate(radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(max_num_radial))
          end do
          radial_idx = 1
        end if

        ! print *, '==> radial_idx = ', radial_idx, azimuth
        do moment_idx = 1, radar_record%tilts(tilt_idx)%num_moment
          radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(radial_idx)%radial_id = radial_number
          radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(radial_idx)%elevation_id = elevation_number
          radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(radial_idx)%azimuth = azimuth
          radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(radial_idx)%elevation = elevation

          read(10) data_type
          read(10) scale
          read(10) offset
          read(10) bin_length
          read(10) flags
          read(10) length
          read(10) reserved7

          select case (data_type)
          case (1)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'dBT'
          case (2)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'dBZ'
          case (3)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'V'
          case (4)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'W'
          case (5)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'SQI'
          case (6)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'CPA'
          case (7)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'ZDR'
          case (8)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'LDR'
          case (9)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'CC'
          case (10)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'DP'
          case (11)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'KDP'
          case (12)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'CP'
          case (14)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'HCL'
          case (15)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'CF'
          case (16)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'SNR'
          case (32)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'Zc'
          case (33)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'Vc'
          case (34)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'Wc'
          case (35)
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'ZDRc'
          case default
            radar_record%tilts(tilt_idx)%moments(moment_idx)%type = 'reserved'
          end select
          ! print *, '===> moment_idx = ', moment_idx, length, data_type, trim(radar_record%tilts(tilt_idx)%moments(moment_idx)%type)

          radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(radial_idx)%num_datum = length
          allocate(radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(radial_idx)%data(length))

          read(10) data(:length)
          do i = 1, length
            radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(radial_idx)%data(i) = ichar(data(i)) * scale + offset
          end do
        end do
        radial_idx = radial_idx + 1

        if (radial_state == 2 .or. radial_state == 4) then
          tilt_idx = tilt_idx + 1
          exit
        end if
      end do
    end do

    close(10)

  end subroutine radarx_base_decode

  subroutine radarx_base_write_laps_netcdf(radar_station, radar_record)

    type(radar_station_type), intent(in) :: radar_station
    type(radar_record_type), intent(in) :: radar_record

    character(256) file_path
    integer ncid, err, tilt_idx, moment_idx
    integer radarNameLen_dimid, siteNameLen_dimid, radial_dimid, Z_bin_dimid, V_bin_dimid
    integer Z_varid, Z_scale_varid, Z_offset_varid
    integer V_varid, V_scale_varid, V_offset_varid
    integer W_varid
    integer elevationNumber_varid
    integer elevationAngle_varid
    integer numRadials_varid
    integer radarName_varid
    integer radialAzim_varid
    integer radialElev_varid
    integer radialTime_varid
    integer siteAlt_varid
    integer siteLat_varid
    integer siteLon_varid
    integer siteName_varid
    integer VCP_varid
    integer esStartTime_varid
    integer esEndTime_varid
    integer firstGateRangeZ_varid
    integer firstGateRangeV_varid
    integer gateSizeZ_varid
    integer gateSizeV_varid
    integer numGatesZ_varid
    integer numGatesV_varid
    integer resolutionV_varid
    integer unambigRange_varid
    integer nyquist_varid
    integer calibConst_varid
    integer atmosAttentFactor_varid
    integer powDiffThreshold_varid

    do tilt_idx = 1, radar_record%num_tilt
      file_path = trim(radar_record%time%format('%y%j%H%M')) // '_elev' // trim(to_string(tilt_idx, zero_pad_width=2))
      err = nf90_create(file_path, nf90_clobber, ncid)
      if (err == nf90_noerr) then
        write(*, *) '[Error]: Failed to create LAPS netcdf file ' // trim(file_path) // '!'
      end if
      err = nf90_def_dim(ncid, 'radarNameLen', 5, radarNameLen_dimid)
      err = nf90_def_dim(ncid, 'siteNameLen', 132, siteNameLen_dimid)
      err = nf90_def_dim(ncid, 'radial', nf90_unlimited, radial_dimid)
      do moment_idx = 1, radar_record%tilts(tilt_idx)%num_moment
        select case (radar_record%tilts(tilt_idx)%moments(moment_idx)%type)
        case ('dBZ')
          err = nf90_def_dim(ncid, 'Z_bin', radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(1)%num_datum, Z_bin_dimid)
        case ('V')
          err = nf90_def_dim(ncid, 'V_bin', radar_record%tilts(tilt_idx)%moments(moment_idx)%radials(1)%num_datum, V_bin_dimid)
        end select
      end do
      ! Z
      err = nf90_def_var(ncid, 'Z', nf90_short, [radial_dimid, Z_bin_dimid], Z_varid)
      err = nf90_put_att(ncid, Z_varid, 'long_name', 'Reflectivity')
      err = nf90_put_att(ncid, Z_varid, 'units', 'dBZ')
      ! Z_scale
      err = nf90_def_var(ncid, 'Z_scale', nf90_float, Z_scale_varid)
      ! Z_offset
      err = nf90_def_var(ncid, 'Z_offset', nf90_float, Z_offset_varid)
      ! V
      err = nf90_def_var(ncid, 'V', nf90_float, [radial_dimid, V_bin_dimid], V_varid)
      err = nf90_put_att(ncid, V_varid, 'long_name', 'Velocity')
      err = nf90_put_att(ncid, V_varid, 'units', 'meter/second')
      ! V_scale
      err = nf90_def_var(ncid, 'V_scale', nf90_float, V_scale_varid)
      ! V_offset
      err = nf90_def_var(ncid, 'V_offset', nf90_float, V_offset_varid)
      ! W
      err = nf90_def_var(ncid, 'W', nf90_short, [radial_dimid, V_bin_dimid], W_varid)
      err = nf90_put_att(ncid, W_varid, 'long_name', 'Spectrum width')
      err = nf90_put_att(ncid, W_varid, 'units', 'meter/second') ! FIXME: Is the units right?
      ! elevationNumber
      err = nf90_def_var(ncid, 'elevationNumber', nf90_short, elevationNumber_varid)
      err = nf90_put_att(ncid, elevationNumber_varid, 'long_name', 'Elevation number')
      err = nf90_put_att(ncid, elevationNumber_varid, 'units', 'count')
      ! elevationAngle
      err = nf90_def_var(ncid, 'elevationAngle', nf90_float, elevationAngle_varid)
      err = nf90_put_att(ncid, elevationAngle_varid, 'long_name', 'Elevation angle')
      err = nf90_put_att(ncid, elevationAngle_varid, 'units', 'degree')
      ! numRadials
      err = nf90_def_var(ncid, 'numRadials', nf90_short, numRadials_varid)
      err = nf90_put_att(ncid, numRadials_varid, 'long_name', 'Number of radials')
      ! radialAzim
      err = nf90_def_var(ncid, 'radialAzim', nf90_float, [radial_dimid], radialAzim_varid)
      err = nf90_put_att(ncid, radialAzim_varid, 'long_name', 'Radial azimuth anlge')
      err = nf90_put_att(ncid, radialAzim_varid, 'units', 'degree')
      ! radialElev
      err = nf90_def_var(ncid, 'radialElev', nf90_float, [radial_dimid], radialElev_varid)
      err = nf90_put_att(ncid, radialElev_varid, 'long_name', 'Radial elevation angle')
      err = nf90_put_att(ncid, radialElev_varid, 'units', 'degree')
      ! radialTime
      err = nf90_def_var(ncid, 'radialTime', nf90_double, [radial_dimid], radialTime_varid)
      err = nf90_put_att(ncid, radialTime_varid, 'long_name', 'Time of radial')
      err = nf90_put_att(ncid, radialTime_varid, 'units', 'seconds since 1970-01-01 00:00:00')
      ! siteName
      err = nf90_def_var(ncid, 'siteName', nf90_char, [siteNameLen_dimid], siteName_varid)
      err = nf90_put_att(ncid, siteName_varid, 'long_name', 'Long name of the radar site')
      ! radarName
      err = nf90_def_var(ncid, 'radarName', nf90_char, [radarNameLen_dimid], radarName_varid)
      err = nf90_put_att(ncid, radarName_varid, 'long_name', 'Official name of the radar')
      ! siteLat
      err = nf90_def_var(ncid, 'siteLat', nf90_float, siteLat_varid)
      err = nf90_put_att(ncid, siteLat_varid, 'long_name', 'Latitude of site')
      err = nf90_put_att(ncid, siteLat_varid, 'units', 'degrees_north')
      ! siteLon
      err = nf90_def_var(ncid, 'siteLon', nf90_float, siteLon_varid)
      err = nf90_put_att(ncid, siteLon_varid, 'long_name', 'Longitude of site')
      err = nf90_put_att(ncid, siteLon_varid, 'units', 'degrees_east')
      ! siteAlt
      err = nf90_def_var(ncid, 'siteAlt', nf90_float, siteAlt_varid)
      err = nf90_put_att(ncid, siteAlt_varid, 'long_name', 'Altitude of site above mean sea level')
      err = nf90_put_att(ncid, siteAlt_varid, 'units', 'meter')
      ! VCP
      err = nf90_def_var(ncid, 'VCP', nf90_short, VCP_varid)
      err = nf90_put_att(ncid, VCP_varid, 'long_name', 'Volume Coverage Pattern')
      ! esStartTime
      err = nf90_def_var(ncid, 'esStartTime', nf90_double, esStartTime_varid)
      err = nf90_put_att(ncid, esStartTime_varid, 'long_name', 'Start time of elevation scan')
      err = nf90_put_att(ncid, esStartTime_varid, 'units', 'seconds since 1970-01-01 00:00:00')
      ! esEndTime
      err = nf90_def_var(ncid, 'esEndTime', nf90_double, esEndTime_varid)
      err = nf90_put_att(ncid, esEndTime_varid, 'long_name', 'End time of elevation scan')
      err = nf90_put_att(ncid, esEndTime_varid, 'units', 'seconds since 1970-01-01 00:00:00')
      ! unambigRange
      err = nf90_def_var(ncid, 'unambigRange', nf90_float, unambigRange_varid)
      err = nf90_put_att(ncid, unambigRange_varid, 'long_name', 'Unambiguous range')
      err = nf90_put_att(ncid, unambigRange_varid, 'units', 'kilometer')
      ! firstGateRangeZ
      err = nf90_def_var(ncid, 'firstGateRangeZ', nf90_float, firstGateRangeZ_varid)
      err = nf90_put_att(ncid, firstGateRangeZ_varid, 'long_name', 'Range to 1st reflectivity gate')
      err = nf90_put_att(ncid, firstGateRangeZ_varid, 'units', 'kilometer')
      ! firstGateRangeV
      err = nf90_def_var(ncid, 'firstGateRangeV', nf90_float, firstGateRangeV_varid)
      err = nf90_put_att(ncid, firstGateRangeV_varid, 'long_name', 'Range to 1st Doppler gate')
      err = nf90_put_att(ncid, firstGateRangeV_varid, 'units', 'kilometer')
      ! gateSizeZ
      err = nf90_def_var(ncid, 'gateSizeZ', nf90_float, gateSizeZ_varid)
      err = nf90_put_att(ncid, gateSizeZ_varid, 'long_name', 'Reflectivity gate spacing')
      err = nf90_put_att(ncid, gateSizeZ_varid, 'units', 'kilometer')
      ! gateSizeV
      err = nf90_def_var(ncid, 'gateSizeV', nf90_float, gateSizeV_varid)
      err = nf90_put_att(ncid, gateSizeV_varid, 'long_name', 'Doppler gate spacing')
      err = nf90_put_att(ncid, gateSizeV_varid, 'units', 'kilometer')
      ! numGatesZ
      err = nf90_def_var(ncid, 'numGatesZ', nf90_short, numGatesZ_varid)
      err = nf90_put_att(ncid, numGatesZ_varid, 'long_name', 'Number of reflectivity gates')
      ! numGatesV
      err = nf90_def_var(ncid, 'numGatesV', nf90_short, numGatesV_varid)
      err = nf90_put_att(ncid, numGatesV_varid, 'long_name', 'Number of Doppler gates')
      ! resolutionV
      err = nf90_def_var(ncid, 'resolutionV', nf90_float, resolutionV_varid)
      err = nf90_put_att(ncid, resolutionV_varid, 'long_name', 'Doppler velocity resolution')
      err = nf90_put_att(ncid, resolutionV_varid, 'units', 'meter/second')
      ! nyquist
      err = nf90_def_var(ncid, 'nyquist', nf90_float, nyquist_varid)
      err = nf90_put_att(ncid, nyquist_varid, 'long_name', 'Nyquist velocity')
      err = nf90_put_att(ncid, nyquist_varid, 'units', 'meter/second')
      ! calibConst
      err = nf90_def_var(ncid, 'calibConst', nf90_float, calibConst_varid)
      err = nf90_put_att(ncid, calibConst_varid, 'long_name', 'System gain calibration constant')
      err = nf90_put_att(ncid, calibConst_varid, 'units', 'dB')
      ! atmosAttentFactor
      err = nf90_def_var(ncid, 'atmosAttentFactor', nf90_float, atmosAttentFactor_varid)
      err = nf90_put_att(ncid, atmosAttentFactor_varid, 'long_name', 'Atmospheric attenuation factor')
      err = nf90_put_att(ncid, atmosAttentFactor_varid, 'units', 'dB/kilometer')
      ! powDiffThreshold
      err = nf90_def_var(ncid, 'powDiffThreshold', nf90_float, powDiffThreshold_varid)
      err = nf90_put_att(ncid, powDiffThreshold_varid, 'long_name', 'Range de-aliasing threshold')
      err = nf90_put_att(ncid, powDiffThreshold_varid, 'units', 'dB')
      err = nf90_enddef(ncid)
      err = nf90_close(ncid)
    end do

  end subroutine radarx_base_write_laps_netcdf

end module radarx_base_mod
