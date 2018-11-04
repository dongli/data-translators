# Code table

## Report types

Reference [here](http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_2.htm).
- 111: SYNDAT - SYNTHETIC (BOGUS) TROPICAL CYCLONE STORM CENTER (generated in SYNDAT_SYNDATA) - q, Pstn.
- 112: N/A    - PSEUDO MEAN SEA-LEVEL PRESSURE AT TROPICAL CYCLONE STORM CENTER (generated in GSI, does not appear in pre-analysis PREPBUFR files) - Pstn.
- 120: ADPUPA - RAWINSONDE - Tv, q, Pstn, sst.
- 122: ADPUPA - CLASS SOUNDING - Tv, q, Pstn.
- 126: RASSDA - RASS [FROM NOAA PROFILER NETWORK (NPN) OR MULTI-AGENCY PROFILER (MAP) NETWORK]  - Tv.
- 130: AIRCFT - AIREP AND PIREP AIRCRAFT - Ts.
- 131: AIRCFT - AMDAR AIRCRAFT - Ts, q (E-AMDAR only).
- 132: ADPUPA - FLIGHT-LEVEL RECONNAISSANCE AND PROFILE DROPSONDE - Tv, q, Pstn.
- 133: AIRCAR - MDCRS ACARS AIRCRAFT - Ts, q.
- 134: AIRCFT - TAMDAR AIRCRAFT - Ts, q.
- 135: AIRCFT - CANADIAN AMDAR AIRCRAFT - Ts.
- 150: SPSSMI - SSM/I SUPEROBED (1 DEGREE LAT/LON) FNMOC (OPERATIONAL) RAIN RATE (DMSP) - rr.
- 151: GOESND - NESDIS 1x1 F-O-V CLOUD TOP PRESSURE, TEMPERATURE; CLOUD AMOUNT (GOES).
- 152: SPSSMI - SSM/I SUPEROBED (1 DEGREE LAT/LON) NEURAL NET-3 PRECIPITABLE WATER OVER OCEAN (DMSP) - PWt.
- 153: GPSIPW - GPS-INTEGRATED PRECIPITABLE WATER (GPS-IPW) - PWt.
- 180: SFCSHP - SURFACE MARINE WITH REPORTED STATION PRESSURE (SHIP, BUOY, C-MAN, TIDE GAUGE) - Tv, q, Pstn, sst.
- 181: ADPSFC - SURFACE LAND [SYNOPTIC (fixed and mobile), METAR] WITH REPORTED STATION PRESSURE - Tv, q, Pstn, sst.
- 182: SFCSHP - SPLASH-LEVEL DROPSONDE OVER OCEAN - Tv, q, Pstn.
- 183: ADPSFC/SFCSHP - SURFACE MARINE (SHIP, BUOY, C-MAN, TIDE GAUGE) OR LAND [SYNOPTIC (fixed and mobile), METAR] WITH MISSING STATION PRESSURE - Tv, q, Pstn, sst.
- 187: ADPSFC - SURFACE LAND (METAR) WITH MISSING STATION PRESSURE - Tv, q, Pstn, sst.
- 220: ADPUPA - RAWINSONDE - u, v (all levels), z (winds-by-height levels)
- 284: ADPSFC/SFCSHP - SURFACE MARINE (SHIP, BUOY, C-MAN, TIDE GAUGE) OR LAND [SYNOPTIC (fixed and mobile), METAR] WITH MISSING STATION PRESSURE - u, v

## CAT

- 0: Surface level (mass reports only)
- 1: Mandatory level (upper-air profile reports)
- 2: Significant temperature level (upper-air profile reports)
- 2: Significant temperature and winds-by-pressure level (future combined mass
  and wind upper-air profile reports)
- 3: Winds-by-pressure level (upper-air profile reports)
- 4: Winds-by-height level (upper-air profile reports)
- 5: Tropopause level (upper-air profile reports)
- 6: Single level report, or report not on any of the levels denoted by values
  0-5 or 9 (e.g., aircraft, satellite wind, surface wind, precipitable water
  retrievals, etc.)
- 7: Auxiliary levels generated via interpolation from spanning levels
  (upper-air profile reports)
- 8-62: Reserved
- 63: Missing value

## Observation quality markers

- 0: All steps: Keep (always assimilate). Applies to pressure, height, wind,
  temperature, specific humidity, rainfall rate, precipitable water and cloud
  top pressure.
- 1: All steps: Good. Applies to pressure, height, wind, temperature, specific
  humidity, rainfall rate, precipitable water and cloud top pressure.
- 2: All steps: Neutral or not checked (default). Applies to pressure, height,
  wind, temperature, specific humidity, rainfall rate, precipitable water and
  cloud top pressure.
- 3: All steps: Suspect. Applies to pressure, height, wind, temperature,
  specific humidity, rainfall rate, precipitable water and cloud top pressure.
- 4-15: All steps: Rejected (don't assimilate), as defined below (see % below table):
- 4: Step OIQC:  An observation with pre-existing quality marker 0 (keep) is
  flagged.  Applies to pressure, height, wind, temperature, specific humidity
  and precipitable water.
- 5: Step OIQC:  An observation with pre-existing quality marker 1 (good) is
  flagged.  Applies to pressure, height, wind, temperature, specific humidity
  and precipitable water.
- 6: Step OIQC:  An observation with pre-existing quality marker 2
  (neutral/default) is flagged.  Applies to pressure, height, wind,
  temperature, specific humidity and precipitable water.
- 7: Step OIQC:  An observation with pre-existing quality marker 3 (suspect) is
  flagged.  Applies to pressure, height, wind, temperature, specific humidity
  and precipitable water.
- 8: Reference [here](http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_7.htm).
- 9: Step PREVENT: An observation error is missing (does not apply for RUC
  network).  Applies to surface pressure, height, wind, temperature, specific
  humidity and precipitable water.  (Note: If  surface pressure observation
  error is missing, this quality marker is set on all other data on surface
  level - i.e., height, wind, temperature and specific humidity - regardless of
  whether or not its observation error is missing.)
- NCEP/SDM: An observation is assigned a purge flag.  Applies to pressure,
  height, wind, temperature and specific humidity.
- 15: Step PREPRO: An observation is flagged for non-use by the analysis.
  Applies to pressure, height, wind, temperature, specific humidity, rainfall
  rate, precipitable water and cloud top pressure.
  
  Step PREVENT: A moisture observation is above 300 mb.  Applies to moisture.

  Step VIRTMP: A virtual temperature is generated from a specific humidity
  observation where the specific humidity has a rejected quality marker of 9 or
  15 and the sensible temperature quality marker is either not rejected or is
  rejected but with a value of 9 or 15.  Applies to temperature.  (Note: Prior
  to 12/04/2007 this case received quality marker 8.)

## Observation program codes

- 0: Reserved.
- 1: Initial PREPBUFR processing step "PREPRO" (performed in PREPOBS_PREPDATA
  program, prior to "PREVENT" and "VIRTMP" steps).
- 2: Synthetic tropical cyclone bogus processing step "SYNDATA" (performed in
  SYNDAT_SYNDATA program, prior to "PREVENT" and "VIRTMP" steps).
- 3: Reserved.
- 4: Pre-quality control step "PREVENT" which adds GFS forecast background and
  observation error (if present) and performs check of surface pressure
  (performed in PREPOBS_PREPDATA program after "PREPRO" step but prior to
  'VIRTMP" step; performed in SYNDAT_SYNDATA program after "SYNDATA" step but
  prior to "VIRTMP" step; performed in PREPOBS_PREVENTS program prior to
  "VIRTMP" step).
- 5: Rawinsonde height/temperature complex quality control step "CQCHT"
  (performed in PREPOBS_CQCBUFR program, prior to "RADCOR" and "VIRTMP" steps).
- 6: Rawinsonde height/temperature intersonde bias (radiation) correction step
  "RADCOR" (performed in PREPOBS_CQCBUFR program, after "CQCHT" step but prior
  to "VIRTMP" step).
- 7: AIREP, PIREP and AMDAR aircraft quality control step "PREPACQC" (performed
  in PREPOBS_PREPACQC program - OBSOLETE AFTER 7/17/2012, replaced by step
  "NRLACQC").
- 8: Virtual temperature/specific humidity processing step "VIRTMP" (performed
  in PREPOBS_PREPDATA program after "PREPRO" and "PREVENT" steps for surface
  data; performed in SYNDAT_SYNDATA program after "SYNDATA" and "PREVENT" steps
  for synthetic tropical cyclone bogus data; performed in PREPOBS_CQCBUFR
    program after "CQCHT" and "RADCOR" steps for upper-air data; performed in
    PREPOBS_PREVENTS program after "PREVENT" step for surface data).
- 9: Wind profiler quality control step "CQCPROF" (performed in PREPOBS_PROFCQC
  program).
- 10: Multi-platform OI-quality control step "OIQC" (performed in
  PREPOBS_OIQCBUFR program which runs only in CDAS).
- 11: SSI analysis step "SSI" (performed only in CDAS_SSI program)
- 12: VAD wind quality control step "CQCVAD" (performed in PREPOBS_CQCVAD program).
- 13: Regional (ETA/EDAS) 3DVAR analysis step "R3DVAR" (OBSOLETE).
- 14: ACARS aircraft quality control step "ACARSQC" (performed in
  PREPOBS_ACARSQC program - OBSOLETE AFTER 7/17/2012, replaced by step
  "NRLACQC").
- 15: AIREP, PIREP, AMDAR and MDCRS aircraft quality control step "NRLACQC"
  (performed in PREPOBS_PREPACQC program - REPLACED steps "PREPACQC" and
  "ACARSQC" 7/17/2012).
- 16: GSI analysis step "GSI" (performed in all networks except CDAS).
- 17-30: Reserved for future use.
- 31: Missing value.

