# RADIOSONDE OBSERVATION (RAOB) DATA TABULATION

The upper air data that are collected and transmitted during the flight of a radiosonde include the air pressure, air temperature and humidity measured continuously by the instruments aboard the radiosonde package. The radiosonde observations (RAOB) are directly transmitted by the radio transmitter for various levels in the free atmosphere. At the receiving station, the height of the package is sequentially computed at each reporting level using an equation (hypsometric equation) from the reported pressure, temperature and humidity of each incremental layer. The wind information at various levels is determined The wind speed and wind direction at various levels are determined from the ground-based radio tracking antenna that tracks the instrument package as it is carried by the wind during the ascent of the radiosonde.

These observations are processed, tabulated and encoded for transmission over communication networks. While the radiosonde transmits an essentially continuous stream of temperature and humidity information back to the station, the RAOB information disseminated over the conventional weather communications network is limited by necessity to the above mentioned weather elements at the following atmospheric levels:

**MANDATORY LEVELS**: By international convention, these specific pressure levels must be reported in the RAOB message: the surface, 1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100, 70, 50, and 10 mb.. The information for some of these levels is plotted routinely on constant pressure charts to show the spatial variability of these levels and it is used as input in the numerical weather prediction models.
**SIGNIFICANT LEVELS**: These pressure levels (other than the mandatory levels) transmitted as part of the RAOB message are at significant or abrupt changes and extrema in the vertical temperature and/or dewpoint temperature profiles. By assuming that the temperature and dewpoint profiles change linearly with height between significant levels (i.e., constant environmental lapse rate), a reasonably accurate reproduction of the RAOB sounding can be made from the sequence of RAOB message information at significant levels and supplemented by the mandatory levels.
Date: The time in UTC (or Z time) and date of the RAOB.
The Station Identifier Block
The three letter FAA or NWS identifier for the station that you have selected appears first.
WMO identifier. A five digit number assigned to this weather station by the World Meteorological Organization (WMO) for identification purposes.
The latitude and longitude of the station are reported to the tenth of a degree of arc. Since the longitude is measured eastward from the Greenwich Prime Meridian, stations in the Western Hemisphere (to include all stations in the continental United States and Canada) will have a negative sign preceding the value.
The Elevation is the reported elevation of the station above mean sea level measured in meters.
Data Table for Individual Levels
The observed and computed values for the Surface (SFC) and all mandatory and significant levels for the particular RAOB sounding is listed in the following table. The data are arranged in columns as indicated:
SYMBOL       VARIABLE and UNITS                     
REMARKS                 
LEV          Index number of layer   The number of the layer starts with    
                                     the first significant or mandatory     
                                     level above the surface, identified    
                                     as "SFC" in the table.                 

PR           Pressure in millibars   The pressure measured by the           
             [mb]                    radiosonde is used to describe its    
                                     vertical displacement in the           
                                     atmosphere and identifies both the     
                                     mandatory and significant levels.      

H            Height in meters [m]    This computed height is reported only  
                                     for the surface and the mandatory      
                                     levels only.                           

T            Air Temperature [deg.C} The measured air temperature and       
TD           Dewpoint [deg. C}       dewpoint are reported to tenths of a   
                                     degree Celsius.                        

DIR          Wind Direction          The wind direction is from where the   
             [degrees]               wind is blowing and is measured as an  
                                     angle in the clockwise direction from  
                                     true north (360 deg).  Calm            
                                     conditions are reported as 0 deg.      

SPD          Wind Speed [knt]        The wind speed is measured in knots    
                                     Note: Wind speed and direction are     
                                     reported in this table at mandatory    
                                     levels only.                           

THETA        Potential Temperature   The potential temperature is a         
             [K]                     computed variable used by              
                                     meteorologists to identify an air      
                                     parcel.  It is defined as the          
                                     absolute temperature [in kelvins]      
                                     that an air parcel would reach if the  
                                     parcel were brought from its initial   
                                     conditions to a pressure of 1000 mb    
                                     by a dry adiabatic process.            

THETAE       Equivalent Potential    The equivalent potential is another    
             Temperature [K}         computed variable used by              
                                     meteorologists to identify an air      
                                     parcel.  This variable differs from    
                                     the potential temperature in that it   
                                     incorporates the effects of humidity   
                                     within the air parcel.  The value is
                                     defined as the temperature that an 
                                     air parcel would have if all its 
                                     water vapor were condensed and removed 
                                     and the parcel brought by a dry 
                                     adiabatically to a pressure of 1000 mb.

W            Mixing Ratio [g/kg]     The mixing ratio that is computed for  
                                     each level is defined as the mass of   
                                     the water vapor in a unit mass of dry  
                                     air, typically reported in grams of    
                                     water vapor per kilogram of dry air.   
At the bottom of this table two other levels may be included:

TROP         Height of the           In this tabulation, the height of the  
             Tropopause              tropopause is given in pressure (mb)   
                                     rather than geometric height units.    
                                     The tropopause
 represents              
                                     the boundary between the troposphere   
                                     and the stratosphere and is usually    
                                     identified as the region in the upper  
                                     troposphere where the usual decrease   
                                     in temperature changes to an           
                                     isothermal or to an inversion          
                                     condition.  The conventional           
                                     tropopause is defined as the first     
                                     tropopause, or the lowest altitude     
                                     level where the average lapse rate     
                                     within the next higher 2 km does not   
                                     exceed 2 Celsius deg per km.           
                                                                            
WIND         Level of Maximum Wind   In this tabulation, the height of the  
                                     tropopause is given in pressure        
                                     rather than geometric height units.    
                                     Typically this maximum wind group      
                                     will only be reported if the maximum   
                                     wind speed were in excess of 60 knots. 
Sounding Variables and indices

The following information has been computed from the tabular data and is intended for use in the assessment of the potential threat of severe weather.
Convective or Convection        The altitude (expressed as a pressure level 
  condensation level            in mb) representing the height of the base 
                                of a cumuliform cloud, produced solely 
                                from thermal convection caused by surface 
                                heating.  Defined as that level to which 
                                an air parcel, if heated sufficiently     
                                from below (with a surface temperature    
                                equal to or greater than the convective  
                                temperature) will rise dry adiabatically 
                                without becoming colder than its 
                                environment until the parcel just 
                                becomes saturated.                       
                                                                           
Mean mixing ratio               The average mixing ratio (see above) in 
                                g per kg for the entire layer from the 
                                surface to the last reported dewpoint level.

Convective temperature          The temperature (in degrees Celsius)       
                                often used to forecast the onset of         
                                convection and cumuliform clouds because    
                                if the surface temperature reaches this   
                                value, surface air parcels would become  
                                sufficiently buoyant to reach the         
                                Convective condensation level.
                                                                           
Freezing Level                  The lowest altitude (expressed as a        
                                pressure level in mb) where the air        
                                temperature falls to 0 degrees Celsius.    
                                The height of this level is important      
                                for predicting hail size and for           
                                forecasting aviation icing hazards.        
                                                                           
Lifting condensation level      The altitude (expressed as a pressure      
                                level in mb) equivalent to the height of 
                                the base of a stratiform cloud, if 
                                condensation were caused by forced accent.  
                                This level is the lowest altitude at 
                                which a parcel of moist air would just 
                                become saturated when lifted dry 
                                adiabatically by a mechanical process.     
                                                                           
1000-500 mb thickness           The vertical distance (in meters) between   
                                the 1000 mb and the 500 mb pressure 
                                surfaces.  The thickness is directly  
                                proportional to mean temperature of this  
                                1000 to 500 mb layer.        
                                                                           
Precipitable water              The total amount of water vapor contained 
                                in a vertical column of unit cross section 
                                area, expressed as the depth (in inches) of
                                the liquid equivalent of the vapor in the 
                                column if all that water vapor were  
                                condensed and collected.

Sfc to 500 mb mean relative     A value (expressed as a percent) that      
    humidity                    indicates on average how close to 
                                saturation that the air is in the surface
                                to 500 mb layer.                

Lifted Index                    A stability index with the numerical value 
                                computed from the sounding, together with 
                                the risk for severe thunderstorms
Showalter Index                 Ditto
Vertical Totals Index           Ditto
Cross Totals Index              Ditto
Totals Totals Index             Ditto
K Index                         Ditto
Sweat Index                     Ditto                                      
Energy Index                    Ditto                                    