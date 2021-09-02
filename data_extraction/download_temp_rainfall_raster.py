# Use the tool here: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=doc

# for temperature:

import cdsapi

c = cdsapi.Client()

c.retrieve(
  'reanalysis-era5-single-levels-monthly-means',
  {
    'format':'netcdf',
    'product_type':'monthly_averaged_reanalysis',
    'variable':[
      '2m_temperature'
    ],
    'year':[
      '2010'
    ],
    'month':[
      '01','02','03',
      '04','05','06',
      '07','08','09',
      '10','11','12'
    ],
    'time':'00:00'
  },
  'C:/Users/ido0493/Documents/temp_rainfall/2010_temperature.nc')


# for rainfall: 

# import cdsapi
#
# c = cdsapi.Client()
#
# c.retrieve(
#   'reanalysis-era5-single-levels-monthly-means',
#   {
#     'product_type':'monthly_averaged_reanalysis',
#     'variable':'total_precipitation',
#     'year':['2010'
#
#     ],
#     'month':[
#       '01','02','03',
#       '04','05','06',
#       '07','08','09',
#       '10','11','12'
#     ],
#     'time':'00:00',
#     'format':'netcdf'
#   },
#    'C:/Users/ido0493/Documents/temp_rainfall/2010_rainfall.nc')