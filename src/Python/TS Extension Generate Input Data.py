# -*- coding: utf-8 -*-
"""
Created on Mon Jan 31 16:04:05 2022

@author: johan
"""

# -*- coding: utf-8 -*-
"""
Created on Thu Jun 25 09:13:08 2020

@author: Johann Baumgartner
"""

import glob
import os.path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


from sklearn import datasets, linear_model, ensemble, neural_network
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import train_test_split
from sklearn.ensemble import BaggingRegressor

from pathlib import Path


import netCDF4
import numpy as np

import pandas as pd
import datetime as dt
import xarray as xr
import scipy as sc
from scipy import spatial

import time

east = 48.178067
west = 48.167052
south = 16.433543
north = 16.456803

48.172857, 16.439628

import cdsapi

#download climate reanalysis input data

c = cdsapi.Client()

era52mt = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': '2m_temperature',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era52mtglc2021.nc')

era5rad = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': 'surface_net_solar_radiation',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era5radglc2021.nc')

era5rad2 = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': 'surface_solar_radiation_downward_clear_sky',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era5rad2glc2021.nc')



era5precip = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': 'total_precipitation',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era5precipglc2021.nc')

era5cc = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': 'total_cloud_cover',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era5cc2021.nc')

era5sk = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': 'skin_temperature',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era5sk2021.nc')

era5sc = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': 'snow_depth',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era5sc2021.nc')

era5tr = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': 'surface_net_thermal_radiation_clear_sky',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era5tr2021.nc')

era5u10 = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': '10m_u_component_of_wind',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era5u2021.nc')

era5v10 = c.retrieve(
  'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': '10m_v_component_of_wind',
        'area': [
            north, west, south, east
        ],
        'grid': [0.5, 0.5],  # grid in 0.5deg steps in longitude/latitude
        'day': [f"{day:02d}" for day in range(1, 32)],
        'time': [f"{hour:02d}:00" for hour in range(24)],
        'month': [f"{month:02d}" for month in range(1, 13)],
        'year': [2021],
    },
    'era5v2021.nc')


nc = netCDF4.Dataset("era52mtglc.nc")
h = nc.variables['t2m']
times = nc.variables['time']
jd = netCDF4.num2date(times[:],times.units)
#hs = pd.Series(h[:,:],index=jd)
dts = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
dts5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
dts[0][dts[0] == -32767] = dts5t[dts5t!=-32767]
dts.insert(0,'date',pd.DataFrame(jd))
nc.close()

nc = netCDF4.Dataset("era5cc.nc")
h = nc.variables['tcc']
tcc = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
tcc5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
tcc[0][tcc[0] == -32767] = tcc5t[tcc5t!=-32767]
dts.insert(2,'cloud',tcc)
nc.close()

nc = netCDF4.Dataset("era5radglc.nc")
h = nc.variables['ssr']
ssr = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
ssr5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
ssr[0][ssr[0] == -32767] = ssr5t[ssr5t!=-32767]
dts.insert(3,'radiation',ssr)
nc.close()

nc = netCDF4.Dataset("era5precipglc.nc")
h = nc.variables['tp']
tp = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
tp5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
tp[0][tp[0] == -32767] = tp5t[tp5t!=-32767]
dts.insert(4,'precipitation',tp)
nc.close()

nc = netCDF4.Dataset("era5rad2glc.nc")
h = nc.variables['ssrdc']
ssrdc = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
ssrdc5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
ssrdc[0][ssrdc[0] == -32767] = ssrdc5t[ssrdc5t!=-32767]
dts.insert(5,'radiation2',ssrdc)
nc.close()

nc = netCDF4.Dataset("era5sk.nc")
h = nc.variables['skt']
skt = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
skt5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
skt[0][skt[0] == -32767] = skt5t[skt5t!=-32767]
dts.insert(6,'skintemp',skt)
nc.close()

nc = netCDF4.Dataset("era5sc.nc")
h = nc.variables['sd']
sc = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
sc5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
sc[0][sc[0] == -32767] = sc5t[sc5t!=-32767]
dts.insert(7,'snowdepth',sc)
nc.close()

nc = netCDF4.Dataset("era5tr.nc")
h = nc.variables['strc']
strc = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
strc5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
strc[0][strc[0] == -32767] = strc5t[strc5t!=-32767]
dts.insert(8,'thermalradiation',strc)
nc.close()

nc = netCDF4.Dataset("era5u.nc")
h = nc.variables['u10']
u10m = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
u10m5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
u10m[0][u10m[0] == -32767] = u10m5t[u10m5t!=-32767]
dts.insert(9,'u10m',u10m)
nc.close()

nc = netCDF4.Dataset("era5v.nc")
h = nc.variables['v10']
v10m = pd.DataFrame(np.array(np.array(np.array(h[:,0,0,:]))))
v10m5t = pd.DataFrame(np.array(np.array(np.array(h[:,1,0,:]))))[0]
v10m[0][v10m[0] == -32767] = v10m5t[v10m5t!=-32767]
dts.insert(10,'v10m',v10m)
nc.close()


dts.rename(columns={dts.columns[1]: 'temp'},inplace=True)


dts.date = dts.date.astype(str)

dts.date = pd.to_datetime(dts.date, utc = True)

dts = dts[dts['date'] < '2021-01-01']

dts['radiation'] = dts['radiation'] / 3600

dts['radiation2'] = dts['radiation2'] / 3600

dts['radiation2'][dts['radiation2']<0]=0

dts['precipitation'][dts['precipitation']==4.336808689942018e-19]=0

dts['radiation'][dts['radiation']==6.467517879274156e-14]=0


# read demand input data

import pandas as pd
import numpy as np
import h5py

f = h5py.File('path/data.h5',"r")
list(f.keys())
pd.DataFrame(np.array(f['hierarchyinfile/table']))

keys = pd.DataFrame(np.array(f['keys']))

dset = pd.DataFrame(np.array(f['hierarchyinfile/table']))

PiNdict =	{
  "Pi1":0,
  "Pi2":0,
  "Pi3":0,
  "Pi4":0,  
  "Pi5":0,
  "Pi6":0,    
  "Pi7":0,
  "Pi8":0,
  "Pi9":0,
  "Pi10":0,
  "Pi11":0,
  "Pi12":0,
  "Pi13":0,
  "Pi14":0,   
  "Pi15":0,
  "Pi17":1,
  "Pi18":1,
  "Pi19":1,
  "Pi20":1,
  "Pi21":1,
  "Pi22":1,
  "Pi23":1,
  "Pi24":1,
  "Pi25":1,
  "Pi26":1,   
  "Pi27":1,
  "Pi28":1,
  "Pi29":1,
  "Pi30":1,
  "Pi31":1,
  "Pi32":1,
  "Pi33":1,
  "Pi34":1
}

PiNgrdict =	{
("Pi1","1"):1,
("Pi1","2"):0,
("Pi1","3"):0,
("Pi2","1"):0,
("Pi2","2"):1,
("Pi2","3"):0,
("Pi3","1"):0,
("Pi3","2"):1,
("Pi3","3"):0,
("Pi4","1"):2,
("Pi4","2"):2,
("Pi4","3"):2,  
("Pi5","1"):0,
("Pi5","2"):0,
("Pi5","3"):0,
("Pi6","1"):0,
("Pi6","2"):0,    
("Pi6","3"):0,
("Pi7","1"):0,
("Pi7","2"):0,
("Pi7","3"):0,
("Pi8","1"):0,
("Pi8","2"):0,
("Pi8","3"):0,
("Pi9","1"):0,
("Pi9","2"):0,
("Pi9","3"):0,
("Pi10","1"):0,
("Pi10","2"):0,
("Pi10","3"):0,
("Pi11","1"):0,
("Pi11","2"):0,
("Pi11","3"):0,
("Pi12","1"):3,
("Pi12","2"):3,
("Pi12","3"):3,
("Pi13","1"):4,
("Pi13","2"):4,
("Pi13","3"):4,
("Pi14","1"):0,
("Pi14","2"):0,
("Pi14","3"):0,
("Pi15","1"):0,
("Pi15","2"):0,
("Pi15","3"):0,
("Pi17","1"):5,
("Pi17","2"):5,
("Pi17","3"):5,
("Pi18","1"):5,
("Pi18","2"):5,
("Pi18","3"):6,
("Pi19","1"):6,
("Pi19","2"):6,
("Pi19","3"):6,
("Pi20","1"):5,
("Pi20","2"):5,
("Pi20","3"):5,
("Pi21","1"):5,
("Pi21","2"):5,
("Pi21","3"):5,
("Pi22","1"):6,
("Pi22","2"):6,
("Pi22","3"):6,
("Pi23","1"):6,
("Pi23","2"):6,
("Pi23","3"):6,
("Pi24","1"):5,
("Pi24","2"):5,
("Pi24","3"):5,
("Pi25","1"):5,
("Pi25","2"):5,
("Pi25","3"):5,
("Pi26","1"):5,   
("Pi26","2"):5,
("Pi26","3"):5,
("Pi27","1"):5,
("Pi27","2"):5,
("Pi27","3"):5,
("Pi28","1"):5,
("Pi28","2"):5,
("Pi28","3"):5,
("Pi29","1"):7,
("Pi29","2"):7,
("Pi29","3"):7,
("Pi30","1"):8,
("Pi30","2"):8,
("Pi30","3"):8,
("Pi31","1"):5,
("Pi31","2"):5,
("Pi31","3"):5,
("Pi32","1"):5,
("Pi32","2"):5,
("Pi32","3"):5,
("Pi33","1"):5,
("Pi33","2"):5,
("Pi33","3"):5,
("Pi34","1"):5,
("Pi34","2"):5,
("Pi34","3"):5,
  
}

f = h5py.File('path/data.h5',"r")

pidf = pd.DataFrame()

for i in range (1,35):
    print(i)
    if i == 15 or i == 16 or i == 25 or i == 27 or i == 28:
      continue
    pid = pd.DataFrame(np.array(f['hierarchyinfile'+str(i)+'/table']))[['index','power_abs_1','power_abs_2','power_abs_3']]
    pid['date'] = pd.to_datetime(pid['index'])
    pid['PiN'] = pd.Series(np.repeat('Pi'+str(i),len(pid)))
    pid['Site'] = pd.Series(np.repeat(PiNdict['Pi'+str(i)],len(pid)))
    pidf = pidf.append(pid)
    

pidf['date'] = pd.to_datetime(pidf['index'])
pidf.drop(['index'], axis=1,inplace=True)

pidf2=pidf[['date','PiN','Site','power_abs_1','power_abs_2','power_abs_3']]
pidf2["grouping"] = pidf2.groupby(['date', 'PiN']).ngroup()

pidf2 = pd.wide_to_long(pidf2,stubnames=['power_abs_'], i=["grouping"], j="phase")
indx = pidf2.index.to_frame()
indx.reset_index(drop=True,inplace=True)
pidf2.reset_index(drop=True,inplace=True)
pidf2['phase'] = indx['phase']

glc = pidf2[['date','PiN','phase','Site','power_abs_']]

glc.reset_index(drop=True,inplace=True)

glc['phase']=glc['phase'].astype(str)
grmindex = glc.set_index(['PiN', 'phase']).index
loadtype=grmindex.map(PiNgrdict)
loadtype=pd.Series(loadtype)
glc['loadtype'] = loadtype
glc['date'] = pidf2['date']

glc['date'] = glc['date'].dt.tz_localize('Europe/Berlin',ambiguous=True,nonexistent="shift_forward")
glc['date'] = glc['date'].dt.tz_convert('UTC')

groups = pd.unique(glc["loadtype"])

glc['date'] = pd.to_datetime(glc.index, format='%Y-%m-%d %H:%M:%S.%f')
glc.date = date

glc.to_feather("path/glc")

# read PV input data

pv = pd.DataFrame(np.array(f['hierarchyinfile/table']))
pv['index'] = pd.to_datetime(pv['index'])
pv.rename(columns={pv.columns[0]: 'date'},inplace=True)
pv = pv.sort_values(by="date")

pv['date'] = pv['date'].dt.tz_localize('Europe/Berlin',ambiguous=True,nonexistent="shift_forward")
pv['date'] = pv['date'].dt.tz_convert('UTC')
pv.index = pv['date']
pv = pv.resample('H').mean()
pv['date'] = pd.to_datetime(pv.index)
pv = pv[['date','Bezug_W','Einspeisung_W','Eigenverbrauch_W','Produktion_W','Verbrauch_W']]
pv.reset_index(inplace=True,drop=True)

# capacity factors
pv['Produktion_W'] = pv['Produktion_W'] / 7990 

