
import os
print('Running... %s'%os.path.basename(__file__))

import re, json
import subprocess
import numpy  as np
import pandas as pd

# AEROSOL CONFIGURATIONS:
# ==================================================================
# READ: AEROSOL CONFIGURATIONS
TEXT = open('./data/d.CONFIG-AERO.BINS.json').read()
TEXT = re.sub('^\#.*$', '', TEXT, 0, re.M)

AEROFIG = json.loads(TEXT)

# GET: NUMBER OF SIZE BINS
nBINS = AEROFIG['nBINS']

# GET: OVERALL BOUNDS [nm]
LOWER = AEROFIG['xLOWER']
UPPER = AEROFIG['xUPPER']

# GET: VOLATILITY THRESHOLD
THRESH = AEROFIG['CSAT-THRESH']

# GET: CURRENT SIZE BINS [nm]
BOUNDS = np.logspace(np.log10(LOWER), np.log10(UPPER), num=nBINS+1)
bDIAM = np.sqrt(BOUNDS[1:]*BOUNDS[:-1])

# READ: AEROSOL BASE CONFIGURATIONS
# ==================================================================
path = './data/d.CONFIG-AERO.BASE.xlsx'

OPTIONS = {}
OPTIONS['na_filter'] = False

df_BASE = pd.read_excel(path, **OPTIONS)

# READ: 2D-VBS CONFIGURATIONS
# ==================================================================
df1 = pd.read_excel('./cache/d.2D-VBS.PPP.xlsx')
df2 = pd.read_excel('./cache/d.2D-VBS.SSS.xlsx')

df_2DVBS = pd.concat([df1,df2], axis=0, ignore_index=True)

# INTEGRATE 2D-VBS CONFIGURATIONS:
# ==================================================================
# SELECT ROWS BASED ON VOLATILITY:
dex = df_2DVBS['CSAT'] < THRESH
df_2DVBS = df_2DVBS.loc[dex].copy(deep=True)

# SELECT COLUMNS AND RENAME:
df_2DVBS = df_2DVBS[['INDEX','NAME','MW','CSAT']]
df_2DVBS = df_2DVBS.rename(columns={'NAME':'AERO-SPECIES'})

# INSERT NEW COLUMNS:
df_2DVBS['NOTE'] = '--'
df_2DVBS['RHO'] = 1400.
df_2DVBS['if-WET'] = 0
df_2DVBS['if-SORB'] = 1
df_2DVBS['if-KNT-OA'] = 1
df_2DVBS['if-ELECTROLYTE'] = 0
df_2DVBS['if-CLOUD-ACT'] = 0

# INSERT NEW COLUMNS:
df_2DVBS['EMISS-SOURCE-BB'] = 'None'
df_2DVBS['EMISS-SOURCE-AA'] = 'None'

for dex in df_2DVBS.index:
    
    name = df_2DVBS.at[dex,'AERO-SPECIES']
    if name[0] == 'S': continue

    df_2DVBS.at[dex,'EMISS-SOURCE-BB'] = '(EBU_%s, M06)'%(name)
    df_2DVBS.at[dex,'EMISS-SOURCE-AA'] = '(E_%s, M06)'%(name)

# INSERT NEW COLUMNS:
df_2DVBS['i-EMISS-PHASE'] = 1

# COMBINE DATAFRAMES:
df_BASE = pd.concat([df_BASE,df_2DVBS], axis=0, ignore_index=True)

# UPDATE INDEX:
df_BASE['INDEX'] = np.arange(df_BASE.shape[0])

# SAVE DATAFRAME:
# ==================================================================
# SET PATH:
path = './cache/d.CONFIG-AERO.BASE+2DVBS.xlsx' 

# COPY TEMPLATE FILE:
# - SET SOURCE AND TARGET FILES:
#ff0 = './data/d.CONFIG-AERO.BASE.xlsx'
#ff1 = 

# - MAKE COPY:
#cmd = 'cp %s %s'%(ff0,ff1)
#pro = subprocess.Popen(cmd, shell=True)
#pro.wait()

# WRITE TO FILE:
#OPTIONS = {}
#OPTIONS['engine'] = 'openpyxl'
#OPTIONS['mode'] = 'a'
#OPTIONS['if_sheet_exists'] = 'overlay'

with pd.ExcelWriter(path) as w:
    df_BASE.to_excel(w, index=False) #, header=False, startrow=2)
