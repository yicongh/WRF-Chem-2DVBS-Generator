
import os
print('Running... %s'%os.path.basename(__file__))

import re, json
import numpy  as np
import pandas as pd

# READ: AEROSOL CONFIGURATION FILE
# ==================================================================
df_BASE = \
pd.read_excel('./cache/d.CONFIG-AERO.BASE+2DVBS.xlsx', skiprows=[1], keep_default_na=False)

# READ: EMISSION MODE CONFIGURATIONS
# ==================================================================
# SET PATH:
path = './data/d.CONFIG-AERO.EMISS-MODES.xlsx'

# READ: MAIN SHEET AND STANDARD SIZE BINS
df_EMODE1 = pd.read_excel(path, sheet_name='main', keep_default_na=False)
df_EMODE2 = pd.read_excel(path, sheet_name='bin-size', skiprows=[1], keep_default_na=False)

# DEFINE: EMISSION TARGET-SOURCE PAIRS
# ==================================================================
# INITIALIZE:
hold = ['INDEX','AERO-SPECIES','i-EMISS-PHASE','CSAT','MW']

df1 = df_BASE[hold + ['EMISS-SOURCE-BB']].copy(deep=True)
df2 = df_BASE[hold + ['EMISS-SOURCE-AA']].copy(deep=True)

df1 = df1.rename(columns={'EMISS-SOURCE-BB':'EMISS'})
df2 = df2.rename(columns={'EMISS-SOURCE-AA':'EMISS'})

df1['LABEL'] = 'EMISS-BB'
df2['LABEL'] = 'EMISS-AA'

df_STACK = pd.concat([df1,df2], axis=0, ignore_index=True)

# INITIALIZE:
cols = ['INDEX','TARGET','SOURCE','MDIST-LABEL']
cols += ['i-EMISS-PHASE','CSAT','MW']

df_EMISSbb = pd.DataFrame(columns=cols)
df_EMISSaa = pd.DataFrame(columns=cols)

# ASSIGN VALUES: BIOMASS-BURNING EMISSIONS
for i,dex in enumerate(df_STACK.index):
    
    # GET: TARGET INDEX
    index = df_STACK.at[dex,'INDEX']

    # GET: TARGET NAME
    TARGET = df_STACK.at[dex,'AERO-SPECIES']

    # GET: TARGET EMISSION PHASE
    EMPHASE = df_STACK.at[dex,'i-EMISS-PHASE']

    # GET: TARGET VOLATILITY
    CSAT = df_STACK.at[dex,'CSAT']

    # GET: MOLECULAR WEIGHT
    MW = df_STACK.at[dex,'MW']

    # GET: EMISSION DEFINITION
    line = df_STACK.at[dex,'EMISS']
    line = re.sub('\ +', '', line)
    if line == 'None': continue
    
    # GET: EMISSION LABEL
    LABEL = df_STACK.at[dex,'LABEL']

    # LOOP OVER ITEMS IN DEFINITION:
    for ii in line.split('+'):

        # FIND PATTERNS:
        hold = '\(([\w\_]+)\,([\w\_]+)\)'
        find = re.findall(hold, ii)

        # GET: SOURCE NAME
        SOURCE = find[0][0]

        # GET: MASS DISTRIBUTION LABEL
        LABELm = find[0][1]

        # APPEND TO DATAFRAME:
        df_ADD = \
        pd.DataFrame([(index,TARGET,SOURCE,LABELm,EMPHASE,CSAT,MW)], columns=cols)

        if LABEL == 'EMISS-BB': df_EMISSbb = \
        pd.concat([df_EMISSbb,df_ADD], axis=0, ignore_index=True)

        if LABEL == 'EMISS-AA': df_EMISSaa = \
        pd.concat([df_EMISSaa,df_ADD], axis=0, ignore_index=True)

# MERGE EMISSION MODE INDEX:
OPTIONS = {}
OPTIONS['on'] = 'MDIST-LABEL'
OPTIONS['how'] = 'left'

hold = {'LABEL':'MDIST-LABEL', 'INDEX':'MDIST-INDEX'}
df_MERGE = df_EMODE1[['LABEL','INDEX']]
df_MERGE = df_MERGE.rename(columns=hold)

df_EMISSbb = df_EMISSbb.merge(df_MERGE, **OPTIONS)
df_EMISSaa = df_EMISSaa.merge(df_MERGE, **OPTIONS)

# WRITE TO FILE:
# ==================================================================
path1 = './cache/d.CONFIG-AERO.EMISS-BB.xlsx'
path2 = './cache/d.CONFIG-AERO.EMISS-AA.xlsx'

with pd.ExcelWriter(path1) as ff:
    df_EMISSbb.to_excel(ff, index=False)

with pd.ExcelWriter(path2) as ff:
    df_EMISSaa.to_excel(ff, index=False)
