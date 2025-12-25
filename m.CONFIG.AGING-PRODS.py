
import os
print('Running... %s'%os.path.basename(__file__))

import re, pickle
import numpy  as np
import pandas as pd

from functions.f_GET_FUNCPROD import f_GET_FUNCPROD
from functions.f_GET_FRAGPROD import f_GET_FRAGPROD

# READ DATA: 2D-VBS SPECIES
# ========================================================
# READ: SECONDARY SPECIES
df1 = pd.read_excel('./cache/d.2D-VBS.SSS.xlsx')

# READ: PRIMARY SPECIES
df2 = pd.read_excel('./cache/d.2D-VBS.PPP.xlsx')

# JOIN DATAFRAMES:
df_ALL = pd.concat([df1,df2], axis=0, ignore_index=False)

# MERGE PRODUCT COORDINATES:
# ========================================================
# READ: 2D-VBS CONFIGURATIONS
df_CONFIG = pd.read_excel('./data/d.CONFIG-2DVBS.xlsx')

# SELECT ACTIVE ROWS:
dex = df_CONFIG['if-ACTIVE'] == 1

df_CONFIG = df_CONFIG.loc[dex].copy(deep=True)
df_CONFIG = df_CONFIG.reset_index()

# MERGE DATAFRAME:
OPTIONS = {}
OPTIONS['how'] = 'left'
OPTIONS['on'] = 'CLASS'

hold = df_CONFIG[['CLASS','s-XXX','s-YYY']]
df_ALL = df_ALL.merge(hold, **OPTIONS)

# READ DATA: EXPLICIT PRECURSORS
# ========================================================
df_PREC = pd.read_excel('./data/d.LIST-GEN1X.xlsx')

# GET OXIDATION PRODUCTS: FROM 2D-VBS PARAMETERS
# ========================================================
# SET COLUMN: PATH TO FILE
df_ALL['PATH-PRODS'] = ['--']*np.shape(df_ALL)[0]

for i,idex in enumerate(df_ALL.index):
    
    # GET ROW AS DICTIONARY:
    pp = df_ALL.iloc[idex].to_dict()
    pp.update(eval(pp['PARAMS']))

    # GET OXIDATION PRODUCTS:
    # ====================================================
    # FIND: FUNCTIONALIZATION
    df_PROD1 = f_GET_FUNCPROD(pp['NAME'], pp['CSAT'], pp['O2C'], \
    pp['dLVP'], pp['pO1'], pp['pO2'], pp['pO3'], pp['pO4'], pp['nCCC'])

    # FIND: FRAGMENTATION
    df_PROD2 = f_GET_FRAGPROD(pp['CSAT'], pp['O2C'])

    # COMBINE THE PRODUCT LISTS:
    # ====================================================
    # GET: FRAG. AND FUNC. PROBABILITIES
    pFRAG = min((pp['xFRAG']*pp['O2C'])**(pp['mFRAG']), 1.0)
    pFUNC = 1.0 - pFRAG

    # UPDATE PRODUCT YIELDS:
    df_PROD1['YIELD'] = df_PROD1['YIELD']*pFUNC
    df_PROD2['YIELD'] = df_PROD2['YIELD']*pFRAG

    # COMBINE DATA FRAMES:
    df_PROD = pd.concat([df_PROD1,df_PROD2], axis=0, ignore_index=True)
    
    # SAVE PRODUCTS TO FILE:
    # ====================================================
    path = './cache/d.GEN1-PRODS/'
    path += 'd.GEN1-PRODS.%s.xlsx'%(pp['NAME'])

    with pd.ExcelWriter(path) as ff:
        df_PROD.to_excel(ff, index=False)

    # APPEND PATH TO DATAFRAME:
    df_ALL.at[idex,'PATH-PRODS'] = path

# GET OXIDATION PRODUCTS: EXPLICIT PRESCRIPTION
# ========================================================
for i,idex in enumerate(df_ALL.index):

    # GET ROW AS DICTIONARY:
    pp = df_ALL.iloc[idex].to_dict()

    # SKIP IF NO EXPLICIT PRESCRIPTION:
    if not any(pp['CLASS'] == df_PREC['CLASS']): continue

    # SKIP IF SECONDARY SPECIES:
    if re.search('^S\d\d', pp['NAME']): continue

    # SELECT TABLE:
    dex = df_PREC['CLASS'] == pp['CLASS']
    df_CLASS = df_PREC.loc[dex].copy(deep=True)

    # CALCULATE DISTANCE TO PRECURSOR:
    df_CLASS['DIST'] = np.sqrt( \
    (df_CLASS['XX'] - pp['CSAT'])**2.0 + \
    (df_CLASS['YY'] - pp['O2C'])**2.0 )

    df_CLASS['DIST-INV'] = np.divide(1.0, np.maximum(df_CLASS['DIST'], 1e-10))

    # CALCULATE WEIGHTS:
    df_CLASS['WEIGHT'] = \
    df_CLASS['DIST-INV']/df_CLASS['DIST-INV'].sum()

    # GET PRODUCT LIST:
    # ====================================================
    LIST = []

    for j,jdex in enumerate(df_CLASS.index):

        path = df_CLASS.at[jdex,'path-GEN1-KERNEL']
        df = pd.read_excel(path)

        df['YIELDc'] = \
        df['YIELDc']*df_CLASS.at[jdex,'WEIGHT']

        df['CSAT'] = \
        df['CSAT'] - df_CLASS.at[jdex,'XX'] + pp['CSAT']

        df['O2C'] = \
        df['O2C'] - df_CLASS.at[jdex,'YY'] + pp['O2C']

        LIST.append(df)

    df_PROD = pd.concat(LIST, axis=0, ignore_index=True)
    df_PROD = df_PROD.rename(columns={'YIELDc': 'YIELD'})
    df_PROD = df_PROD.rename(columns={'CSAT': 'XX'})
    df_PROD = df_PROD.rename(columns={'O2C': 'YY'})

    # SAVE PRODUCTS TO FILE:
    # ====================================================
    path = './cache/d.GEN1-PRODS/'
    path += 'd.GEN1-PRODS.%s.xlsx'%(pp['NAME'])

    with pd.ExcelWriter(path) as ff:
        df_PROD.to_excel(ff, index=False)

    # APPEND PATH TO DATAFRAME:
    df_ALL.at[idex,'PATH-PRODS'] = path    

# SAVE UPDATED DATAFRAME:
# ========================================================
path = './cache/d.2DVBS.AGING-PRODS.xlsx'

with pd.ExcelWriter(path) as ff:
    df_ALL.to_excel(ff, index=False)
