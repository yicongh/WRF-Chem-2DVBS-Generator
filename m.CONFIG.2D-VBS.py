
import itertools
import numpy  as np
import pandas as pd

# READ: 2D-VBS CONFIGURATIONS.
# ===============================================================
df_CONFIG = pd.read_excel('./data/d.CONFIG-2DVBS.xlsx')

# SELECT ACTIVE ROWS:
dex = df_CONFIG['if-ACTIVE'] == 1

df_CONFIG = df_CONFIG.loc[dex].copy(deep=True)
df_CONFIG = df_CONFIG.reset_index()

# SET DATAFRAME FOR PRIMARY AND SECONDARY SPECIES:
# ===============================================================
# LIST OF SPECIES: GRID AND PRECURSOR SPECIES.
cols = ['INDEX','NAME','CLASS','CSAT','O2C','MW','nCCC']
cols +=['kOH','PARAMS','if-SAPRC']

df_OUT1 = pd.DataFrame(columns=cols)
df_OUT2 = pd.DataFrame(columns=cols)

# LOOP OVER 2D-VBS GRIDS:
for i,dex in enumerate(df_CONFIG.index):
    
    # GET CLASS NAME:
    CLASS = df_CONFIG.at[dex,'CLASS']

    # PREFIX FOR PRIMARY AND SECONDARY SPECIES:
    PREFIX1 = CLASS.replace('C','P')
    PREFIX2 = CLASS.replace('C','S')

    # SET: SECONDARY SPECIES.
    # ===========================================================
    # GET X AND Y DIMENSIONS:
    hold1 = eval(df_CONFIG.at[dex,'s-XXX'])
    hold2 = eval(df_CONFIG.at[dex,'s-YYY'])

    XXX = np.array(hold1, dtype=np.float64)
    YYY = np.array(hold2, dtype=np.float64)

    # GET AGING PARAMETERS:
    PARAMS = df_CONFIG.at[dex,'PARAMS']

    # LOOP OVER GRID POINTS:
    LIST = list(itertools.product(XXX,YYY))

    for j,(xx,yy) in enumerate(LIST):

        # GET SPECIES NAME:
        if xx >= 0:
            name = '%sX%02iY%02i'%(PREFIX2,abs(xx),yy*10)
        else:
            name = '%sU%02iY%02i'%(PREFIX2,abs(xx),yy*10) 

        # GET VOLATILITY AND O:C RATIO:
        CSAT = xx; O2C = yy

        # GET kOH:
        kOH = eval(PARAMS)['kOH-AGE']

        # GET MAXIMUM CARBON NUMBER:
        CMAX = eval(PARAMS)['nCCC']

        # GET CARBON NUMBER:
        nC0 = 25.; bC = 0.5; bO = 2.3; bCO = -0.3
        nCCC = (nC0*bC - CSAT)/(bC + bO*O2C + 2*O2C*bCO/(1 + O2C))
        nCCC = max(nCCC, 1.0)
        #nCCC = min(nCCC, CMAX)

        # GET MOLECULAR WEIGHT [g mol-1]:
        MW = nCCC*12. + nCCC*O2C*16.

        # GET DENSITY [kg m-3]:
        RHO = 1400.0

        # APPEND:
        hold = [i*len(LIST)+j, name, CLASS, CSAT]
        hold+= [O2C, MW, nCCC, kOH, PARAMS, 0]
        hold = tuple(hold)

        df_ADD = pd.DataFrame([hold], columns=cols)
    
        df_OUT1 = \
        pd.concat([df_OUT1,df_ADD], axis=0, ignore_index=True)

    # SET: PRIMARY SPECIES.
    # ===========================================================
    # GET X AND Y DIMENSIONS:
    hold1 = eval(df_CONFIG.at[dex,'p-XXX'])
    hold2 = eval(df_CONFIG.at[dex,'p-YYY'])

    XXX = np.array(hold1, dtype=np.float64)
    YYY = np.array(hold2, dtype=np.float64)

    # SKIP IF DIMENSION NOT DEFINED:
    if any([np.isnan(ii) for ii in XXX]): continue
    if any([np.isnan(ii) for ii in YYY]): continue

    # GET AGING PARAMETERS:
    PARAMS = df_CONFIG.at[dex,'PARAMS']

    # LOOP OVER GRID POINTS:
    LIST = list(itertools.product(XXX,YYY))

    for j,(xx,yy) in enumerate(LIST):

        # GET SPECIES NAME:
        if xx >= 0:
            name = '%sX%02iY%02i'%(PREFIX1,abs(xx),yy*10)
        else:
            name = '%sU%02iY%02i'%(PREFIX1,abs(xx),yy*10) 

        # GET VOLATILITY AND O:C RATIO:
        CSAT = xx; O2C = yy

        # GET kOH:
        #kOH = eval(PARAMS)['kOH-AGE']

        # GET MAXIMUM CARBON NUMBER:
        CMAX = eval(PARAMS)['nCCC']

        # GET CARBON NUMBER:
        nC0 = 25.; bC = 0.5; bO = 2.3; bCO = -0.3
        nCCC = (nC0*bC - CSAT)/(bC + bO*O2C + 2*O2C*bCO/(1 + O2C))
        nCCC = max(nCCC, 1.0)
        #nCCC = min(nCCC, CMAX)

        # GET MOLECULAR WEIGHT [g mol-1]:
        MW = nCCC*12. + nCCC*O2C*16.

        # GET DENSITY [kg m-3]:
        RHO = 1400.0

        # GET kOH:
        kOH0 = df_CONFIG.at[dex,'kOH-PRIME-BASE']

        kOH = kOH0*(nCCC + 9.*(nCCC*O2C) - 10.*(O2C**2.))

        # APPEND:
        hold = [i*len(LIST)+j, name, CLASS, CSAT]
        hold+= [O2C, MW, nCCC, kOH, PARAMS, 0]
        hold = tuple(hold)
        
        df_ADD = pd.DataFrame([hold], columns=cols)
    
        df_OUT2 = \
        pd.concat([df_OUT2,df_ADD], axis=0, ignore_index=True)

# MERGE PRIMARY SPECIES FROM SAPRC WITH AGING PARAMETERS:
# ===============================================================
# READ DATA:
df_PREC0 = pd.read_excel('./data/d.PRECS-SAPRC.xlsx')

# MERGE AGING PARAMETERS:
df_HOLD = df_CONFIG[['CLASS','PARAMS']]
#df_PREC0 = df_PREC0.merge(df_HOLD, how='left', on='CLASS') 
df_PREC0 = df_PREC0.merge(df_HOLD, how='inner', on='CLASS') 

# ADD NEW COLUMN:
df_PREC0['if-SAPRC'] = 1

# DROP EXTRA COLUMN:
df_PREC0 = df_PREC0.drop(columns=['NOTE'])

# MERGE WITH DATAFRAME FOR PRIMARY SPECIES:
df_OUT2 = pd.concat([df_OUT2,df_PREC0], axis=0, ignore_index=False)

# SAVE DATA:
# ===============================================================
path1 = './cache/d.2D-VBS.SSS.xlsx'
path2 = './cache/d.2D-VBS.PPP.xlsx'

with pd.ExcelWriter(path1) as ff:
    df_OUT1.to_excel(ff, index=False)

with pd.ExcelWriter(path2) as ff:
    df_OUT2.to_excel(ff, index=False)

