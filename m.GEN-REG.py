
import os
print('Running... %s'%os.path.basename(__file__))

import re, json, itertools
import numpy  as np
import pandas as pd

# READ DATA:
# ===============================================================
df_AERO = \
pd.read_excel('./data/d.CONFIG-AERO.BASE.xlsx', na_filter=False)

# READ DATA:
# ===============================================================
# READ: 2D-VBS PRIMARY AND SECONDARY SPECIES
df_PPP = pd.read_excel('./cache/d.2D-VBS.PPP.xlsx')
df_SSS = pd.read_excel('./cache/d.2D-VBS.SSS.xlsx')

# SELECT ROWS:
dex1 = df_PPP['if-SAPRC'] == 0
dex2 = df_SSS['if-SAPRC'] == 0

df_PPP = df_PPP.loc[dex1].copy(deep=True)
df_SSS = df_SSS.loc[dex2].copy(deep=True)

# READ: AEROSOL CONFIGURATIONS
TEXT = open('./data/d.CONFIG-AERO.BINS.json').read()
TEXT = re.sub('^\#.*$', '', TEXT, 0, re.M)

AEROFIG = json.loads(TEXT)

# GET: NUMBER OF SIZE BINS
nBINS = AEROFIG['nBINS']

# READ DATA: TEMPLATE
# ===============================================================
TEXT = open('./data/tmpl/tmpl.registry.chem','r').read()

# STATE DEFINITION: AEROSOL-PHASE BASE SPECIES
# ===============================================================
LIST1 = []; LIST2 = []

for i,idex in enumerate(df_AERO.index):
    for k in range(nBINS):
    
        # GET SPECIES NAME:
        hold = df_AERO.at[idex,'AERO-SPECIES'].lower()
        name1 = '%s_a%02i'%(hold, k+1)
        name2 = '%s_cw%02i'%(hold, k+1)

        # APPEND TO LIST:
        LIST1.append(name1)

        # APPEND TO LIST:
        if df_AERO.at[idex,'if-WET'] == 0: LIST2.append(name2)

# APPEND NUMBER SPECIES:
LIST1 += ['num_a%02i'%(i+1) for i in range(nBINS)]
LIST2 += ['num_cw%02i'%(i+1) for i in range(nBINS)]

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG\-x1\>'
xx2 = ','.join(LIST1)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG\-x2\>'
xx2 = ','.join(LIST2)

TEXT = re.sub(xx1, xx2, TEXT)

# STATE DEFINITION: GAS-PHASE SECONDARY SPECIES
# ===============================================================
# INITIALZE LINES:
LINESa = ['# GAS-PHASE 2D-VBS: SECONDARY.\n# ' + '='*80]
LINESb = []

for i,dex in enumerate(df_SSS.index):
    
    # GET SPECIES NAME:
    name = df_SSS.at[dex,'NAME']

    # DEFINITION LINE:
    LINE = [0]*11

    LINE[0] = 'state'
    LINE[1] = 'real'
    LINE[2] = name
    LINE[3] = 'ikjftb'
    LINE[4] = 'chem'
    LINE[5] = '1'
    LINE[6] = '-'
    LINE[7] = 'i0{12}rhusdf=(bdy_interp:dt)'
    LINE[8] = '"%s"'%name
    LINE[9] = '"-----"'
    LINE[10] = '"ppm"'

    LINE = ' '.join(LINE)
    
    # WRITE LINE:
    LINESa.append(LINE)

    # WRITE LINE:
    LINESb.append(name)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG1a\>'
xx2 = '\n'.join(LINESa)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG1b\>'
xx2 = ','.join(LINESb)

TEXT = re.sub(xx1, xx2, TEXT)

# STATE DEFINITION: GAS-PHASE PRIMARY SPECIES.
# ===============================================================
# INITIALZE LINES:
LINESa = ['# GAS-PHASE 2D-VBS: PRIMARY.\n# ' + '='*80]
LINESb = []

for i,dex in enumerate(df_PPP.index):
    
    # GET SPECIES NAME:
    name = df_PPP.at[dex,'NAME']

    # DEFINITION LINE:
    LINE = [0]*11

    LINE[0] = 'state'
    LINE[1] = 'real'
    LINE[2] = name
    LINE[3] = 'ikjftb'
    LINE[4] = 'chem'
    LINE[5] = '1'
    LINE[6] = '-'
    LINE[7] = 'i0{12}rhusdf=(bdy_interp:dt)'
    LINE[8] = '"%s"'%name
    LINE[9] = '"-----"'
    LINE[10] = '"ppm"'

    LINE = ' '.join(LINE)
    
    # WRITE LINE:
    LINESa.append(LINE)

    # WRITE LINE:
    LINESb.append(name)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG2a\>'
xx2 = '\n'.join(LINESa)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG2b\>'
xx2 = ','.join(LINESb)

TEXT = re.sub(xx1, xx2, TEXT)

# STATE DEFINITION: AEROSOL-PHASE SECONDARY SPECIES
# ===============================================================
# INITIALZE LINES:
LINESa = ['# PARTICLE-PHASE 2D-VBS: SECONDARY.\n# ' + '='*80]
LINESb = []

# SELECT BASED ON SATURATION CONCENTRATION:
dex = df_SSS['CSAT'] < AEROFIG['CSAT-THRESH']
df_SELECT = df_SSS.loc[dex].copy(deep=True)

for i,dex in enumerate(df_SELECT.index):
    for k in range(nBINS):

        # GET: SPECIES NAME.
        name = '%s_a%02i'%(df_SELECT.at[dex,'NAME'], k+1)

        # DEFINITION LINE:
        LINE = [0]*11

        LINE[0] = 'state'
        LINE[1] = 'real'
        LINE[2] = name
        LINE[3] = 'ikjftb'
        LINE[4] = 'chem'
        LINE[5] = '1'
        LINE[6] = '-'
        LINE[7] = 'i0{12}rhusdf=(bdy_interp:dt)'
        LINE[8] = '"%s"'%name
        LINE[9] = '"-----"'
        LINE[10] = '"ug (kg-air)-1"'

        LINE = ' '.join(LINE)

        # ADD BREAK:
        bool1 = (k == nBINS - 1)
        bool2 = (dex != df_SELECT.index[-1])

        if bool1 and bool2: LINE += '\n'

        # WRITE LINE:
        LINESa.append(LINE)

        # WRITE LINE:
        LINESb.append(name)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG3a\>'
xx2 = '\n'.join(LINESa)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG3b\>'
xx2 = ','.join(LINESb)

TEXT = re.sub(xx1, xx2, TEXT)

# STATE DEFINITION: CLOUD-PHASE SECONDARY SPECIES
# ===============================================================
# INITIALZE LINES:
LINESa = ['# CLOUD-PHASE 2D-VBS: SECONDARY.\n# ' + '='*80]
LINESb = []

# SELECT BASED ON SATURATION CONCENTRATION:
dex = df_SSS['CSAT'] < AEROFIG['CSAT-THRESH']
df_SELECT = df_SSS.loc[dex].copy(deep=True)

for i,dex in enumerate(df_SELECT.index):
    for k in range(nBINS):

        # GET: SPECIES NAME.
        name = '%s_cw%02i'%(df_SELECT.at[dex,'NAME'], k+1)

        # DEFINITION LINE:
        LINE = [0]*11

        LINE[0] = 'state'
        LINE[1] = 'real'
        LINE[2] = name
        LINE[3] = 'ikjftb'
        LINE[4] = 'chem'
        LINE[5] = '1'
        LINE[6] = '-'
        LINE[7] = 'i0{12}rhusdf=(bdy_interp:dt)'
        LINE[8] = '"%s"'%name
        LINE[9] = '"-----"'
        LINE[10] = '"ug (kg-air)-1"'

        LINE = ' '.join(LINE)

        # ADD BREAK:
        bool1 = (k == nBINS - 1)
        bool2 = (dex != df_SELECT.index[-1])

        if bool1 and bool2: LINE += '\n'

        # WRITE LINE:
        LINESa.append(LINE)

        # WRITE LINE:
        LINESb.append(name)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG3a-cw\>'
xx2 = '\n'.join(LINESa)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG3b-cw\>'
xx2 = ','.join(LINESb)

#TEXT = re.sub(xx1, xx2, TEXT)

# STATE DEFINITION: PARTICLE-PHASE PRIMARY SPECIES.
# ===============================================================
# INITIALZE LINES:
LINESa = ['# PARTICLE-PHASE 2D-VBS: PRIMARY\n# ' + '='*80]
LINESb = []

# SELECT BASED ON SATURATION CONCENTRATION:
dex = df_PPP['CSAT'] < AEROFIG['CSAT-THRESH']
df_SELECT = df_PPP.loc[dex].copy(deep=True)

for i,dex in enumerate(df_SELECT.index):
    for k in range(nBINS):

        # GET: SPECIES NAME.
        name = '%s_a%02i'%(df_SELECT.at[dex,'NAME'], k+1)

        # DEFINITION LINE:
        LINE = [0]*11

        LINE[0] = 'state'
        LINE[1] = 'real'
        LINE[2] = name
        LINE[3] = 'ikjftb'
        LINE[4] = 'chem'
        LINE[5] = '1'
        LINE[6] = '-'
        LINE[7] = 'i0{12}rhusdf=(bdy_interp:dt)'
        LINE[8] = '"%s"'%name
        LINE[9] = '"-----"'
        LINE[10] = '"ug (kg-air)-1"'

        LINE = ' '.join(LINE)
    
        # ADD BREAK:
        bool1 = (k == nBINS - 1)
        bool2 = (dex != df_SELECT.index[-1])

        if bool1 and bool2: LINE += '\n'

        # WRITE LINE:
        LINESa.append(LINE)

        # WRITE LINE:
        LINESb.append(name)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG4a\>'
xx2 = '\n'.join(LINESa)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG4b\>'
xx2 = ','.join(LINESb)

TEXT = re.sub(xx1, xx2, TEXT)

# STATE DEFINITION: CLOUD-PHASE PRIMARY SPECIES.
# ===============================================================
# INITIALZE LINES:
LINESa = ['# CLOUD-PHASE 2D-VBS: PRIMARY\n# ' + '='*80]
LINESb = []

# SELECT BASED ON SATURATION CONCENTRATION:
dex = df_PPP['CSAT'] < AEROFIG['CSAT-THRESH']
df_SELECT = df_PPP.loc[dex].copy(deep=True)

for i,dex in enumerate(df_SELECT.index):
    for k in range(nBINS):

        # GET: SPECIES NAME.
        name = '%s_cw%02i'%(df_SELECT.at[dex,'NAME'], k+1)

        # DEFINITION LINE:
        LINE = [0]*11

        LINE[0] = 'state'
        LINE[1] = 'real'
        LINE[2] = name
        LINE[3] = 'ikjftb'
        LINE[4] = 'chem'
        LINE[5] = '1'
        LINE[6] = '-'
        LINE[7] = 'i0{12}rhusdf=(bdy_interp:dt)'
        LINE[8] = '"%s"'%name
        LINE[9] = '"-----"'
        LINE[10] = '"ug (kg-air)-1"'

        LINE = ' '.join(LINE)
    
        # ADD BREAK:
        bool1 = (k == nBINS - 1)
        bool2 = (dex != df_SELECT.index[-1])

        if bool1 and bool2: LINE += '\n'

        # WRITE LINE:
        LINESa.append(LINE)

        # WRITE LINE:
        LINESb.append(name)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG4a-cw\>'
xx2 = '\n'.join(LINESa)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG4b-cw\>'
xx2 = ','.join(LINESb)

#TEXT = re.sub(xx1, xx2, TEXT)

# STATE DEFINITION: PRIMARY SPECIES EMISSIONS (EBU-IN).
# ===============================================================
# INITIALZE LINES:
LINESa = ['# EMISSIONS: 2D-VBS GRIDS (EBU-IN).\n# ' + '='*80]
LINESb = []

for i,dex in enumerate(df_PPP.index):
    
    # GET SPECIES NAME:
    name = 'ebu_in_%s'%(df_PPP.at[dex,'NAME'])

    # DEFINE LINE:
    LINE = [0]*11

    LINE[0] = 'state'
    LINE[1] = 'real'
    LINE[2] = name
    LINE[3] = 'i]jf'
    LINE[4] = 'ebu_in'
    LINE[5] = '1'
    LINE[6] = '-'
    LINE[7] = 'i07r'
    LINE[8] = '"%s"'%name
    LINE[9] = '"-----"'
    LINE[10] = '"mol km-2 hr-1"'

    LINE = ' '.join(LINE)

    # WRITE LINE:
    LINESa.append(LINE)

    # WRITE LINE:
    LINESb.append(name)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG5a\>'
xx2 = '\n'.join(LINESa)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG5b\>'
xx2 = ','.join(LINESb)

TEXT = re.sub(xx1, xx2, TEXT)

# STATE DEFINITION: PRIMARY SPECIES EMISSIONS (EBU).
# ===============================================================
# INITIALZE LINES:
LINESa = ['# EMISSIONS: 2D-VBS GRIDS (EBU).\n# ' + '='*80]
LINESb = []

for i,dex in enumerate(df_PPP.index):
    
    # GET SPECIES NAME:
    name = 'ebu_%s'%(df_PPP.at[dex,'NAME'])

    # DEFINE LINE:
    LINE = [0]*11

    LINE[0] = 'state'
    LINE[1] = 'real'
    LINE[2] = name
    LINE[3] = 'ikjf'
    LINE[4] = 'ebu'
    LINE[5] = '1'
    LINE[6] = 'Z'
    LINE[7] = 'rh'
    LINE[8] = '"%s"'%name
    LINE[9] = '"-----"'
    LINE[10] = '"mol km-2 hr-1"'

    LINE = ' '.join(LINE)

    # WRITE LINE:
    LINESa.append(LINE)

    # WRITE LINE:
    LINESb.append(name)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG6a\>'
xx2 = '\n'.join(LINESa)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG6b\>'
xx2 = ','.join(LINESb)

TEXT = re.sub(xx1, xx2, TEXT)

# STATE DEFINITION: PRIMARY SPECIES EMISSIONS (E; ANTHROPOGENIC).
# ===============================================================
# INITIALZE LINES:
LINESa = ['# EMISSIONS: 2D-VBS GRIDS (E; ANTHROPOGENIC).\n# ' + '='*80]
LINESb = []

for i,dex in enumerate(df_PPP.index):
    
    # GET SPECIES NAME:
    name = 'e_%s'%(df_PPP.at[dex,'NAME'])

    # DEFINE LINE:
    LINE = [0]*11

    LINE[0] = 'state'
    LINE[1] = 'real'
    LINE[2] = name
    LINE[3] = 'i+jf'
    LINE[4] = 'emis_ant'
    LINE[5] = '1'
    LINE[6] = 'Z'
    LINE[7] = 'i5rh'
    LINE[8] = '"%s"'%name
    LINE[9] = '"-----"'
    LINE[10] = '"mol km-2 hr-1"'

    LINE = ' '.join(LINE)

    # WRITE LINE:
    LINESa.append(LINE)

    # WRITE LINE:
    LINESb.append(name)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG7a\>'
xx2 = '\n'.join(LINESa)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE TO OUTPUT TEXT:
xx1 = '\<FLAG7b\>'
xx2 = ','.join(LINESb)

TEXT = re.sub(xx1, xx2, TEXT)

# WRITE OUTPUT:
# ===============================================================
open('./gen/registry.chem','w').write(TEXT)
