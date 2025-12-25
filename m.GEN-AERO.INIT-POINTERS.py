
import os
print('Running... %s'%os.path.basename(__file__))

import re, json
import itertools
import numpy  as np
import pandas as pd

# READ DATA:
# =============================================================
# READ: LIST OF 2D-VBS SPECIES TO PARTITION.
df1 = pd.read_excel('./cache/d.2D-VBS.PPP.xlsx')
df2 = pd.read_excel('./cache/d.2D-VBS.SSS.xlsx')

df_AERO = pd.concat([df1,df2], axis=0, ignore_index=True)

# SELECT ROWS:
dex = df_AERO['if-SAPRC'] == 0
df_AERO = df_AERO.loc[dex].copy(deep=True)

# READ: AEROSOL CONFIGURATIONS.
TEXT = open('./data/d.CONFIG-AERO.BINS.json').read()
TEXT = re.sub('^\#.*$', '', TEXT, 0, re.M)

CONFIG = json.loads(TEXT)

# SELECT ROWS:
dex = df_AERO['CSAT'] < CONFIG['CSAT-THRESH']
df_AERO = df_AERO.loc[dex].copy(deep=True)

# GET: LIST OF AEROSOL SPECIES.
LIST1 = list(df_AERO['NAME'])

# GET: LIST OF SIZE BINS:
LIST2 = ['%02i'%(i+1) for i in range(CONFIG['nBINS'])]

# READ: SOURCE FILE TEMPLATE
# =============================================================
TEXT = open('./data/tmpl/tmpl.sub.INIT-POINTERS.f90','r').read()

# SET TEXT: GAS-PHASE SPECIES.
# =============================================================
for i,pp in enumerate(LIST1):

    # INITIALIZE:
    if i == 0: SUB = ''

    # SET SUB-STRING:
    sub1 = 'PTRARRAY_GAS(%02i)'%(i+1)

    # SET SUB-STRING:
    sub2 = 'p_%s'%(pp)
    
    # SET LINE:
    line = ' '*9 + sub1 + ' = ' + sub2 + '\n'

    # WRITE:
    SUB += line

# SUBSTITUTE:
TEXT = re.sub('\!\<FLAG1\>\ *\n', SUB, TEXT)

# SET TEXT: PARTICLE-PHASE SPECIES.
# =============================================================
for i,pp1 in enumerate(LIST1):
    for j,pp2 in enumerate(LIST2):

        # INITIALIZE:
        if (i == 0) and (j == 0): SUB = ''

        # SET SUB-STRING:
        sub1 = 'PTRARRAY_AERO(%02i,%02i)'%(i+1,j+1)

        # SET SUB-STRING:
        sub2 = 'p_%s_a%s'%(pp1,pp2)
    
        # SET LINE:
        line = ' '*9 + sub1 + ' = ' + sub2 + '\n'

        if (i != 0) and (j == 0): line = '\n' + line

        # WRITE:
        SUB += line                

# SUBSTITUTE:
TEXT = re.sub('\!\<FLAG2\>\ *\n', SUB, TEXT)

# SET TEXT: NUMBER SIZE BINS.
# =============================================================
for i,pp in enumerate(LIST2):

    # INITIALIZE:
    if i == 0: SUB = ''

    # SET SUB-STRING:
    sub1 = 'PTRARRAY_NUM(%02i)'%(i+1)

    # SET SUB-STRING:
    sub2 = 'p_NUM_a%s'%(pp)
    
    # SET LINE:
    line = ' '*9 + sub1 + ' = ' + sub2 + '\n'

    # WRITE:
    SUB += line

# SUBSTITUTE:
TEXT = re.sub('\!\<FLAG3\>\ *\n', SUB, TEXT)

# WRITE TO FILE:
# ======================================================================
open('./gen/sub.INIT-POINTERS.f90','w').write(TEXT)
