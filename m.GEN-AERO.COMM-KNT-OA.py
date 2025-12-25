
import os
print('Running... %s'%os.path.basename(__file__))

import re, json
import numpy  as np
import pandas as pd

# READ DATA:
# =============================================================
# READ: LIST OF 2D-VBS PRIMARY AND SECONDARY SPECIES.
df1 = pd.read_excel('./cache/d.2D-VBS.PPP.xlsx')
df2 = pd.read_excel('./cache/d.2D-VBS.SSS.xlsx')

df_AERO = pd.concat([df1,df2], axis=0, ignore_index=True)

# SELECT ROWS:
dex = df_AERO['if-SAPRC'] == 0
df_AERO = df_AERO.loc[dex].copy(deep=True)

# READ: CONFIGURATION FOR AEROSOLS.
TEXT = open('./data/d.CONFIG-AERO.BINS.json').read()
TEXT = re.sub('^\#.*$', '', TEXT, 0, re.M)

AEROFIG = json.loads(TEXT)

# SELECT ROWS BASED ON VOLATILITY:
dex = df_AERO['CSAT'] < AEROFIG['CSAT-THRESH']
df_AERO = df_AERO.loc[dex].copy(deep=True)

# GET: NUMBER OF PARTITIONING SPECIES.
nCOMP = df_AERO.shape[0]

# READ: TEMPLATE
# =============================================================
TEXT = open('./data/tmpl/tmpl.comm.KNT-OA.f90', 'r').read()

# MODIFY TEXT: nCOMP.
# =============================================================
xx0 = 'nCOMP\ *\=.*$'
xx1 = 'nCOMP = %i'%(nCOMP)

TEXT = re.sub(xx0, xx1, TEXT, 0, re.M)

# MODIFY TEXT: CSAT.
# =============================================================
# TEXT ARRAY:
CSAT = ['%.1e'%(10.**i) for i in df_AERO['CSAT']]
CSAT = [i.rjust(10,' ') for i in CSAT]

# SPLIT ARRAY INTO A NUMBER OF GROUPS:
CSAT = np.array_split(CSAT, len(CSAT) // 5 + 1)

# SET REPLACEMENT TEXT:
SUB = 'CSAT = (/ & \n'

for i,line in enumerate(CSAT):

    add = ' '*3 + ','.join(line)

    if i != len(CSAT) - 1:
        add = add + ', &\n'
    else:
        add = add + ' /)'

    SUB = SUB + add

# REPLACE:
xx0 = 'CSAT\ *\=[^\)]*\)[^\n]*$'
xx1 = SUB

TEXT = re.sub(xx0, xx1, TEXT, 0, re.DOTALL|re.M)

# MODIFY TEXT: MW.
# =============================================================
# TEXT ARRAY:
MW = ['%.2f'%(i) for i in df_AERO['MW']]
MW = [i.rjust(10,' ') for i in MW]

# SPLIT ARRAY INTO A NUMBER OF GROUPS:
MW = np.array_split(MW, len(MW) // 5 + 1)

# SET REPLACE TEXT:
SUB = 'MW_ARRAY = (/ & \n'

for i,line in enumerate(MW):

    add = ' '*2 + ','.join(line)

    if i != len(MW) - 1:
        add = add + ', &\n'
    else:
        add = add + ' /)'

    SUB = SUB + add

# REPLACE:
xx0 = 'MW_ARRAY\ *\=[^\)]*\)[^\n]*$'
xx1 = SUB

TEXT = re.sub(xx0, xx1, TEXT, 0, re.DOTALL|re.M)

# WRITE OUTPUT:
# =============================================================
open('./gen/comm.KNT-OA.f90', 'w').write(TEXT)
