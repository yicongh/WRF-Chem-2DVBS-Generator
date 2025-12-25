
import os
print('Running... %s'%os.path.basename(__file__))

import re, pickle
import numpy  as np
import pandas as pd

from functions.f_GET_STANDPRODS import f_GET_STANDPRODS

# READ DATA: 2D-VBS SPECIES
# ====================================================
# READ: SECONDARY SPECIES
df1 = pd.read_excel('./cache/d.2D-VBS.SSS.xlsx')

# READ: PRIMARY SPECIES
df2 = pd.read_excel('./cache/d.2D-VBS.PPP.xlsx')

# JOIN DATAFRAMES:
df_OUT = pd.concat([df1,df2], axis=0, ignore_index=True)

# SELECT ROWS:
dex = df_OUT['if-SAPRC'] == 0
df_OUT = df_OUT.loc[dex].copy(deep=True)

# WRITE LINES:
# ====================================================
for i,dex in enumerate(df_OUT.index):

    # GET SPECIES NAME:
    name = df_OUT.at[dex,'NAME']

    # WRITE LINE:
    # - SET LINE:
    line = ' %s = IGNORE; \n'%(name)

    # - INITIALIZE:
    if i == 0: LINES = ''

    # - WRITE:
    LINES += line

# WRITE LINES:
# ====================================================
# READ TEMPLATE:
hold = './data/tmpl/tmpl.saprc99_mosaic_4bin_2dvbs.spc'
TEXT0 = open(hold, 'r').read()

# WRITE LINE:
xx1 = '\<FLAG1\>'
xx2 = LINES

TEXT1 = re.sub(xx1, xx2, TEXT0)

# WRITE:
hold = './gen/saprc99_mosaic_4bin_2dvbs.spc'
open(hold, 'w').write(TEXT1)




