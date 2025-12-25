
import os
print('Running... %s'%os.path.basename(__file__))

import re, json
import numpy  as np
import pandas as pd

# READ: 2D-VBS PRIMARY SPECIES
# ==================================================================
df_PPP = pd.read_excel('./cache/d.2D-VBS.PPP.xlsx')

# SELECT ROWS:
dex = df_PPP['if-SAPRC'] == 0
df_PPP = df_PPP.loc[dex].copy(deep=True)

# WRITE LINES:
# ==================================================================
for i,dex in enumerate(df_PPP.index):

    # GET: SPECIES NAME
    name = df_PPP.at[dex,'NAME']

    # SET: LINE
    line = 'ebu(i,kts,j,p_ebu_%s) = '%(name)
    line += 'ebu_in(i,1,j,p_ebu_in_%s)'%(name)

    # INITIALIZE:
    if i == 0: LIST = []

    # ADD TO LIST:
    LIST.append(' '*15 + line)

# READ: TEMPLATE
# ==================================================================
TEXT = open('./data/tmpl/tmpl.module_plumerise1.F','r').read()

# WRITE TO FILE:
# ==================================================================
xx1 = '^.*\<FLAG\>.*$'
xx2 = '\n'.join(LIST)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE FILE:
# ==================================================================
open('./gen/module_plumerise1.F','w').write(TEXT)
