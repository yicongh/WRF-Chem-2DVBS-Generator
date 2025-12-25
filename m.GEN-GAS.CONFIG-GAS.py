
import os
print('Running... %s'%os.path.basename(__file__))

import re, json
import numpy  as np
import pandas as pd

# READ: GAS-PHASE SPECIES CONFIGURATION FILE
# ==================================================================
OPTIONS = {}
OPTIONS['skiprows'] = [1]
OPTIONS['keep_default_na'] = False
#OPTIONS['engine'] = 'xlrd'

df_BASE = \
pd.read_excel('./cache/d.CONFIG-GAS.BASE+2DVBS.xlsx', **OPTIONS)

# NUMBER OF AREOSOL SPECIES:
nCOMP = df_BASE.shape[0]

# READ: TEMPLATE
# ==================================================================
TEXT = open('./data/tmpl/tmpl.mod.CONFIG-GAS.f90','r').read()

# WRITE: nCOMP
# ==================================================================
xx1 = 'nCOMP\ *\=.+$'
xx2 = 'nCOMP = %i'%nCOMP

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: PSEUDO-DATAFRAME (AERO)
# ==================================================================
for i,dex in enumerate(df_BASE.index):

    # WRITE COLUMN: SPECIES NAME
    hold = df_BASE.at[dex,'SPECIES']
    line1 = 'df_GAS_COL01(%04i) = \'%s\''%(i+1,hold)

    # WRITE COLUMN: CHEM-INDEX
    hold = df_BASE.at[dex,'SPECIES'].upper()
    line2 = 'df_GAS_COL02(%04i) = p_%s'%(i+1,hold)

    # WRITE COLUMN: HENRY'S LAW CONSTANT
    hold1 = df_BASE.at[dex,'H-STAR']
    hold2 = df_BASE.at[dex,'SPECIES'].upper()
    line3 = 'df_GAS_COL03(%04i) = %.2e ! << %s'%(i+1,hold1,hold2)

    # WRITE COLUMN: ENTHALPY OVER GAS CONSTANT
    hold1 = df_BASE.at[dex,'z-TEMP']
    hold2 = df_BASE.at[dex,'SPECIES'].upper()
    line4 = 'df_GAS_COL04(%04i) = %.2e ! << %s'%(i+1,hold1,hold2)

    # WRITE COLUMN: SURFACE REACTIVITY
    hold1 = df_BASE.at[dex,'f0']
    hold2 = df_BASE.at[dex,'SPECIES'].upper()
    line5 = 'df_GAS_COL05(%04i) = %.2e ! << %s'%(i+1,hold1,hold2)

    # WRITE COLUMN: BROWNIAN DIFFUSIVITY
    hold1 = df_BASE.at[dex,'DIFFg']
    hold2 = df_BASE.at[dex,'SPECIES'].upper()
    line6 = 'df_GAS_COL06(%04i) = %.2e ! << %s'%(i+1,hold1,hold2)

    # INITIALZE:
    if i == 0: LIST1 = []
    if i == 0: LIST2 = []
    if i == 0: LIST3 = []
    if i == 0: LIST4 = []
    if i == 0: LIST5 = []
    if i == 0: LIST6 = []

    # APPEND:
    LIST1.append(' '*9 + line1)
    LIST2.append(' '*9 + line2)
    LIST3.append(' '*9 + line3)
    LIST4.append(' '*9 + line4)
    LIST5.append(' '*9 + line5)
    LIST6.append(' '*9 + line6)

# WRITE TO TEXT: SPECIES NAME
xx1 = '^.*\<df_GAS_COL01\>.*$'
xx2 = '\n'.join(LIST1)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: CHEM-INDEX
xx1 = '^.*\<df_GAS_COL02\>.*$'
xx2 = '\n'.join(LIST2)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: HENRY'S LAW CONSTANT
xx1 = '^.*\<df_GAS_COL03\>.*$'
xx2 = '\n'.join(LIST3)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: ENTHALPY OVER GAS CONSTANT
xx1 = '^.*\<df_GAS_COL04\>.*$'
xx2 = '\n'.join(LIST4)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: SURFACE REACTIVITY
xx1 = '^.*\<df_GAS_COL05\>.*$'
xx2 = '\n'.join(LIST5)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: BROWNIAN DIFFUSIVITY
xx1 = '^.*\<df_GAS_COL06\>.*$'
xx2 = '\n'.join(LIST6)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE FILE:
# ==================================================================
open('./gen/mod.CONFIG-GAS.f90','w').write(TEXT)
