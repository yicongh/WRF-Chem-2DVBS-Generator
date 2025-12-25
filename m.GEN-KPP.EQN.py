
import os
print('Running... %s'%os.path.basename(__file__))

import re, pickle
import numpy  as np
import pandas as pd

from functions.f_GET_STANDPRODS import f_GET_STANDPRODS
from functions.f_CALC_CARBNUM import f_CALC_CARBNUM

# READ DATA: 2D-VBS PRIMARY AND SECONDARY SPECIES
# ====================================================
df_ALL = pd.read_excel('./cache/d.2DVBS.AGING-PRODS.xlsx')

# WRITE EQUATIONS:
# ====================================================
# INITIALIZE LINE:
LINES = ''

# START COUNTER:
nSTART = 300

for i,idex in enumerate(df_ALL.index):

    # GET ROW AS DICTIONARY:
    # ================================================
    pp = df_ALL.iloc[idex].to_dict()
    
    # FIND STANDARD PRODCUTS:
    # ================================================
    # GET: AGING PRODUCTS
    df_PRODS = pd.read_excel(pp['PATH-PRODS'])

    # GET X AND Y COORDINATES:
    XXX = np.array(eval(pp['s-XXX']))
    YYY = np.array(eval(pp['s-YYY']))

    # GET PREFIX NAME:
    PREFIX = pp['CLASS'].replace('C','S')

    # FIND PRODUCTS:
    df_STAND = f_GET_STANDPRODS(df_PRODS, XXX, YYY, PREFIX)
    
    # CONVERT TO MOLAR YILED:
    # ================================================
    # GET CARBON NUMNER:
    fun = lambda _: f_CALC_CARBNUM(_['XX'], _['YY'])

    df_STAND['nCCC'] = df_STAND.apply(fun, axis=1)

    # UPDATE YIELD:
    df_STAND['YIELD'] = \
    df_STAND['YIELD']*np.divide(pp['nCCC'], df_STAND['nCCC'])
    
    # WRITE EQUATIONS TO FILE:
    # ================================================    
    # SET: HEAD AND TAIL OF LINE
    if pp['if-SAPRC'] == 0: hold = ''
    if pp['if-SAPRC'] == 1: hold = pp['NAME'] + ' + '

    head = '{%i} %s + OH = OH + %s'%(nSTART+i,pp['NAME'],hold)
    tail = ' : %.2e ;\n\n'%(pp['kOH'])

    # SET: BODY OF LINE
    body = []; CTR = 0

    for j,jdex in enumerate(df_STAND.index):

        new = ''
        if CTR%5 == 0: new = '\n'

        hold1 = df_STAND.at[jdex,'XX']
        hold2 = df_STAND.at[jdex,'YY']

        hold1 = df_STAND.at[jdex,'YIELD']
        hold2 = df_STAND.at[jdex,'NAME']

        add = new + '%.3f'%(hold1) + hold2
        
        body.append(add); CTR = CTR + 1
        
    # COMBINE HEAD, BODY AND TAIL:
    line = head + ' + '.join(body) + tail
                
    # WRITE LINE TO FILE:
    LINES += line
    
# WRITE TO FILE:
# ====================================================
# READ TEMPLATE:
hold = './data/tmpl/tmpl.saprc99_mosaic_4bin_2dvbs.eqn'
TEXT0 = open(hold, 'r').read()

# WRITE LINE:
xx1 = '\<FLAG1\>'
xx2 = LINES

TEXT1 = re.sub(xx1, xx2, TEXT0)

# WRITE:
hold = './gen/saprc99_mosaic_4bin_2dvbs.eqn'
open(hold, 'w').write(TEXT1)


