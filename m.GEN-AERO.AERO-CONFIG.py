
import os
print('Running... %s'%os.path.basename(__file__))

import re, json, itertools
import numpy  as np
import pandas as pd

# AEROSOL CONFIGURATIONS:
# ==================================================================
# READ: AEROSOL CONFIGURATIONS
TEXT = open('./data/d.CONFIG-AERO.BINS.json').read()
TEXT = re.sub('^\#.*$', '', TEXT, 0, re.M)

AEROFIG = json.loads(TEXT)

# GET: NUMBER OF SIZE BINS
nBINS = AEROFIG['nBINS']

# GET: OVERALL BOUNDS [nm]
LOWER = AEROFIG['xLOWER']
UPPER = AEROFIG['xUPPER']

# GET: VOLATILITY THRESHOLD
THRESH = AEROFIG['CSAT-THRESH']

# GET: CURRENT SIZE BINS [nm]
BOUNDS = np.logspace(np.log10(LOWER), np.log10(UPPER), num=nBINS+1)
bDIAM = np.sqrt(BOUNDS[1:]*BOUNDS[:-1])

# READ: AEROSOL CONFIGURATION FILE
# ==================================================================
OPTIONS = {}
OPTIONS['keep_default_na'] = False

df_BASE = \
pd.read_excel('./cache/d.CONFIG-AERO.BASE+2DVBS.xlsx', **OPTIONS)

# NUMBER OF AREOSOL SPECIES:
nAERO = df_BASE.shape[0]

# SELECT: DATAFRAME FOR KINETICALLY-PARTITIONING ORGANICS
# ==================================================================
# FIND INDEX:
dex = df_BASE['if-KNT-OA'] == 1

# SELECT:
df_KNTOA = df_BASE.loc[dex].copy(deep=True)

# NUMBER OF SUCH SPECIES:
nKNTOA = df_KNTOA.shape[0]

# READ: EMISSION MODE CONFIGURATIONS
# ==================================================================
# SET PATH:
path = './data/d.CONFIG-AERO.EMISS-MODES.xlsx'

# READ: MAIN SHEET AND STANDARD SIZE BINS
df_EMODE1 = pd.read_excel(path, sheet_name='main')
df_EMODE2 = pd.read_excel(path, sheet_name='bin-size', skiprows=[1])

# READ: EMISSION PAIRS
# ==================================================================
df_EMISSbb = pd.read_excel('./cache/d.CONFIG-AERO.EMISS-BB.xlsx')
df_EMISSaa = pd.read_excel('./cache/d.CONFIG-AERO.EMISS-AA.xlsx')

# READ: TEMPLATE
# ==================================================================
TEXT = open('./data/tmpl/tmpl.mod.AERO-CONFIG.f90','r').read()

# WRITE: BIN SETTINGS
# ==================================================================
# WRITE: nBINS
xx1 = 'nBINS\ *\=.+$'
xx2 = 'nBINS = %i'%nBINS

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: nCOMP
xx1 = 'nCOMP\ *\=.+$'
xx2 = 'nCOMP = %i'%nAERO

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: nKNTOA
xx1 = 'nKNTOA\ *\=.+$'
xx2 = 'nKNTOA = %i'%(nKNTOA)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: LOWERx
xx1 = 'LOWERx\ *\=.+$'
xx2 = 'LOWERx = %.1f'%LOWER

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: UPPERx
xx1 = 'UPPERx\ *\=.+$'
xx2 = 'UPPERx = %.1f'%UPPER

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: EMISSION SETTINGS
# ==================================================================
# WRITE: nEMISSbb
xx1 = 'nEMISSbb\ *\=.+$'
xx2 = 'nEMISSbb = %i'%(df_EMISSbb.shape[0])

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: nEMISSaa
xx1 = 'nEMISSaa\ *\=.+$'
xx2 = 'nEMISSaa = %i'%(df_EMISSaa.shape[0])

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: EMISSION MASS MODES
# ==================================================================
# WRITE: nEMISSMODE
xx1 = 'nEMISSMODE\ *\=.+$'
xx2 = 'nEMISSMODE = %i'%(df_EMODE1.shape[0])

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: MASS MODES
for i,dex in enumerate(df_EMODE1.index):

    # GET: MASS DISTRIBUTION 
    # ON STANDARD SIZE BINS [nm]
    XXX0 = df_EMODE2['SIZE'].values
    YYY0 = df_EMODE1.loc[dex].values[3:]

    # RESAMPLE:
    XXX1 = np.concatenate([[0.], bDIAM, [5e4]], axis=0)
    YYY1 = np.zeros(len(XXX1))

    for xxx,yyy in zip(XXX0,YYY0):

        dex2 = np.where(XXX1 >= xxx)[0][0]
        dex1 = dex2 - 1

        YYY1[dex1] = \
        YYY1[dex1] + yyy*(XXX1[dex2]-xxx)/(XXX1[dex2]-XXX1[dex1])

        YYY1[dex2] = \
        YYY1[dex2] + yyy*(xxx-XXX1[dex1])/(XXX1[dex2]-XXX1[dex1])

    # WRITE LINE:
    if i == 0: LINES = []

    line = '!' + ' '*5
    line += 'EMISSION MODE: %s\n'%(df_EMODE1.at[dex,'NOTE'])
    line += 'REAL(8),DIMENSION(nBINS),PARAMETER :: '
    line += '%s = (/ &\n'%(df_EMODE1.at[dex,'LABEL'])

    for ii,yy in enumerate(YYY1[1:-1]):

        BOOL1 = (ii != len(YYY1[1:-1])-1)
        BOOL2 = (ii % 4 == 3)

        line = \
        line + '%.2e'%(yy) + ', '*BOOL1 + '&\n'*(BOOL1 and BOOL2)

    line += '/)'

    line = re.sub('\n', '\n' + ' '*6, line)
    
    # APPEND TO LINES:
    LINES.append(line)

# WRITE TO TEXT:
xx1 = '^.*\<EMISS\_MODES\>.*$'
xx2 = '\n\n'.join(LINES)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: df_AERO; INITIALIZATION 
# ==================================================================
ADD0 = [] # << AEROSOL-PHASE POINTERS
ADD1 = [] # << CLOUD-PHASE POINTERS

for i in range(nBINS):

    key0 = 'PTRaa-%02i'%(i+1)
    key1 = 'PTRcw-%02i'%(i+1)

    line0 = 'CALL df_AERO%%APPEND_EMPTYi(nCOMP, \'%s\')'%(key0)
    line1 = 'CALL df_AERO%%APPEND_EMPTYi(nCOMP, \'%s\')'%(key1)

    line0 = ' '*9 + line0
    line1 = ' '*9 + line1

    ADD0.append(line0)
    ADD1.append(line1)

ADD0 = '\n'.join(ADD0)
ADD1 = '\n'.join(ADD1)

# WRITE:
patt1 = '^.*\<df_AERO: APPEND-PTRaa>.*$'
patt2 = ADD0

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE:
patt1 = '^.*\<df_AERO: APPEND-PTRcw>.*$'
patt2 = ADD1

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_AERO; SPECIES NAMES
# ==================================================================
ADD = []

for i,idex in enumerate(df_BASE.index):

    # GET: SPECIES NAME
    name = df_BASE.at[idex,'AERO-SPECIES']

    # SET LINE:
    line = 'CALL df_AERO%%SETh(\'NAMES\', %04i, \'%s\')'%(i+1,name)
    line = ' '*9 + line

    # APPEND:
    ADD.append(line)

# WRITE:
ADD = '\n'.join(ADD)

patt1 = '^.*\<df_AERO; NAMES>.*$'
patt2 = ADD

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_AERO; MOLECULAR WEIGHT
# ==================================================================
ADD = []

for i,idex in enumerate(df_BASE.index):

    # GET: SPECIES NAME
    MW = df_BASE.at[idex,'MW']

    # SET LINE:
    line = 'CALL df_AERO%%SETr(\'MW\', %04i, %.2fd0)'%(i+1,MW)
    line = ' '*9 + line

    # APPEND:
    ADD.append(line)

# WRITE:
ADD = '\n'.join(ADD)

patt1 = '^.*\<df_AERO; MW>.*$'
patt2 = ADD

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_AERO; DENSITY
# ==================================================================
ADD = []

for i,idex in enumerate(df_BASE.index):

    # GET: SPECIES NAME
    RHO = df_BASE.at[idex,'RHO']

    # SET LINE:
    line = 'CALL df_AERO%%SETr(\'DENSITY\', %04i, %.2fd0)'%(i+1,RHO)
    line = ' '*9 + line

    # APPEND:
    ADD.append(line)

# WRITE:
ADD = '\n'.join(ADD)

patt1 = '^.*\<df_AERO; RHO>.*$'
patt2 = ADD

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_AERO; WET FLAG
# ==================================================================
ADD = []

for i,idex in enumerate(df_BASE.index):

    # GET: SPECIES NAME
    FLAG = df_BASE.at[idex,'if-WET']

    # SET LINE:
    line = 'CALL df_AERO%%SETi(\'if-WET\', %04i, %01i)'%(i+1,FLAG)
    line = ' '*9 + line

    # APPEND:
    ADD.append(line)

# WRITE:
ADD = '\n'.join(ADD)

patt1 = '^.*\<df_AERO; IF-WET>.*$'
patt2 = ADD

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_AERO; ABSORBING FLAG
# ==================================================================
ADD = []

for i,idex in enumerate(df_BASE.index):

    # GET: SPECIES NAME
    FLAG = df_BASE.at[idex,'if-SORB']

    # SET LINE:
    line = 'CALL df_AERO%%SETi(\'if-SORB\', %04i, %01i)'%(i+1,FLAG)
    line = ' '*9 + line

    # APPEND:
    ADD.append(line)

# WRITE:
ADD = '\n'.join(ADD)

patt1 = '^.*\<df_AERO; IF-SORB>.*$'
patt2 = ADD

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_AERO COLUMN={if-CLOUD-ACT} 
# ==================================================================
ADD = []

for i,idex in enumerate(df_BASE.index):

    # GET: SPECIES NAME
    FLAG = df_BASE.at[idex,'if-CLOUD-ACT']

    # SET LINE:
    line = 'CALL df_AERO%%SETi(\'if-CLOUD-ACT\', %04i, %01i)'%(i+1,FLAG)
    line = ' '*9 + line

    # APPEND:
    ADD.append(line)

# WRITE:
ADD = '\n'.join(ADD)

patt1 = '^.*\<df_AERO; IF-CLOUD-ACT>.*$'
patt2 = ADD

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_AERO; AEROSOL-PHASE POINTERS
# ==================================================================
ADD0 = []

for k in range(nBINS):

    ADD1 = []

    for i,idex in enumerate(df_BASE.index):

        # GET: SPECIES NAME
        name = df_BASE.at[idex,'AERO-SPECIES']
    
        # SET LINE:
        hold1 = 'PTRaa-%02i'%(k+1)
        hold2 = 'p_%s_a%02i'%(name, k+1)

        line = 'CALL df_AERO%%SETi(\'%s\', %04i, %s)'%(hold1,i+1,hold2)
        line = ' '*9 + line

        # APPEND:
        ADD1.append(line)
    
    ADD0.append('\n'.join(ADD1))

# WRITE:
ADD0 = '\n\n'.join(ADD0)

patt1 = '^.*\<df_AERO; PTRaa>.*$'
patt2 = ADD0

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_AERO COLUMN-{PTRcw-XX}
# ==================================================================
# SET ITERATORS:
VECT1 = np.arange(1, nBINS+1, 1)
VECT2 = df_BASE.index

# INITIALIZE ARRAY:
ARRAY = np.empty((len(VECT1),len(VECT2)), dtype=object)

# LOOP OVER PAIRS:
for kdex,k in enumerate(VECT1):
    for jdex,j in enumerate(VECT2):
    
        # GET: SPECIES NAME
        name = df_BASE.at[j,'AERO-SPECIES']
    
        # SET LINE:
        hold1 = 'PTRcw-%02i'%(k)
        hold2 = 'p_%s_cw%02i'%(name, k)

        if df_BASE.at[j,'if-WET'] == 1: hold2 = '%i'%(1)
        if df_BASE.at[j,'if-KNT-OA'] == 1: hold2 = 'p_CLOUDSOA_cw%02i'%(k)

        line = ' '*9
        line += 'CALL df_AERO%%SETi(\'%s\', %04i, %s)'%(hold1,jdex+1,hold2)

        # APPEND TO LIST:
        ARRAY[kdex,jdex] = line

# JOIN ARRAY:
JOINED  = '\n\n'.join(['\n'.join(i) for i in ARRAY])

# WRITE:
patt1 = '^.*\<df_AERO; PTRcw>.*$'
patt2 = JOINED 

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_BINS COLUMN-{PTRaa}
# ==================================================================
ADD = []

for k in range(nBINS):
    
    # SET LINE:
    hold1 = 'PTRnn'
    hold2 = 'p_NUM_a%02i'%(k+1)
    
    line = 'CALL df_BINS%%SETi(\'%s\', %04i, %s)'%(hold1,k+1,hold2)
    line = ' '*9 + line

    # APPEND:
    ADD.append(line)

# WRITE:
ADD = '\n'.join(ADD)

xx1 = '^.*\<df_BINS; PTRnn\>.*$'
xx2 = ADD

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: df_BINS COLUMN-{PTRcw}
# ==================================================================
ADD = []

for k in range(nBINS):
    
    # SET LINE:
    hold1 = 'PTRcw'
    hold2 = 'p_NUM_cw%02i'%(k+1)
    
    line = 'CALL df_BINS%%SETi(\'%s\', %04i, %s)'%(hold1,k+1,hold2)
    line = ' '*9 + line

    # APPEND:
    ADD.append(line)

# WRITE:
ADD = '\n'.join(ADD)

xx1 = '^.*\<df_BINS; PTRcw\>.*$'
xx2 = ADD

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: df_BINS; EMISSION FRACTIONS
# ==================================================================
LINES = []

for i,dex in enumerate(df_EMODE1.index):

    # GET: MASS DISTRIBUTION ON STANDARD SIZE BINS [nm]
    XXX0 = df_EMODE2['SIZE'].values
    YYY0 = df_EMODE1.loc[dex].values[3:]

    # RESAMPLE:
    XXX1 = np.concatenate([[0.], bDIAM, [5e4]], axis=0)
    YYY1 = np.zeros(len(XXX1))

    for xxx,yyy in zip(XXX0,YYY0):

        dex2 = np.where(XXX1 >= xxx)[0][0]
        dex1 = dex2 - 1

        YYY1[dex1] += \
        yyy*(XXX1[dex2]-xxx)/(XXX1[dex2]-XXX1[dex1])

        YYY1[dex2] += \
        yyy*(xxx-XXX1[dex1])/(XXX1[dex2]-XXX1[dex1])

    # WRITE LINE:
    for k,yy in enumerate(YYY1[1:-1]):

        line = 'CALL df_BINS%%SETr(\'EM-%02i\', %04i, %.4fd0)'%(i+1,k+1,yy)
        line = ' '*9 + line

        LINES.append(line)

    if nBINS > 1: LINES.append(' ')

# WRITE TO TEXT:
xx1 = '^.*\<df_BINS; EM\>.*'
xx2 = '\n'.join(LINES)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)



# WRITE: df_KNTOA; INITIALIZATION 
# ==================================================================
LIST = [] # << AEROSOL-PHASE POINTERS

for i in range(nBINS):

    key = 'PTRaa-%02i'%(i+1)

    line = 'CALL df_KNTOA%%APPEND_EMPTYi(nKNTOA, \'%s\')'%(key)
    line = ' '*9 + line

    LIST.append(line)

# WRITE:
patt1 = '^.*\<df_KNTOA: APPEND-PTRaa>.*'
patt2 = '\n'.join(LIST)

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_KNTOA; SPECIES NAMES
# ==================================================================
LIST = []

for i,idex in enumerate(df_KNTOA.index):

    # GET: SPECIES NAME
    name = df_KNTOA.at[idex,'AERO-SPECIES']

    # SET LINE:
    line = 'CALL df_KNTOA%%SETh(\'NAMES\', %04i, \'%s\')'%(i+1,name)
    line = ' '*9 + line

    # APPEND:
    LIST.append(line)

# WRITE:
patt1 = '^.*\<df_KNTOA; NAMES>.*$'
patt2 = '\n'.join(LIST)

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_KNTOA; POINTERS TO GAS-PHASE SPECIES
# ==================================================================
LIST = []

for i,idex in enumerate(df_KNTOA.index):

    # GET: SPECIES NAME
    name = df_KNTOA.at[idex,'AERO-SPECIES']
    hold = 'p_%s'%(name)

    # SET LINE:
    line = 'CALL df_KNTOA%%SETi(\'PTRqq\', %04i, %s)'%(i+1,hold)
    line = ' '*9 + line

    # APPEND:
    LIST.append(line)

# WRITE:
patt1 = '^.*\<df_KNTOA; PTRqq>.*$'
patt2 = '\n'.join(LIST)

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_KNTOA; VOLATILITY
# ==================================================================
LIST = []

for i,idex in enumerate(df_KNTOA.index):

    # GET: VOLATILITY
    CSAT = df_KNTOA.at[idex,'CSAT']
    hold = ('%.3e'%(10.**CSAT)).replace('e','d')

    # SET LINE:
    line = 'CALL df_KNTOA%%SETr(\'CSAT\', %04i, %s)'%(i+1,hold)
    line = ' '*9 + line

    # APPEND:
    LIST.append(line)

# WRITE:
patt1 = '^.*\<df_KNTOA; CSAT>.*$'
patt2 = '\n'.join(LIST)

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_KNTOA; MOLECULAR WEIGHT
# ==================================================================
LIST = []

for i,idex in enumerate(df_KNTOA.index):

    # GET: MOLECULAR WEIGHT
    MW = df_KNTOA.at[idex,'MW']
    hold = '%.2fd0'%(MW)

    # SET LINE:
    line = 'CALL df_KNTOA%%SETr(\'MW\', %04i, %s)'%(i+1,hold)
    line = ' '*9 + line

    # APPEND:
    LIST.append(line)

# WRITE:
patt1 = '^.*\<df_KNTOA; MW>.*$'
patt2 = '\n'.join(LIST)

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)

# WRITE: df_KNTOA; POINTER TO AEROSOL-PHASE SPECIES
# ==================================================================
LIST = []

for i,idex in enumerate(df_KNTOA.index):

    LIST1 = []

    for k in range(nBINS):

        # GET: SPECIES NAME
        name = df_BASE.at[idex,'AERO-SPECIES']
    
        # SET LINE:
        hold1 = 'PTRaa-%02i'%(k+1)
        hold2 = 'p_%s_a%02i'%(name, k+1)

        line = 'CALL df_KNTOA%%SETi(\'%s\', %04i, %s)'%(hold1,i+1,hold2)
        line = ' '*9 + line

        # APPEND:
        LIST.append(line)

    if nBINS > 1: LIST.append(' ')

# WRITE:
patt1 = '^.*\<df_KNTOA; PTRaa>.*'
patt2 = '\n'.join(LIST)

TEXT = re.sub(patt1, patt2, TEXT, 0, re.M)













# WRITE: POINTER ARRAY FOR AEROSOL SPECIES
# ==================================================================
# DEFINE TEXT:
for i,dex in enumerate(df_BASE.index):
    for j in range(nBINS):

        if i == 0 and j == 0: ADD = ''

        # GET: SPECIES NAME
        name = df_BASE.at[dex,'AERO-SPECIES']

        # GET: INDEX OF SPECIES
        index = df_BASE.at[dex,'INDEX'] + 1
    
        # DEFINE LINE:
        # - DEFINE SUB-STRING:
        sub1 = 'PTRARRAY_AERO(%02i,%04i)'%(j+1,index)

        # - DEFINE SUB-STRING:
        sub2 = 'p_%s_a%02i'%(name,j+1)

        # - COMBINE SUB-STRINGS:
        line = ' '*9 + sub1 + ' = ' + sub2 + '\n'
         
        # APPEND:
        ADD += line

        if (j == nBINS - 1) and (i != nAERO - 1): 
            ADD += '\n'

# WRITE:
xx1 = '^.*\<PTRARRAY\_AERO\>.*$\n'
xx2 = ADD

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: POINTER ARRAY FOR AEROSOL SPECIES IN CLOUD PHASE
# ==================================================================
# DEFINE TEXT:
for i,dex in enumerate(df_BASE.index):
    for j in range(nBINS):

        if i == 0 and j == 0: ADD = ''

        # GET: SPECIES NAME
        name = df_BASE.at[dex,'AERO-SPECIES']

        # GET: INDEX OF SPECIES
        index = df_BASE.at[dex,'INDEX'] + 1
    
        # DEFINE LINE:
        # - DEFINE SUB-STRING:
        sub1 = 'PTRARRAYaw(%02i,%04i)'%(j+1,index)

        # - DEFINE SUB-STRING:
        sub2 = 'p_%s_cw%02i'%(name,j+1)

        if df_BASE.at[dex,'if-WET'] == 1: sub2 = '%i'%(1)

        # - COMBINE SUB-STRINGS:
        line = ' '*9 + sub1 + ' = ' + sub2 + '\n'
         
        # APPEND:
        ADD += line

        if (j == nBINS - 1) and (i != nAERO - 1): 
            ADD += '\n'

# WRITE:
xx1 = '^.*\<PTRARRAYaw\>.*$\n'
xx2 = ADD

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)



# WRITE: POINTER ARRAY FOR AEROSOL NUMBER.
# ==================================================================
ADD = ''

for i in range(nBINS):
    
    # DEFINE LINE:
    # - DEFINE SUB-STRING:
    sub1 = 'PTRARRAY_NUM(%02i)'%(i+1)

    # - DEFINE SUB-STRING:
    sub2 = 'p_NUM_a%02i'%(i+1)

    # - COMBINE SUB-STRINGS:
    line = ' '*9 + sub1 + ' = ' + sub2 + '\n'

    # APPEND:
    ADD += line

# WRITE:
xx1 = '^.*\<PTRARRAY\_NUM\>.*$\n'
xx2 = ADD

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: POINTER ARRAY FOR AEROSOL NUMBER IN CLOUD PHASE
# ==================================================================
ADD = ''

for i in range(nBINS):
    
    # DEFINE LINE:
    # - DEFINE SUB-STRING:
    sub1 = 'PTRARRAYnw(%02i)'%(i+1)

    # - DEFINE SUB-STRING:
    sub2 = 'p_NUM_cw%02i'%(i+1)

    # - COMBINE SUB-STRINGS:
    line = ' '*9 + sub1 + ' = ' + sub2 + '\n'

    # APPEND:
    ADD += line

# WRITE:
xx1 = '^.*\<PTRARRAYnw\>.*$\n'
xx2 = ADD

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: WET AEROSOL BOOLEAN.
# ==================================================================
ADD = ''

for i,name in enumerate(df_BASE['AERO-SPECIES']):
    
    # GET BOOLEAN:
    BOOL = df_BASE.at[i,'if-WET']

    # DEFINE LINE:
    # - DEFINE SUB-STRING:
    sub1 = 'ARRAY_WETBOOL(%04i)'%(i+1)

    # - DEFINE SUB-STRING:
    sub2 = '%01i'%(BOOL)

    # - COMBINE SUB-STRINGS:
    line = ' '*9 + sub1 + ' = ' + sub2 + '\n'
         
    # APPEND:
    ADD += line

# WRITE:
xx1 = '^.*\<ARRAY\_WETBOOL\>.*$\n'
xx2 = ADD

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: AEROSOL DENSITIES.
# ==================================================================
ADD = ''

for i,name in enumerate(df_BASE['AERO-SPECIES']):
    
    # GET DENSITY:
    RHO = df_BASE.at[i,'RHO']

    # DEFINE LINE:
    # - DEFINE SUB-STRING:
    sub1 = 'ARRAY_AERODENS(%04i)'%(i+1)

    # - DEFINE SUB-STRING:
    sub2 = '%.1f'%(RHO)

    # - COMBINE SUB-STRINGS:
    line = ' '*9 + sub1 + ' = ' + sub2 + '\n'
         
    # APPEND:
    ADD += line

# WRITE:
xx1 = '^.*\<ARRAY\_AERODENS\>.*$\n'
xx2 = ADD

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: STACKED EMISSION MODES
# ==================================================================
for i,dex in enumerate(df_EMODE1.index):

    if i == 0: LIST = []

    # WRITE LINE:
    hold1 = df_EMODE1.at[dex,'INDEX']
    hold2 = df_EMODE1.at[dex,'LABEL']

    line = 'ARRxEMISSMODE(%02i,:) = %s'%(hold1,hold2)

    # APPEND:
    LIST.append(' '*9 + line)

# WRITE TO TEXT:
xx1 = '^.*\<ARRxEMISSMODE\>.*$'
xx2 = '\n'.join(LIST)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)





# WRITE: EMISSION PAIR ARRAYS (AA)
# ==================================================================
for i,dex in enumerate(df_EMISSaa.index):

    # WRITE LINE: INDEX FOR EMISSION TARGETS
    hold = df_EMISSaa.at[dex,'INDEX'] + 1
    line1 = 'df_EMaa_COL01(%04i) = %04i'%(i+1,hold)

    # WRITE LINE: INDEX FOR EMISSION SOURCES
    hold = 'p_' + df_EMISSaa.at[dex,'SOURCE']
    line2 = 'df_EMaa_COL02(%04i) = %s'%(i+1,hold)

    # WRITE LINE: INDEX FOR EMISSION MASS MODE
    hold = df_EMISSaa.at[dex,'MDIST-INDEX']
    line3 = 'df_EMaa_COL03(%04i) = %04i'%(i+1,hold)

    # WRITE LINE: INDEX FOR EMISSION MASS MODE
    hold = df_EMISSaa.at[dex,'i-EMISS-PHASE']
    line4 = 'df_EMaa_COL04(%04i) = %04i'%(i+1,hold)

    # WRITE LINE: EMISSION PHASE
    hold = df_EMISSaa.at[dex,'CSAT']
    line5 = 'df_EMaa_COL05(%04i) = %.2e'%(i+1,hold)

    # WRITE LINE: TARGET MOLECULAR WEIGHT
    hold = df_EMISSaa.at[dex,'MW']
    line6 = 'df_EMaa_COL06(%04i) = %.2e'%(i+1,hold)

    # INITIALZE:
    if i == 0: LIST1 = []; LIST2 = []; LIST3 = []
    if i == 0: LIST4 = []; LIST5 = []; LIST6 = []

    # APPEND:
    LIST1.append(' '*9 + line1)
    LIST2.append(' '*9 + line2)
    LIST3.append(' '*9 + line3)
    LIST4.append(' '*9 + line4)
    LIST5.append(' '*9 + line5)
    LIST6.append(' '*9 + line6)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMaa_COL01\>.*$'
xx2 = '\n'.join(LIST1)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMaa_COL02\>.*$'
xx2 = '\n'.join(LIST2)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMaa_COL03\>.*$'
xx2 = '\n'.join(LIST3)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMaa_COL04\>.*$'
xx2 = '\n'.join(LIST4)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMaa_COL05\>.*$'
xx2 = '\n'.join(LIST5)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMaa_COL06\>.*$'
xx2 = '\n'.join(LIST6)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: EMISSION PAIR ARRAYS (BB)
# ==================================================================
for i,dex in enumerate(df_EMISSbb.index):

    # WRITE LINE: INDEX FOR EMISSION TARGETS
    hold = df_EMISSbb.at[dex,'INDEX'] + 1
    line1 = 'df_EMbb_COL01(%04i) = %04i'%(i+1,hold)

    # WRITE LINE: INDEX FOR EMISSION SOURCES
    hold = 'p_' + df_EMISSbb.at[dex,'SOURCE']
    line2 = 'df_EMbb_COL02(%04i) = %s'%(i+1,hold)

    # WRITE LINE: INDEX FOR EMISSION MASS MODE
    hold = df_EMISSbb.at[dex,'MDIST-INDEX']
    line3 = 'df_EMbb_COL03(%04i) = %04i'%(i+1,hold)

    # WRITE LINE: INDEX FOR EMISSION MASS MODE
    hold = df_EMISSbb.at[dex,'i-EMISS-PHASE']
    line4 = 'df_EMbb_COL04(%04i) = %04i'%(i+1,hold)

    # WRITE LINE: EMISSION PHASE
    hold = df_EMISSbb.at[dex,'CSAT']
    line5 = 'df_EMbb_COL05(%04i) = %.2e'%(i+1,hold)

    # WRITE LINE: MOLECULAR WEIGHT
    hold = df_EMISSbb.at[dex,'MW']
    line6 = 'df_EMbb_COL06(%04i) = %.2e'%(i+1,hold)

    # INITIALZE:
    if i == 0: LIST1 = []; LIST2 = []; LIST3 = []
    if i == 0: LIST4 = []; LIST5 = []; LIST6 = []

    # APPEND:
    LIST1.append(' '*9 + line1)
    LIST2.append(' '*9 + line2)
    LIST3.append(' '*9 + line3)
    LIST4.append(' '*9 + line4)
    LIST5.append(' '*9 + line5)
    LIST6.append(' '*9 + line6)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMbb_COL01\>.*$'
xx2 = '\n'.join(LIST1)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMbb_COL02\>.*$'
xx2 = '\n'.join(LIST2)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMbb_COL03\>.*$'
xx2 = '\n'.join(LIST3)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMbb_COL04\>.*$'
xx2 = '\n'.join(LIST4)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMbb_COL05\>.*$'
xx2 = '\n'.join(LIST5)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT:
xx1 = '^.*\<df_EMbb_COL06\>.*$'
xx2 = '\n'.join(LIST6)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE: PSEUDO-DATAFRAME (AERO)
# ==================================================================
for i,dex in enumerate(df_BASE.index):

    # WRITE COLUMN: SPECIES NAME
    name = df_BASE.at[dex,'AERO-SPECIES']
    line1 = 'df_AERO_COL01(%04i) = \'%s\''%(i+1,name)

    # WRITE COLUMN: CHEM-INDEX
    hold = 'df_AERO_COL02(%04i,%02i) = p_%s_a%02i'
    nnnn = df_BASE.at[dex,'AERO-SPECIES']
    xxxx = df_BASE.at[dex,'INDEX'] + 1

    line2 = [hold%(xxxx,ii+1,nnnn,ii+1) for ii in range(nBINS)]
    line2 = ('\n' + ' '*9).join(line2)

    # WRITE COLUMN: MOLECULAR WEIGHT
    hold = df_BASE.at[dex,'MW']
    line3 = 'df_AERO_COL03(%04i) = %.2e ! << %s'%(i+1,hold,name)

    # WRITE COLUMN: DENSITY
    hold = df_BASE.at[dex,'RHO']
    line4 = 'df_AERO_COL04(%04i) = %.2e ! << %s'%(i+1,hold,name)

    # WRITE COLUMN: if-WET
    hold = df_BASE.at[dex,'if-WET']
    line5 = 'df_AERO_COL05(%04i) = %i ! << %s'%(i+1,hold,name)

    # WRITE COLUMN: IF ORGANIC SPECIES
    hold = df_BASE.at[dex,'if-SORB']
    line6 = 'df_AERO_COL06(%04i) = %i ! << %s'%(i+1,hold,name)

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
xx1 = '^.*\<df_AERO_COL01\>.*$'
xx2 = '\n'.join(LIST1)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: CHEM-INDEX
xx1 = '^.*\<df_AERO_COL02\>.*$'
xx2 = '\n\n'.join(LIST2)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: MOLECULAR WEIGHT
xx1 = '^.*\<df_AERO_COL03\>.*$'
xx2 = '\n'.join(LIST3)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: DENSITY
xx1 = '^.*\<df_AERO_COL04\>.*$'
xx2 = '\n'.join(LIST4)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: if-WET
xx1 = '^.*\<df_AERO_COL05\>.*$'
xx2 = '\n'.join(LIST5)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE TO TEXT: if-ORGANIC
xx1 = '^.*\<df_AERO_COL06\>.*$'
xx2 = '\n'.join(LIST6)

TEXT = re.sub(xx1, xx2, TEXT, 0, re.M)

# WRITE FILE:
# ==================================================================
open('./gen/mod.AERO-CONFIG.f90','w').write(TEXT)
