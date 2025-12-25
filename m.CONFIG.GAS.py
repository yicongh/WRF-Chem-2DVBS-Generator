
import os
print('Running... %s'%os.path.basename(__file__))

import re, json
import subprocess
import numpy  as np
import pandas as pd

# READ: AEROSOL BASE CONFIGURATIONS
# ==================================================================
path = './data/d.CONFIG-GAS.BASE.xlsx'

# READ DATA:
OPTIONS = {}
OPTIONS['skiprows'] = [1]
OPTIONS['na_filter'] = False

df_BASE = pd.read_excel(path, **OPTIONS)

# READ HEADER INFOS:
df_INFO = pd.read_excel(path, nrows=1)

# READ: 2D-VBS CONFIGURATIONS
# ==================================================================
df1 = pd.read_excel('./cache/d.2D-VBS.PPP.xlsx')
df2 = pd.read_excel('./cache/d.2D-VBS.SSS.xlsx')

df_2DVBS = pd.concat([df1,df2], axis=0, ignore_index=True)

# SELECT ROWS:
dex = df_2DVBS['if-SAPRC'] == 0

df_2DVBS = df_2DVBS.loc[dex].copy(deep=True)

# INTEGRATE 2D-VBS CONFIGURATIONS:
# ==================================================================
# SELECT COLUMNS AND RENAME:
df_2DVBS = df_2DVBS[['INDEX','NAME','MW','CSAT']]
df_2DVBS = df_2DVBS.rename(columns={'NAME':'SPECIES'})

# INSERT NEW COLUMNS:
df_2DVBS['NOTE'] = '-'

# INSERT NEW COLUMNS:
# NOTE: BASED ON HODZIC ET AL. (JGR; 2014)
fun = lambda x: 10.**(8. - 0.75*max(x,-4.))

df_2DVBS['H-STAR'] = df_2DVBS['CSAT'].apply(fun)
df_2DVBS['z-TEMP'] = 6013.95
df_2DVBS['f0'] = 0.0
df_2DVBS['DIFFg'] = 0.1

# COMBINE DATAFRAMES:
df_BASE = pd.concat([df_BASE,df_2DVBS], axis=0, ignore_index=True)

# UPDATE INDEX:
df_BASE['INDEX'] = np.arange(df_BASE.shape[0])

# SAVE DATAFRAME:
# ==================================================================
# OPEN FILE:
path = './cache/d.CONFIG-GAS.BASE+2DVBS.xlsx'

OPTIONS = {}
OPTIONS['engine'] = 'xlsxwriter'

ff = pd.ExcelWriter(path, **OPTIONS)

# CLEAR DEFAULT HEADER FORMATS:
#pd.io.formats.excel.ExcelFormatter.header_style = None

# WRITE DATA:
OPTIONS = {}
OPTIONS['index'] = False
OPTIONS['header'] = None
OPTIONS['startrow'] = 2

df_BASE.to_excel(ff, **OPTIONS)

# SET FORMAT:
wb = ff.book
ws = ff.sheets['Sheet1']

fmt = wb.add_format()
fmt.set_font_name('Courier New')
fmt.set_font_size(11)
fmt.set_num_format('0.00E+00')

ws.set_column('D:I', 11, fmt)

# SET FORMAT:
fmt = wb.add_format()
fmt.set_font_name('Courier New')
fmt.set_font_size(11)

ws.set_column('A:A', None, fmt)

# SET FORMAT:
fmt = wb.add_format()
fmt.set_font_name('Courier New')
fmt.set_font_size(11)

ws.set_column('B:B', 12, fmt)

# SET FORMAT:
fmt = wb.add_format()
fmt.set_font_name('Courier New')
fmt.set_font_size(11)
fmt.set_align('center')

ws.set_column('C:C', None, fmt)

# WRITE HEADER:
fmt = wb.add_format()
fmt.set_font_name('Courier New')
fmt.set_font_size(11)
fmt.set_bold(True)
fmt.set_align('center')
fmt.set_bg_color('A6A6A6')
fmt.set_border(1)

ws.write_row('A1:I1', list(df_INFO.columns), fmt)

# WRITE HEADER:
fmt = wb.add_format()
fmt.set_font_name('Courier New')
fmt.set_font_size(11)
fmt.set_align('fill')
fmt.set_bg_color('D9D9D9')
fmt.set_border(1)

ws.write_row('A2:I2', list(df_INFO.values[0]), fmt)

# CLOSE FILE:
#ff.save()

ff.close()
