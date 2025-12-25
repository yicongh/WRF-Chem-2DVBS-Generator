# ===============================================================
#         THIS FUNCTION GETS THE FUNCTIONALIZATION PRODUCTS
# ===============================================================

import math
import numpy  as np
import pandas as pd
import itertools as itt

from scipy.stats import norm

def f_GET_FUNCPROD(name, pXX, pYY, dLVP, pO1, pO2, pO3, pO4, nCCC):

    # INITIALIZE OUTPUT:
    df_OUT = pd.DataFrame(columns=['XX','YY','YIELD'])

    # SET: NUMBER OF OXYGEN ADDED.
    nOXADD = np.array([1, 2, 3, 4])

    # SET: OXYGEN ADDITION PROBABILITIES.
    pOXADD = np.array([pO1, pO2, pO3, pO4])

    # CHECK ERROR: SUM NOT EQUAL TO ONE
    if not math.isclose(sum(pOXADD), 1.0):
        print('SUM OF pOXY VALUES NOT EQUAL TO 1: %s'%(name))
        exit(1)

    # SET: SPREAD AROUND CENTER PRODUCT.
    # - DISTRIBUTION WIDTH:
    SIGMA = 0.8

    # - SET NORMAL DISTRIBUTION:
    xDIST = np.array([-2, -1, 0, 1, 2])
    yDIST = np.array([norm.pdf(i, scale=SIGMA) for i in xDIST])
    yDIST = yDIST/np.sum(yDIST)

    # GET LIST OF PRODUCTS:
    for i,pp in enumerate(nOXADD):
        for j,qq in enumerate(xDIST):

            # TARGET X AND Y POSITION:
            tXX = pXX - dLVP*pp + qq
            tYY = pYY + (float(pp)/float(nCCC))

            # GET YIELD:
            YIELD = pOXADD[i]*yDIST[j]
        
            # APPEND TO DATA FRAME:
            df_ADD = \
            pd.DataFrame([(tXX,tYY,YIELD)], columns=df_OUT.columns)

            df_OUT = \
            pd.concat([df_OUT,df_ADD], axis=0, ignore_index=True)

    return df_OUT

    
