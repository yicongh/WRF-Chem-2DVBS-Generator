# ===============================================================
#         THIS FUNCTION GETS THE FRAGMENTATION PRODUCTS
# ===============================================================

import numpy  as np
import pandas as pd
import itertools as itt

from scipy.stats import norm

def f_GET_FRAGPROD(pXX, pYY):

    # INITIALIZE OUTPUT:
    df_OUT = pd.DataFrame(columns=['XX','YY','YIELD'])

    # SET: ANCHOR POINT IN X AND Y.
    xANCH = 9.0
    yANCH = 2.1

    # CHECK: MAXIMUM VOLATILITY.
    if (pXX >= xANCH):
        exit('pXX MUST BE LESS THAN %i. STOP.'%(xANCH))

    # SET: HALFWAY LOCATION:
    xHALF = 0.5*(xANCH + pXX)
    
    # FIND INTERMEDIATE PRODUCTS:
    # - DEFINE: LAMBDA FUNCTION.
    fun = lambda a: \
    pYY + (yANCH - pYY)/(xANCH - pXX)*(a - pXX)

    # - DEFINE: INTERMEDIATE POINTS.
    xINTERM = np.arange(np.ceil(pXX), xANCH+1, 1)
    yINTERM = np.zeros(len(xINTERM))

    for i in range(len(xINTERM)):
        if xINTERM[i] < xHALF:
            yINTERM[i] = pYY
        else:
            yINTERM[i] = fun(xINTERM[i])

    # - DEFINE: CARBON NUMBER OF EACH PRODUCT.
    nCC = 25; bCC = 0.5; bOO = 2.3; bCO = -0.3
    
    hold1 = (nCC*bCC - xINTERM)
    hold2 = (bCC + bOO*yINTERM + 2*yINTERM*bCO/(1 + yINTERM))

    nCARB = hold1/hold2
    
    # FIND INTERMEDIATE PRODUCT YIELDS:
    # - DEFINE DESCENDING HEIGHTS:
    HH = [len(xINTERM) + 1 - i for i in range(len(xINTERM))]

    # - GET YIELDS BASED ON DESCENDING HEIGHT:
    YIELDim = np.array([i/sum(HH) for i in HH])

    # DEFINE: OXIDATION PRODUCTS OF INTERMEDIATES.
    # NOTE: THIS IS PRESCRIBED.
    # - SET: DIMENSIONS.
    nOXCOLS = 8
    nOXROWS = 4

    # - PROBABILITIES OF ADDING OXYGENS:
    pO0 = 0.5; pO1 = 0.3; pO2 = 0.1; pO3 = 0.1

    # - SET: PRODUCTS.
    # YICONG HE 20250814:
    OXKERNEL = np.zeros((nOXROWS,nOXCOLS))
    OXKERNEL[0,0] = pO0
    OXKERNEL[1,0] = 0.0 #pO1*(9./30.)
    OXKERNEL[1,1] = pO1 #pO1*(15./30.)
    OXKERNEL[1,2] = 0.0 #pO1*(6./30.)
    OXKERNEL[2,1] = 0.0 #pO2*(2./10.)
    OXKERNEL[2,2] = pO2 #pO2*(4./10.)
    OXKERNEL[2,3] = 0.0 #pO2*(3./10.)
    OXKERNEL[2,4] = 0.0 #pO2*(1./10.)
    OXKERNEL[3,2] = 0.0 #pO3*(1./10.)
    OXKERNEL[3,3] = pO3 #pO3*(2./10.)
    OXKERNEL[3,4] = 0.0 #pO3*(4./10.)
    OXKERNEL[3,5] = 0.0 #pO3*(2./10.)
    OXKERNEL[3,6] = 0.0 #pO3*(1./10.)

    # GET THE LIST OF PRODUCTS:
    for i,xx in enumerate(xINTERM):
        for j in range(np.shape(OXKERNEL)[0]):
            for k in range(np.shape(OXKERNEL)[1]):
                
                # GET: TARGET X AND Y.
                tXX = xx - k
                tYY = yINTERM[i] + float(j)/float(nCARB[i])

                # GET: YIELD.
                YIELD = YIELDim[i]*OXKERNEL[j,k]

                # APPEND TO OUTPUT:
                df_ADD = \
                pd.DataFrame([(tXX,tYY,YIELD)], columns=df_OUT.columns)

                df_OUT = \
                pd.concat([df_ADD,df_OUT], axis=0, ignore_index=True)

    return df_OUT


    
