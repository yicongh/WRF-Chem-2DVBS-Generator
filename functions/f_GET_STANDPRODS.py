# ====================================================================
#        THIS FUNCTION CONVERTS TO THE STANDARD 2D-VBS PRODUCTS
# ====================================================================

import numpy  as np
import pandas as pd

def f_GET_STANDPRODS(df_PROD, XXX, YYY, CLASS, THRESH=1e-3):

    # GET PRODUCT DISTRIBUTION:
    # - GET PRODUCT VALUES:
    pXX = df_PROD['XX'].values
    pYY = df_PROD['YY'].values
    pFF = df_PROD['YIELD'].values
        
    # - INITIALIZE KERNEL:
    KERNEL = np.zeros((len(YYY),len(XXX)))
    
    # - LOOP OVER PRODUCTS:
    for j,(pX,pY,pF) in enumerate(zip(pXX,pYY,pFF)):
        
        # LIMIT VALUES:
        if pX >= max(XXX): pX = max(XXX) - 0.05
        if pX <= min(XXX): pX = min(XXX) + 0.05
        
        if pY >= max(YYY): pY = max(YYY) - 0.01
        if pY <= min(YYY): pY = min(YYY) + 0.01
        
        # FOR THE X DIMENSION:
        # - FIND POSITION OF PRODUCT:
        dex1 = np.where(pX >= XXX)[0][-1]
            
        # - FIND THE FRACTIONS TO NEIGHBORING BINS:
        middle = pX; upper = XXX[dex1+1]; lower = XXX[dex1]
        
        f1a = (middle - lower)/(upper - lower)
        f2a = (upper - middle)/(upper - lower)
        
        # FOR THE Y DIMENSION:
        # - FIND POSITION OF PRODUCT:
        dex2 = np.where(pY >= YYY)[0][-1]
        
        # - FIND THE FRACTIONS TO NEIGHBORING BINS:
        middle = pY; upper = YYY[dex2+1]; lower = YYY[dex2]
        
        f1b = (middle - lower)/(upper - lower)
        f2b = (upper - middle)/(upper - lower)
        
        # ASSIGN VALUES TO KERNEL:
        KERNEL[dex2+1,dex1+1] = \
        KERNEL[dex2+1,dex1+1] + pF*f1a*f1b
        
        KERNEL[dex2,dex1+1] = \
        KERNEL[dex2,dex1+1] + pF*f1a*f2b

        KERNEL[dex2+1,dex1] = \
        KERNEL[dex2+1,dex1] + pF*f2a*f1b
        
        KERNEL[dex2,dex1] = \
        KERNEL[dex2,dex1] + pF*f2a*f2b

    # APPEND DATA TO OUTPUT:
    LISTOUT = []
    
    for j in range(len(XXX)):
        for k in range(len(YYY)):
            
            # INITIALIZE OUTPUT:
            if (j == 0) and (k == 0): OUT = {}
            
            # SKIP: IF YIELD TOO SMALL.
            if (KERNEL[k,j] < THRESH): continue

            # PRODUCT NAME:
            hold1 = 'X%02iY%02i'%(XXX[j],YYY[k]*10)
            hold2 = 'U%02iY%02i'%(abs(XXX[j]),YYY[k]*10)

            if XXX[j] >= 0:
                name = CLASS + hold1
            else:
                name = CLASS + hold2

            # APPEND TO OUTPUT:
            hold = ['NAME', 'XX', 'YY', 'YIELD']
            df_ADD = pd.DataFrame([(name, XXX[j], YYY[k], KERNEL[k,j])], columns=hold)

            LISTOUT.append(df_ADD)

    df_OUT = pd.concat(LISTOUT, axis=0, ignore_index=True)
    
    return df_OUT
