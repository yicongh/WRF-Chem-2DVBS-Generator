# ====================================================================
#      THIS FUNCTION CALCULATE CARBON NUMBER FOR 2D-VBS SPECIES
# ====================================================================

import numpy  as np
import pandas as pd

def f_CALC_CARBNUM(pXX, pYY):

    nC0 = 25.; bC = 0.5; bO = 2.3; bCO = -0.3

    nCCC = (nC0*bC - pXX)/(bC + bO*pYY + 2*pYY*bCO/(1 + pYY))

    nCCC = np.clip(nCCC, 1.0, 1000.)    
    
    return nCCC
