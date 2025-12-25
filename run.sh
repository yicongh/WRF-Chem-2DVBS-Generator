#!/bin/sh

python m.CONFIG.2D-VBS.py || { exit 1; }
python m.CONFIG.AERO.py || { exit 1; }
python m.CONFIG.AERO-EMISS.py || { exit 1; }
python m.CONFIG.AGING-PRODS.py || { exit 1; }
python m.CONFIG.GAS.py || { exit 1; }

python m.GEN-AERO.AERO-CONFIG.py || { exit 1; }
python m.GEN-AERO.COMM-KNT-OA.py || { exit 1; }
python m.GEN-AERO.INIT-POINTERS.py || { exit 1; }
python m.GEN-EMISS.ADD-EMISS-BURN.py || { exit 1; }
python m.GEN-EMISS.MAIN-DRIVER.py || { exit 1; }
python m.GEN-EMISS.PLUMERISE.py || { exit 1; }
python m.GEN-GAS.CONFIG-GAS.py || { exit 1; }
python m.GEN-KPP.EQN.py || { exit 1; }
python m.GEN-KPP.SPC.py || { exit 1; }
python m.GEN-REG.py || { exit 1; }
