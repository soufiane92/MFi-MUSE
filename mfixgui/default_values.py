# Default values for MFIX keys which are REQUIRED.
# User is not allowed to un-set these, so we need some defaults
#
# Note, these are here instead of constants.py since the constants defined there are internal to the GUI.

# generic values: (Per Jordan Musser 2017-08-14)

# gas density = 1.0 kg/m3
# solids density = 1000.0 kg/m3
# solids diameter = 100.0e-6 m
# temperature = 300. K

from mfixgui.project import FloatExp

ro_g0 = 1.0
ro_s0 = 1000.0
d_p0 = 1e-4 # FloatExp('100.0e-6') - gets converted to 1e-4 anyway, so no point in using FloatExp
t = 300.0
mu_g0 = FloatExp('1.e+03')
