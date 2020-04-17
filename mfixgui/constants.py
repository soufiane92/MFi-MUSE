# Constants
from collections import OrderedDict

# Solver types
# must match combobox_solver in model.ui
SINGLE, TFM, DEM, PIC, HYBRID = range(5)

# model types
CONSTANT, AIR, UDF = 0, 1, 2
VARIABLE = MIXTURE = OTHER = AIR  # continuum, etc

DRAG_TYPES = ['SYAM_OBRIEN', # (DEFAULT)
              'BVK',
              'GIDASPOW',
              'GIDASPOW_BLEND',
              'GIDASPOW_PCF',
              'GIDASPOW_BLEND_PCF',
              'HYS',
              'KOCH_HILL',
              'KOCH_HILL_PCF',
              'WEN_YU',
              'WEN_YU_PCF',
              'USER_DRAG']

DEFAULT_DRAG_TYPE = 'SYAM_OBRIEN'

TURBULENCE_MODELS = ['NONE', 'MIXING_LENGTH', 'K_EPSILON']
DEFAULT_TURBULENCE_MODEL = 'NONE'

SUBGRID_TYPES = ['NONE', 'IGCI', 'MILIOLI']
DEFAULT_SUBGRID_TYPE = 'NONE'

# Viscous stress model
KT_TYPES = ['ALGEBRAIC', 'LUN_1984', 'IA_NONEP', 'SIMONIN',
            'AHMADI', 'GD_99', 'GTSH', 'GHD']

DEFAULT_KT_TYPE = 'ALGEBRAIC'

FRICTION_MODELS = ['SCHAEFFER', 'SRIVASTAVA', 'NONE']
DEFAULT_FRICTION_MODEL = 'SCHAEFFER'

RDF_TYPES = [None, 'LEBOWITZ',
             'MANSOORI', 'MODIFIED_LEBOWITZ', 'MODIFIED_MANSOORI']


BLENDING_FUNCTIONS = ['NONE', 'TANH_BLEND', 'SIGM_BLEND']
DEFAULT_BLENDING_FUNCTION = 'NONE'

BC_TYPES = ['MI', 'PO', 'NSW', 'FSW', 'PSW', 'PI', 'MO', 'CYCLIC'] # 'CYCLIC' is not really a bc_type

BC_NAMES = ['Mass Inflow', 'Pressure Outflow', 'No Slip Wall',
            'Free Slip Wall', 'Partial Slip Wall',
            'Pressure Inflow', 'Mass Outflow',
            'Cyclic Boundary']

(MASS_INFLOW, PRESSURE_OUTFLOW,
 NO_SLIP_WALL, FREE_SLIP_WALL, PARTIAL_SLIP_WALL,
 PRESSURE_INFLOW, MASS_OUTFLOW, CYCLIC_BOUNDARY) = range(8)

(NO_FLUX, SPECIFIED_TEMPERATURE, SPECIFIED_FLUX, CONVECTIVE_FLUX) = range(4)

DEFAULT_BC_TYPE = 'NSW'


IS_NAMES = ['Impermeable', 'X-Axis Impermeable', 'Y-Axis Impermeable', 'Z-Axis Impermeable',
            'Semi-permeable', 'X-Axis semi-permeable','Y-Axis semi-permeable','Z-Axis semi-permeable']

IS_TYPES = ['IMPERMEABLE', 'X_IMPERMEABLE', 'Y_IMPERMEABLE', 'Z_IMPERMEABLE',
            'SEMIPERMEABLE', 'X_SEMIPERMEABLE', 'Y_SEMIPERMEABLE', 'Z_SEMIPERMEABLE']

(IMPERMEABLE, X_IMPERMEABLE, Y_IMPERMEABLE, Z_IMPERMEABLE,
 SEMIPERMEABLE, X_SEMIPERMEABLE, Y_SEMIPERMEABLE, Z_SEMIPERMEABLE) = range(8)

DEFAULT_IS_TYPE = 'IMPERMEABLE'

MONITOR_TYPE_NAMES = ['Value', 'Sum', 'Min', 'Max', 'Average', 'Standard deviation',
                      'Area-weighted average', 'Flow rate', 'Mass flow rate',
                      'Mass-weighted average', 'Volume flow rate', 'Volume integral',
                      'Volume-weighted average', 'Mass-weighted integral',
                      'Mass-weighted average']

MONITOR_TYPES = (VALUE, SUM, MIN, MAX, AVERAGE, STANDARD_DEVIATION,
                 AREA_WEIGHTED_AVERAGE, FLOW_RATE, MASS_FLOW_RATE,
                 MASS_WEIGHTED_AVERAGE_SURFACE, VOLUME_FLOW_RATE, VOLUME_INTEGRAL,
                 VOLUME_WEIGHTED_AVERAGE, MASS_WEIGHTED_INTEGRAL, MASS_WEIGHTED_AVERAGE_VOLUME) = range(15)


MONITOR_TYPES_POINT = [VALUE]
MONITOR_TYPES_PLANE = [SUM, MIN, MAX, AVERAGE, STANDARD_DEVIATION,
                       AREA_WEIGHTED_AVERAGE, FLOW_RATE, MASS_FLOW_RATE,
                       MASS_WEIGHTED_AVERAGE_SURFACE, VOLUME_FLOW_RATE]
MONITOR_TYPES_VOLUME = [SUM, MIN, MAX, AVERAGE, STANDARD_DEVIATION,
                        VOLUME_INTEGRAL, VOLUME_WEIGHTED_AVERAGE,
                        MASS_WEIGHTED_INTEGRAL, MASS_WEIGHTED_AVERAGE_VOLUME]

# ./model/param_mod.f:67:      INTEGER, PARAMETER :: DIM_M = 10 # max # of solids phases
DIM_M = 10
#model/param_mod.f:      INTEGER, PARAMETER :: DIM_EQS = 10
DIM_EQS = 10

PRECON_TYPES = ['NONE', 'LINE', 'DIAG']
SWEEP_TYPES = ['RSRS', 'ASAS', 'ISIS', 'JSJS', 'KSKS']

DES_OUTPUT_TYPES = ['PARAVIEW', 'TECPLOT']

CONVERSION_TO_METERS = OrderedDict([
    ('km',     1000.0),
    ('m',      1.0),
    ('cm',     0.01),
    ('mm',     0.001),
    ('um',     1e-6),
    ('mile',   1609.34),
    ('yard',   0.9144),
    ('ft',     0.3048),
    ('ins',    0.0254),
])

SPECIAL_PARAMETERS = [a+b
                      for a in ('', 'x', 'y', 'z')
                      for b in ('min', 'max')]

PARAMETER_DICT = OrderedDict([(key, 0.0) for key in SPECIAL_PARAMETERS])

MAX_RECENT_PROJECTS = 20

# these are fnmatch and glob patterns to find output files
# Note, we apply them in case-insensitive manner
RESTART_FILES = ['*.res']
SPX_FILES = ['*.sp?']
VTK_FILES = ['*.vtp', '*.vtu', '*.pvd',
             '*_frame_index.txt']
MONITOR_FILES = ['*.csv']
OTHER_FILES = ['*.out', '*.log', '*.pid', '*.env',
               '*.error', '*.e[0-9]*', '*.o[0-9]*',
               '*.pe[0-9]*', '*.po[0-9]*',
               'mfix.stop']
