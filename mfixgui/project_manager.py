"""ProjectManager is a subclass of Project,
  intermediating between Gui and Project objects

 It handles loading and storing MFiX-GUI projects,
   using primitives from Project

 It handles interaction between gui and Project objects,
updating gui widgets when keywords are changed, and vice-versa.

 It is a subclass of Project, and the .project member in the main MfixGui
object is actually a ProjectManager, not a Project.

Widgets get associated with keywords via the 'register' method.  Multiple
widgets may map to the same keyword.  (Can a widget register multiple keywords?)
Widgets must support updateValue method and emit value_updated signal (see
widgets in widgets/base.py)

It is important that widgets emit the 'value_updated" signal only on user
interaction, not a programmatic setValue/setText/setChecked/etc, otherwise
a notification loop is possible """


from collections import OrderedDict
import sys
import os
import glob
import shutil
import traceback
import logging
log = logging.getLogger(__name__)
import warnings

from mfixgui.project import Project, Keyword, Equation
from mfixgui.constants import *

from mfixgui.widgets.base import LineEdit # a little special handling needed

from mfixgui.tools import (format_key_with_args, parse_key_with_args,
                           plural, read_burcat)

from mfixgui.tools.keyword_args import keyword_args
from mfixgui.unit_conversion import cgs_to_SI


class ProjectManager(Project):
    """handles interaction between gui and mfix project"""
    def __init__(self, gui=None, keyword_doc=None):
        Project.__init__(self, keyword_doc=keyword_doc)
        self.gui = gui
        self.error = self.gui.error
        self.warn = self.warning = self.gui.warning

        self.registered_widgets = {} # Map: key: keyword,  val:  list of [(args,widget),...]

        self.registered_keywords = set()
        self.solver = SINGLE  # default
        self.modified_time = ""

    def submit_change(self, widget, newValueDict, args=None):
        # Note, this may be a callback from Qt (in which case 'widget' is
        # the widget that the user activated), or from the initial mfix
        # loading (in which case widget == None)
        if isinstance(args, int):
            args = [args]
        elif args is None:
            args = []
        # Special argument handling!
        args = self.expand_args(args)

        if args:
            for arg in args:
                if not (isinstance(arg, int) or
                        isinstance(arg, (tuple, list)) and all(isinstance(a,int) for a in arg)):
                    raise TypeError("keyword indices must be integer, not %s" % repr(arg))

        for (key, newValue) in newValueDict.items():
            if isinstance(newValue, dict):
                for ind, value in newValue.items(): # Where is this getting used?
                    self.change(widget, key, value, args=args+[ind])
            else:
                self.change(widget, key, newValue, args=args)


    def change(self, widget, key, newValue, args=None):
        key = key.lower()
        if isinstance(args, int):
            args = [args]
        elif args is None:
            args = []

        # If any element of 'args' is itself a list, iterate over all values
        # This does not handle more than one list/tuple
        # See related code in gui.unset_keyword

        if any(isinstance(arg, (list,tuple)) for arg in args):
            copy_args = list(args)
            for (i, arg) in enumerate(args):
                if isinstance(arg, (list,tuple)):
                    for a in arg:
                        copy_args[i] = a
                        self.change(widget, key, newValue, copy_args)
                    break
            return

        updatedValue = None
        assert not isinstance(newValue, Keyword)

        previousValue = self.get_value(key, args=args)
        try:
            updatedKeyword = self.updateKeyword(key, newValue, args)
            updatedValue = updatedKeyword.value
        except Exception as e:
            self.gui.print_internal("Warning: %s: %s" %
                                       (format_key_with_args(key, args), e),
                                       color='red')
            traceback.print_exception(*sys.exc_info())
            return

        updates = self.registered_widgets.get(key,[])
        for (a, w) in updates:
            if w == widget:
                continue
            if not self.args_match(a, args):
                continue
            try:
                w.updateValue(key, updatedValue, self.expand_args(args))
                if isinstance(w, LineEdit):
                    w.text_changed_flag = False # Needed?
            except Exception as e:
                ka = format_key_with_args(key, args)
                msg = "Cannot set %s = %s: %s" % (ka, updatedValue, e)
                self.gui.warn(msg)
                traceback.print_exception(*sys.exc_info())
                raise ValueError(msg)

        if (updatedValue is None or updatedValue==''):
            self.gui.unset_keyword(key, args) # prints msg in window.
        else:
            # Show string values with single quotes in the console
            val_str = (repr if isinstance(updatedValue, str) else str)(updatedValue)

            if updatedValue != previousValue:
                self.gui.set_unsaved_flag()
                self.gui.print_internal("%s = %s" % (format_key_with_args(key, args), val_str),
                                        font="Monospace")

        # 'Parameters' are user-defined variables
        # TODO: methods to handle these ('is_param', etc)
        if self.gui and key in ['x_min', 'x_max', 'y_min', 'y_max', 'z_min', 'z_max']:
            # prevent values that are not floats ('', None) from propagating
            # since other equations can depend on these values
            try:
                float(newValue)
                self.gui.update_parameters([key.replace('_', '')]) #??  length==max-min?
            except ValueError:
                pass

        if hasattr(widget, 'post_update'):
            widget.post_update()


    def args_match(self, args, target):
        if len(args) != len(target):
            return False
        for (a,b) in zip(args, target):

            if a=='*':
                continue # matches everything

            elif a=='P':
                if b != self.gui.P:
                    return False

            elif a=='IC':
                if b not in self.gui.ics_current_indices:
                    return False

            elif a=='BC':
                if b not in self.gui.bcs_current_indices:
                    return False

            elif a=='PS':
                if b not in self.gui.pss_current_indices:
                    return False

            elif a=='IS':
                if b not in self.gui.iss_current_indices:
                    return False

            elif a=='IDX':
                continue

            elif a=='VTK': # VTK output region
                if b not in self.gui.vtk_current_indices:
                    return False

            elif a=='MONITOR':
                if b != self.gui.monitors_current_index:
                    return False

            elif a != b:
                return False

        return True

    def expand_args(self, args_in):
        if not args_in: # None or empty list
            return args_in
        if isinstance(args_in, int):
            args_in = [args_in]
        if any (a == 'S' for a in args_in):
            raise DeprecationWarning("keyword argument S is deprecated, use P")
        return [(self.gui.P if a=='P'
                 else self.gui.ics_current_indices if a == 'IC'
                 else self.gui.bcs_current_indices if a == 'BC'
                 else self.gui.pss_current_indices if a == 'PS'
                 else self.gui.iss_current_indices if a == 'IS'
                 else self.gui.vtk_current_indices if a == 'VTK'
                 else self.gui.monitors_current_index if a == 'MONITOR'
                 else a)
                for a in args_in]


    def guess_solver(self):
        """ Attempt to derive solver type, after reading mfix file"""
        mmax = self.get_value('mmax', default=len(self.solids))
        if mmax == 0:
            return SINGLE
        solids_models = set(self.get_value('solids_model', args=n, default='TFM').upper()
                           for n in range(1, mmax+1))
        if solids_models == set(["TFM"]):
            return TFM
        elif solids_models == set(["DEM"]):
            return DEM
        elif solids_models == set(["PIC"]):
            return PIC
        elif solids_models == set(["TFM", "DEM"]):
            return HYBRID
        # mfix settings are inconsistent, warn user.  (Popup here?)
        msg = "Warning, cannot deduce solver type"
        self.gui.print_internal(msg, color='red')
        #log.warn(msg)
        #default
        return SINGLE


    def load_project_file(self, project_file):
        """Loads an MFiX project file updating certain keywords to match expectations
        of MFiX-GUI:
           * reject certain types of files (cylindrical coordinates)
           * autoconvert CGS to SI
           * filter out keywords which will be passed on commandline  (issues/149)
           * migrate mw_g/mw_s to THERMO DATA section
           * prefetch entries from Burcat db and add to THERMO DATA
           * set ic_ep_s from ic_ep_g  (issues/142)
           * convert gravity scalar to vector
           * convert VTK_VAR and VTK_VARLIST keys to vtk_* booleans
           * convert [XYZ]LENGTH to [XYZ]_MIN and [XYZ]_MAX (issues/238)

        Reports any non-fatal load errors via the 'warnings' module

        Returns False if input file rejected, True if opened (with possible warnings)

        This is the intermediary between gui.open_project and project.parsemfixdat
        """

        n_errs = 0
        excs = []

        units_converted = False

        with warnings.catch_warnings(record=True) as ws:
            # Load the file and do basic keyword parsing
            self.parsemfixdat(fname=project_file)

            # Check for some invalid conditions
            if self.get_value('use_rrates'):
                raise ValueError('use_rrates not supported')

            if self.get_value('cylindrical'):
                raise ValueError('Cylindrical coordinates not supported')

            # Don't allow specifying regons by cell indices.
            # Note, this still allows 'usr_i_e' etc
            # TODO:  Should these keys be deprecated in the solver?
            banned = {a+b for a in ('bc', 'ic', 'is', 'ps')
                      for b in ('_i_e', '_i_w', '_j_n', '_j_s', '_k_b', '_k_t')}
            for kw in self.keywordItems():
                if kw.key in banned:
                    raise ValueError('%s: Cell indices not supported.  Specify regions by position.' %
                                     kw.key.upper())

            # Make sure we're in SI
            units = self.get_value('units', default='SI')
            if units.lower() == 'cgs':
                units_converted = True
                cgs_file = project_file + '.cgs'
                msg = 'CGS units detected!  Converting to SI. Original file saved as %s' % os.path.basename(cgs_file)
                self.gui.print_internal(msg, color='blue')
                warnings.warn(msg)
                shutil.copyfile(project_file, cgs_file)
                self.gui.update_keyword('units', 'SI')

                for kw in self.keywordItems():
                    # Don't attempt to convert non-floating point values
                    dtype = self.keyword_doc.get(kw.key,{}).get('dtype')
                    if dtype != 'DP':
                        continue

                    factor = cgs_to_SI.get(kw.key)

                    #Special case is_pc, per Jeff Dietiker
                    #is_pc: 0.0001 for 1st index (permeability, Length2),
                    # 100.0 for 2nd index (Inertial resistance factor, 1/Length)
                    if kw.key == 'is_pc' and len(kw.args) == 2:
                        factor = {1: 0.001, 2:100}.get(kw.args[1])

                    if factor == 1:
                        continue
                    if factor is not None:
                        try:
                            kw.value *= factor
                            if isinstance(kw.value, float):
                                kw.value = round(kw.value, 16)
                            self.gui.set_unsaved_flag()
                        except Exception as e:
                            warnings.warn('%s: %s * %s' % (str(e), factor, kw.value))
                    else:
                        warnings.warn('no conversion for %s' % (kw.key))
                # issues/191 convert particle_input.dat if found
                particle_file = os.path.join(os.path.dirname(project_file), 'particle_input.dat')
                if os.path.exists(particle_file):
                    try:
                        self.convert_particle_file_units(particle_file)
                    except Exception as e:
                        warnings.warn("Error %s while converting %s" % (e, particle_file))
                # convert poly.dat
                poly_file = os.path.join(os.path.dirname(project_file), 'poly.dat')
                if os.path.exists(poly_file):
                    try:
                        self.convert_polygon_file_units(poly_file)
                    except Exception as e:
                        warnings.warn("Error %s while converting %s" % (e, poly_file))
                # convert stl file
                stl_file = os.path.join(os.path.dirname(project_file), 'geometry.stl')
                if os.path.exists(stl_file):
                    try:
                        self.convert_stl_file_units(stl_file)
                    except Exception as e:
                        warnings.warn("Error %s while converting %s" % (e, stl_file))

            elif units.lower() == 'si':
                pass
            else:
                warnings.warn('Invalid units %s' % units) # Should this be fatal err?


            # Let's guess the solver type from the file
            self.solver = self.guess_solver()
            # Now put the GUI into the correct state before setting up interface
            self.gui.set_solver(self.solver)

            # deal with species, since they can cause other widgets to instantiate
            nmax_g = self.get_value('nmax_g', default=0)
            # TODO: handle cases which use rrates.f, like tutorials/reactor1b
            # which has 'nmax(0)' instead of 'nmax_g'
            if len(self.gasSpecies) != nmax_g:
                warnings.warn("nmax_g = %d, %d gas species defined" %
                              (nmax_g, len(self.gasSpecies)))

            # Make sure they are sorted by index before inserting into gui
            self.gasSpecies.sort(key=lambda a: a.ind)
            self.solids.sort(key=lambda a:a.ind)

            # Parse THERMO DATA section NB: parsemfixdat does not handle this
            user_species = {}
            if self.thermo_data is not None:
                for (species,lines) in self.thermo_data.items():
                    section = ['User defined', 'loaded from %s' % project_file] + lines
                    data = read_burcat.parse_section(section)
                    for (species, phase, tmin, tmax, mol_weight, coeffs, comment) in data:
                        user_species[(species, phase)] = (tmin, tmax, mol_weight, coeffs, comment)

            for g in self.gasSpecies:
                # First look for definition in THERMO DATA section
                phase = g.phase.upper() # phase and species_g are guaranteed to be set
                species = g.get('species_g')

                source = "User Defined" # TODO load comment from THERMO DATA
                if species is None: # Should not happen (?)
                    species = 'Gas %s' % g.ind
                    source = "Auto"
                    #warnings.warn("no species_g for gas %d" % g.ind)
                alias = g.get('species_alias_g', species)
                mw_g = g.get('mw_g', None)
                # Note, we're going to unset mw_g and migrate it into THERMO DATA

                user_def = user_species.get((species, phase))
                # Look for mismatched phase
                if not user_def:
                    for ((s,p), v) in user_species.items():
                        if s == species:
                            # This is all-too-common.  existing mfix files all have 'S' for
                            # phase in thermo data.
                            #warnings.warn("species '%s' defined as phase '%s', expected '%s'"
                            #              % (species, p, phase))
                            user_def = v
                            break
                if user_def:
                    (tmin, tmax, mol_weight, coeffs, comment) = user_def
                    if tmin == tmax == 0.0:
                        tmin, tmax = 200.0, 6000.0
                    if mw_g is not None:
                        mol_weight = mw_g # mw_g overrides values in THERMO DATA
                    species_data = {'source': source,
                                    'phase': phase,
                                    'alias': alias,
                                    'mol_weight': mol_weight,
                                    'h_f': coeffs[14],
                                    'tmin': tmin,
                                    'tmax': tmax,
                                    'a_high': coeffs[:7],
                                    'a_low': coeffs[7:14]}

                else:
                    # get this from the species popup so we don't have to load
                    # another copy of the database.  the database is owned by
                    # the species popup.
                    species_data = self.gui.species_popup.get_species_data(species, phase)
                    if not species_data:
                        # Look for mismatched phase definition
                        # This is not really 'mismatched'.  Fluid phase may contain solids and v.v.
                        for p in 'GLSC':
                            if p == phase:
                                continue
                            species_data = self.gui.species_popup.get_species_data(species, p)
                            if species_data:
                                #warnings.warn("species '%s' defined as phase '%s', expected '%s'"
                                #              % (species, p, phase))
                                log.info("species '%s' defined as phase '%s', expected '%s'"
                                         % (species, p, phase))
                                break
                    if species_data:
                        species_data['alias'] = alias
                        if mw_g is not None:
                            species_data['mol_weight'] = mw_g
                            source = 'Burcat*' # Modifed mol. weight

                if not species_data:
                    warnings.warn("no definition found for species '%s' phase '%s'" % (species, phase))
                    species_data = {
                        'alias' : alias,
                        'source': 'Auto',
                        'phase': phase,
                        'mol_weight': mw_g or 0,
                        'h_f': 0.0,
                        'tmin':  200.0,
                        'tmax': 6000.0,
                        'a_high': [0.0]*7,
                        'a_low': [0.0]*7}


                # Species/alias unification!
                if species != alias:
                    self.gui.print_internal("Renaming %s to %s" % (species, alias), font="Monospace")
                    self.gui.set_unsaved_flag()
                    self.thermo_data.pop(species, None)
                    species = alias # Create a new species, so we can override mol. weight, etc
                    self.gui.update_keyword('species_g', species, args=[g.ind])

                self.gui.fluid_species[species] = species_data

            self.update_thermo_data(self.gui.fluid_species)

            # Build the 'solids' dict that the gui expects.
            #  Note that this is derived from the project.solids SolidsCollection
            #  but is slightly different.
            default_solids_model = ("DEM" if self.solver==DEM
                                   else "TFM" if self.solver==TFM
                                   else "PIC" if self.solver==PIC
                                   else "TFM")

            for s in self.solids:

                def sget(key):
                    x = s.get(key)
                    return x.value if x else None

                name = s.name
                model = sget('solids_model')
                diameter = sget('d_p0')
                density = sget('ro_s0')

                if not model:
                    # Make sure all solids have a defined model, even if its just the default
                    model = default_solids_model
                    self.updateKeyword('solids_model', model, args=[s.ind])

                solids_data =  {'model': model,
                                'diameter': diameter,
                                'density': density}

                self.gui.solids[name] = solids_data

                self.gui.solids_species[s.ind] = OrderedDict()
                species = list(s.species)
                species.sort(key=lambda a:a.ind)

                for (i,sp) in enumerate(species,1):
                    # First look for definition in THERMO DATA section
                    phase = sp.phase.upper() # phase and species_g are guaranteed to be set
                    species = sp.get('species_s')
                    source = "User Defined"
                    if species is None:
                        species = 'Solid %s' % sp.ind
                        source = "Auto"
                        #warnings.warn("no species_s for solid %d" % sp.ind)
                    alias = sp.get('species_alias_s', species)
                    mw_s = sp.get('mw_s', None)
                    density = sp.get('ro_xs0',None)
                    # Note, we're going to unset mw_s and migrate it into THERMO DATA

                    # TODO:  make sure alias is set & unique
                    user_def = user_species.get((species, phase))
                    # Hack, look for mismatched phase
                    if not user_def:
                        for ((s1,p), v) in user_species.items():
                            if s1 == species:
                                # This is all-too-common.  existing mfix files all have 'S' for
                                # phase in thermo data.
                                #warnings.warn("species '%s' defined as phase '%s', expected '%s'"
                                #              % (species, p, phase))
                                user_def = v
                                break
                    if user_def:
                        (tmin, tmax, mol_weight, coeffs, comment) = user_def
                        if tmin == tmax == 0.0:
                            tmin, tmax = 200.0, 6000.0
                        if mw_s is not None:
                            mol_weight = mw_s # mw_s overrides values in THERMO DATA
                        species_data = {'source': source,
                                        'phase': phase,
                                        'alias': alias,
                                        'mol_weight': mol_weight,
                                        'h_f': coeffs[14],
                                        'tmin': tmin,
                                        'tmax': tmax,
                                        'a_high': coeffs[:7],
                                        'a_low': coeffs[7:14]}

                    else:
                        # get this from the species popup so we don't have to load
                        # another copy of the database.  (the database is owned by
                        # the species popup.
                        species_data = self.gui.species_popup.get_species_data(species, phase)
                        if not species_data:
                            # Look for mismatched phase definition
                            # This is not really 'mismatched'.  Solids phase may contain fluid and v.v.
                            for p in 'SCLG':
                                if p == phase:
                                    continue
                                species_data = self.gui.species_popup.get_species_data(species, p)
                                if species_data:
                                    #warnings.warn("species '%s' defined as phase '%s', expected '%s'"
                                    #              % (species, p, phase))
                                    log.info("species '%s' defined as phase '%s', expected '%s'"
                                             % (species, p, phase))
                                    break
                        if species_data:
                            species_data['alias'] = alias
                            if mw_s is not None:
                                species_data['mol_weight'] = mw_s
                                source = 'Burcat*' # Modifed mol. weight

                    if not species_data:
                        warnings.warn("no definition found for species '%s' phase '%s'" % (species, phase))
                        species_data = {
                            'alias' : alias,
                            'source': 'Auto',
                            'phase': phase,
                            'mol_weight': mw_s or 0,
                            'h_f': 0.0,
                            'tmin':  200.0,
                            'tmax': 6000.0,
                            'a_high': [0.0]*7,
                            'a_low': [0.0]*7}

                    species_data['density'] = density

                    # Species/alias unification!
                    if species != alias:
                        self.gui.print_internal("Renaming %s to %s" % (species, alias), font="Monospace") #log?
                        self.gui.set_unsaved_flag()
                        self.thermo_data.pop(species, None)
                        species = alias # Create a new species, so we can override mol. weight, etc
                        self.gui.update_keyword('species_s', species, args=[s.ind, i])

                    self.gui.solids_species[s.ind][species] = species_data

                self.update_thermo_data(self.gui.solids_species[s.ind])

            # Make sure mmax is set
            self.gui.update_keyword('mmax', len(self.gui.solids_species))

            thermo_keys = set(['mw_g', 'mw_s'])
            vector_keys = set(['des_en_input', 'des_en_wall_input',
                        'des_et_input', 'des_et_wall_input'])

            # issues/142: set ic_ep_s from ic_ep_g
            # Volume fraction may be inferred from BC_EP_G resp IC_EP_G
            # Only valid for one solids phase (MMAX=1)
            #  or if IC_EP_G == 1.0, when all IC_EP_S must be 0
            for IC in range(1, len(self.ics)+1):
                ic_ep_g = self.get_value('ic_ep_g', args=[IC])
                if ic_ep_g == 1.0:
                    for P in range(1, len(self.solids)+1):
                        self.gui.update_keyword('ic_ep_s', 0.0, args=[IC,P])
                elif len(self.solids) == 1:
                    ic_ep_s = self.get_value('ic_ep_s', args=[IC,1])
                    if ic_ep_s is None and ic_ep_g is not None:
                        # For files saved by the gui, this should not happen,
                        # we explicitly save all ic_ep keys
                        val = round(1.0-ic_ep_g, 10)
                        self.gui.update_keyword('ic_ep_s', val, args=[IC,1])

            # same for bc_ep_s
            for BC in range(1, len(self.bcs)+1):
                bc_ep_g = self.get_value('bc_ep_g', args=[BC])
                if bc_ep_g == 1.0:
                    for P in range(1, len(self.solids)+1):
                        self.gui.update_keyword('bc_ep_s', 0.0, args=[BC,P])
                elif len(self.solids) == 1:
                    bc_ep_s = self.get_value('bc_ep_s', args=[BC,1])
                    if bc_ep_s is None and bc_ep_g is not None:
                        val = round(1.0-bc_ep_g, 10)
                        self.gui.update_keyword('bc_ep_s', val, args=[BC,1])

            # issues/149 - don't save nodes[ijk] in file, pass on cmdline
            #skipped_keys = set(['nodesi', 'nodesj', 'nodesk'])
            # reconsidering this decision since it causes a lot of extra
            #  complexity, esp w.r.t.  restart


            # SRS: Many mfix.dat files may use a different specification
            # for VTK input. There will need to be a way of catching
            # the 'old' format and converting it to this input style.
            #  Note - maybe this doesn't belong here.  But we have to
            #   do it somewhere.
            # following values are extracted from mfix_user_guide
            varlist_map = {
                1:    ('ep_g',),                 #  Void fraction (EP_g)
                2:    ('p_g', 'p_star'),         #  Gas pressure, solids pressure
                3:    ('u_g', 'v_g', 'w_g'),     #  Gas velocity
                4:    ('u_s', 'v_s', 'w_s'),     #  Solids velocity
                5:    ('rop_s',),                #  Solids density
                6:    ('t_g', 't_s'),            #  Gas and solids temperature
                7:    ('x_g', 'x_s'),            #  Gas and solids mass fractions
                8:    ('theta_m',),              #  Granular temperature
                9:    ('scalar',),               #  Scalar
                10:   ('rrate',),                #  Reaction rates
                11:   ('k_turb_g', 'e_turb_g'),  #  K and Epsilon
                12:   ('vorticity', 'lambda_2'), #  Vorticity magnitude and lambda_2
                100:  ('partition',),            #  Grid Partition
                101:  ('bc_id',),                #  Boundary Condition ID
                102:  ('dwall',),                #  Distance to wall
                103:  ('facet_count_des',),      #  DEM facet count
                104:  ('nb_facet_des',),         #  DEM Neighboring facets
                999:  ('ijk',),                  #  Cell IJK index
                1000: ('normal',)                #  Cut face normal vector
                }
            key ='vtk_varlist'
            vtk_varlist = [(args[0], self.get_value(key, args=args))
                            for args in self.get_key_indices(key)]
            key = 'vtk_var'
            vtk_var = [self.get_value(key, args=args)
                       for args in self.get_key_indices(key)]

            if vtk_var:
                # We need to create a region for the vtk output region, default index=1
                idx = 1
                # We don't expect vtk_var and vtk_varlist to both be set, but
                #  it's not explicitly forbidden
                if vtk_varlist:
                    idx = 1 + max(v for (v,i) in vtk_varlist)

                vtk_varlist += [(idx,x) for x in vtk_var]

                no_k = self.get_value('no_k')
                for (key, val) in zip(('x_w', 'y_s', 'z_b',
                                       'x_e', 'y_n', 'z_t'),
                                      ('xmin', 'ymin', 'zmin',
                                       'xmax', 'ymax', 'zmax')):
                    if no_k and key[0]=='z':
                        continue
                    self.gui.update_keyword('vtk_'+key, Equation(val), args=[idx])

                # more keys
                self.gui.update_keyword('vtk_filebase', 'IC_1', args=[idx])
                for key in [ 'vtk_nxs', 'vtk_nys', 'vtk_nzs']:
                    self.gui.update_keyword(key, 0, args=[idx])

            for (v,i) in vtk_varlist:
                varnames = varlist_map.get(i)
                if not varnames:
                    warnings.warn("unknown vtk_var %s" % i)
                mmax = len(self.gui.solids)
                for vn in varnames:
                    key = 'vtk_' + vn
                    args = keyword_args.get(key)
                    if args == ('vtk',):
                        self.gui.update_keyword(key, True, args=[v])
                    elif args == ('vtk', 'species'): # Fluid
                        for s in range(1, 1+len(self.gui.fluid_species)):
                            self.gui.update_keyword(key, True, args=[v, s])
                    elif args == ('vtk', 'phase'):
                        for p in range(1, 1+mmax):
                            self.gui.update_keyword(key, True, args=[v, p])
                    elif args == ('vtk', 'phase', 'species'):
                        for p in range(1, 1+mmax):
                            for s in range(1, 1+len(self.gui.solids_species.get(p,[]))):
                                self.gui.update_keyword(key, True, args=[v, p, s])
                    elif args == ('vtk', 'scalar'):
                        for s in range(1, 1+self.get_value('nscalar', default=0)):
                            self.gui.update_keyword(key, True, args=[v, s])
                    elif args == ('vtk', 'reaction'):
                        for r in range(1, 1+self.get_value('nrr', default=0)):
                            self.gui.update_keyword(key, True, args=[v, r])
                    else:
                        warnings.warn('unhandled key %s' % key)

            for key in ('vtk_var', 'vtk_varlist'):
                for args in self.get_key_indices(key):
                    self.gui.unset_keyword(key, args=args)


            # Compute BC_EP_S from BC_ROP_S if present, same for IC

            # Some input decks may or may not contain BC_EP_S keyword:
            for prefix in 'bc_', 'ic_':
                key = prefix + 'rop_s'
                for args in self.get_key_indices(key):
                    if len(args) != 2:
                        warnings.warn('invalid key %s' %
                                      format_key_with_args(key, args))
                        self.gui.unset_keyword(key, args=args)
                        continue
                    ep_s_key = prefix + 'ep_s'
                    if self.get_value(ep_s_key, args=args) is not None:
                        warnings.warn('%s and %s both set' % (
                            format_key_with_args(key, args),
                            format_key_with_args(ep_s_key, args)))
                        self.gui.unset_keyword(key, args=args) #? remove redundant key
                        continue

                    # Volume fraction is specified using the solids bulk density
                    # BC_EP_S(#,#) == BC_ROP_S(#,#) / BC_ROs(#)

                    # Solids density BC_ROs is determined by the solids density
                    # model. For constant solids density, use RO_S0.
                    rop = self.get_value(key, args=args)
                    self.gui.unset_keyword(key, args=args) # Don't keep redundant key defined
                    N, M = args
                    ro = self.get_value('ro_s0', args=[M])
                    if ro is None:  # Variable solids density
                        # SRS Appendix E
                        #Calculating the solids density is done in the context an
                        # initial condition (IC) region, but it is the same calculations
                        # hold for boundary conditions (BCs).
                        #    Keyword RO_S0(#) must be undefined
                        #    Requires material densities for all solids species, RO_XS0(#,#)
                        nmax_s = self.get_value('nmax_s', args=[M], default=0)
                        if nmax_s == 0:
                            warnings.warn('no species for phase %s' % M)
                            continue
                        ro_xs0 = [self.get_value(prefix+'ro_xs0', args=[M,S])
                              for S in range(1, 1+nmax_s)]
                        if None in ro_xs0:
                            warnings.warn('no density (ro_xs0) for phase %s species %s' %
                                          (M, 1+ro_xs0.index(None)))
                            continue
                        #    Requires base composition be specified, X_S0(#,#)
                        x_s0 = [self.get_value('x_s0', args=[M,S], default=0.0)
                                for S in range(1, 1+nmax_s)]
                        #if None in x_s0:
                        #    warnings.warn('no volume fraction (xs0) for phase %s species %s' %
                        #                  (M, 1+xs0.index(None)))
                        #    continue

                        #    Requires an inert species be identified, INERT_SPECIES(#)
                        I = self.get_value('inert_species', args=[M])
                        if I is None:
                            warnings.warn('no inert species for phase %s' % M)
                            continue

                        #1) Calculate the baseline density,
                        # RO_S0(M) = 1.0/sumk(X_S0(M,K)/RO_XS0(M,K))
                        ro_s0 = 1.0 / sum(a/b for (a,b) in zip(x_s0, ro_xs0))

                        #2) Calculate the solids density:
                        #a. Let I=INERT_SPECIES(M)
                        #b. IC_RO_S(M) = RO_S0(M) * X_S0(M,I)/IC_X_S(#,M,I)
                        x_s = self.get_value(prefix+'x_s', args=[N,M,I], default=0.0)
                        try:
                            ro = ro_s0 * x_s0[I-1] / x_s # I-1 because of 0-based indexing
                        except Exception as e:
                            excs.append(e)
                            continue
                    try:
                        ep = round(rop / ro, 10)

                    except Exception as e:
                        excs.append(e)
                        continue
                    self.gui.update_keyword(ep_s_key, ep, args=args)

            # Submit all remaining keyword updates, except the ones we're skipping
            # some of these changes may cause new keywords to be instantiated,
            # so iterate over a copy of the list, which may change
            kwlist = list(self.keywordItems())
            for kw in kwlist:

                if kw.key == 'gravity': # convert scalar 'gravity' to x-y-z vector
                    y_val = -kw.value  # minus - gravity points down along y-axis
                    self.gui.unset_keyword('gravity')
                    for axis in 'xz':
                        self.submit_change(None, {'gravity_%s'%axis: 0.0}, args=None)
                    self.submit_change(None, {'gravity_y': y_val}, args=None)
                    continue

                # convert [XYZ]LENGTH to [XYZ]_MIN and [XYZ]_MAX (issues/238)
                if kw.key in ['xlength', 'ylength', 'zlength']:
                    self.gui.unset_keyword(kw.key)
                    axis = kw.key[0]
                    val = self.get_value('%s_min'%axis, 0)
                    self.submit_change(None, {'%s_min'%axis: val, '%s_max'%axis: kw.value}, args=None)
                    continue

                if kw.key in thermo_keys:
                    self.gui.print_internal("%s=%s moved to THERMO DATA section" % (
                        format_key_with_args(kw.key, kw.args),
                        kw.value))
                    self.gui.unset_keyword(kw.key, args=kw.args) # print msg in window
                    continue

                if kw.key in vector_keys: # Make sure they are really vectors
                    if not kw.args:
                        self.gui.unset_keyword(kw.key)
                        kw.args = [1]

                try:
                    self.submit_change(None, {kw.key: kw.value}, args=kw.args)
                except ValueError as e:
                    excs.append(e)

        return (units_converted, excs, ws)


    def convert_particle_file_units(self, filename):
        basename = os.path.basename(filename)
        cgs_file = filename + '.cgs'
        if os.path.exists(cgs_file):
            # If we've converted before, use the '.cgs' file as input to avoid
            # double-converting
            warnings.warn('%s found, not converting particle data (already converted?)' % cgs_file)
            return
        else:
            warnings.warn('%s saved as %s' % (basename, basename+'.cgs')) # info?
            os.rename(filename, cgs_file)
        lineno = 0
        rdmn = None
        with open(filename, 'w', encoding='utf-8', errors='ignore') as out_file:
            for line in open(cgs_file, encoding='utf-8', errors='ignore'):
                lineno += 1
                line = line.strip()
                if not line: # blank
                    continue
                line = line.replace('d', 'e').replace('D', 'E')
                tok = line.split()
                # from read_particle_input.f
                #! In distributed IO the first line of the file will be number of
                #! particles in that processor
                if lineno == 1 and len(tok) == 1:
                    out_line = line # Pass through
                else:
                    # read (lunit,*,IOSTAT=IOS)                               &
                    # (dpar_pos(lcurpar,k),k=1,RDMN),dpar_rad(lcurpar),       &
                    # dpar_den(lcurpar),(dpar_vel(lcurpar,k),k=1,RDMN)
                    if len(tok) == 6: # 2D
                        if rdmn is None:
                            rdmn = 2
                        elif rdmn != 2: # Changed mid-file
                            warnings.warn('%s: bad line %s' % (filename, lineno))
                            break
                    elif len(tok) == 8: # 3d
                        if rdmn is None:
                            rdmn = 3
                        elif rdmn != 3: # Changed mid-file
                            warnings.warn('%s: bad line %s' % (filename, lineno))
                            break
                    else:
                        warnings.warn('%s: bad line %s' % (filename, lineno))
                        break
                    try:
                        vals = list(map(float, tok))
                    except ValueError:
                        warnings.warn('%s: bad value on line %s' % (filename, lineno))
                        break

                    factors = ([0.01] * rdmn +  # position, cm -> m
                               [0.01] +         # radius,   cm -> m
                               [1000] +         # density,  g/cm^3 -> kg/m^3
                               [0.01] * rdmn)   # velocity

                    out_line = ''.join('%-16s'%round((v*f),12) for (f,v) in zip(vals, factors))

                out_file.write(out_line.strip()+'\n')

    def convert_polygon_file_units(self, filename):
        basename = os.path.basename(filename)
        cgs_file = filename + '.cgs'
        if os.path.exists(cgs_file):
            # If we've converted before, use the '.cgs' file as input to avoid
            # double-converting
            warnings.warn('%s found, not converting particle data (already converted?)' % cgs_file)
            return
        else:
            warnings.warn('%s saved as %s' % (basename, basename+'.cgs')) # info?
            os.rename(filename, cgs_file)
        lineno = 0
        with open(filename, 'w', encoding='utf-8', errors='replace') as out_file:
            for line in open(cgs_file, encoding='utf-8', errors='replace'):
                lineno += 1
                line = line.strip()
                if not line: # blank
                    continue
                line = line.replace('d', 'e').replace('D', 'E')
                tok = line.split()
                # data starts on line 13
                if lineno <= 13:
                    out_line = line # Pass through
                # line 14 should be the number of polygons
                elif lineno == 14 and len(tok) == 1:
                    out_line = line # Pass through
                elif len(tok) == 2: # new poly def, n verticies, sign
                    out_line = line # Pass through
                else:
                    if len(tok) != 3: # vertex, x y bc_id
                        warnings.warn('%s: bad line %s' % (filename, lineno))
                        break
                    try:
                        vals = list(map(float, tok))
                    except ValueError:
                        warnings.warn('%s: bad value on line %s' % (filename, lineno))
                        break
                    factors = [0.01] * 2 # position, cm -> m


                    out_line = ''.join('%-16s'%round((v*f),12) for (f,v) in zip(vals, factors))
                    out_line += str(int(vals[-1]))
                out_file.write(out_line.strip()+'\n')

    def convert_stl_file_units(self, filename):
        '''convert an stl file in cm to m
        stl ASCII format:
        solid name
        facet normal ni nj nk
            outer loop
                vertex v1x v1y v1z
                vertex v2x v2y v2z
                vertex v3x v3y v3z
            endloop
        endfacet
        endsolid name
        '''

        # TODO: catch and report errors, handle binary STL

        basename = os.path.basename(filename)
        cgs_file = filename + '.cgs'
        if os.path.exists(cgs_file):
            # If we've converted before, use the '.cgs' file as input to avoid
            # double-converting
            warnings.warn('%s found, not converting stl file (already converted?)' % cgs_file)
            return
        else:
            warnings.warn('%s saved as %s' % (basename, basename+'.cgs')) # info?
            os.rename(filename, cgs_file)

        def cm_to_m_str(s):
            '''attempt converting string value s from cgs to SI, returning original
            string if not a float'''
            try:
                return "%.10e" % (float(s)/100.0) # Why do we need 10 digits?
            except ValueError:
                return s


        with open(filename, 'w', encoding='utf-8', errors='replace') as out_file:
            for line in open(cgs_file, 'r', encoding='utf-8', errors='replace'):
                leading_spaces = len(line) - len(line.lstrip(' '))
                # convert floats to SI
                vals = [cm_to_m_str(t) for t in line.strip().split()]
                out_file.write(' '.join([' '*leading_spaces]+vals) + '\n')


    def register_widget(self, widget, keys=None, args=None, force=False):
        """ Register a widget with the project manager. The widget must have a
        value_updated signal to connect to.  If args is not None, widget will
        be updated only when the keyword with matching args is updated.  If
        args=['*'], widget recieves updates regardless of args.

        Special args:  'P' will be substituted with the currently selected
                        solids phase (the one the user is editing)
                       '*' : described above
                       'IC' : current initial condition index
                       'BC' : current initial condition index
                       'PS' : current point source index
                       'IS' : current internal surface index
                       'VTK':  VTK output region index
                       'MONITOR': Monitor region index
        """

        # Species? Scalar index?

        if isinstance(args, int):
            args = [args]
        elif args is None:
            args = []

        # add widget to dictionary of widgets to update
        d = self.registered_widgets
        for key in keys:
            key = key.lower()
            # Note 1: The BC/IC/PS panes set themselves up and do not rely on the
            # project manager for widget updates.  We still need to register
            # these widgets so that keyword updating works.
            # Note 2: Nost widgets don't really need the callback from the
            # project manager, especially after the file is initially loaded.
            # Maybe remove project manager -> widget callbacks completely
            # and follow the IC/BC model everywhere ?
            if not force and any(key.startswith(x) for x in ('ic_', 'bc_', 'ps_', 'is_', 'vtk_', 'monitor_')):
                continue
            if key not in d:
                d[key] = []
            d[key].append((args, widget))

        if hasattr(widget, 'value_updated'):
            widget.value_updated.connect(self.submit_change)

        self.registered_keywords = self.registered_keywords.union(set(keys))
        # TODO:  check args vs keyword_args

    def unregister_widget(self, widget):
        key = getattr(widget, 'key', None)
        if key is None:
            return
        try:
            widget.value_updated.disconnect()
        except Exception as e:
            self.error("%s: widget=%s key=%s" % (str(e), widget.objectName(), key))

        updates = self.registered_widgets.get(key, [])
        updates = [(a,w) for (a,w) in updates if w != widget]

        if updates:
            self.registered_widgets[key] = updates
        else:
            if key in self.registered_widgets:
                del self.registered_widgets[key]
            if key in self.registered_keywords:
                self.registered_keywords.remove(key)


    def update_thermo_data(self, species_dict):
        """Update definitions in self.thermo_data based on data in species_dict."""

        update_dict = dict((species, format_burcat(species, data))
                           for (species, data) in species_dict.items())

        changed = any(self.thermo_data.get(k) != update_dict[k] for k in update_dict)

        if changed:
            log.info("Update THERMO_DATA")
            self.thermo_data.update(update_dict)
            self.gui.set_unsaved_flag()


    def update_parameters(self, params):
        """parameter values have changed, loop through and update"""
        for p in params:
            if p in self.parameter_key_map:
                key_args = self.parameter_key_map[p]
                for key_arg in key_args:
                    key, args = parse_key_with_args(key_arg)
                    value = self.get_value(key, args=args)
                    self.change(self, key, value, args=args)


    def objectName(self):
        return 'Project Manager'


def format_burcat(species, data):
    """Return a list of lines in BURCAT.THR format"""

    lines = []
    calc_quality = 'B' # This appears in column 68, valid values are A-F.
                       # We'll just assign 'B' to fill the space, it's the
                       # most common value.
    # First line
    # TODO: Would be  nice to put the Burcat species name in the comment field, if available
    row = (species, 'User Defined', data['phase'].upper(),
           data['tmin'], data['tmax'],
           calc_quality, data['mol_weight'])
    lines.append('%-24s%-18s0.%c%10.3f%10.3f%3s%10.5f 1' % row)

    # remaining 3 lines
    fmt = '%15.8E' * 5
    a_high = data['a_high']
    a_low = data['a_low']
    row = tuple(a_high[:5])
    lines.append( (fmt%row) + '    2')
    row = tuple(a_high[5:7] + a_low[:3])
    lines.append( (fmt%row) + '    3')
    row = tuple(a_low[3:] + [data['h_f']])
    lines.append( (fmt%row) + '    4')
    #lines.append('') # blank line
    return lines
