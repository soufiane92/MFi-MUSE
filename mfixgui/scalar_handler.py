from mfixgui.tools import keyword_args

class ScalarHandler:
    def init_scalar_handler(self):
        pass

    def reset_scalars(self):
        pass

    def set_nscalar_phase(self, val, phase):
        if phase is None:
            # When no solid is selected, spinbox is zeroed, leading to a callback
            # Nothing to do
            return
        prev = self.get_nscalar_phase(phase)
        if val == prev: # Nothing to do
            return
        # Compute new nscalar
        nscalar = val + sum(self.get_nscalar_phase(P)
                            for P in range(0, 1+len(self.solids)) if P!=phase)
        self.update_keyword('nscalar', nscalar)
        if val < prev:
            start_index = 1 + val + sum(self.get_nscalar_phase(P) for P in range(0, phase))
            self.scalar_delete_indices(start_index, prev-val)
        elif val > prev:
            start_index = 1 + prev + sum(self.get_nscalar_phase(P) for P in range(0, phase))
            self.scalar_insert_indices(start_index, val-prev)
            for i in range(start_index, start_index + val-prev):
                self.update_keyword('phase4scalar', phase, args=[i])
            # There's probably more initializing of IC_/BC_ keys that needs to be done here
        # ICs enabled/disabled depends on nscalar
        self.update_nav_tree()


    def get_nscalar_phase(self, phase):
        nscalar = self.project.get_value('nscalar', default=0)
        nscalar_phase = sum(1 for i in range(1, nscalar+1)
                            if self.project.get_value('phase4scalar', args=i) == phase)
        return nscalar_phase


    def scalar_delete_indices(self, start_index, num_indices=1):
        """Delete all keywords associated with specified scalar index (1-based),
        fixing up the resulting gap in sequence"""
        # We assume that nscalar is already decremented
        nscalar = self.project.get_value('nscalar', default=0)
        prev_size = nscalar + num_indices # Size before deletion
        for key in keyword_args.keys_by_type['scalar']:
            arg_types = keyword_args.keyword_args[key]
            indices = self.project.get_key_indices(key)
            if not indices:
                continue

            if arg_types == ('scalar',):
                # Copy/slide/trim
                vals = [self.project.get_value(key, args=i)
                        for i in range(1, 1+prev_size)]
                del vals[start_index-1:start_index+num_indices-1] # Slide (1-based)
                for (i, val) in enumerate(vals, 1):
                    self.update_keyword(key, val, args=i)
                for i in range(nscalar+1, prev_size+1):
                    self.unset_keyword(key, args=[i]) #Trim off the end

            else:
                scalar_pos = arg_types.index('scalar')
                new_vals = {}
                for args in indices:
                    args_scalar = args[scalar_pos]
                    if args_scalar < start_index: # Unaffected
                        continue
                    if start_index <= args_scalar <= start_index + num_indices:
                        continue # skip the scalars we're deleting
                    new_args = list(args)
                    if args_scalar > start_index + num_indices:
                        new_args[scalar_pos] -= num_indices #Slide along 'scalar' axis
                    new_vals[tuple(new_args)] = self.project.get_value(key, args=args)
                for (args, val) in new_vals.items():
                    self.update_keyword(key, val, args=args)
                for args in indices: # Trim
                    key_scalar = args[scalar_pos]
                    if key_scalar > nscalar:
                        self.unset_keyword(key, args)
        # Deal with memoized BC scalar_eq_type data
        # This should perhaps be done in bcs.py since it depends on BC internals
        for (region, data) in self.bcs.items():
            memo_dict = data.get('scalar_eq_type')
            if memo_dict:
                for (index, eq_type) in list(memo_dict.items()): # Going to modify dict
                    if index < start_index: # Unaffected
                        continue
                    if index > start_index+num_indices:
                        memo_dict[index-num_indices] = eq_type
                    del memo_dict[index]


    def scalar_insert_indices(self, start_index, num_indices=1):
        """Re-index keywords which depend on a scalar index after adjusting
        number of scalar equations.  Dual to scalar_delete_indices"""

        # We assume that nscalar is already incremented
        nscalar = self.project.get_value('nscalar', default=0)
        for key in keyword_args.keys_by_type['scalar']:
            arg_types = keyword_args.keyword_args[key]
            indices = self.project.get_key_indices(key)
            if not indices:
                continue

            if arg_types == ('scalar',):
                for i in range(nscalar, start_index-1, -1):
                    # Bump index up by num_indices
                    val = self.project.get_value(key, args=[i])
                    self.update_keyword(key, val, args=[i+num_indices])
                # Clear out the new range.  Special-case 'phase4scalar' since
                # that will get set right away
                if key != 'phase4scalar':
                    for i in range(start_index, start_index+num_indices):
                        self.unset_keyword(key, args=[i])

            else:
                scalar_pos = arg_types.index('scalar')
                new_vals = {}
                for args in indices:
                    args_scalar = args[scalar_pos]
                    if args_scalar < start_index: # Unaffected by insertion
                        continue
                    new_args = list(args)
                    new_args[scalar_pos] += num_indices #Slide along 'scalar_pos' axis
                    new_vals[tuple(new_args)] = self.project.get_value(key, args=args)
                for (args, val) in new_vals.items():
                    self.update_keyword(key, val, args=args)
                for args in indices: # Trim
                    args_scalar = args[scalar_pos]
                    if start_index <= args_scalar < start_index+num_indices:
                        self.unset_keyword(key, args)

        # Deal with memoized BC scalar_eq_type data
        # This should perhaps be done in bcs.py since it depends on BC internals
        for (region, data) in self.bcs.items():
            memo_dict = data.get('scalar_eq_type')
            if memo_dict:
                for (index, eq_type) in list(memo_dict.items()): # Going to modify dict
                    if index >= start_index:
                        memo_dict[index+num_indices] = eq_type
                        del memo_dict[index]


    def dump_scalars(self): # Diagnostic
        nscalar = self.project.get_value('nscalar', default=0)
        print("nscalar=%s"% nscalar)
        for P in range(0, 1+len(self.solids)):
            print("Phase %s scalars=%s" %(P, self.get_nscalar_phase(P)))
        for i in range(1, 1+nscalar):
            print('phase4scalar(%s) = %s' % (i, self.project.get_value('phase4scalar', args=[i])))
