# Common to fluid and solid phases


class SpeciesHandler(object):
    def species_all_aliases(self):
        for (name, data) in self.fluid_species.items():
            yield data.get('alias', name)
        for phase in self.solids_species.values():
            for (name, data) in phase.items():
                yield data.get('alias', name)

    def species_alias_unique(self, alias):
        alias = alias.lower()
        for a in self.species_all_aliases():
            if a.lower() == alias.lower():
                return False
        return True


    def species_make_alias(self, species):
        #Aliases must be unique.
        #Aliases are limited to 32 characters and must follow FORTRAN variable
        #naming conventions (i.e., alphanumeric combinations with a letter as the first
        #character).
        #Aliases are not case sensitive.
        #Aliases cannot conflict with existing MFiX variable names (e.g., a species
        # alias of MU_g will cause an error when compiling MFiX).

        alias = species.replace(' ', '_')
        alias = alias.replace('(', '_')
        alias = alias.replace(')', '')
        alias = alias.replace('__', '_')
        alias = ''.join([c for c in alias if c.isalnum() or c=='_'])

        while alias and not alias[0].isalpha(): # strip leading _ and digits
            alias = alias[1:]

        if len(alias) > 32:
            alias = alias[:32]

        if self.species_alias_unique(alias):
            return alias
        count = 1
        # Strip _nn suffix
        if '_' in alias:
            i = alias.rindex('_')
            if i > 0 and alias[i+1:].isdigit():
                count = 1 + int(alias[i+1:])
                alias = alias[:i]

        if len(alias) > 28: # Leave space for _nn
            alias = alias[:28]
        while True:
            alias = '%s_%s' % (alias, count) # Prefer underscore so we don't get things like H2O2
            if self.species_alias_unique(alias):
                return alias
            count += 1


    def find_species_phase(self, species):
        # case insensitive
        slower= species.lower()
        for p in range(0, 1+len(self.solids)):
            for s in self.species_of_phase(p):
                if s.lower() == slower:
                    return p
        self.warning("Species %s is not associated with any phase" % species)


    def species_mol_weight(self, species):
        if species in self.fluid_species:
            return self.fluid_species[species].get('mol_weight')
        for (p, subdict) in self.solids_species.items():
            if species in subdict:
                return subdict[species].get('mol_weight')

        return None


    def species_of_phase(self, p):
        if p is None or p > len(self.solids):
            return []
        elif p == 0:
            return self.fluid_species.keys()
        else:
            return self.solids_species[p].keys()
