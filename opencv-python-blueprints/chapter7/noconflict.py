#<noconflict.py>

metadic={}

def _generatemetaclass(bases,metas,priority):
    trivial=lambda m: sum([issubclass(M,m) for M in metas],m is type)
    # hackish!! m is trivial if it is 'type' or, in the case explicit
    # metaclasses are given, if it is a superclass of at least one of them
    metabs=tuple([mb for mb in map(type,bases) if not trivial(mb)])
    metabases=(metabs+metas, metas+metabs)[priority]
    if metabases in metadic: # already generated metaclass
        return metadic[metabases]
    elif not metabases: # trivial metabase
        meta=type
    elif len(metabases)==1: # single metabase
        meta=metabases[0]
    else: # multiple metabases
        metaname="_"+''.join([m.__name__ for m in metabases])
        meta=makecls()(metaname,metabases,{})
    return metadic.setdefault(metabases,meta)

def makecls(*metas,**options):
    """Class factory avoiding metatype conflicts. The invocation syntax is
    makecls(M1,M2,..,priority=1)(name,bases,dic). If the base classes have
    metaclasses conflicting within themselves or with the given metaclasses,
    it automatically generates a compatible metaclass and instantiate it.
    If priority is True, the given metaclasses have priority over the
    bases' metaclasses"""

    priority=options.get('priority',False) # default, no priority
    return lambda n,b,d: _generatemetaclass(b,metas,priority)(n,b,d)

#</noconflict.py>


# import inspect
# import types
# import __builtin__

# ############## preliminary: two utility functions #####################

# def skip_redundant(iterable, skipset=None):
# 	"Redundant items are repeated items or items in the original skipset."
# 	if skipset is None:
# 		skipset = set()
# 	for item in iterable:
# 		if item not in skipset:
# 			skipset.add(item)
# 			yield item

# def remove_redundant(metaclasses):
# 	skipset = set([types.ClassType])
# 	for meta in metaclasses:  # determines the metaclasses to be skipped
# 		skipset.update(inspect.getmro(meta)[1:])
# 	return tuple(skip_redundant(metaclasses, skipset))

# ##################################################################
# ## now the core of the module: two mutually recursive functions ##
# ##################################################################

# memoized_metaclasses_map = {}

# def get_noconflict_metaclass(bases, left_metas, right_metas):
# 	"""Not intended to be used outside of this module, unless you know
# 	what you are doing."""
# 	# make tuple of needed metaclasses in specified priority order
# 	metas = left_metas + tuple(map(type, bases)) + right_metas
# 	needed_metas = remove_redundant(metas)

# 	# return existing confict-solving meta, if any
# 	if needed_metas in memoized_metaclasses_map:
# 		return memoized_metaclasses_map[needed_metas]
# 	# nope: compute, memoize and return needed conflict-solving meta
# 	elif not needed_metas:         # wee, a trivial case, happy us
# 		meta = type
# 	elif len(needed_metas) == 1:  # another trivial case
# 		meta = needed_metas[0]
# 		# check for recursion, can happen i.e. for Zope ExtensionClasses
# 	elif needed_metas == bases:
# 		raise TypeError("Incompatible root metatypes", needed_metas)
# 	else:  # gotta work ...
# 		metaname = '_' + ''.join([m.__name__ for m in needed_metas])
# 		meta = classmaker()(metaname, needed_metas, {})
# 		memoized_metaclasses_map[needed_metas] = meta
# 		return meta

# def classmaker(left_metas=(), right_metas=()):
# 	def make_class(name, bases, adict):
# 		metaclass = get_noconflict_metaclass(
# 				bases, left_metas, right_metas)
# 		return metaclass(name, bases, adict)
# 	return make_class
