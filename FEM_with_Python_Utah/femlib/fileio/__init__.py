import numpy as np
from os.path import splitext, isfile
from collections import OrderedDict

import dbx
import exodus
from _netcdf import NetCDFFile

__all__ = ['File', 'loaddb_single_element', 'read_exodus_legacy']

def File(filename, mode='r'):
    if mode not in 'wr':
        raise ValueError('unknown File mode {0}'.format(mode))
    root, ext = splitext(filename)
    if ext in ('.e', '.exo'):
        return exodus.File(filename, mode=mode)
    elif ext in ('.dbx', '.base_dbx'):
        return dbx.File(filename, mode=mode)
    else:
        return tabular.File(filename, mode=mode)

def loaddb_single_element(filename, variables=None, disp=1, elem_num=1, blk_num=1):
    '''Read all field variables through time for elem_num'''

    root, ext = splitext(filename)
    if ext == '.base_exo':
        # punt and use old reader
        return read_exodus_legacy(filename, variables=variables,
                                  disp=disp, blk_num=blk_num, elem_num=elem_num)

    elif ext in ('.dbx', '.base_dbx'):
        fh = dbx.File(filename, mode='r')
    else:
        fh = exodus.File(filename, mode='r')

    fields = OrderedDict()
    times = []
    for step in fh.steps.values():
        for frame in step.frames:
            if times and abs(times[-1] - frame.value) < 1.e-14:
                continue
            times.append(frame.value)
            for fo in frame.field_outputs.values():
                keys, values = fo.get_data(element=elem_num)
                for (i, key) in enumerate(keys):
                    fields.setdefault(key, []).append(values[i])

    head = ['TIME'] + [x.upper() for x in fields.keys()]
    data = np.column_stack((np.asarray(times), np.array(fields.values()).T))

    if variables is not None:
        variables = [x.upper() for x in variables]
        idx = []
        for name in variables:
            try:
                idx.append(head.index(name))
            except IndexError:
                raise KeyError('{0} not in output database'.format(name))
        head = [head[i] for i in idx]
        data = data[:, idx]

    if disp:
        return head, data

    return data

def read_exodus_legacy(filename, variables=None, disp=1, blk_num=1, elem_num=1):
    '''Read the specified variables from the exodus file in filepath

    '''
    if not isfile(filename):
        raise IOError('{0}: no such file'.format(filename))

    fh = NetCDFFile(filename, 'r')

    # global/element vars and mapping
    num_glo_var = fh.dimensions.get('num_glo_var', 0)
    if num_glo_var:
        name_glo_var = exodus.stringify(fh.variables['name_glo_var'].data)
        gmap = dict(zip(name_glo_var, range(len(name_glo_var))))

    name_elem_var = exodus.stringify(fh.variables['name_elem_var'].data)
    emap = dict(zip(name_elem_var, range(len(name_elem_var))))

    # retrieve the data from the database
    head = ['TIME']
    if num_glo_var:
        head.extend([H.upper() for H in name_glo_var])
    head.extend([H.upper() for H in name_elem_var])

    data = []
    times = fh.variables['time_whole'].data.flatten()
    for (i, time) in enumerate(times):
        row = [time]
        if num_glo_var:
            vals_glo_var = fh.variables['vals_glo_var'].data[i]
            for var in name_glo_var:
                var_num = gmap[var]
                try: row.append(vals_glo_var[var_num])
                except KeyError: continue
        for var in name_elem_var:
            var_num = emap[var]+1
            name = 'vals_elem_var{0}eb{1}'.format(var_num, blk_num)
            row.append(fh.variables[name].data[i, elem_num-1])
        data.append(row)
    fh.close()
    data = np.asarray(data)
    if len(head) != data.shape[1]:
        raise ValueError('inconsistent data')

    data = np.array(data)

    if variables is not None:
        variables = [x.upper() for x in variables]
        idx = []
        for name in variables:
            try:
                idx.append(head.index(name))
            except IndexError:
                raise KeyError('{0} not in output database'.format(name))
        head = [head[i] for i in idx]
        data = data[:, idx]

    if disp:
        return head, data

    return data

def expand_var_names(master, slave):
    mstring = ' '.join(master)
    matches = []
    v = []
    def endsort(item):
        endings = {'_XX': 0, '_YY': 1, '_ZZ': 2,
                   '_XY': 3, '_YZ': 4, '_XZ': 5,
                   '_YX': 6, '_ZY': 7, '_ZX': 8,
                   '_X': 0, '_Y': 1, '_Z': 2}
        for (ending, order) in endings.items():
            if item.endswith(ending):
                return order
        return 9

    for i, name in enumerate(slave):
        if name in master:
            matches.append(name)
            slave[i] = None
            continue
        vt = []
        for match in re.findall(r'(?i)\b{0}_[XYZ]+'.format(name), mstring):
            vt.append(match.strip())
            slave[i] = None
            continue
        vt = sorted(vt, key=lambda x: endsort(x))
        matches.extend(vt)
    return matches
