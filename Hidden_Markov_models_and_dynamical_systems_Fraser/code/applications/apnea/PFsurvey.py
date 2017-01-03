""" PFsurvey.py

Takes 49 minutes per point

This file is part of HMM_DS_Code you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

"""
Copyright = '''Copyright 2005, 2008, 2012 Andrew M. Fraser, and 2013
Andrew M. Fraser and Los Alamos National Laboroatory

This file is part of hmmds3.

Hmmds3 is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Hmmds3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

See the file gpl.txt in the root directory of the hmmds3 distribution
or see <http://www.gnu.org/licenses/>.
'''
import sys

def read_records(report):
    '''Read the pass_1 report (specified by report[0]) and select record
    names, eg, "a01", for which the group name, eg, "High" matches
    report[1] and that have classification data (no "x" files).

    '''
    records = []
    for line in open(report[0], 'r'):
        parts = line.split()
        if parts[2] != report[1]:
            continue
        if parts[0].startswith('x'):
            continue
        records.append(parts[0])
    records.sort() # For easier reading and debugging
    return records
def main(argv=None):
    
    import argparse
    import pickle
    import numpy as np
    import ApOb
    if argv is None:                    # Usual case
        argv = sys.argv[1:]
    parser = argparse.ArgumentParser(description=
             '''Study classification performance on training data''')
    parser.add_argument('model',
                        help='A trained HMM for data with classification.')
    parser.add_argument('expert', type=str,
                       help='Path to file of expert annotations')
    parser.add_argument('hr_dir', type=str,
                       help='Path to low pass heart rate data files')
    parser.add_argument('resp_dir', type=str,
                       help='Path to respiration data files')
    parser.add_argument('report', type=str, nargs=2,
      help='Path to pass1_report and record group: "High", "Medium", or "Low"')
    parser.add_argument('power', type=float, nargs=3,
              help='from, to, step for study.  Suggest 0.5 2.65 0.1')
    parser.add_argument('fudge', type=float, nargs=3,
              help='from, to, step for study.  Suggest 0.8 1.61 .05')
    parser.add_argument('out', type=str,
                        help='Location to put result')
    args = parser.parse_args(argv)

    args.record = read_records(args.report)
    model = pickle.load(open(args.model, 'rb'))
    y_mod_0 = model.y_mod.y_mod
    n_seg, segs, data_class = model.y_mod.join(list(ApOb.build_data(
        model.y_mod, args).values()))
    n_y = len(data_class[0])
    data = data_class[1:]
    # data [class, hr, context, resp]
    s2c = model.y_mod.s2c
    out = open(args.out, 'w')
    for power in np.arange(*args.power):
        for fudge in np.arange(*args.fudge):
            model.y_mod.y_mod = ApOb.fudge_pow(y_mod_0, fudge, power, s2c)
            errors = model.class_decode(data) ^ data_class[0]
            frac_right = 1.0 - errors.sum()/n_y
            print('%5.3f  %5.3f  %6.4f'%(power,fudge,frac_right), file=out)
    return 0
        
if __name__ == "__main__":
    sys.exit(main())

#Local Variables:
#mode:python
#End:
