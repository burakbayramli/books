""" ApTrain.py command_options list_of_data_files

EG: python3 ApTrain.py
--hr_dir=../../../derived_data/apnea/low_pass_heart_rate \
--resp_dir=../../../derived_data/apnea/respiration \
--expert=../../../raw_data/apnea/data/summary_of_training \
../../../derived_data/apnea/init_1_1_4 \
../../../derived_data/apnea/modH_1_1_4 \
a06 b01 ...

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
def main(argv=None):
    '''Call with arguments: 

    '''

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    import argparse
    parser = argparse.ArgumentParser(
        description='Train an existing model on specified data')
    parser.add_argument('--hr_dir', type=str,
                       help='Path to low pass heart rate data files')
    parser.add_argument('--resp_dir', type=str,
                       help='Path to respiration data files')
    parser.add_argument('--expert', type=str,
                       help='Path to file of expert annotations')
    parser.add_argument('--iterations', type=int, default=1,
                       help='Number of Baum Welch iterations')
    parser.add_argument('mod_in', type=str,
                       help='File from which to read initial model')
    parser.add_argument('mod_out', type=str,
                       help='Write trained model to this file')
    parser.add_argument(
        '--pass1', type=str, nargs=2,
        help='Path to pass1_report and group, eg, pass1_report Medium')
    parser.add_argument('--record', type=str, nargs='*',
                       help='Record names, eg, a01 a02 ... a20')
    args = parser.parse_args(argv)

    if args.pass1:
        import PFsurvey
        args.record = PFsurvey.read_records(args.pass1)
    import pickle
    import ApOb
    mod = pickle.load(open(args.mod_in, 'rb'))
    data_dict = ApOb.build_data(mod.y_mod, args)
    mod.multi_train(list(data_dict.values()), args.iterations)
    pickle.dump(mod, open(args.mod_out, 'wb'))
    return 0

if __name__ == "__main__":
    sys.exit(main())

#Local Variables:
#mode:python
#End:
