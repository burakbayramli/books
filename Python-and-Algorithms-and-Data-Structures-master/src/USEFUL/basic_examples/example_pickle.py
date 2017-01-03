#!/usr/bin/env python

__author__ = "bt3"


import pickle

def import_pickle(filename):
    fh = None
    try:
        fh = open(filename, "rb")
        mydict2 = pickle.load(fh)
        return mydict2

    except (EnvironmentError) as err:
        print ("{0}: import error: {0}".format(os.path.basename(sys.arg[0]), err))
        return false

    finally:
        if fh is not None:
            fh.close()


def test_import_pickle():
    pkl_file = 'test.dat'
    mydict = import_pickle(pkl_file)
    print(mydict)



def export_pickle(data, filename='test.dat', compress=False):

    fh = None
    try:
        if compress:
            fh = gzip.open(filename, "wb") # write binary
        else:
            fh = open(filename, "wb") # compact binary pickle format
            pickle.dump(data, fh, pickle.HIGHEST_PROTOCOL)

    except(EnvironmentError, pickle.PickingError) as err:
        print("{0}: export error: {1}".format(os.path.basename(sys.argv[0], err)))
        return False

    finally:
        if fh is not None:
            fh.close()


def test_export_pickle():
    mydict = {'a': 1, 'b': 2, 'c': 3}
    export_pickle(mydict)



if __name__ == '__main__':
    test_export_pickle()
    test_import_pickle()
