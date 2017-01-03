#!/usr/bin/env python
"""demonstrate the use of getopt and optparse to parse the command line"""
import getopt, sys, os, shutil
usage = """
Usage: %s [-h | --help] [-d dir | --directory dir] [-i | --confirm] file1 file2 ...
       moves file1 file2 ... to the destination directory dir
""" % sys.argv[0]


def getopt_parsing():
    try:
        options, args = getopt.getopt(sys.argv[1:], 
                        'hd:i', ['help', 'directory=', 'confirm'])
    except getopt.GetoptError, msg:
        # illegal options (not -h, -d, --help, --directory)
        # or missing values (for -d and --directory)
        print sys.exc_value  # explains what is wrong
        print usage
        sys.exit(1)  # 1 signifies error

    # put all information in a dictionary cmlargs:
    directory = None
    confirm = False
    files = args

    print "options=",options
    print "args=",args
    
    # process options and values:
    for option, value in options:
        if option in ('-h', '--help'):
            print usage; sys.exit(0)  # 0: this exit is no error
        elif option in ('-d', '--directory'):
            directory = value
        elif option in ('-i', '--confirm'):
            confirm = True
    return directory, confirm, files

from optparse import OptionParser

class OptionParserNoError(OptionParser):
    def error(self, msg):
        return 
    
def optparse_parsing():
    parser = OptionParser()
    parser = OptionParserNoError()
    # help message is automatically provided
    parser.add_option('-d', '--directory', dest='directory',
                      help='destination directory')
    parser.add_option('-i', '--confirm', dest='confirm',
                      action='store_true', default=False,
                      help='confirm each move')
    options, args = parser.parse_args(sys.argv[1:])

    print "options=",options
    print "args=",args

    return options.directory, options.confirm, args
    
destination, confirm, files = getopt_parsing()
#destination, confirm, files = optparse_parsing()
print destination, confirm, files
from scitools.misc import movefiles
#movefiles(files, destination, confirm=confirm)
