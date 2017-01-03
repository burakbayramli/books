'''Get data for the Python programs for statistics, FH OOe.
Most are from the tables in the Altman book "Practical Statistics for Medical Research.
I use these data quite often, so I have put those by default in a subdirectory
"data_altman". This function reads them from there.

If the data are not found locally, they are retrieved from the WWW.
'''

#Author:  Thomas Haslwanter, June-2014

from os.path import join
from numpy import genfromtxt
import os
import sys
if sys.version_info[0] == 3:
    from urllib.request import urlopen
    from urllib.parse import urlparse
else:
    from urlparse import urlparse
    from urllib import urlopen

def getData(inFile, subDir=r'.\Data'):
    '''Data are taken from examples in D. Altman, "Practical Statistics for Medical Research" '''
    dataDir = os.path.join(os.path.dirname(__file__), subDir)
    fullInFile = join(dataDir, inFile)
    try:
        data = genfromtxt(fullInFile, delimiter=',')
    except IOError:
        print((fullInFile + ' does not exist: Trying to read from WWW'))
        try:
            url_base = 'https://raw.github.com/thomas-haslwanter/statsintro/master/Data/'
            url = os.path.join(url_base, inFile)
            print(url)
            fileHandle = urlopen(url)
            data = genfromtxt(fileHandle, delimiter=',')
        except:
            print((url + ' also does not exist!'))
            data = ()
    return data
    
if __name__ == '__main__':
    data = getData(r'data_altman\altman_93.txt')
    print(data)
