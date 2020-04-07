""" misc.py """
import os
import requests

for c in "123456":
    try:                          # if it does not yet exist
        os.mkdir("MyDir"+ c)      # make a directory
    except:                       # otherwise
        pass                      # do nothing

uname = "https://github.com/DSML-book/Programs/tree/master/Appendices/Python Primer/"
fname = "ataleof2cities.txt"
r = requests.get(uname + fname )
print (r.text)
open('MyDir1/ato2c.txt', 'wb').write(r.content) # write to a file
# bytes mode is important here
