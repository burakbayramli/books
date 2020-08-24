# Super-3 Numbers

import string

i = input('Please enter the upper bound: ')
for n in range(i):
	x = 3*n**3
        if string.find(str(x), '333') <> -1:
		print n, x

# End of program
