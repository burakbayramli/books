# branching.py
# -----------------------------------------------------------------------------
# This script illustrates branching.
# ----------------------------------------------------------------------------- 
import numpy as np

for trial in range(5):
	userInput = input('Pick a number: ')
	userNumber = float(userInput)
	if userNumber < 0:
		print('Square root is not real.')
	else:
		print('Square root of {} is {:.4f}.'.format(userNumber, np.sqrt(userNumber)))
	userAgain = input('Another [y/n]? ')
	if userAgain != 'y':
		break

if trial == 4:
	print('Sorry, only 5 per customer.')
elif userAgain == 'n':
	print('Bye!')
else:
	print('Sorry, I did not understand that.')
