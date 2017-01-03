
import sys
import random as rnd

strategy = sys.argv[1]   # must be 'stick', 'choose', or 'switch'

wins = 0
for trial in range( 1000 ):    
    # The prize is always in envelope 0 ... but we don't know that!
    envelopes = [0, 1, 2]

    first_choice = rnd.choice( envelopes )

    if first_choice == 0:
        envelopes = [0, rnd.choice( [1,2] ) ] # Randomly retain 1 or 2
    else:
        envelopes = [0, first_choice] # Retain winner and first choice

    if strategy == 'stick':
        second_choice = first_choice
    elif strategy == 'choose':
        second_choice = rnd.choice( envelopes )
    elif strategy == 'switch':
        envelopes.remove( first_choice )
        second_choice = envelopes[0]

    # Remember that the prize is in envelope 0
    if second_choice == 0:
        wins += 1

print wins
