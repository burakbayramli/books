
# a
ranks = ['A', '2', '3', '4', '5', '6', '7',
         '8', '9', '10', 'J', 'Q', 'K']
suits = ['C', 'D', 'H', 'S']
deck = []
for s in suits:
    for r in ranks:
        deck.append(s + r)
print deck

# b
import string
letters = string.ascii_uppercase
digits = range(10)
registration_numbers = []
for place1 in letters:
    for place2 in letters:
        for place3 in digits:
            for place4 in digits:
                for place5 in digits:
                    registration_numbers.append(
                        '%s%s%s%s%s' %
                        (place1, place2, place3, place4, place5))
print registration_numbers

# c
dice = []
for d1 in range(1, 7):
    for d2 in range(1, 7):
        dice.append((d1, d2))

n = 0
for d1, d2 in dice:
    if d1 + d2 == 7:
        n += 1
print '%d combinations results in the sum 7' % n
