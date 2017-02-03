import random
#random.seed(20)
import sys

def roll_dice_and_compute_sum(ndice):
    return sum([random.randint(1, 6) \
                for i in range(ndice)])

def computer_guess(ndice):
    return random.randint(ndice, 6*ndice)

def player_guess(ndice):
    return input('Guess the sum of the no of eyes '\
                 'in the next throw: ')

def play_one_round(ndice, capital, guess_function):
    guess = guess_function(ndice)
    throw = roll_dice_and_compute_sum(ndice)
    if guess == throw:
        capital += guess
    else:
        capital -= 1
    return capital, throw, guess

def play(nrounds, ndice=2):
    player_capital = computer_capital = nrounds  # start capital

    for i in range(nrounds):
        player_capital, throw, guess = \
             play_one_round(ndice, player_capital, player_guess)
        print 'YOU guessed %d, got %d' % (guess, throw)
        if player_capital == 0:
            print 'Machine won!'; sys.exit(0)

        computer_capital, throw, guess = \
            play_one_round(ndice, computer_capital, computer_guess)
        print 'Machine guessed %d, got %d' % (guess, throw)
        if computer_capital == 0:
            print 'You won!'; sys.exit(0)

        print 'Status: you have %d euros, machine has %d euros\n' % \
              (player_capital, computer_capital)

    if computer_capital > player_capital:
        winner = 'Machine'
    else:
        winner = 'You'
    print winner, 'won!'
            
if __name__ == '__main__':
    play(10)
