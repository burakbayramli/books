# starting point: random/balls_in_hat.py

import random

def draw_ball(hat):
    """Draw a ball using list index."""
    index = random.randint(0, len(hat)-1)
    color = hat.pop(index)
    return color, hat

def new_hat():
    colors = 'red', 'yellow', 'green', 'brown'
    hat = []
    for color in colors:
        for i in range(5):
            hat.append(color)
    return hat

def experiments(n, question, N):
    """Run experiments."""
    M = 0  # no of successes
    for e in range(N):
        hat = new_hat()
        balls = []           # the n balls we draw
        for i in range(n):
            color, hat = draw_ball(hat)
            balls.append(color)
        if examine(question, balls):
            M += 1
    return float(M)/N

def examine(question, balls):
    """
    Perform boolean test corresponding
    to the questions in the exercise.
    """
    # We store the boolean tests in a list
    questions = [
        balls.count('red') >= 1 and balls.count('brown') == 1,
        balls.count('red') == 1,
        balls.count('red') == 2,
        balls.count('green') >= 3,
        ]
    return questions[question-1]
    # Alternative
    #if question == 1:
    #    return balls.count('red') >= 1 and balls.count('brown') == 1
    #elif question == 2:
    #    return balls.count('red') == 1,
    ## etc

N = int(raw_input('How many experiments? '))
question = int(raw_input('Question 1, 2, 3, or 4? '))

for n in 3, 5, 7, 10, 15:
    p = experiments(n, question, N)
    print 'n=%2d, question %d, probability: %.3f' % (n, question, p)



