#!/usr/bin/env python

__author__ = "bt3"

import random

def testing_random():
    ''' testing the module random'''
    values = [1, 2, 3, 4]
    print(random.choice(values))
    print(random.choice(values))
    print(random.choice(values))
    print(random.sample(values, 2))
    print(random.sample(values, 3))

    ''' shuffle in place '''
    random.shuffle(values)
    print(values)

    ''' create random integers '''
    print(random.randint(0,10))
    print(random.randint(0,10))


if __name__ == '__main__':
    testing_random()

