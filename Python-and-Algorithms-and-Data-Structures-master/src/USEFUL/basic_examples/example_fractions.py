#!/usr/bin/env python

__author__ = "bt3"


from fractions import Fraction

def rounding_floats(number1, places):
    return round(number1, places)


def float_to_fractions(number):
    return  Fraction(*number.as_integer_ratio())


def get_denominator(number1, number2):
    a = Fraction(number1, number2)
    return a.denominator


def get_numerator(number1, number2):
    a = Fraction(number1, number2)
    return a.numerator


def test_testing_floats(module_name='this module'):
    number1 = 1.25
    number2 = 1
    number3 = -1
    number4 = 5/4
    number6 = 6
    assert(rounding_floats(number1, number2) == 1.2)
    assert(rounding_floats(number1*10, number3) == 10)
    assert(float_to_fractions(number1) == number4)
    assert(get_denominator(number2, number6) == number6)
    assert(get_numerator(number2, number6) == number2)

    s = 'Tests in {name} have {con}!'
    print(s.format(name=module_name, con='passed'))


if __name__ == '__main__':
    test_testing_floats()


