#!/usr/bin/env python

__author__ = "bt3"


def balance_par_str_with_stack(str1):
    i, stack = 0, []

    while i < len(str1):
        symbol = str1[i]
        if symbol == "(":
            stack.append(symbol)
        elif symbol == ")":
            stack.pop()
        i += 1
    return not stack



if __name__ == '__main__':
    print(balance_par_str_with_stack('((()))'))
    print(balance_par_str_with_stack('(()'))