#!/usr/bin/env python

__author__ = "bt3"



def beating_stock(array):

    imin = 0

    # first deal is just buying in the next day (1)
    deal = [array[1] - array[imin], imin, 1]

    for i, d in enumerate(array):

        deal_here = d - array[imin]

        if deal_here > deal[0]:
            deal = [deal_here, imin, i]

        elif d < array[imin]:
            imin = i

    return deal[0], array[deal[1]], array[deal[2]]


def beating_stock2(array):

    deal = 0
    min_value = array[0]

    for i, d in enumerate(array):

        deal_here = d - min_value

        if deal_here > deal:
            deal = deal_here

        else:
            if min_value > array[i]:
                min_value = array[i]

    return deal



if __name__ == '__main__':
    array = [7, 2, 3, 6, 5, 8, 5, 3, 4]

    deal = beating_stock(array)
    print("Profit: %d, buying at %d, selling at %d." %(deal[0], deal[1], deal[2]))

    deal = beating_stock2(array)
    print "Profit: " + str(deal)