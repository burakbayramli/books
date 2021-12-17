# Compute the odds of getting a given hand in straight 5-card poker.
#
# This requires python 3.x because of the Unicode handling
#
# M. Zingale
#
# this is the MPI version

import numpy as np
import random
from mpi4py import MPI

class Card(object):

    def __init__(self, suit=1, rank=2):
        if suit < 1 or suit > 4:
            print("invalid suit, setting to 1")

        self.suit = suit
        self.rank = rank

    def value(self):
        """ we want things order primarily by rank then suit """
        return self.suit + (self.rank-1)*14

    def __lt__(self, other):
        return self.value() < other.value()

    def __unicode__(self):
        suits = [u"\u2660",  # spade
                 u"\u2665",  # heart
                 u"\u2666",  # diamond
                 u"\u2663"]  # club

        r = str(self.rank)
        if self.rank == 11:
            r = "J"
        elif self.rank == 12:
            r = "Q"
        elif self.rank == 13:
            r = "K"
        elif self.rank == 14:
            r = "A"

        return r +':'+suits[self.suit-1]

    def __str__(self):
        return unicode(self).encode('utf-8')


class Deck(object):
    """ the deck is a collection of cards """

    def __init__(self):

        self.nsuits = 4
        self.nranks = 13
        self.minrank = 2
        self.maxrank = self.minrank + self.nranks - 1

        self.cards = []

        for rank in range(self.minrank, self.maxrank+1):
            for suit in range(1, self.nsuits+1):
                self.cards.append(Card(rank=rank, suit=suit))

    def shuffle(self):
        random.shuffle(self.cards)

    def deal(self, num=1):
        hand = []

        for n in range(num):
            hand.append(self.cards.pop())

        return hand

    def __str__(self):
        string = ""
        for c in self.cards:
            string += str(c) + " "
        return string


class Hands(object):
    """ a simple container class to hold the number of each hand """

    def __init__(self):
        self.n_straight_flush = 0
        self.n_four_of_a_kind = 0
        self.n_full_house = 0
        self.n_flush = 0
        self.n_straight = 0
        self.n_three_of_a_kind = 0
        self.n_two_pair = 0
        self.n_pair = 0
        self.nmax = 0

    def as_array(self):
        """ return the values as an array, in order -- this is for
            MPI operations """
        
        return np.array([self.n_straight_flush,
                         self.n_four_of_a_kind,
                         self.n_full_house,
                         self.n_flush,
                         self.n_straight,
                         self.n_three_of_a_kind,
                         self.n_two_pair,
                         self.n_pair,
                         self.nmax])

    def from_array(self, arr):
        """ unpack an array and reinitialize the object data """
        self.n_straight_flush = arr[0]
        self.n_four_of_a_kind = arr[1]
        self.n_full_house = arr[2]
        self.n_flush = arr[3]
        self.n_straight = arr[4]
        self.n_three_of_a_kind = arr[5]
        self.n_two_pair = arr[6]
        self.n_pair = arr[7]
        self.nmax = arr[8]

    def __str__(self):
        s = "Number of hands: {}\n\n".format(self.nmax)
        s += "  Straight Flush: ({:9d})  {}\n".format(self.n_straight_flush, self.n_straight_flush/float(self.nmax))
        s += "  Four of a kind: ({:9d})  {}\n".format(self.n_four_of_a_kind, self.n_four_of_a_kind/float(self.nmax))
        s += "  Full House:     ({:9d})  {}\n".format(self.n_full_house, self.n_full_house/float(self.nmax))
        s += "  Flush:          ({:9d})  {}\n".format(self.n_flush, self.n_flush/float(self.nmax))
        s += "  Straight:       ({:9d})  {}\n".format(self.n_straight, self.n_straight/float(self.nmax))
        s += "  Three of a kind:({:9d})  {}\n".format(self.n_three_of_a_kind, self.n_three_of_a_kind/float(self.nmax))
        s += "  Two pair:       ({:9d})  {}\n".format(self.n_two_pair, self.n_two_pair/float(self.nmax))
        s += "  One pair:       ({:9d})  {}\n".format(self.n_pair, self.n_pair/float(self.nmax))

        return s

def play(nmax, comm, rank):

    hh = Hands()

    for n in range(nmax):

        hh.nmax += 1

        mydeck = Deck()
        mydeck.shuffle()

        # get a hand
        hand = mydeck.deal(5)
        hand.sort()

        found = False

        # check for the different hands...

        # straight flush

        # the hand is sorted by rank then suit, make sure
        # that they all have the same suit and that they are
        # sequential
        if (not found and
            (hand[0].suit == \
             hand[1].suit == \
             hand[2].suit == \
             hand[3].suit == \
             hand[4].suit) and
            (hand[0].rank == \
             hand[1].rank - 1 == \
             hand[2].rank - 2 == \
             hand[3].rank - 3 == \
             hand[4].rank - 4)):
            hh.n_straight_flush += 1
            found = True

        # four of a kind

        # they are sorted so either cards 0,1,2,3 have the same rank
        # or 1,2,3,4 have the same rank.
        if (not found and
            ((hand[0].rank == hand[1].rank == hand[2].rank == hand[3].rank) or
             (hand[1].rank == hand[2].rank == hand[3].rank == hand[4].rank))):
            hh.n_four_of_a_kind += 1
            found = True

        # full house

        # we are sorted again, so make sure that the first two are equal
        # and then the last three are equal or reverse
        if (not found and
            (((hand[0].rank == hand[1].rank) and
              (hand[2].rank == hand[3].rank == hand[4].rank)) or
             ((hand[0].rank == hand[1].rank == hand[2].rank) and
              (hand[3].rank == hand[4].rank)))):
            hh.n_full_house += 1
            found = True

        # flush

        # look for all the same suit
        if (not found and
            (hand[0].suit == \
             hand[1].suit == \
             hand[2].suit == \
             hand[3].suit == \
             hand[4].suit)):
            hh.n_flush += 1
            found = True

        # straight

        # we are already sorted, so just look at the rank
        if (not found and
            (hand[0].rank == \
             hand[1].rank - 1 == \
             hand[2].rank - 2 == \
             hand[3].rank - 3 == \
             hand[4].rank - 4)):
            hh.n_straight += 1
            found = True


        # three of a kind

        # since we are sorted, only 0,1,2 or 1,2,3, or 2,3,4 can be
        # equal
        if (not found and
            ((hand[0].rank == hand[1].rank == hand[2].rank) or
             (hand[1].rank == hand[2].rank == hand[3].rank) or
             (hand[2].rank == hand[3].rank == hand[4].rank))):
            hh.n_three_of_a_kind += 1
            found = True


        # two pair and one pair
        if not found:

            num_pairs = 0

            if hand[0].rank == hand[1].rank:
                num_pairs += 1

            if hand[1].rank == hand[2].rank:
                num_pairs += 1

            if hand[2].rank == hand[3].rank:
                num_pairs += 1

            if hand[3].rank == hand[4].rank:
                num_pairs += 1

            if num_pairs == 2:
                hh.n_two_pair += 1
                found = True

            elif num_pairs == 1:
                hh.n_pair += 1
                found = True


    return hh


if __name__== "__main__":

    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()
    size = comm.Get_size()

    N = 100000
    N_sub = int(N/size)

    hands = play(N_sub, comm, rank)
    harr = hands.as_array()

    if rank == 0:
        htotal = np.zeros_like(harr)
    else:
        htotal = None

    comm.Reduce([harr, MPI.INT], [htotal, MPI.INT], op=MPI.SUM, root=0)

    if rank == 0:
        hall = Hands()
        hall.from_array(htotal)

        print(hall)

