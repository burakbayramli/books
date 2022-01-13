# Compute the odds of getting a given hand in straight 5-card poker.
#
# This requires python 3.x because of the Unicode handling
#
# M. Zingale

import random

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


def play(nmax):

    n_straight_flush = 0
    n_four_of_a_kind = 0
    n_full_house = 0
    n_flush = 0
    n_straight = 0
    n_three_of_a_kind = 0
    n_two_pair = 0
    n_pair = 0

    for n in range(nmax):

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
            n_straight_flush += 1
            found = True

        # four of a kind

        # they are sorted so either cards 0,1,2,3 have the same rank
        # or 1,2,3,4 have the same rank.
        if (not found and
            ((hand[0].rank == hand[1].rank == hand[2].rank == hand[3].rank) or
             (hand[1].rank == hand[2].rank == hand[3].rank == hand[4].rank))):
            n_four_of_a_kind += 1
            found = True

        # full house

        # we are sorted again, so make sure that the first two are equal
        # and then the last three are equal or reverse
        if (not found and
            (((hand[0].rank == hand[1].rank) and
              (hand[2].rank == hand[3].rank == hand[4].rank)) or
             ((hand[0].rank == hand[1].rank == hand[2].rank) and
              (hand[3].rank == hand[4].rank)))):
            n_full_house += 1
            found = True

        # flush

        # look for all the same suit
        if (not found and
            (hand[0].suit == \
             hand[1].suit == \
             hand[2].suit == \
             hand[3].suit == \
             hand[4].suit)):
            n_flush += 1
            found = True

        # straight

        # we are already sorted, so just look at the rank
        if (not found and
            (hand[0].rank == \
             hand[1].rank - 1 == \
             hand[2].rank - 2 == \
             hand[3].rank - 3 == \
             hand[4].rank - 4)):
            n_straight += 1
            found = True


        # three of a kind

        # since we are sorted, only 0,1,2 or 1,2,3, or 2,3,4 can be
        # equal
        if (not found and
            ((hand[0].rank == hand[1].rank == hand[2].rank) or
             (hand[1].rank == hand[2].rank == hand[3].rank) or
             (hand[2].rank == hand[3].rank == hand[4].rank))):
            n_three_of_a_kind += 1
            found = True


        # two pair and one pair
        if not found:

            numPairs = 0

            if hand[0].rank == hand[1].rank:
                numPairs += 1

            if hand[1].rank == hand[2].rank:
                numPairs += 1

            if hand[2].rank == hand[3].rank:
                numPairs += 1

            if hand[3].rank == hand[4].rank:
                numPairs += 1

            if numPairs == 2:
                n_two_pair += 1
                found = True

            elif numPairs == 1:
                n_pair += 1
                found = True


    print("Number of hands: ", nmax)
    print(" ")
    print("  Straight Flush: ({:9d})  {}".format(n_straight_flush, n_straight_flush/float(nmax)))
    print("  Four of a kind: ({:9d})  {}".format(n_four_of_a_kind, n_four_of_a_kind/float(nmax)))
    print("  Full House:     ({:9d})  {}".format(n_full_house, n_full_house/float(nmax)))
    print("  Flush:          ({:9d})  {}".format(n_flush, n_flush/float(nmax)))
    print("  Straight:       ({:9d})  {}".format(n_straight, n_straight/float(nmax)))
    print("  Three of a kind:({:9d})  {}".format(n_three_of_a_kind, n_three_of_a_kind/float(nmax)))
    print("  Two pair:       ({:9d})  {}".format(n_two_pair, n_two_pair/float(nmax)))
    print("  One pair:       ({:9d})  {}".format(n_pair, n_pair/float(nmax)))


if __name__== "__main__":
    play(1000000)
