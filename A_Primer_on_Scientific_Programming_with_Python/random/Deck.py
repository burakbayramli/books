import random
#random.seed(10)

class Deck:
    def __init__(self):
        ranks = ['A', '2', '3', '4', '5', '6', '7',
                 '8', '9', '10', 'J', 'Q', 'K']
        suits = ['C', 'D', 'H', 'S']
        self.deck = [s+r for s in suits for r in ranks]
        random.shuffle(self.deck)

    def hand(self, n=1):
        """Deal n cards. Return hand as list."""
        hand = [self.deck[i] for i in range(n)]  # pick cards
        del self.deck[:n]                        # remove cards
        return hand

    def deal(self, cards_per_hand, no_of_players):
        """Deal no_of_players hands. Return list of lists."""
        return [self.hand(cards_per_hand) \
                for i in range(no_of_players)]

    def putback(self, card):
        """Put back a card under the rest."""
        self.deck.append(card)

    def __str__(self):
        return str(self.deck)

if __name__ == '__main__':
    deck = Deck()
    print deck
    players = deck.deal(5, 4)
    import pprint; pprint.pprint(players)
    from cards import same_rank, same_suit
    for hand in players:
        print """\
The hand %s
    has %d pairs, %s 3-of-a-kind and
    %s cards of the same suit.""" % \
        (', '.join(hand), same_rank(hand, 2),
         same_rank(hand, 3),
         '+'.join([str(s) for s in same_suit(hand).values()]))

