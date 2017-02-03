import random
#random.seed(10)

def make_deck():
    ranks = ['A', '2', '3', '4', '5', '6', '7',
             '8', '9', '10', 'J', 'Q', 'K']
    suits = ['C', 'D', 'H', 'S']
    deck = [s+r for s in suits for r in ranks]
    random.shuffle(deck)
    return deck

def deal_hand(n, deck):
    hand = [deck[i] for i in range(n)]
    del deck[:n]
    return hand, deck

def deal(cards_per_hand, no_of_players):
    deck = make_deck()
    hands = []
    for i in range(no_of_players):
        hand, deck = deal_hand(cards_per_hand, deck)
        hands.append(hand)
    return hands

def same_rank(hand, n_of_a_kind):
    """
    Given a hand of cards, return the number of
    n_of_a_kind combinations of ranks.
    For example, with n_of_a_kind=2, the function
    returns the number of pairs in the hand.
    """
    ranks = [card[1:] for card in hand]
    counter = 0
    already_counted = []
    for rank in ranks:
        if rank not in already_counted and \
               ranks.count(rank) == n_of_a_kind:
            counter += 1
            already_counted.append(rank)
    return counter

def same_suit(hand):
    """
    Given a hand of cards, return the number of
    cards of the same suit, counter[suit], for each
    of the suits in the hand.
    """
    suits = [card[0] for card in hand]
    counter = {}   # counter[suit] = how many cards of suit
    for suit in suits:
        # Attention only to count > 1:
        count = suits.count(suit)
        if count > 1:
            counter[suit] = count
    return counter

if __name__ == '__main__':
    players = deal(5, 4)
    import pprint; pprint.pprint(players)

    for hand in players:
        print """\
The hand %s
    has %d pairs, %s 3-of-a-kind and
    %s cards of the same suit.""" % \
        (', '.join(hand), same_rank(hand, 2),
         same_rank(hand, 3),
         '+'.join([str(s) for s in same_suit(hand).values()]))



        
    
