from Deck1 import Deck
deck = Deck()
print deck
p = 4
players = [deck.hand(5) for i in range(p)]
import pprint
pprint.pprint(players)
