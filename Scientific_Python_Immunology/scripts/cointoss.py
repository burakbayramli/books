from __future__ import division
import numpy
import pylab
import random
import sys

def toss(phead=0.5):
  """Simulate a coin toss - returns 1 with probabiliyt phead and 0 otherwise"""
  x = random.random()
  if x < phead:
    return 1
  else:
    return 0

def game(phead):
  heads = 0.0
  tails = 0.0

  tosses = []
  while True:
    line = raw_input('Press Enter for toss or G, Enter to guess: ')
    if line.startswith('g') or line.startswith('G'):
      guess = raw_input('Enter F for a fair coin or B for a biased coin and press Enter: ')
      print "True probability = %.2f" % phead
      if (phead==0.5 and (guess.startswith('f') or guess.startswith('F'))) or \
            (phead!=0.5 and (guess.startswith('b') or guess.startswith('B'))):
        print "You are correct!"
      else:
        print "You are wrong!"
      return tosses
    else:
      t = toss(phead)
      if t == 1:
        print "Heads"
        heads +=1 
      else:
        print "Tails"
        tails += 1
      tosses.append(t)
        
      print "N(H) = %d, N(t) = %d, Average(H) = %.2f" % (heads, tails, heads/(heads+tails))

if __name__ == '__main__':
  phead = 0.45
  tosses = game(phead)
  cstosses = numpy.cumsum(tosses)
  ratio = cstosses/numpy.arange(1, (len(cstosses)+1))
  print ratio
  pylab.plot(ratio, '-o')
  pylab.axhline(phead)
  pylab.xlabel('Number of tosses')
  pylab.ylabel('Ratio of heads to tails')
  pylab.title('True value of P(H) = %.2f' % phead)
  pylab.show()
    
