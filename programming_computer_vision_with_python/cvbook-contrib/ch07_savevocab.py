import os
import cPickle as pickle

import imtools
import sift
import vocabulary

print 'Note: This program runs for about 1 hour on a fast 2013 laptop.'

imlist = imtools.get_imlist('/Users/thakis/Downloads/ukbench/first1000')
imcount = len(imlist)

featlist = [imlist[i][:-3] + 'sift' for i in range(imcount)]
for i in range(imcount):
  if not os.path.exists(featlist[i]):
    sift.process_image(imlist[i], featlist[i])

voc = vocabulary.Vocabulary('ukbenchtest')
voc.train(featlist, k=1000, subsampling=10)

with open('vocabulary.pkl', 'wb') as f:
  pickle.dump(voc, f)
print 'vocabulary is:', voc.name, voc.word_count
