import os
import cPickle as pickle

import imtools
import sift
import imagesearch

"""After ch07_savevocab.py has created vocabulary.pkl and sift feature files,
this program creates an sqlite db with an index.
"""

imlist = imtools.get_imlist('/Users/thakis/Downloads/ukbench/first1000')
imcount = len(imlist)

featlist = [imlist[i][:-3] + 'sift' for i in range(imcount)]

with open('vocabulary.pkl', 'rb') as f:
  voc = pickle.load(f)

try:
  os.remove('test.db')
except OSError:
  pass
idx = imagesearch.Indexer('test.db', voc)
idx.create_tables()

for i in range(imcount)[:100]:
  locs, descr = sift.read_features_from_file(featlist[i])
  idx.add_to_index(imlist[i], descr)

idx.db_commit()
