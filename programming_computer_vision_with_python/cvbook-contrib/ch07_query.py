import cPickle as pickle

import imtools
import sift
import imagesearch

"""After ch07_buildindex.py has built an index in test.db, this program can
query it.
"""

imlist = imtools.get_imlist('/Users/thakis/Downloads/ukbench/first1000')[:100]
imcount = len(imlist)
featlist = [imlist[i][:-3] + 'sift' for i in range(imcount)]

with open('vocabulary.pkl', 'rb') as f:
  voc = pickle.load(f)

searcher = imagesearch.Searcher('test.db', voc)

locs, descr = sift.read_features_from_file(featlist[0])
imwords = voc.project(descr)

print 'ask using a histogram...'
print searcher.candidates_from_histogram(imwords)[:10]

print 'try a query...'
res = searcher.query(imlist[0])[:10]
print res

print 'score:'
# Score a small subset, so this runs fast.
print imagesearch.compute_ukbench_score(searcher, imlist[:10])

# Plot images most similar to imlist[0].
imagesearch.plot_results(searcher, [r[1] for r in res[:6]])
