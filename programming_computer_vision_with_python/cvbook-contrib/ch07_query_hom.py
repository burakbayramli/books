import cPickle as pickle

import homography
import imtools
import sift
import imagesearch

"""After ch07_buildindex.py has built an index in test.db, this program
queries it, and fits a homography to improve query results.
"""

imlist = imtools.get_imlist('/Users/thakis/Downloads/ukbench/first1000')[:100]
imcount = len(imlist)
featlist = [imlist[i][:-3] + 'sift' for i in range(imcount)]

with open('vocabulary.pkl', 'rb') as f:
  voc = pickle.load(f)

searcher = imagesearch.Searcher('test.db', voc)

query_imid = 50
res_count = 20

res = [w[1] for w in searcher.query(imlist[query_imid])[:res_count]]
print 'regular results for query %d:' % query_imid, res

# Rerank by trying to fit a homography.
q_locs, q_descr = sift.read_features_from_file(featlist[query_imid])
fp = homography.make_homog(q_locs[:, :2].T)

model = homography.RansacModel()

rank = {}
for ndx in res[1:]:
  locs, descr = sift.read_features_from_file(featlist[ndx - 1]) # res is 1-based

  matches = sift.match(q_descr, descr)
  ind = matches.nonzero()[0]
  ind2 = [int(matches[i]) for i in ind]
  tp = homography.make_homog(locs[:, :2].T)

  try:
    H, inliers = homography.H_from_ransac(fp[:, ind], tp[:, ind2],
                                          model, match_threshold=4)
  except:
    inliers = []

  rank[ndx] = len(inliers)

sorted_rank = sorted(rank.items(), key=lambda t: t[1], reverse=True)
res_geom = [res[0]] + [s[0] for s in sorted_rank]
print 'homography results for query %d' % query_imid, res_geom

imagesearch.plot_results(searcher, res[:8])
imagesearch.plot_results(searcher, res_geom[:8])
