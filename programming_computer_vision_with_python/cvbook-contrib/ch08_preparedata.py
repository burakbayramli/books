"""Splits the hand dataset into train and test folders."""

import glob, os, shutil

import dsift

s = '/Users/thakis/Downloads/Marcel-Test'
d = 'out_hands'

# Symlink images to train/ and test/ folders.
# Intentionally ignore the MiniTrieschGallery folder.
ls = ['A', 'B', 'C', 'Five', 'Point', 'V']
train = []
test = []
for l in ls:
  imgs = glob.glob(os.path.join(s, l, 'uniform', '*'))
  train += imgs[::2]
  test += imgs[1::2]

shutil.rmtree(d)

def md(p):
  try: os.makedirs(p)
  except: pass
md(os.path.join(d, 'train'))
md(os.path.join(d, 'test'))

for p in train: os.symlink(p, os.path.join(d, 'train', os.path.basename(p)))
for p in test: os.symlink(p, os.path.join(d, 'test', os.path.basename(p)))

# Compute dsift descriptors.
imlist = glob.glob(d + '/*/*.ppm')
dsiftlist = [os.path.splitext(im)[0] + '.dsift' for im in imlist]
# About 10s:
#for im, dim in zip(imlist, dsiftlist):
#  dsift.process_image_dsift(im, dim, 10, 5, resize=(50, 50))
# About 7s:
from multiprocessing.dummy import Pool as ThreadPool
pool = ThreadPool(8)
pool.map(lambda x: dsift.process_image_dsift(x[0], x[1], 10, 5, resize=(50,50)),
         zip(imlist, dsiftlist))
pool.close()
pool.join()
