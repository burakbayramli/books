from PIL import Image
import numpy
import os
import scipy.io as sio

def process_image(imagename, resultname, histeq=False,
                  params='--edge-thresh 10 --peak-thresh 5'):
  if not imagename.endswith('pgm') or histeq:
    im = Image.open(imagename).convert('L')
    if histeq:
      import imtools
      im = Image.fromarray(numpy.uint8(imtools.histeq(numpy.array(im))[0]))
    im.save('out_tmp.pgm')
    imagename = 'out_tmp.pgm'

  # Assumes that vlfeat's sift binary is in PATH.
  cmd = ' '.join(['sift', imagename, '--output=' + resultname, params])
  os.system(cmd)

  # Re-write as .mat file, which loads faster.
  f = numpy.loadtxt(resultname)
  sio.savemat(resultname + '.mat', {'f':f}, oned_as='row')


def read_features_from_file(filename):
  '''Returns feature locations, descriptors.'''
  f = sio.loadmat(filename + '.mat')['f']
  return f[:, :4], f[:, 4:]


def read_or_compute(imname, siftname, histeq=False):
  '''Returns features from 'siftname' if it exists. Else, computes features from
  'imname', writes them to 'siftname', and returns them.'''
  if not os.path.exists(siftname):
    process_image(imname, siftname, histeq=histeq)
  return read_features_from_file(siftname)


def plot_features(im, locs, circle=False):
  import pylab
  def draw_circle(c, r):
    t = numpy.arange(0, 1.01, .01) * 2 * numpy.pi
    x = r * numpy.cos(t) + c[0]
    y = r * numpy.sin(t) + c[1]
    pylab.plot(x, y, 'b', linewidth=2)
  pylab.imshow(im)
  if circle:
    for p in locs:
      draw_circle(p[:2], p[2])
  else:
    pylab.plot(locs[:, 0], locs[:, 1], 'ob')
  pylab.axis('off')


def match(desc1, desc2):
  desc1 = numpy.array([d / numpy.linalg.norm(d) for d in desc1])
  desc2 = numpy.array([d / numpy.linalg.norm(d) for d in desc2])

  dist_ratio = 0.6
  desc1_size = desc1.shape

  matchscores = numpy.zeros((desc1_size[0], 1), 'int')
  desc2t = desc2.T
  for i in range(desc1_size[0]):
    dotprods = numpy.dot(desc1[i, :], desc2t)
    dotprods = 0.9999 * dotprods

    # arccos is monotonous, can sort on just its sign.
    #indx = numpy.argsort(numpy.arccos(dotprods))
    indx = numpy.argsort(-dotprods)

    # Only keep best match if it's noticeably better than the next best match.
    if (numpy.arccos(dotprods)[indx[0]] <
        dist_ratio * numpy.arccos(dotprods)[indx[1]]):
      matchscores[i] = int(indx[0])

  return matchscores


def match_twosided(desc1, desc2):
  matches_12 = match(desc1, desc2)
  #return matches_12  # XXX doesn't seem to make things worse?
  matches_21 = match(desc2, desc1)

  ndx_12 = matches_12.nonzero()[0]
  for n in ndx_12:
    if matches_21[int(matches_12[n])] != n:
      matches_12[n] = 0
  return matches_12


def appendimages(im1, im2):
  return numpy.concatenate((im1, im2), axis=1)


def plot_matches(im1, im2, locs1, locs2, matchscores, show_below=True):
  import pylab
  im3 = appendimages(im1, im2)
  if show_below:
    im3 = numpy.vstack((im3, im3))

  pylab.imshow(im3)
  cols1 = im1.shape[1]
  for i, m in enumerate(matchscores):
    if m > 0:
      pylab.plot([locs1[i, 0], locs2[m, 0] + cols1],
                 [locs1[i, 1], locs2[m, 1]], 'c')
  pylab.axis('off')
