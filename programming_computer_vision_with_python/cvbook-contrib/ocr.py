import glob
import numpy
import os

from PIL import Image

import imtools

def compute_feature(im):
  """Returns a feature vector for an ocr image patch."""
  norm_im = imtools.imresize(im, (30, 30))
  norm_im = norm_im[3:-3, 3:-3]  # Strip border.
  return norm_im.flatten()


def load_ocr_data(path):
  """Return labels and ocr features for all images in path."""
  imlist = glob.glob(os.path.join(path, '*.jpg'))
  labels = [int(os.path.basename(imfile)[0]) for imfile in imlist]

  features = []
  for imname in imlist:
    im = numpy.array(Image.open(imname).convert('L'))
    features.append(compute_feature(im))
  return numpy.array(features), labels
