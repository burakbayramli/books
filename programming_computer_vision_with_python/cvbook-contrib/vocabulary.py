import numpy
import scipy.cluster.vq as vq
import sift

class Vocabulary(object):
  def __init__(self, name):
    self.name = name
    self.voc = []
    self.idf = []
    self.trainingdata = []
    self.word_count = 0

  def train(self, featurefiles, k=100, subsampling=10):
    """Train a vocabulary from features in files listed in |featurefiles| using
    k-means with k words. Subsampling of training data can be used for speedup.
    """
    image_count = len(featurefiles)

    descr = []
    descr.append(sift.read_features_from_file(featurefiles[0])[1])
    descriptors = descr[0]  # Stack features for k-means.
    for i in numpy.arange(1, image_count):
      descr.append(sift.read_features_from_file(featurefiles[i])[1])
      descriptors = numpy.vstack((descriptors, descr[i]))

    # Run k-means.
    self.voc, distortion = vq.kmeans(descriptors[::subsampling, :], k, 1)
    self.word_count = self.voc.shape[0]

    # Project training data on vocabulary.
    imwords = numpy.zeros((image_count, self.word_count))
    for i in range(image_count):
      imwords[i] = self.project(descr[i])

    occurence_count = numpy.sum((imwords > 0)*1, axis=0)
    
    self.idf = numpy.log(image_count / (occurence_count + 1.0))
    self.trainingdata = featurefiles

  def project(self, descriptors):
    """Project descriptors on the vocabulary to create a histogram of words."""
    imhist = numpy.zeros((self.word_count))
    words, distance = vq.vq(descriptors, self.voc)
    for w in words:
      imhist[w] += 1
    return imhist
