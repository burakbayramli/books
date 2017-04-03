import numpy
import os

from PIL import Image


def get_imlist(path):
  """Returns a list of filenames for all jpg images in a directory."""
  return [os.path.join(path, f) for f in os.listdir(path) if f.endswith('.jpg')]


def imresize(im, sz):
  """Resize an image array using PIL."""
  pil_im = Image.fromarray(numpy.uint8(im))
  return numpy.array(pil_im.resize(sz))


def Htransform(im, H, out_size):
  """Applies a homography transform to im"""
  pil_im = Image.fromarray(im)
  pil_size = out_size[1], out_size[0]
  return numpy.array(pil_im.transform(
    pil_size, Image.PERSPECTIVE, H.reshape(9)[0:8] / H[2,2], Image.LINEAR))


def histeq(im, bin_count=256):
  """Histogram equalization of a grayscale image."""
  imhist, bins = numpy.histogram(im.flatten(), bin_count, normed=True)
  cdf = imhist.cumsum()
  cdf = 255 * cdf / cdf[-1]  # normalize

  im2 = numpy.interp(im.flatten(), bins[:-1], cdf)
  return im2.reshape(im.shape), cdf


def compute_average(imlist):
  """Compute the average of a lsit of images."""
  avg = numpy.array(Image.open(imlist[0]), 'f')
  count = 1
  for name in imlist[1:]:
    try:
      avg += numpy.array(Image.open(name))
      count += 1
    except:
      print name + '...skipped'
  avg /= count
  return numpy.array(avg, 'uint8')


def plot_2d_boundary(plot_range, points, decisionfun, labels, values=[0]):
  """plot_range is (xmin, xmax, ymin, ymax), points is a list of class points,
  decisionfun is a function to evaluate, labels is a list of labels that
  decisionfun returns for each class, values is a list of decision contours to
  show."""
  import matplotlib.pyplot

  clist = ['b', 'r', 'g', 'k', 'm', 'y']  # Colors for the classes.

  # Plot contour.
  x = numpy.arange(plot_range[0], plot_range[1], 0.5)
  y = numpy.arange(plot_range[2], plot_range[3], 0.5)
  xx, yy = numpy.meshgrid(x, y)
  xxx, yyy = xx.flatten(), yy.flatten()
  zz = numpy.array(decisionfun(xxx, yyy))
  zz = zz.reshape(xx.shape)
  matplotlib.pyplot.contour(xx, yy, zz, values)

  # For each class, plot the points with '*' for correct, 'o' for incorrect.
  for i in range(len(points)):
    d = decisionfun(points[i][:, 0], points[i][:, 1])
    cor_ndx = labels[i] == d
    incor_ndx = labels[i] != d
    matplotlib.pyplot.plot(
        points[i][cor_ndx, 0], points[i][cor_ndx, 1], '*', color=clist[i])
    matplotlib.pyplot.plot(
        points[i][incor_ndx, 0], points[i][incor_ndx, 1], 'o', color=clist[i])

  matplotlib.pyplot.axis('equal')
