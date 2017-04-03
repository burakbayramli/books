from scipy.ndimage import filters
import matplotlib
import numpy

def compute_harris_response(im, sigma=3):
  """Compute harris image for each pixel in a graylevel."""

  imx = numpy.zeros(im.shape)
  filters.gaussian_filter(im, sigma, (0, 1), imx)
  imy = numpy.zeros(im.shape)
  filters.gaussian_filter(im, sigma, (1, 0), imy)

  Wxx = filters.gaussian_filter(imx*imx, sigma)
  Wxy = filters.gaussian_filter(imx*imy, sigma)
  Wyy = filters.gaussian_filter(imy*imy, sigma)

  Wdet = Wxx*Wyy - Wxy**2
  Wtr = Wxx + Wyy

  #return Wdet - 0.06 * Wtr**2
  # Use Noble's measure (see wikipedia)
  return 2 * Wdet / (Wtr + 0.000001)


def get_harris_points(harrisim, min_dist=10, threshold=0.1):
  """Return corners from a harris image."""

  corner_threshold = harrisim.max() * threshold
  harrisim_t = (harrisim > corner_threshold) * 1

  coords = numpy.array(harrisim_t.nonzero()).T
  candidate_values = [harrisim[c[0], c[1]] for c in coords]

  index = numpy.argsort(candidate_values)

  allowed_locations = numpy.zeros(harrisim.shape)
  allowed_locations[min_dist:-min_dist, min_dist:-min_dist] = 1

  filtered_coords = []
  for i in index:
    if allowed_locations[coords[i, 0], coords[i, 1]] == 1:
      filtered_coords.append(coords[i])
      allowed_locations[(coords[i, 0] - min_dist):(coords[i, 0] + min_dist),
                        (coords[i, 1] - min_dist):(coords[i, 1] + min_dist)] = 0
  return filtered_coords


def plot_harris_points(image, filtered_coords):
  """Plot corners found in an image."""
  import pylab
  pylab.figure()
  pylab.gray()
  pylab.imshow(image)
  pylab.plot([p[1] for p in filtered_coords],
             [p[0] for p in filtered_coords], '*')
  pylab.axis('off')


def get_descriptors(image, filtered_coords, wid=5):
  """Extract a patch of size 2*wid+1 around each coord."""
  desc = []
  for coord in filtered_coords:
    patch = image[coord[0] - wid:coord[0] + wid + 1, 
                  coord[1] - wid:coord[1] + wid + 1].flatten()
    desc.append(patch)
  return desc


def ncc(patch1, patch2):
  """Returns normalized cross-correlation between two patches."""
  d1 = (patch1 - numpy.mean(patch1)) / numpy.std(patch1)
  d2 = (patch2 - numpy.mean(patch2)) / numpy.std(patch2)
  return numpy.sum(d1 * d2) / (len(patch1) - 1)


def match(desc1, desc2, threshold=0.5):
  """Matches all patches in desc1 with a patch in desc2."""
  n = len(desc1[0])

  d = -numpy.ones((len(desc1), len(desc2)))
  for i in range(len(desc1)):
    for j in range(len(desc2)):
      ncc_value = ncc(desc1[i], desc2[j])
      if ncc_value > threshold:
        d[i,j] = ncc_value

  ndx = numpy.argsort(-d)
  return ndx[:, 0]


def match_twosided(desc1, desc2, threshold=0.5):
  matches_12 = match(desc1, desc2, threshold)
  matches_21 = match(desc2, desc1, threshold)

  ndx_12 = numpy.where(matches_12 >= 0)[0]
  for n in ndx_12:
    if matches_21[matches_12[n]] != n:
      matches_12[n] = -1
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
      pylab.plot([locs1[i][1], locs2[m][1] + cols1],
                 [locs1[i][0], locs2[m][0]], 'c')
  pylab.axis('off')
