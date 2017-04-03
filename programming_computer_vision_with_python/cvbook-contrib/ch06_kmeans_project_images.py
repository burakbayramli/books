import imtools
import pca
from PIL import Image, ImageDraw
from pylab import *

# PCA on all images.
imlist = imtools.get_imlist('/Users/thakis/Downloads/data/a_thumbs')
imcount = len(imlist)
immatrix = array([array(Image.open(im)).flatten() for im in imlist], 'f')
V, S, immean = pca.pca(immatrix)

# Visualize only selected images.
imlist = imtools.get_imlist('/Users/thakis/Downloads/data/a_selected_thumbs')
imcount = len(imlist)
immatrix = array([array(Image.open(im)).flatten() for im in imlist], 'f')

# Project on 2 PCs.
projected = array(
    [dot(V[[0, 1]], immatrix[i] - immean) for i in range(imcount)])

h, w = 1200, 1200

img = Image.new('RGB', (w, h), (255, 255, 255))
draw = ImageDraw.Draw(img)

# axes
draw.line((0, h/2, w, h/2), fill=(255, 0, 0))
draw.line((w/2, 0, w/2, h), fill=(255, 0, 0))

scale = abs(projected).max(0)
scaled = floor(array([(p/scale) * (w/2 - 20, h/2 - 20) + (w/2, h/2)
                      for p in projected])).astype(int)

for i in range(imcount):
  nodeim = Image.open(imlist[i])
  nodeim.thumbnail((25, 25))
  ns = nodeim.size
  box = (scaled[i][0] - ns[0] // 2, scaled[i][1] - ns[1] // 2,
         scaled[i][0] + ns[0] // 2 + 1, scaled[i][1] + ns[1] // 2 + 1)
  img.paste(nodeim, box)

img.save('out_pca_font.png')
