from matplotlib.widgets import Slider
from pylab import *
from scipy import misc
import numpy
import rof

im = misc.lena()
#im = im + 20 * numpy.random.standard_normal(im.shape)

U, _ = rof.denoise(im, im, tv_weight=30, tolerance=0.01)

gray()
subplot(1, 2, 1)
imshow(im)

subplot(1, 2, 2)
imshow(U)

axrof = axes([0.15, 0.15, 0.65, 0.03])
srof = Slider(axrof, 'rof', 0.0, 255.0, valinit=30)

def update(val):
  U, _ = rof.denoise(im, im, tv_weight=srof.val, tolerance=0.01)
  subplot(1, 2, 2)
  imshow(U)
  draw()

srof.on_changed(update)

show()
