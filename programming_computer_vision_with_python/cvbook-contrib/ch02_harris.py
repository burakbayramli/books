from PIL import Image
from pylab import *
import harris
import imtools

im = array(Image.open('board.jpeg').convert('L'))
#im, _ = imtools.histeq(im)
harrisim = harris.compute_harris_response(im)
figure()
gray()
imshow(harrisim)

filtered_coords = harris.get_harris_points(harrisim, threshold=0.1)
harris.plot_harris_points(im, filtered_coords)
