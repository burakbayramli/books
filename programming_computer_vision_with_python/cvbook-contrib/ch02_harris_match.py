from PIL import Image
from pylab import *
import harris
import imtools

im1 = array(Image.open('/Users/thakis/src/PCV/data/sf_view1.jpg').convert('L'))
im2 = array(Image.open('/Users/thakis/src/PCV/data/sf_view2.jpg').convert('L'))

harrisim1 = harris.compute_harris_response(im1)
filtered_coords1 = harris.get_harris_points(harrisim1)
patches1 = harris.get_descriptors(im1, filtered_coords1)

harris.plot_harris_points(im1, filtered_coords1)


harrisim2 = harris.compute_harris_response(im2)
filtered_coords2 = harris.get_harris_points(harrisim2)
patches2 = harris.get_descriptors(im2, filtered_coords2)

harris.plot_harris_points(im2, filtered_coords2)


matches = harris.match_twosided(patches1, patches2)

figure()
gray()
harris.plot_matches(im1, im2,
                    filtered_coords1, filtered_coords2,
                    matches, show_below=False)
show()
