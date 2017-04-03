from PIL import Image
import numpy
import stereo

im_l = numpy.array(Image.open('out_stereo1.ppm').convert('L'), 'f')
im_r = numpy.array(Image.open('out_stereo2.ppm').convert('L'), 'f')

steps = 12
start = 4
wid = 9

res1 = stereo.plane_sweep_ncc(im_l, im_r, start, steps, wid)
res2 = steps - 1 - stereo.plane_sweep_ncc(im_r, im_l, -start - steps + 1,
    steps, wid)

import scipy.misc
scipy.misc.imsave('out_depth_twosided.png', res1)
scipy.misc.imsave('out_depth_twosided2.png', res2)

res = (res1 == res2) * res1
scipy.misc.imsave('out_depth_twosided_c_sparse.png', res)

# Use first disparity image to align second disparity with first.
ir, ic = numpy.mgrid[:res1.shape[0], :res1.shape[1]]
mv = res2[ir, (ic + res1) % res1.shape[1]]
scipy.misc.imsave('out_tmp.png', mv)

res = (res1 == mv) * res1
scipy.misc.imsave('out_depth_twosided_c.png', res)
