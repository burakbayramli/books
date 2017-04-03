from PIL import Image
import numpy
import stereo

im_l = numpy.array(Image.open('out_stereo1.ppm').convert('L'), 'f')
im_r = numpy.array(Image.open('out_stereo2.ppm').convert('L'), 'f')

steps = 12
start = 4
wid = 9

res_ncc = stereo.plane_sweep_ncc(im_l, im_r, start, steps, wid)
res_ssd = stereo.plane_sweep_ssd(im_l, im_r, start, steps, wid)

wid = 3
res_gauss_ncc = stereo.plane_sweep_gauss(im_l, im_r, start, steps, wid)
res_gauss_ssd = stereo.plane_sweep_gauss_ssd(im_l, im_r, start, steps, wid)

import scipy.misc
scipy.misc.imsave('out_depth_ex04_ncc.png', res_ncc)
scipy.misc.imsave('out_depth_ex04_ssd.png', res_ssd)
scipy.misc.imsave('out_depth_ex04_gauss_ncc.png', res_gauss_ncc)
scipy.misc.imsave('out_depth_ex04_gauss_ssd.png', res_gauss_ssd)
