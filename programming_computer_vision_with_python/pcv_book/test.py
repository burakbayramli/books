import ncut, numpy as np
from scipy.misc import imresize
from PIL import Image

im = np.array(Image.open('C-uniform03.ppm'))
m,n = im.shape[:2]

wid = 50
rim = imresize(im,(wid,wid),interp='bilinear')
rim = np.array(rim,'f')

A = ncut.ncut_graph_matrix(rim,sigma_d=1,sigma_g=1e-2)
code,V = ncut.cluster(A,k=3,ndim=3)

codeim = imresize(code.reshape(wid,wid),(m,n),interp='nearest')

figure()
imshow(codeim)
gray()
show()
