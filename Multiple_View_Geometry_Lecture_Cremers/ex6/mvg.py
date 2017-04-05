import numpy as np
import cv2
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import axes3d
import scipy.linalg as lin

img1 = cv2.imread("batinria0.tif")
img2 = cv2.imread("batinria1.tif")

pts1 = [[10, 232], [92, 230], [8, 334], [92, 333], [289, 230], \
        [354, 278], [289, 340], [353, 332], [69, 90], [294, 149], \
        [44, 475], [336, 433]]

pts2 = [[123, 239], [203, 237], [123, 338], [202, 338], [397, 236],\
        [472, 286], [398, 348], [472, 341], [182, 99], [401, 153], \
        [148, 471], [447, 445]]

for pt in pts1:
    cv2.circle(img1, tuple(pt), 4, (0,0,255), -1)
cv2.imwrite('batin_01.png',img1)
for pt in pts2:
    cv2.circle(img2, tuple(pt), 4, (0,0,255), -1)
cv2.imwrite('batin_02.png',img2)







