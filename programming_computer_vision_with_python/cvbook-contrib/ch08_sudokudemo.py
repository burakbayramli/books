import numpy
import os
from PIL import Image

from libsvm import svmutil

import ocr
import sudoku
import tic


# Load sudoku image, find cells.
tic.k('start')
SUDOKU_PATH = '/Users/thakis/Downloads/data/sudoku_images/sudokus/'
imname = os.path.join(SUDOKU_PATH, 'sudoku18.jpg')
vername = os.path.join(SUDOKU_PATH, 'sudoku18.sud')

im = numpy.array(Image.open(imname).convert('L'))

x = sudoku.find_sudoku_edges(im, axis=0)
y = sudoku.find_sudoku_edges(im, axis=1)
tic.k('found edges')


# Extract cells, run OCR.
OCR_PATH = '/Users/thakis/Downloads/data/sudoku_images/ocr_data/'
features, labels = ocr.load_ocr_data(os.path.join(OCR_PATH, 'training'))
problem = svmutil.svm_problem(labels, map(list, features))
param = svmutil.svm_parameter('-q -t 0')
model = svmutil.svm_train(problem, param)
tic.k('built OCR model')

crops = []
for col in range(9):
  for row in range(9):
    crop = im[y[col]:y[col + 1], x[row]:x[row + 1]]
    crops.append(ocr.compute_feature(crop))
tic.k('extracted cells')

res = svmutil.svm_predict(numpy.loadtxt(vername), map(list, crops), model)[0]
tic.k('recognized cells')

res = numpy.array(res).reshape(9, 9)
print 'Recognized board:'
print res
