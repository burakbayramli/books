import os

from libsvm import svmutil

import ocr

OCR_PATH = '/Users/thakis/Downloads/data/sudoku_images/ocr_data/'
features, labels = ocr.load_ocr_data(os.path.join(OCR_PATH, 'training'))
test_features, test_labels = \
    ocr.load_ocr_data(os.path.join(OCR_PATH, 'testing'))

features = map(list, features)
test_features = map(list, test_features)


problem = svmutil.svm_problem(labels, features)
param = svmutil.svm_parameter('-q -t 0')
model = svmutil.svm_train(problem, param)

print 'Training data fit:'
svmutil.svm_predict(labels, features, model)
print 'Testing data fit:'
svmutil.svm_predict(test_labels, test_features, model)
