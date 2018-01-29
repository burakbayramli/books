# -*- coding: utf-8 -*-

import numpy               

from keras.datasets import cifar10
from keras.models import Sequential 
from keras.layers.core import Dense, Activation
from keras.layers import Convolution2D, MaxPooling2D, Flatten
from keras.layers import Dropout
from keras.utils import np_utils

batch_size = 100     
hidden_neurons = 200
classes = 10     
epochs = 20

(X_train, Y_train), (X_test, Y_test) = cifar10.load_data()


Y_train = np_utils.to_categorical(Y_train, classes)     
Y_test = np_utils.to_categorical(Y_test, classes)

model = Sequential() 
model.add(Convolution2D(32, (3, 3), input_shape=(32, 32, 3)))
model.add(Activation('relu'))
model.add(Convolution2D(32, (3, 3)))  
model.add(Activation('relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))   

model.add(Convolution2D(64, (3, 3))) 
model.add(Activation('relu'))     
model.add(Convolution2D(64, (3, 3)))     
model.add(Activation('relu'))     
model.add(MaxPooling2D(pool_size=(2, 2)))     
model.add(Dropout(0.25))
               
model.add(Flatten())
 
model.add(Dense(hidden_neurons)) 
model.add(Activation('relu')) 
model.add(Dropout(0.5))      
model.add(Dense(classes)) 
model.add(Activation('softmax'))
     

model.compile(loss='categorical_crossentropy', metrics=['accuracy'], optimizer='adadelta')

model.fit(X_train, Y_train, batch_size=batch_size, epochs=epochs, validation_split = 0.1, verbose=1)

score = model.evaluate(X_test, Y_test, verbose=1)
print('Test accuracy:', score[1]) 

numpy.set_printoptions(threshold='nan')  
index = 0   
for layer in model.layers:       
  filename = "conv_layer_" + str(index)       
  f1 = open(filename, 'w+')       
  f1.write(repr(layer.get_weights()))       
  f1.close()       
  print (filename + " has been opened and closed")     
  index = index+1
