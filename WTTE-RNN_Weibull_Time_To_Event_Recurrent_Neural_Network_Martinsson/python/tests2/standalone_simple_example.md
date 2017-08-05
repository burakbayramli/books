
# WTTE-RNN in keras ( standalone )
A template to use with data of varying shape (but fixed sequence length for now).

This notebook is written to be as explicit as possible, hope you can follow. Everything is inlined so it has no dependencies with the rest of the repo and I have no plan to keep it updated but PRs are welcome. See See `simple_example.ipynb` for an updated example.


```python
%matplotlib inline
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
import theano as T
from keras import backend as K

from keras.models import Sequential
from keras.layers import Dense

from keras.layers import LSTM,GRU
from keras.layers import Lambda
from keras.layers.wrappers import TimeDistributed

from keras.optimizers import RMSprop,adam
from keras.callbacks import History, TensorBoard

print('tensorflow version '+tf.__version__) 
print('theano     version '+T.__version__) 
```

    Using Theano backend.


    tensorflow version 1.0.1
    theano     version 0.9.0



```python
def _keras_unstack_hack(ab):
    """Implements tf.unstack(y_true_keras, num=2, axis=-1).
       Keras-hack adopted to be compatible with theano backend.
    """
    ndim = len(K.int_shape(ab))
    if ndim == 0:
        print('can not unstack with ndim=0')
    else:
        a = ab[..., 0]
        b = ab[..., 1]
    return a, b

def weibull_loss_discrete(y_true, y_pred, name=None):
    """calculates a keras loss op designed for the sequential api.
    
        Discrete log-likelihood for Weibull hazard function on censored survival data.
        For math, see 
        https://ragulpr.github.io/assets/draft_master_thesis_martinsson_egil_wtte_rnn_2016.pdf (Page 35)
        
        Args:
            y_true: tensor with last dimension having length 2
                with y_true[:,...,0] = time to event, 
                     y_true[:,...,1] = indicator of not censored
                
            y_pred: tensor with last dimension having length 2 
                with y_pred[:,...,0] = alpha, 
                     y_pred[:,...,1] = beta

        Returns:
            A positive `Tensor` of same shape as input
            
    """    
    y,u = _keras_unstack_hack(y_true)
    a,b = _keras_unstack_hack(y_pred)

    hazard0 = K.pow((y + 1e-35) / a, b)
    hazard1 = K.pow((y + 1.0) / a, b)
    
    loglikelihoods = u * K.log(K.exp(hazard1 - hazard0) - 1.0) - hazard1
    loss = -1 * K.mean(loglikelihoods)
    return loss


def output_lambda(x, init_alpha=1.0, max_beta_value=5.0, max_alpha_value=None):
    """Elementwise (Lambda) computation of alpha and regularized beta.

        Alpha: 
        (activation) 
        Exponential units seems to give faster training than 
        the original papers softplus units. Makes sense due to logarithmic
        effect of change in alpha. 
        (initialization) 
        To get faster training and fewer exploding gradients,
        initialize alpha to be around its scale when beta is around 1.0,
        approx the expected value/mean of training tte. 
        Because we're lazy we want the correct scale of output built
        into the model so initialize implicitly; 
        multiply assumed exp(0)=1 by scale factor `init_alpha`.

        Beta: 
        (activation) 
        We want slow changes when beta-> 0 so Softplus made sense in the original 
        paper but we get similar effect with sigmoid. It also has nice features.
        (regularization) Use max_beta_value to implicitly regularize the model
        (initialization) Fixed to begin moving slowly around 1.0

        Assumes tensorflow backend.

        Args:
            x: tensor with last dimension having length 2
                with x[...,0] = alpha, x[...,1] = beta

        Usage:
            model.add(Dense(2))
            model.add(Lambda(output_lambda, arguments={"init_alpha":100., "max_beta_value":2.0}))
        Returns:
            A positive `Tensor` of same shape as input
    """
    a, b = _keras_unstack_hack(x)

    # Implicitly initialize alpha:
    if max_alpha_value is None:
        a = init_alpha * K.exp(a)
    else:
        a = init_alpha * K.clip(x=a, min_value=K.epsilon(),
                                max_value=max_alpha_value)

    m = max_beta_value
    if m > 1.05:  # some value >>1.0
        # shift to start around 1.0
        # assuming input is around 0.0
        _shift = np.log(m - 1.0)

        b = K.sigmoid(b - _shift)
    else:
        b = K.sigmoid(b)

    # Clipped sigmoid : has zero gradient at 0,1
    # Reduces the small tendency of instability after long training
    # by zeroing gradient.
    b = m * K.clip(x=b, min_value=K.epsilon(), max_value=1. - K.epsilon())

    x = K.stack([a, b], axis=-1)

    return x

```

## Generate evenly spaced sequences with noisy measurements


```python
def get_data(n_timesteps, every_nth,n_repeats,noise_level,n_features,use_censored = True):
    def get_equal_spaced(n, every_nth):
        # create some simple data of evenly spaced events recurring every_nth step
        # Each is on (time,batch)-format
        events = np.array([np.array(xrange(n)) for _ in xrange(every_nth)])
        events = events + np.array(xrange(every_nth)).reshape(every_nth, 1) + 1

        tte_actual = every_nth - 1 - events % every_nth

        was_event = (events % every_nth == 0) * 1.0
        was_event[:, 0] = 0.0

        events = tte_actual == 0

        is_censored = (events[:, ::-1].cumsum(1)[:, ::-1] == 0) * 1
        tte_censored = is_censored[:, ::-1].cumsum(1)[:, ::-1] * is_censored
        tte_censored = tte_censored + (1 - is_censored) * tte_actual

        events = np.copy(events.T * 1.0)
        tte_actual = np.copy(tte_actual.T * 1.0)
        tte_censored = np.copy(tte_censored.T * 1.0)
        was_event = np.copy(was_event.T * 1.0)
        not_censored = 1 - np.copy(is_censored.T * 1.0)

        return tte_censored, not_censored, was_event, events, tte_actual
    
    tte_censored,not_censored,was_event,events,tte_actual = get_equal_spaced(n=n_timesteps,every_nth=every_nth)

    # From https://keras.io/layers/recurrent/
    # input shape rnn recurrent if return_sequences: (nb_samples, timesteps, input_dim)

    u_train      = not_censored.T.reshape(n_sequences,n_timesteps,1)
    x_train      = was_event.T.reshape(n_sequences,n_timesteps,1)
    tte_censored = tte_censored.T.reshape(n_sequences,n_timesteps,1)
    y_train      = np.append(tte_censored,u_train,axis=2) # (n_sequences,n_timesteps,2)

    u_test       = np.ones(shape=(n_sequences,n_timesteps,1))
    x_test       = np.copy(x_train)
    tte_actual   = tte_actual.T.reshape(n_sequences,n_timesteps,1)
    y_test       = np.append(tte_actual,u_test,axis=2) # (n_sequences,n_timesteps,2)

    if not use_censored:
        x_train = np.copy(x_test)
        y_train = np.copy(y_test)
    # Since the above is deterministic perfect fit is feasible. 
    # More noise->more fun so add noise to the training data:
    
    x_train = np.tile(x_train.T,n_repeats).T
    y_train = np.tile(y_train.T,n_repeats).T

    # Try with more than one feature TODO
    x_train_new = np.zeros([x_train.shape[0],x_train.shape[1],n_features])
    x_test_new = np.zeros([x_test.shape[0],x_test.shape[1],n_features])
    for f in xrange(n_features):
        x_train_new[:,:,f] = x_train[:,:,0]
        x_test_new[:,:,f]  = x_test[:,:,0]
        
    x_train = x_train_new
    x_test  = x_test_new
    
    # xtrain is signal XOR noise with probability noise_level
    noise = np.random.binomial(1,noise_level,size=x_train.shape)
    x_train = x_train+noise-x_train*noise
    return y_train,x_train, y_test,x_test,events

```


```python
n_timesteps    = 200
n_sequences = every_nth = 80
n_features = 1
n_repeats = 1000
noise_level = 0.005
use_censored = True

y_train,x_train, y_test,x_test,events = get_data(n_timesteps, every_nth,n_repeats,noise_level,n_features,use_censored)

print 'test shape',y_test.shape,x_test.shape
plt.imshow(x_test[:,:,:].sum(axis=2)>0,interpolation="none",cmap='Accent',aspect='auto')
plt.title('x_test (lagged/deterministic event indicator)')
plt.show()
plt.imshow(y_test[:,:,0],interpolation="none")
plt.title('y_test[:,:,0] tte')
plt.show()

print 'train shape',y_train.shape,x_train.shape
# (might look like the pattern is different but set n_repeats 
#  lower and you'll see that it's the same):
plt.imshow(x_train[:,:,:].sum(axis=2)>0,interpolation="none",cmap='Accent',aspect='auto')
plt.title('x_train (lagged/noisy event indicator)')
plt.show()
plt.imshow(y_train[:,:,0],interpolation="none",aspect='auto')
plt.title('y_train[:,:,0] censored tte')
plt.show()
plt.imshow(y_train[:,:,1],interpolation="none",cmap='Accent',aspect='auto')
plt.title('y_train[:,:,1] u')
plt.show()

```

    test shape (80, 200, 2) (80, 200, 1)



![png](output_5_1.png)



![png](output_5_2.png)


    train shape (80000, 200, 2) (80000, 200, 1)



![png](output_5_4.png)



![png](output_5_5.png)



![png](output_5_6.png)


### To test your implementation, feed raw weibull data
Uncomment and run cell below


```python
# ## SANITY CHECK: Use pure Weibull data censored at C(ensoring point). 
# ## Should converge to the generating A(alpha) and B(eta) for each timestep
# def generate_data(A,B,C,shape,discrete_time):
#     # Generate Weibull random variables
#     W = np.sort(A*np.power(-np.log(np.random.uniform(0,1,shape)),1/B))
    
#     if discrete_time:
#         C = np.floor(C)
#         W = np.floor(W)

#     U = np.less_equal(W, C)*1
#     Y = np.minimum(W,C)    
#     return W,Y,U

# n_sequences = 10000
# n_timesteps = 100
# n_features = 1

# y_test,y_train,u_train = generate_data(A=2.,
#                                        B=2.,
#                                        C=np.Inf, # <np.inf -> impose censoring
#                                        shape=[n_sequences,n_timesteps,n_features],
#                                        discrete_time=True)
# x_train = x_test = np.ones_like(y_train)

# y_test   = np.append(y_test,np.ones_like(y_test),axis=-1)
# y_train  = np.append(y_train,u_train,axis=-1)

# xlim_temp = y_test[:,:,0].max()
# plt.hist(y_test[:,:,0].flatten(),bins=100)
# plt.xlim(0,xlim_temp)
# plt.title('actual tte')
# plt.show()
# plt.hist(y_train[:,:,0].flatten(),bins=100)
# plt.title('censored tte')
# plt.xlim(0,xlim_temp)
# plt.show()
# print y_test.shape
# print y_train.shape
```


```python
# Prep. output activation layer.
# Start at naive geometric (beta=1) MLE:
tte_mean_train = np.nanmean(y_train[:,:,0])
init_alpha = -1.0/np.log(1.0-1.0/(tte_mean_train+1.0) )
init_alpha = init_alpha/np.nanmean(y_train[:,:,1]) # use if lots of censoring
print 'init_alpha: ',init_alpha

np.random.seed(1)
# Store some history
history = History()

# Start building the model
model = Sequential()
#model.add(TimeDistributed(Dense(2), input_shape=(None, n_features)))
model.add(GRU(1, input_shape=(n_timesteps, n_features),activation='tanh',return_sequences=True))

model.add(Dense(2))
model.add(Lambda(output_lambda, arguments={"init_alpha":init_alpha, 
                                               "max_beta_value":4.0}))

model.compile(loss=weibull_loss_discrete, optimizer=adam(lr=.01))

model.summary()
```

    init_alpha:  43.4425042957


    /usr/local/lib/python2.7/site-packages/keras/layers/core.py:640: UserWarning: `output_shape` argument not specified for layer lambda_1 and cannot be automatically inferred with the Theano backend. Defaulting to output shape `(None, 200, 2)` (same as input shape). If the expected output shape is different, specify it via the `output_shape` argument.
      .format(self.name, input_shape))


    _________________________________________________________________
    Layer (type)                 Output Shape              Param #   
    =================================================================
    gru_1 (GRU)                  (None, 200, 1)            9         
    _________________________________________________________________
    dense_1 (Dense)              (None, 200, 2)            4         
    _________________________________________________________________
    lambda_1 (Lambda)            (None, 200, 2)            0         
    =================================================================
    Total params: 13.0
    Trainable params: 13.0
    Non-trainable params: 0.0
    _________________________________________________________________



```python
# Fit! (really don't need to train this long)
np.random.seed(1)
model.fit(x_train, y_train,
          epochs=80, 
          batch_size=x_train.shape[0]/10, 
          verbose=2, 
          validation_data=(x_test, y_test),
          callbacks=[history]
          )
```

    Train on 80000 samples, validate on 80 samples
    Epoch 1/80
    11s - loss: 3.8191 - val_loss: 4.6541
    Epoch 2/80
    10s - loss: 3.7907 - val_loss: 4.6171
    Epoch 3/80
    10s - loss: 3.7663 - val_loss: 4.5857
    Epoch 4/80
    10s - loss: 3.7485 - val_loss: 4.5637
    Epoch 5/80
    10s - loss: 3.7397 - val_loss: 4.5530
    Epoch 6/80
    10s - loss: 3.7370 - val_loss: 4.5486
    Epoch 7/80
    11s - loss: 3.7347 - val_loss: 4.5456
    Epoch 8/80
    11s - loss: 3.7319 - val_loss: 4.5434
    Epoch 9/80
    11s - loss: 3.7291 - val_loss: 4.5401
    Epoch 10/80
    11s - loss: 3.7261 - val_loss: 4.5355
    Epoch 11/80
    11s - loss: 3.7228 - val_loss: 4.5310
    Epoch 12/80
    11s - loss: 3.7186 - val_loss: 4.5243
    Epoch 13/80
    11s - loss: 3.7122 - val_loss: 4.5136
    Epoch 14/80
    10s - loss: 3.6993 - val_loss: 4.4880
    Epoch 15/80
    10s - loss: 3.6919 - val_loss: 4.4621
    Epoch 16/80
    11s - loss: 3.6798 - val_loss: 4.4209
    Epoch 17/80
    11s - loss: 3.6447 - val_loss: 4.3645
    Epoch 18/80
    11s - loss: 3.5865 - val_loss: 4.2377
    Epoch 19/80
    10s - loss: 3.5341 - val_loss: 4.1947
    Epoch 20/80
    11s - loss: 3.4872 - val_loss: 4.1166
    Epoch 21/80
    10s - loss: 3.4486 - val_loss: 4.0797
    Epoch 22/80
    10s - loss: 3.4188 - val_loss: 4.0404
    Epoch 23/80
    11s - loss: 3.3948 - val_loss: 4.0021
    Epoch 24/80
    11s - loss: 3.3757 - val_loss: 3.9743
    Epoch 25/80
    11s - loss: 3.3635 - val_loss: 3.9557
    Epoch 26/80
    11s - loss: 3.3512 - val_loss: 3.9331
    Epoch 27/80
    11s - loss: 3.3417 - val_loss: 3.9167
    Epoch 28/80
    11s - loss: 3.3342 - val_loss: 3.9060
    Epoch 29/80
    11s - loss: 3.3283 - val_loss: 3.8885
    Epoch 30/80
    10s - loss: 3.3227 - val_loss: 3.8787
    Epoch 31/80
    10s - loss: 3.3186 - val_loss: 3.8672
    Epoch 32/80
    10s - loss: 3.3155 - val_loss: 3.8665
    Epoch 33/80
    10s - loss: 3.3119 - val_loss: 3.8523
    Epoch 34/80
    10s - loss: 3.3091 - val_loss: 3.8429
    Epoch 35/80
    11s - loss: 3.3055 - val_loss: 3.8411
    Epoch 36/80
    11s - loss: 3.3047 - val_loss: 3.8330
    Epoch 37/80
    11s - loss: 3.3015 - val_loss: 3.8266
    Epoch 38/80
    10s - loss: 3.2989 - val_loss: 3.8194
    Epoch 39/80
    10s - loss: 3.2961 - val_loss: 3.8151
    Epoch 40/80
    10s - loss: 3.3029 - val_loss: 3.8122
    Epoch 41/80
    10s - loss: 3.2975 - val_loss: 3.8166
    Epoch 42/80
    10s - loss: 3.2955 - val_loss: 3.8085
    Epoch 43/80
    11s - loss: 3.2927 - val_loss: 3.8018
    Epoch 44/80
    11s - loss: 3.2912 - val_loss: 3.7984
    Epoch 45/80
    11s - loss: 3.2894 - val_loss: 3.7936
    Epoch 46/80
    10s - loss: 3.2884 - val_loss: 3.7900
    Epoch 47/80
    10s - loss: 3.2880 - val_loss: 3.7885
    Epoch 48/80
    11s - loss: 3.2915 - val_loss: 3.7957
    Epoch 49/80
    11s - loss: 3.2887 - val_loss: 3.7883
    Epoch 50/80
    10s - loss: 3.2860 - val_loss: 3.7837
    Epoch 51/80
    11s - loss: 3.2852 - val_loss: 3.7788
    Epoch 52/80
    11s - loss: 3.2954 - val_loss: 3.7846
    Epoch 53/80
    12s - loss: 3.2907 - val_loss: 3.7821
    Epoch 54/80
    11s - loss: 3.2854 - val_loss: 3.7786
    Epoch 55/80
    11s - loss: 3.2840 - val_loss: 3.7744
    Epoch 56/80
    11s - loss: 3.2835 - val_loss: 3.7717
    Epoch 57/80
    11s - loss: 3.2841 - val_loss: 3.7843
    Epoch 58/80
    10s - loss: 3.2891 - val_loss: 3.7705
    Epoch 59/80
    11s - loss: 3.2844 - val_loss: 3.7701
    Epoch 60/80
    13s - loss: 3.2824 - val_loss: 3.7674
    Epoch 61/80
    12s - loss: 3.2826 - val_loss: 3.7726
    Epoch 62/80
    14s - loss: 3.2871 - val_loss: 3.7733
    Epoch 63/80
    21s - loss: 3.2853 - val_loss: 3.7680
    Epoch 64/80
    14s - loss: 3.2832 - val_loss: 3.7687
    Epoch 65/80
    12s - loss: 3.2836 - val_loss: 3.7716
    Epoch 66/80
    13s - loss: 3.2841 - val_loss: 3.7689
    Epoch 67/80
    11s - loss: 3.2839 - val_loss: 3.7638
    Epoch 68/80
    11s - loss: 3.2819 - val_loss: 3.7622
    Epoch 69/80
    12s - loss: 3.2818 - val_loss: 3.7596
    Epoch 70/80
    11s - loss: 3.2819 - val_loss: 3.7604
    Epoch 71/80
    11s - loss: 3.2820 - val_loss: 3.7582
    Epoch 72/80
    11s - loss: 3.2827 - val_loss: 3.7700
    Epoch 73/80
    10s - loss: 3.2813 - val_loss: 3.7620
    Epoch 74/80
    11s - loss: 3.2821 - val_loss: 3.7586
    Epoch 75/80
    11s - loss: 3.2799 - val_loss: 3.7554
    Epoch 76/80
    11s - loss: 3.2790 - val_loss: 3.7553
    Epoch 77/80
    11s - loss: 3.2802 - val_loss: 3.7536
    Epoch 78/80
    11s - loss: 3.2812 - val_loss: 3.7793
    Epoch 79/80
    10s - loss: 3.2875 - val_loss: 3.7648
    Epoch 80/80
    11s - loss: 3.2811 - val_loss: 3.7564





    <keras.callbacks.History at 0x138392450>




```python
plt.plot(history.history['loss'],    label='training')
plt.plot(history.history['val_loss'],label='validation')
plt.legend()
```




    <matplotlib.legend.Legend at 0x10c765cd0>




![png](output_10_1.png)


# Predictions
Try out training the model with different levels of noise. With more noise confidence gets lower (smaller beta). With less noise beta goes to maximum value and the predicted mode/peak probability is centered around the actual TTE.


```python
# Make some parametric predictions
print 'All test cases (no noise)'
print '(each horizontal line is a sequence)'
predicted = model.predict(x_test)
plt.imshow(x_test[:every_nth,:,:].sum(axis=2)>0,interpolation="none",cmap='Accent')
plt.title('x features')
plt.show()
print(predicted.shape)
plt.imshow(predicted[:,:,0],interpolation="none")
plt.title('alpha')
plt.show()
plt.imshow(predicted[:,:,1],interpolation="none")
plt.title('beta')
plt.show()

print 'Some training cases (noisy features)'
predicted = model.predict(x_train[:every_nth,:,:])
print(predicted.shape)
plt.imshow(x_train[:every_nth,:,:].sum(axis=2)>0,interpolation="none",cmap='Accent')
plt.title('x features')
plt.show()
plt.imshow(predicted[:,:,0],interpolation="none")
plt.title('alpha')
plt.show()
plt.imshow(predicted[:,:,1],interpolation="none")
plt.title('beta')
plt.show()

```

    All test cases (no noise)
    (each horizontal line is a sequence)



![png](output_12_1.png)


    (80, 200, 2)



![png](output_12_3.png)



![png](output_12_4.png)


    Some training cases (noisy features)
    (80, 200, 2)



![png](output_12_6.png)



![png](output_12_7.png)



![png](output_12_8.png)



```python
def weibull_quantiles(a, b, p):
    return a*np.power(-np.log(1.0-p),1.0/b)

def weibull_mode(a, b):
    # Continuous mode. 
    # TODO (mathematically) prove how close it is to discretized mode
    mode = a*np.power((b-1.0)/b,1.0/b)
    mode[b<=1.0]=0.0
    return mode

def weibull_mean(a, b):
    # Continuous mean. Theoretically at most 1 step below discretized mean 
    # E[T ] <= E[Td] + 1 true for positive distributions. 
    from scipy.special import gamma
    return a*gamma(1.0+1.0/b)

# TTE, Event Indicator, Alpha, Beta
drawstyle = 'steps-post'

print 'one training case:'
batch_indx =every_nth/2
a = predicted[batch_indx,:,0]
b = predicted[batch_indx,:,1]
this_x_train = x_train[batch_indx,:,:].mean(axis=1)
this_x_test =  x_test[batch_indx,:,:].mean(axis=1)

this_tte_train = y_train[batch_indx,:,0]
this_tte_test =  y_test[batch_indx,:,0]

plt.plot(a,drawstyle='steps-post')
plt.title('predicted alpha')
plt.show()
plt.plot(b,drawstyle='steps-post')
plt.title('predicted beta')
plt.show()

plt.bar(xrange(n_timesteps),this_x_test,color='black', label='lagged event')
plt.bar(xrange(n_timesteps),this_x_train,color='red',linewidth=0,label='mean feature input')
plt.title('event')
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.show()

plt.plot(this_tte_test ,label='actual tte',color='black',linewidth=2,drawstyle=drawstyle)
plt.plot(this_tte_train,label='censored tte',color='black',linestyle='dashed',linewidth=2,drawstyle=drawstyle)

plt.plot(weibull_quantiles(a,b,0.75),color='blue',label='pred <0.75',drawstyle=drawstyle)
plt.plot(weibull_mode(a, b), color='red',linewidth=2,label='pred mode/peak prob',drawstyle=drawstyle)
#plt.plot(weibull_mean(a, b), color='green',linewidth=1,label='pred mean',drawstyle='steps-post')
plt.plot(weibull_quantiles(a,b,0.25),color='blue',label='pred <0.25',drawstyle=drawstyle)
plt.xlabel('time')
plt.ylabel('time to event')
plt.bar(xrange(n_timesteps),this_x_train,color='red',label='mean feature input')

plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.show()
```

    one training case:



![png](output_13_1.png)



![png](output_13_2.png)



![png](output_13_3.png)



![png](output_13_4.png)

