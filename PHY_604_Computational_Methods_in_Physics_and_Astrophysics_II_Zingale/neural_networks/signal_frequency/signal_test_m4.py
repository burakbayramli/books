# a neural network w/ hidden layers example.  We feed in noisy data with one
# of four frequencies and we want the net to return the frequency.

import random

import numpy as np
import matplotlib.pyplot as plt

ALLOWED_FREQS = [1, 2, 3, 4]

class SignalData(object):
    """this is the model data for our frequency training set.  We produce
    a signal (cosine with one of the allowed frequencies) and pollute
    it with noise.  The output is an array with an 1 in the slot
    corresponding to the correct frequency.

    """
    def __init__(self, N=101):
        self.N = N

        self.t = np.linspace(0.0, 1.0, N)

        # frequency
        self.f = random.choice(ALLOWED_FREQS)

        # random numbers in (-1, 1)
        noise = 2.0*np.random.random(N) - 1.0

        # input data
        self.x_orig = np.cos(2.0*np.pi*self.f*self.t)
        self.x = self.x_orig + 5.0*noise

        # output (scaled)
        self.y = np.zeros((len(ALLOWED_FREQS))) + 0.01
        self.y[self.f-1] = 1.0

    def interpret_frequency(self, out):
        """given the output from the neural network, determine which frequency
        was most preferred.

        """
        return ALLOWED_FREQS[np.argmax(out)]

    def plot(self, oname="model.png"):
        """plot the signal data"""
        plt.clf()
        plt.scatter(self.t, self.x, color="C0")
        plt.plot(self.t, self.x_orig, color="C1", ls=":", label="f = {}".format(self.f))
        plt.legend(frameon=False)
        plt.savefig(oname)


class NeuralNetwork(object):
    """A neural network class with a single hidden layer."""

    def __init__(self, num_training_unique=100, n_epochs=10,
                 learning_rate=0.1,
                 hidden_layer_size=100):

        self.num_training_unique = num_training_unique
        self.n_epochs = n_epochs

        self.train_set = []
        for _ in range(self.num_training_unique):
            self.train_set.append(SignalData())

        # learning rate
        self.eta = learning_rate

        # we get the size of the layers from the length of the input
        # and output
        model = self.train_set[0]

        # the number of nodes/neurons on the output layer
        self.m = len(model.y)

        # the number of nodes/neurons on the input layer
        self.n = len(model.x)

        # the number of nodes/neurons on the hidden layer
        self.k = hidden_layer_size

        # we will initialize the weights with Gaussian normal random
        # numbers centered on 0 with a width of 1/sqrt(n), where n is
        # the length of the input state

        # A is the set of weights between the hidden layer and output layer
        self.A = np.random.normal(0.0, 1.0/np.sqrt(self.k), (self.m, self.k))

        # B is the set of weights between the input layer and hidden layer
        self.B = np.random.normal(0.0, 1.0/np.sqrt(self.n), (self.k, self.n))

    def g(self, p):
        """our sigmoid function that operates on the hidden layer"""
        return 1.0/(1.0 + np.exp(-p))

    def train(self):
        """Train the neural network by doing gradient descent with back
        propagation to set the matrix elements in B (the weights
        between the input and hidden layer) and A (the weights between
        the hidden layer and output layer)

        """

        for i in range(self.n_epochs):

            print("epoch {} of {}".format(i+1, self.n_epochs))

            for _ in range(self.num_training_unique):

                model = random.choice(self.train_set)

                x = model.x.reshape(self.n, 1)
                y = model.y.reshape(self.m, 1)

                z_tilde = self.g(self.B @ x)
                z = self.g(self.A @ z_tilde)

                e = z - y
                e_tilde = self.A.T @ e

                dA = -2*self.eta * e * z*(1-z) @ z_tilde.T
                dB = -2*self.eta * e_tilde * z_tilde*(1-z_tilde) @ x.T

                self.A[:, :] += dA
                self.B[:, :] += dB

    def predict(self, model):
        """ predict the outcome using our trained matrix A """
        y = self.g(self.A @ (self.g(self.B @ model.x)))
        return y

def main(k=2):

    # length of our input vector
    nn = NeuralNetwork(num_training_unique=1000, n_epochs=5, hidden_layer_size=k,
                       learning_rate=0.05)

    # train
    nn.train()

    # try it out -- first on our original training set data
    err = []
    for q, model in enumerate(nn.train_set):
        y_nn = nn.predict(model)
        err.append(abs(model.interpret_frequency(y_nn) - model.f))
        if q == 0:
            model.plot()

    plt.clf()
    markerline, stemlines, baseline = plt.stem(err, ":", color="C0")
    plt.setp(stemlines, "color", "C0")
    plt.savefig("trained_data_{}.png".format(k), dpi=150)


    plt.clf()
    bins = list(range(5))
    plt.hist(err, bins=bins)
    bins_labels(bins)
    plt.savefig("trained_hist_{}.png".format(k), dpi=150)

    # now try it out on 100 different new random sequences
    err = []
    for _ in range(100):
        model = SignalData()
        y_nn = nn.predict(model)
        err.append(abs(model.interpret_frequency(y_nn) - model.f))

    plt.clf()
    markerline, stemlines, baseline = plt.stem(err, ":", color="C0")
    plt.setp(stemlines, "color", "C0")
    plt.savefig("random_data_{}.png".format(k), dpi=150)

    plt.clf()
    plt.hist(err, bins=bins)
    bins_labels(bins)
    plt.savefig("random_hist_{}.png".format(k), dpi=150)


def bins_labels(bins, **kwargs):
    """pretty histogram bin labels from
    https://stackoverflow.com/questions/23246125/how-to-center-labels-in-histogram-plot"""
    bin_w = (max(bins) - min(bins)) / (len(bins) - 1)
    plt.xticks(np.arange(min(bins)+bin_w/2, max(bins), bin_w), bins, **kwargs)
    plt.xlim(bins[0], bins[-1])

if __name__ == "__main__":
    for hidden_size in [1, 2, 4, 8, 16, 32, 64]:
        main(k=hidden_size)
