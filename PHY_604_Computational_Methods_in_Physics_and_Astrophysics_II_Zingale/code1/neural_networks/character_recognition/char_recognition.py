"""A neural network for recognizing the handdrawn digits in the MNIST
database.  This follows the ideas from Franklin Ch 14 and the book by
Rashid.

"""

# Note: this uses progressbar2 to show status durning training.  Do
# pip3 install progressbar2 --user

import numpy as np
import matplotlib.pyplot as plt
import progressbar

class TrainingDigit(object):
    """a handwritten digit from the MNIST training set"""

    def __init__(self, raw_string):
        """we feed this a single line from the MNIST data set"""
        self.raw_string = raw_string

        # make the data range from 0.01 to 1.00
        _tmp = raw_string.split(",")
        self.scaled = np.asfarray(_tmp[1:])/255.0 * 0.99 + 0.01

        # the correct answer
        self.num = int(_tmp[0])

        # the output for the NN as a bit array -- make this lie in [0.01, 0.99]
        self.out = np.zeros((10)) + 0.01
        self.out[self.num] = 0.99

    def plot(self, outfile=None, output=None):
        """plot the digit"""
        plt.clf()
        plt.imshow(self.scaled.reshape((28, 28)),
                   cmap="Greys", interpolation="nearest")
        if output is not None:
            dstr = ["{}: {:6.4f}".format(n, v) for n, v in enumerate(output)]
            ostr = "correct digit: {}\n".format(self.num)
            ostr += "  ".join(dstr[0:5]) + "\n" + "  ".join(dstr[5:])
            plt.title("{}".format(ostr), fontsize="x-small")
        if outfile is not None:
            plt.savefig(outfile, dpi=150)

class TrainingSet(object):
    """Manage reading digits from the MNIST training set csv and provide
    methods for returning the next digit, reading only what is needed
    each time.

    """

    def __init__(self):
        self.f = open("mnist_train.csv", "r")

    def get_next(self):
        """return the next digit from the training database"""
        return TrainingDigit(self.f.readline())

    def reset(self):
        """reset the training set to the first digit in the database"""
        self.f.seek(0)

    def close(self):
        """close the training set file"""
        # should implement a context manager
        self.f.close()

class UnknownDigit(TrainingDigit):
    """A digit from the MNIST test database.  This provides a method to
    compare a NN result to the correct answer

    """
    def __init__(self, raw_string):
        super().__init__(raw_string)
        self.out = None

    def check_output(self, out):
        """given the output array from the NN, return True if it is
        correct for this digit"""
        guess = np.argmax(out)
        return guess == self.num

class TestSet(object):
    """read the next digit from the test set csv file and return a
    UnknownDigit object"""

    def __init__(self):
        self.f = open("mnist_test.csv", "r")

    def get_next(self):
        """return the next digit from the test database"""
        return UnknownDigit(self.f.readline())

    def close(self):
        """close the test set file"""
        # should implement a context manager
        self.f.close()


class NeuralNetwork(object):
    """A neural network class with a single hidden layer."""

    def __init__(self, num_training_unique=100, n_epochs=10,
                 learning_rate=0.1,
                 hidden_layer_size=100):

        self.num_training_unique = num_training_unique
        self.n_epochs = n_epochs

        self.train_set = TrainingSet()

        # learning rate
        self.eta = learning_rate

        # we get the size of the layers from the length of the input
        # and output
        d = self.train_set.get_next()

        # the number of nodes/neurons on the output layer
        self.m = len(d.out)

        # the number of nodes/neurons on the input layer
        self.n = len(d.scaled)

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
            self.train_set.reset()
            print("epoch {} of {}".format(i+1, self.n_epochs))
            bar = progressbar.ProgressBar()
            for q in bar(range(self.num_training_unique)):

                model = self.train_set.get_next()

                x = model.scaled.reshape(self.n, 1)
                y = model.out.reshape(self.m, 1)

                z_tilde = self.g(self.B @ x)
                z = self.g(self.A @ z_tilde)

                e = z - y
                e_tilde = self.A.T @ e

                dA = -2*self.eta * e * z*(1-z) @ z_tilde.T
                dB = -2*self.eta * e_tilde * z_tilde*(1-z_tilde) @ x.T

                self.A[:, :] += dA
                self.B[:, :] += dB


        self.train_set.close()

    def predict(self, model):
        """ predict the outcome using our trained matrix A """
        y = self.g(self.A @ (self.g(self.B @ model.scaled)))
        return y


def main(n_epochs=5, hidden_layer_size=100,
         num_training_unique=60000,
         learning_rate=0.1,
         do_plots=True):
    """a driver for the NN"""

    nn = NeuralNetwork(num_training_unique=num_training_unique,
                       learning_rate=learning_rate,
                       hidden_layer_size=hidden_layer_size,
                       n_epochs=n_epochs)

    # train
    nn.train()

    # histogram of the matrix elements
    if do_plots:
        plt.clf()
        plt.hist(nn.A.flatten(), bins=20)
        plt.title(r"${\bf A}$")
        plt.savefig("A_hist.png", dpi=150, bbox_inches="tight")

        plt.clf()
        plt.hist(nn.B.flatten(), bins=20)
        plt.title(r"${\bf B}$")
        plt.savefig("B_hist.png", dpi=150, bbox_inches="tight")

    # now try it out on the unseen data from the test database
    unk = TestSet()
    n_test = 10000
    n_correct = 0
    for t in range(n_test):
        d = unk.get_next()
        num_guess = nn.predict(d)
        if d.check_output(num_guess):
            n_correct += 1
        else:
            if do_plots:
                d.plot(outfile="incorrect_digit_{:04d}.png".format(t), output=num_guess)

    unk.close()

    return n_correct/n_test


if __name__ == "__main__":
    f_right = main(do_plots=False)

    print("correct fraction: {}".format(f_right))
 
