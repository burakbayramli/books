# compare to the neural net by taking an FFT and returning simply the
# frequency with the most power

import numpy as np
import matplotlib.pyplot as plt

import signal_test as st

def fft_predict(model, do_plot=False):

    # the fft expects periodic data, so leave off the last point
    x = model.x[:-1]

    x_fft = np.fft.rfft(x)

    # power spectrum
    x_ps = np.abs(x_fft)**2

    # just return the postive frequencies
    k = np.fft.rfftfreq(len(x))
    dt = model.t[1] - model.t[0]
    kfreq = k/dt

    idx = (np.where(x_ps == x_ps.max()))[0][0]

    if do_plot:
        plt.clf()
        plt.plot(kfreq, x_fft)
        plt.savefig("fft_model.png")

    # the model scales the frequencies, so make sure we do too
    return kfreq[idx]/model.scaling

def main():

    # now try it out on 100 different new random sequences
    err = []
    for q in range(100):
        model = st.SignalData()
        if q == 0:
            do_plot = True
        else:
            do_plot = False

        y_fft = fft_predict(model, do_plot=do_plot)
        err.append(abs(float(model.round_to_allowed(y_fft) - model.f)))

    plt.clf()
    bins = list(range(5))
    plt.hist(err, bins=bins)
    st.bins_labels(bins)
    plt.savefig("fft_hist.png", dpi=150)


if __name__ == "__main__":
    main()
