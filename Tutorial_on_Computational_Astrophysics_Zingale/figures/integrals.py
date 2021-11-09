import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon

mpl.rcParams['mathtext.fontset'] = 'cm'
mpl.rcParams['mathtext.rm'] = 'serif'


def f(x):
    """ the function we are integrating """
    return 1.0 + x*0.25*np.sin(np.pi*x)


plt.rcParams.update({'xtick.labelsize': 18,
                     'ytick.labelsize': 18,
                     'font.size': 18})



def plot_base(xp, fp, xfine, a, b, label_mid=False):

    fmax = fp.max()

    for xl in xp:
        plt.plot([xl,xl], [0.0, 1.2*fmax], ls="--", color="0.5", zorder=-1)

    plt.scatter(xp, fp, marker="o", color="r", zorder=100)

    plt.figtext(0.9, 0.05, '$x$', fontsize=20)
    plt.figtext(0.1, 0.9, '$y$', fontsize=20)

    ax = plt.gca()

    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.xaxis.set_ticks_position('bottom')

    if label_mid:
        ax.set_xticks((a, (a+b)/2, b))
        ax.set_xticklabels(('$a$', r'$\frac{(a+b)}{2}$', '$b$'))
    else:
        ax.set_xticks((a, b))
        ax.set_xticklabels(('$a$', '$b$'))

    ax.set_yticks([])

    plt.plot(xfine, f(xfine), "r", linewidth=2)

    plt.xlim(np.min(xfine), 1.05*np.max(xfine))
    plt.ylim(ymin = 0)



def rectangle(xp, fp, a, b):

    ax = plt.gca()

    integral = 0.0

    for n in range(len(xp)-1):

        xl = xp[n]
        xr = xp[n+1]

        fl = fp[n]

        # shade region
        verts = [(xl, 0), (xl, fl), (xr, fl), (xr, 0)]
        ax.add_patch(Polygon(verts, facecolor="0.8", edgecolor="k"))

        # and bonus! actually compute the integral in this approximation
        integral += (xr - xl) * fl

    return integral


def trapezoid(xp, fp, a, b):

    ax = plt.gca()

    integral = 0.0

    for n in range(len(xp)-1):

        xl = xp[n]
        xr = xp[n+1]

        # shade region
        fl = f(xl)
        fr = f(xr)

        verts = [(xl, 0), (xl, fl), (xr, fr), (xr, 0)]
        ax.add_patch(Polygon(verts, facecolor="0.8", edgecolor="k"))

        integral += 0.5 * (xr - xl) * (fl + fr)

    return integral

def simpsons(xp, fp, a, b):

    ax = plt.gca()

    integral = 0.0

    for n in range(0, len(xp)-1, 2):

        # we need to handle the 1 bin case specially

        if len(xp) == 2:

            xl = xp[0]
            xr = xp[1]
            xm = 0.5 * (xl + xr)

            f0 = f(xl)
            f1 = f(xm)
            f2 = f(xr)

        else:
            f0 = fp[n]
            f1 = fp[n+1]
            f2 = fp[n+2]

            xl = xp[n]
            xr = xp[n+2]

        delta = 0.5*(xr - xl)

        A = (f0 - 2*f1 + f2)/(2*delta**2)
        B = -(f2 - 4*f1 + 3*f0)/(2*delta)
        C = f0

        xsimp = np.linspace(xl, xr, 100)
        fsimp = A * (xsimp - xl)**2  + B * (xsimp - xl) + C

        simpvert = list(zip(xsimp, fsimp))

        verts = [(xl, 0)] + simpvert + [(xr, 0)]
        ax.add_patch(Polygon(verts, facecolor="0.8", edgecolor="k"))

        integral += (xr - xl) / 6.0 * (f0 + 4 * f1 + f2)

    return integral

def main():

    N_fine = 200

    # the number of bins to divide [a, b]
    N_bins = 6

    xmin = 0.5
    xmax = 1.5

    dx_extra  = 0.5

    # add a bin on each end of the domain outside of the integral
    xmin_plot = xmin - dx_extra
    xmax_plot = xmax + dx_extra

    xfine = np.linspace(xmin_plot, xmax_plot, N_fine+2)

    xp = np.linspace(xmin, xmax, N_bins+1)

    # integral range
    a = xmin
    b = xmax

    # function points
    fp = f(xp)


    # rectangle method

    plt.clf()

    plot_base(xp, fp, xfine, a, b)
    I_r = rectangle(xp, fp, a, b)

    plt.savefig(f"rectangle_N{N_bins}.png", bbox_inches="tight")

    # trapezoid method

    plt.clf()

    plot_base(xp, fp, xfine, a, b)
    I_t = trapezoid(xp, fp, a, b)

    plt.savefig(f"trapezoid_N{N_bins}.png", bbox_inches="tight")


    # simpsons method

    plt.clf()

    xp_tmp = list(xp)
    fp_tmp = list(fp)
    label_mid = False

    # if N_bins is 1, we need an extra point for Simpsons
    if N_bins == 1:
        xp_tmp.append((a + b)/2)
        fp_tmp.append(f((a + b)/2))
        label_mid = True

    plot_base(np.array(xp_tmp), np.array(fp_tmp), xfine, a, b,
              label_mid=label_mid)


    I_s = simpsons(xp, fp, a, b)

    plt.savefig(f"simpsons_N{N_bins}.png", bbox_inches="tight")

    print(f"integral approximations: {I_r}, {I_t}, {I_s}")

if __name__ == "__main__":
    main()



