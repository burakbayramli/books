"""
*NOTE:* This version is slightly modified from the one in
    $CLAW/visclaw/src/python/visclaw/animation_tools.py

Some functions requires JSAnimation, either from Clawpack 
or by installing it separately from
    https://github.com/jakevdp/JSAnimation

This animation_tools module contains tools to create animations in Python and
Jupyter notebooks.

Three types of animations are supported:
 - using the ipywidget interact to create a figure with a slider bar,
 - using JSAnimation to create Javascript code that loops over a set of
   images and adds controls to play as an animation.
 - creation of mp4 files using ffmpeg (provided this package is installed).

The set of images to combine in an animation can be specified as a
list of images, a list of `matplotlib` figures, or a directory of
`png` or other image files.

Utilities are provided to convert between these.

Functions are provided to create inline animations in Jupyter notebooks or
stand-alone files that can be viewed in other ways, including
 - An html file with the JSAnimation version,
 - A mp4 file,
 - A reStructured text file with the JSAnimation for inclusion in Sphinx docs.

The utility function make_anim_from_plotdir can be used to convert the png
files in a Clawpack _plots directory into standalone animations of the types
listed above.  See the file make_anim.py for an example of how this can be
invoked from an applications directory.

See also:
 https://ipywidgets.readthedocs.io/en/latest/#ipywidgets
 https://github.com/jakevdp/JSAnimation

More documentation of these functions is needed and they can probably be
improved.

"""

# use Python 3 style print function rather than Python 2 print statements:
from __future__ import print_function

from IPython.display import display
from matplotlib import image, animation
from IPython.display import HTML
from matplotlib import pyplot as plt
from ipywidgets import interact, interact_manual
import ipywidgets

try:
    from JSAnimation import IPython_display
except:
    try:
        from clawpack.visclaw.JSAnimation import IPython_display
    except:
        print("*** Warning: JSAnimation not found")
        


def make_plotdir(plotdir='_plots', clobber=True):
    """
    Utility function to create a directory for storing a sequence of plot
    files, or if the directory already exists, clear out any old plots.
    If clobber==False then it will abort instead of deleting existing files.
    """

    import os
    if os.path.isdir(plotdir):
        if clobber:
            os.system("rm %s/*" % plotdir)
        else:
            raise IOError('*** Cannot clobber existing directory %s' % plotdir)
    else:
        os.system("mkdir %s" % plotdir)
    print("Figure files for each frame will be stored in ", plotdir)


def save_frame(frameno, plotdir='_plots', fname_base='frame', format='png',
               verbose=False, **kwargs):
    """
    After giving matplotlib commands to create the plot for a single frame
    of the desired animation, this can be called to save the figure with
    the appropriate file name such as _plots/frame00001.png.
    """

    plt.draw()
    filename = '%s/%s%s.%s' % (plotdir, fname_base, str(frameno).zfill(5), format)
    plt.savefig(filename, **kwargs)
    if verbose:
        print("Saved ",filename)


def make_anim(plotdir, fname_pattern='frame*.png', figsize=(10,6), dpi=None):
    """
    Assumes that a set of frames are available as png files in directory _plots,
    numbered consecutively, e.g. frame0000.png, frame0001.png, etc.

    Creates an animation based display each frame in turn, and returns anim.

    You can then display anim in an IPython notebook, or
    call make_html(anim) to create a stand-alone webpage.
    """

    import matplotlib

    if matplotlib.backends.backend in ['MacOSX']:
        print("*** animation.FuncAnimation doesn't work with backend %s"
              % matplotlib.backends.backend)
        print("*** Suggest using 'Agg'")
        return

    import glob   # for finding all files matching a pattern

    # Find all frame files:
    filenames = glob.glob('%s/%s' % (plotdir, fname_pattern))

    # sort them into increasing order:
    filenames=sorted(filenames)

    fig = plt.figure(figsize=figsize, dpi=dpi)
    ax = fig.add_axes([0, 0, 1, 1])
    ax.axis('off')  # so there's not a second set of axes
    im = plt.imshow(image.imread(filenames[0]))

    def init():
        im.set_data(image.imread(filenames[0]))
        return im,

    def animate(i):
        image_i=image.imread(filenames[i])
        im.set_data(image_i)
        return im,

    anim = animation.FuncAnimation(fig, animate, init_func=init,
                                   frames=len(filenames), interval=200,
                                   blit=True)

    return anim


def JSAnimate_images(images, figsize=(10,6), dpi=None):
    "Turn a list of images into a JSAnimation."

    import matplotlib

    if matplotlib.backends.backend in ['MacOSX']:
        print("*** animation.FuncAnimation doesn't work with backend %s"
              % matplotlib.backends.backend)
        print("*** Suggest using 'Agg'")
        return

    fig = plt.figure(figsize=figsize, dpi=dpi)
    ax = fig.add_axes([0, 0, 1, 1])
    ax.axis('off')  # so there's not a second set of axes

    im = plt.imshow(images[0])

    def init():
        im.set_data(images[0])
        return im,

    def animate(i):
        im.set_data(images[i])
        return im,

    anim = animation.FuncAnimation(fig, animate, init_func=init,
                                   frames=len(images), interval=200,
                                   blit=True)

    plt.close(fig)
    return HTML(anim.to_jshtml())


def make_html(anim, file_name='anim.html', title=None, raw_html='',
              fps=None, embed_frames=True, default_mode='once'):
    """
    Take an animation created by make_anim and convert it into a stand-alone
    html file.
    """
    try:
        from JSAnimation.IPython_display import anim_to_html
    except:
        try:
            from clawpack.visclaw.JSAnimation.IPython_display import anim_to_html
        except:
            print("*** Warning: JSAnimation not found, cannot import anim_to_html")

    html_body = anim_to_html(anim, fps=fps, embed_frames=embed_frames, \
                 default_mode=default_mode)

    html_file = open(file_name,'w')
    html_file.write("<html>\n <h1>%s</h1>\n" % title)
    html_file.write(raw_html)
    html_file.write(html_body)
    html_file.close()
    print("Created %s" % file_name)


def make_rst(anim, file_name='anim.rst',
              fps=None, embed_frames=True, default_mode='once'):
    """
    Take an animation created by make_anim and convert it into an rst file
    (reStructuredText, for inclusion in Sphinx documentation, for example).
    """
    try:
        from JSAnimation.IPython_display import anim_to_html
    except:
        try:
            from clawpack.visclaw.JSAnimation.IPython_display import anim_to_html
        except:
            print("*** Warning: JSAnimation not found, cannot import anim_to_html")

    rst_body = anim_to_html(anim, fps=fps, embed_frames=embed_frames, \
                 default_mode=default_mode)

    rst_body = rst_body.split('\n')

    rst_file = open(file_name,'w')
    rst_file.write(".. raw:: html\n")
    for line in rst_body:
        rst_file.write("   %s\n" % line)
    rst_file.close()
    print("Created %s" % file_name)
    print("Imbed this in another rst file using:")
    print(".. include:: %s" % file_name)


def make_mp4(anim, file_name='anim.mp4',
              fps=None, embed_frames=True, default_mode='once'):
    """
    Take an animation and covert to mp4 file using ffmpeg, which must be
    installed.
    """
    import os

    if not animation.writers.is_available('ffmpeg'):
        print("** ffmpeg must be installed to create mp4 file")
        return

    if os.path.splitext(file_name)[1] != '.mp4':
        print("*** Might not work if file extension is not .mp4")
    if fps is None:
        fps = 3
    writer = animation.writers['ffmpeg'](fps=fps)
    anim.save(file_name, writer=writer)
    print("Created %s" % file_name)


def read_images(plotdir, fname_pattern='*.png'):

    import glob, os
    images = []
    files = glob.glob(os.path.join(plotdir, fname_pattern))
    for file in files:
        im = plt.imread(file)
        images.append(im)
    return images

def save_images(images, figsize=(8,6), plotdir='_plots', clobber=True,
                fname_base='frame', format='png', verbose=False, **kwargs):

    make_plotdir(plotdir=plotdir, clobber=clobber)
    for frameno, img in enumerate(images):
        fig = imshow_noaxes(img, figsize)
        filename = '%s/%s%s.%s' % (plotdir, fname_base, str(frameno).zfill(5), format)
        plt.savefig(filename, format=format, **kwargs)
        plt.close(fig)
        if verbose:
            print("Saved ",filename)

def save_figs(figs, plotdir='_plots', clobber=True,
              fname_base='frame', format='png', verbose=False, **kwargs):

    make_plotdir(plotdir=plotdir, clobber=clobber)
    for frameno,fig in enumerate(figs):
        filename = '%s/%s%s.%s' % (plotdir, fname_base, str(frameno).zfill(5), format)
        fig.savefig(filename, format=format, **kwargs)
        plt.close(fig)
        if verbose:
            print("Saved ",filename)


def make_image(fig, **kwargs):
    """
    Take a matplotlib figure *fig* and convert it to an image *im* that
    can be viewed with imshow.
    """

    import io
    png = io.BytesIO()
    fig.savefig(png,format='png', **kwargs)
    png.seek(0)
    im = plt.imread(png)
    return im

def make_images(figs, **kwargs):
    """
    Take a list of matplotlib figures *figs* and convert to list of images.
    """

    images = []
    for fig in figs:
        im = make_image(fig, **kwargs)
        images.append(im)
    return images

def imshow_noaxes(im, figsize=(8,6)):
    fig = plt.figure(figsize=figsize)
    ax = plt.axes()
    plt.imshow(im)
    ax.axis('off')
    return fig

def interact_animate_images(images, figsize=(10,6), manual=False, TextInput=False):
    "Create an interact that loops over all the frames contained in a list of images."

    def display_frame(frameno):
        imshow_noaxes(images[frameno], figsize=figsize)
        plt.show()

    if TextInput:
        if TextInput:
            print("Valid frameno values: from %i to %i" % (0,len(images)-1))
        widget = ipywidgets.IntText(min=0,max=len(images)-1, value=0)
    else:
        widget = ipywidgets.IntSlider(min=0,max=len(images)-1, value=0)

    if manual:
        interact_manual(display_frame, frameno=widget)
    else:
        interact(display_frame, frameno=widget)

def interact_animate_figs(figs, manual=False, TextInput=False):
    """
    Create an interact that loops over all the frames contained in a list of figures.

    Passing in the argument `manual=True` will use the widget `interact_manual`
    instead of `interact`.  This refrains from updating the image as you move
    the slider bar.  Instead you move the slider as desired and then click on
    the `Run` button to re-display the image.  This is useful if there are many
    frames and you want to be able to jump to around without all the
    intermediate frames being displayed, which can slow down the response
    significantly.

    The argument `TextInput=True` can be specified to produce a text input cell
    rather than a slider bar.
    """

    def display_frame(frameno):
        display(figs[frameno])

    if TextInput:
        widget = ipywidgets.IntText(min=0,max=len(figs)-1, value=0)
    else:
        widget = ipywidgets.IntSlider(min=0,max=len(figs)-1, value=0)

    if manual:
        if TextInput:
            print("Valid frameno values: from %i to %i" % (0,len(figs)-1))
        interact_manual(display_frame, frameno=widget)
    else:
        interact(display_frame, frameno=widget)


def animate_figs(figs, style='ipywidgets', figsize=(10,6), **kwargs):
    """
    Create an animation from a set of figures,
    style = 'ipywidgets' or 'JSAnimation'
    returns anim
    """

    images = make_images(figs)

    if style == 'ipywidgets':
        anim = interact_animate_images(images, figsize=figsize, **kwargs)
    elif style == 'JSAnimation':
        anim = JSAnimate_images(images, figsize=figsize, **kwargs)
    else:
        raise ValueError('** Unrecognized style = %s' % style)

    return anim

def make_anim_from_plotdir(plotdir='_plots', fignos='all', outputs=[],
                           file_name_prefix='', figsize=(5,4), dpi=None,
                           fps=5):

    """
    After running `make plots` using VisClaw, convert the png files in
    the plots directory into an animation, and perhaps also
    stand-alone files that can be embedded in webpages or Sphinx documentation.

    outputs can be a list containing any of 'mp4','html','rst'

    Call this from a script that starts with:
        import matplotlib
        matplotlib.use('Agg')
    """
    import glob, re

    if fignos == 'all':
        # determine what fignos are used in the plotdir
        movie_files = glob.glob(plotdir + '/movie*html')
        if len(movie_files) == 0:
            print('No movie files found in %s' % plotdir)
            return

        fignos = []
        regexp = re.compile(r"movie[^ ]*fig(?P<figno>[0-9]*)[.html]")
        for f in movie_files:
            result = regexp.search(f)
            fignos.append(result.group('figno'))

        print("Found these figures: %s" % fignos)

    for figno in fignos:

        fname_pattern = 'frame*fig%s.png' % figno
        anim = make_anim(plotdir, fname_pattern, figsize, dpi)

        if 'mp4' in outputs:
            file_name = file_name_prefix + 'fig%s.mp4' % figno
            make_mp4(anim, file_name, fps=fps,
                     embed_frames=True, default_mode='once')

        if 'html' in outputs:
            file_name = file_name_prefix + 'fig%s.html' % figno
            make_html(anim, file_name, fps=fps,
                      embed_frames=True, default_mode='once')

        if 'rst' in outputs:
            file_name = file_name_prefix + 'fig%s.rst' % figno
            make_rst(anim, file_name, fps=fps,
                     embed_frames=True, default_mode='once')

    return anim
