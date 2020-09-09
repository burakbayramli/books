# html_movie.py
# -----------------------------------------------------------------------------
# This module provides a single function called movie that will create a Web
# page to display a sequence of images as a movie.
# -----------------------------------------------------------------------------
# This module is adapted from the scitools library developed by Hans Petter
# Langtangen.  The source from which this module was derived may be found in the
# file <scitools/lib/scitools/easyviz/movie.py> of the current distribution of
# the scitools module (as of May 30, 2015), available at
# 
# 	https://code.google.com/p/scitools/
# 
# The MovieEncoder class and the movie function of <movie.py> from the scitools
# library have been merged into a single function called movie.  The html_movie
# function has been renamed get_html.
# 
# The movie function in the current module takes a collection of input files and
# generates an HTML file that displays the images as a movie.  The output is
# nearly identical to that of scitools.std.movie called with the default 'html'
# encoder.  This module is self-contained and does not require any of the other
# functions or libraries of the scitools module.  The print statements have also
# been modified to work with both Python 2 and Python 3.
# 
# scitools:
# ---------
# 	Copyright (c) 2007-2009, Hans Petter Langtangen <hpl@simula.no> and
# 	Simula Resarch Laboratory.
# 
# 	All rights reserved.
# 
# Both the current module and scitools are distributed under the BSD license.
# See LICENSE.txt for details.
# -----------------------------------------------------------------------------
"""
This module provides a single function called movie that will create a Web page
to display a sequence of images as a movie.

The input files can be given in a list or tuple of file names or specified by a
regular expression that will generate the filenames.  I.e.,
input_files="frame_*.jpg" will use all .jpg files in the current directory with
a name of the form "frame_001.jpg", "frame_002.jpg", etc.
"""

# Modules that provide tools to search for file names using regular expressions.
import os
import glob
import re

# Specify types of image files that can be used to create a movie.
_legal_file_types = 'png gif jpg jpeg'.split()

# ------------------------------------------------------------------------- 
def movie(input_files, output_file="movie.html", fps=25):
	"""
	Take a list or tuple of image file names or a regular expression that will
	generate those file names and then create an HTML file that uses JavaScript
	to display those images as a movie.

	Images must be PNG, JPEG, or GIF files, with extension .png, .jpg, .jpeg, or
	.gif, respectively.

		input_files:	a list or tuple of file names, or regular expression
		output_file:	name of the output HTML movie file
		fps:			frame rate, in frames per second
	
	The sequence of images will be determined by their order in input_files if
	input_files is a list or tuple.  If input_files is a regular expression like
	"frame_*.jpg", the sequence of images will be the alphanumeric order of all
	filenames that match the search pattern.  Because "frame_10.jpg" comes
	before "frame_2.jpg" in alphanumeric order, it is important to include
	leading zeros in file names: "frame_002.jpg" comes before "frame_010.jpg".
	"""

	# Print blank lines before function output statements.
	print('\n\n')

	# Determine the file type of the input files.
	if isinstance(input_files, (tuple,list)):
		file_ = input_files[0]
	elif isinstance(input_files, str):
		file_ = input_files
	else:
		raise ValueError("The input files must be given as either a "\
						 "list or tuple of strings or a string, not '%s'" % \
						 type(input_files))

	# Check that the input files do exist.
	if isinstance(input_files, str):
		# Are input_files on ffmpeg/mpeg2enc format or Unix wildcard format?
		ffmpeg_format = r'(.+)%\d+d(\..+)'
		m = re.search(ffmpeg_format, input_files, re.DOTALL)
		if m:
			wildcard_format = m.group(1) + '*' + m.group(2)
		else:
			wildcard_format = input_files
		all_input_files = glob.glob(wildcard_format)
		if not all_input_files:
			print('No files of the form %s exist.' % input_files)
		else:
			print('Found %d files of the format %s.' % \
			(len(all_input_files), input_files))
	else:
		# User provided a list or tuple of specific filenames.
		all_input_files = input_files
	error_encountered = False
	for f in all_input_files:
		if not os.path.isfile(f):
			print('Input file %s does not exist.' % f)
			error_encountered = True
	if error_encountered:
		raise IOError('Some input files were not found.')

	fname, ext = os.path.splitext(file_)
	if not ext:
		raise ValueError("Unable to determine file type from file name.")
	file_type = ext[1:] # remove the . (dot)
	if not file_type in _legal_file_types:
		raise TypeError("File type must be %s, not '%s'" % \
						(_legal_file_types, file_type))

	# Create an html file that can play image files
	files = input_files
	if isinstance(files, str):
		files = glob.glob(files)
		files.sort()
	print('\nMaking HTML code for displaying ' + ', '.join(files))

	# Turn frame rate into a pause, in milliseconds, between images.
	interval_ms = 1000.0/fps

	outf = output_file
	if outf is None:
		outf = 'movie.html'
	else:
		# Ensure .html extension
		outf = os.path.splitext(outf)[0] + '.html'
	casename = os.path.splitext(outf)[0]

	# Get the necessary HTML text to create the movie.
	header, jscode, form, footer, files = \
		get_html(files, interval_ms, casename=casename)

	# Write the HTML text to the output file.
	f = open(outf, 'w')
	f.write(header + jscode + form + footer)
	f.close()
	print("\n\nmovie in output file " + outf)

	return None

# ------------------------------------------------------------------------- 
def get_html(plotfiles, interval_ms=300, width=800, height=600,
               casename=None):
    """
    Takes a list plotfiles, such as::

        'frame00.png', 'frame01.png', ...

    and creates javascript code for animating the frames as a movie in HTML.

    The `plotfiles` argument can be of three types:

      * A Python list of the names of the image files, sorted in correct
        order. The names can be filenames of files reachable by the
        HTML code, or the names can be URLs.
      * A filename generator using Unix wildcard notation, e.g.,
        ``frame*.png`` (the files most be accessible for the HTML code).
      * A filename generator using printf notation for frame numbering
        and limits for the numbers. An example is ``frame%0d.png:0->92``,
        which means ``frame00.png``, ``frame01.png``, ..., ``frame92.png``.
        This specification of `plotfiles` also allows URLs, e.g.,
        ``http://mysite.net/files/frames/frame_%04d.png:0->320``.

    If `casename` is None, a casename based on the full relative path of the
    first plotfile is used as tag in the variables in the javascript code
    such that the code for several movies can appear in the same file
    (i.e., the various code blocks employ different variables because
    the variable names differ).

    The returned result is text strings that incorporate javascript to
    loop through the plots one after another.  The html text also features
    buttons for controlling the movie.
    The parameter `iterval_ms` is the time interval between loading
    successive images and is in milliseconds.

    The `width` and `height` parameters do not seem to have any effect
    for reasons not understood.

    The following strings are returned: header, javascript code, form
    with movie and buttons, footer, and plotfiles::

       header, jscode, form, footer, plotfiles = html_movie('frames*.png')
       # Insert javascript code in some HTML file
       htmlfile.write(jscode + form)
       # Or write a new standalone file that act as movie player
       filename = plotfiles[0][:-4] + '.html'
       htmlfile = open(filename, 'w')
       htmlfile.write(header + jscode + form + footer)
       htmlfile.close

    This function is based on code written by R. J. LeVeque, based on
    a template from Alan McIntyre.
    """
    # Alternative method:
    # http://stackoverflow.com/questions/9486961/animated-image-with-javascript

    # Start with expanding plotfiles if it is a filename generator
    if not isinstance(plotfiles, (tuple,list)):
        if not isinstance(plotfiles, (str,unicode)):
            raise TypeError('plotfiles must be list or filename generator,\
							not %s' % type(plotfiles))

        filename_generator = plotfiles
        if '*' in filename_generator:
            # frame_*.png
            if filename_generator.startswith('http'):
                raise ValueError(	'Filename generator %s cannot contain *;\
									must be like\
									http://some.net/files/frame_%%04d.png:0->120'\
									% filename_generator)

            plotfiles = glob.glob(filename_generator)
            if not plotfiles:
                raise ValueError('No plotfiles on the form' %
                                 filename_generator)
            plotfiles.sort()
        elif '->' in filename_generator:
            # frame_%04d.png:0->120
            # http://some.net/files/frame_%04d.png:0->120
            p = filename_generator.split(':')
            filename = ':'.join(p[:-1])
            if not re.search(r'%0?\d+', filename):
                raise ValueError(	'Filename generator %s has wrong syntax;\
									missing printf specification as in\
									frame_%%04d.png:0->120' % filename_generator)
            if not re.search(r'\d+->\d+', p[-1]):
                raise ValueError(	'Filename generator %s has wrong syntax;\
									must be like frame_%%04d.png:0->120'\
									% filename_generator)
            p = p[-1].split('->')
            lo, hi = int(p[0]), int(p[1])
            plotfiles = [filename % i for i in range(lo,hi+1,1)]

    # Check that the plot files really exist, if they are local on the computer
    if not plotfiles[0].startswith('http'):
        missing_files = [fname for fname in plotfiles
                         if not os.path.isfile(fname)]
        if missing_files:
            raise ValueError('Missing plot files: %s' %
                             str(missing_files)[1:-1])

    if casename is None:
        # Use plotfiles[0] as the casename, but remove illegal
        # characters in variable names since the casename will be
        # used as part of javascript variable names.
        casename = os.path.splitext(plotfiles[0])[0]
        # Use _ for invalid characters
        casename = re.sub('[^0-9a-zA-Z_]', '_', casename)
        # Remove leading illegal characters until we find a letter or underscore
        casename = re.sub('^[^a-zA-Z_]+', '', casename)

    filestem, ext = os.path.splitext(plotfiles[0])
    if ext == '.png' or ext == '.jpg' or ext == '.jpeg' or ext == '.gif':
        pass
    else:
        raise ValueError(	'Plotfiles (%s, ...) must be PNG, JPEG, or GIF\
							files with extension .png, .jpg/.jpeg, or .gif'\
							% plotfiles[0])

    header = """\
<html>
<head>
</head>
<body>
"""
    no_images = len(plotfiles)
    jscode = """
<script language="Javascript">
<!---
var num_images_%(casename)s = %(no_images)d;
var img_width_%(casename)s = %(width)d;
var img_height_%(casename)s = %(height)d;
var interval_%(casename)s = %(interval_ms)d;
var images_%(casename)s = new Array();

function preload_images_%(casename)s()
{
   t = document.getElementById("progress");
""" % vars()

    i = 0
    for fname in plotfiles:
        jscode += """
   t.innerHTML = "Preloading image ";
   images_%(casename)s[%(i)s] = new Image(img_width_%(casename)s, img_height_%(casename)s);
   images_%(casename)s[%(i)s].src = "%(fname)s";
        """ % vars()
        i = i+1
    jscode += """
   t.innerHTML = "";
}

function tick_%(casename)s()
{
   if (frame_%(casename)s > num_images_%(casename)s - 1)
       frame_%(casename)s = 0;

   document.name_%(casename)s.src = images_%(casename)s[frame_%(casename)s].src;
   frame_%(casename)s += 1;
   tt = setTimeout("tick_%(casename)s()", interval_%(casename)s);
}

function startup_%(casename)s()
{
   preload_images_%(casename)s();
   frame_%(casename)s = 0;
   setTimeout("tick_%(casename)s()", interval_%(casename)s);
}

function stopit_%(casename)s()
{ clearTimeout(tt); }

function restart_%(casename)s()
{ tt = setTimeout("tick_%(casename)s()", interval_%(casename)s); }

function slower_%(casename)s()
{ interval_%(casename)s = interval_%(casename)s/0.707; }

function faster_%(casename)s()
{ interval_%(casename)s = interval_%(casename)s*0.707; }

// --->
</script>
""" % vars()
    plotfile0 = plotfiles[0]
    form = """
<form>
&nbsp;
<input type="button" value="Start movie" onClick="startup_%(casename)s()">
<input type="button" value="Pause movie" onClick="stopit_%(casename)s()">
<input type="button" value="Resume movie" onClick="restart_%(casename)s()">
&nbsp;
<input type="button" value="Slower" onClick="slower_%(casename)s()">
<input type="button" value="Faster" onClick="faster_%(casename)s()">
</form>

<p><div ID="progress"></div></p>
<img src="%(plotfile0)s" name="name_%(casename)s" border=2/>
""" % vars()
    footer = '\n</body>\n</html>\n'
    return header, jscode, form, footer, plotfiles
