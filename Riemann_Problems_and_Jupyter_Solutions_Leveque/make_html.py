"""
Convert notebooks listed in `chapters` into html files in the directory
`build_html`.

Run this code on the master branch with the latest set of notebooks,
adjusting the specification of `chapters` below first if you want to
process only a subset of the notebooks.

To post on the website, check out the `gh-pages` branch and then 
    cp -r build_html/* html/
and then git add, commit, and push to Github.

The files `build_html/*.ipynb` can be deleted, but copy over all the
subdirectories (`figures`, `exact_solvers`, etc.) in order for figures to
display in html files and links to Python code to work properly.

Note:

 - The notebooks are first copied into the directory `build_html` and pre-processed
   to use static_widgets (or jsanimation_widgets in certain notebooks),
   and cross references to other notebooks have `.ipynb` replaced by `.html`
   so the links work in the html files.

 - The directories `utils`, `exact_solvers`, and `figures` are also copied
   in before processing.

 - The list `all_chapters` is used to replace cross-reference links
   `chapter.ipynb` by `chapter.html`.

 - The bibliography files `riemann.html` and riemann_bib.html` are 
   copied into `build_html`.  These might need to be updated before
   running this script (using make_html_bib.sh).

"""

import re
import subprocess
import os
import glob

part0 = ['Preface', 'Index']

part1 = ['Introduction',
            'Advection',
            'Acoustics',
            'Traffic_flow',
            'Burgers',
            'Nonconvex_scalar',
            'Shallow_water',
            'Shallow_tracer',
            'Euler']

part2 = ['Approximate_solvers',
         'Burgers_approximate',
         'Shallow_water_approximate',
         'Euler_approximate',
         'FV_compare']

others = ['Index2',
            'Acoustics_heterogeneous',
            'Traffic_variable_speed',
            'Nonlinear_elasticity',
            'Euler_Tammann',
            'Traffic_with_ramps',
            'Pressureless_flow',
            'Kitchen_sink_problem']

all_chapters = part0 + part1 + part2 + others
book_chapters = part0 + part1 + part2

# which chapters to process:
chapters = all_chapters
#chapters = book_chapters

# test on a small subset:
#chapters = part0 + ['Introduction']


template_path = os.path.realpath('files_for_html/html.tpl')


os.system('mkdir -p build_html')  # for intermediate processing
# copy some things needed for processing
os.system('cp -r exact_solvers build_html/')
os.system('cp -r utils build_html/')
os.system('cp -r figures build_html/')
os.system('cp -r phase_plane build_html/')
os.system('cp files_for_html/custom.css build_html/')
os.system('cp html_animations/* build_html/')

# Putting figures inside an img folder doesn't seem to be needed now:
#os.system('mkdir -p build_html/img')  # for viewing images
#os.system('cp -r figures build_html/img/')

# Might need to update bibliography first with make_html_bib.sh
os.system('cp riemann.html build_html/') # bibliography
os.system('cp riemann_bib.html build_html/') # bibtex version of bibliography

os.chdir('build_html')

if 0:
    # fix interact import statements in exact_solver demo codes:
    # This does not work in several cases so commented out for now.
    os.chdir('exact_solvers')
    print('Fixing interacts in %s' % os.getcwd())
    files = glob.glob('*.py')
    for file in files:
        infile = open(file,'r')
        lines = infile.readlines()
        infile.close()
        #print('Fixing %s' % file)
        with open(file,'w') as outfile:
            for line in lines:
                line = re.sub(r"context = 'notebook'", "context = 'html'", line)
                line = re.sub(r'from ipywidgets import interact',
                              'from utils.jsanimate_widgets import interact', line)
                outfile.write(line)
    os.chdir('..')

for i, chapter in enumerate(chapters):
    filename = chapter + '.ipynb'
    print("Processing %s" % filename)
    input_filename = os.path.join('..',filename)
    with open(input_filename, "r") as source:
        lines = source.readlines()
    output_filename = filename
    html_filename = chapter+'.html'

    with open(output_filename, "w") as output:
        if chapter in ['Introduction','Traffic_flow','Shallow_water',
                       'Nonconvex_scalar']:
            widget = 'from utils.jsanimate_widgets import interact'
        else:
            widget = 'from utils.snapshot_widgets import interact'

        for line in lines:
            line = re.sub(r'from ipywidgets import interact', widget, line)
            for j, chapter_name in enumerate(all_chapters):
                line = re.sub(chapter_name+'.ipynb', chapter_name+'.html', line)
            output.write(line)

    args = ["jupyter", "nbconvert", "--to", "html", "--execute",
            "--ExecutePreprocessor.kernel_name=python2",
            "--output", html_filename,
            "--template", template_path,
            "--ExecutePreprocessor.timeout=60", output_filename]
    subprocess.check_call(args)

# remove the notebooks from html/
os.system('rm *.ipynb')

# go back to the main directory:
os.chdir('..')

print("The html files can be found in build_html")
print("Open build_html/Index.html for the index")

