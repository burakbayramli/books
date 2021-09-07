"""
Convert notebooks listed in `chapters` into latex and then a PDF.

Note:

 - The notebooks are first copied into the build_pdf directory (with a chapter
   number prepended).
"""
import re
import subprocess
import os
import glob

# To test a subset, adjust the list of chapters and
# remove the build_pdf directory before running this script.

part0 = ['Preface']

part1 = ['Introduction',
            'Advection',
            'Acoustics',
            'Burgers',
            'Traffic_flow',
            'Nonconvex_scalar',
            'Shallow_water',
            'Shallow_tracer',
            'Euler']

part2 = ['Approximate_solvers',
         'Burgers_approximate',
         'Shallow_water_approximate',
         'Euler_approximate',
         'FV_compare']

chapters = part0 + part1 + part2

# For testing purposes:
if 0:
    chapters = ['Shallow_water',
                'Shallow_tracer']

# Create build_dir directory and copy notebooks, templates, etc. into it:

build_dir = 'build_pdf/'
if not os.path.exists(build_dir):
    os.makedirs(build_dir)

os.system('cp -r exact_solvers '+build_dir)
os.system('cp -r utils '+build_dir)
os.system('cp -r figures '+build_dir)
os.system('cp files_for_latex_pdf/riemann.tplx '+build_dir)
os.system('cp files_for_latex_pdf/SIAMbook2016.cls '+build_dir)
os.system('cp files_for_latex_pdf/siamplain.bst '+build_dir)
os.system('cp riemann.bib '+build_dir)
os.system('cp files_for_latex_pdf/latexdefs.tex '+build_dir)

# fix interact import statements in exact_solver demo codes:
os.chdir(os.path.join(build_dir, 'exact_solvers'))
print('Fixing interacts in %s' % os.getcwd())
files = glob.glob('*.py')
for file in files:
    infile = open(file,'r')
    lines = infile.readlines()
    infile.close()
    #print('Fixing %s' % file)
    with open(file,'w') as outfile:
        for line in lines:
            line = re.sub(r"context = 'notebook'", "context = 'pdf'", line)
            line = re.sub(r'from ipywidgets import interact',
                          'from utils.snapshot_widgets import interact', line)
            outfile.write(line)
            
os.chdir('../..')

# edit each notebook to modify interact imports, 
# then execute the notebook using nbcovert:

for i, chapter in enumerate(chapters):
    filename = chapter + '.ipynb'
    with open(filename, "r") as source:
        lines = source.readlines()
    output_filename = str(i).zfill(2)+'-'+filename
    with open(build_dir+output_filename, "w") as output:
        for line in lines:

            # fix cross references to other chapters
            # notebook names are preprended with 00- etc. for ordering
            # when copied to this directory, so fix the cross refs accordingly:
            for j, chapter_name in enumerate(chapters):
                line = re.sub(chapter_name+'.ipynb',
                              str(j).zfill(2)+'-'+chapter_name+'.ipynb', line)

            # not using context now, so these lines not needed:
            #line = re.sub(r"context = 'notebook'", "context = 'pdf'", line)
            #line = re.sub(r"#sns.set_context('paper')",
            #              r"sns.set_context('paper')", line)

            # replace widgets:
            line = re.sub(r'from ipywidgets import interact',
                          'from utils.snapshot_widgets import interact', line)
            line = re.sub(r'Widget Javascript not detected.  It may not be installed or enabled properly.',
                          '', line)
            output.write(line)

    # execute to create output with snapshot_widgets:
    args = ["jupyter", "nbconvert", "--to", "notebook", "--execute",
            "--ExecutePreprocessor.kernel_name=python",
            "--output", output_filename,
            "--ExecutePreprocessor.timeout=60", build_dir+output_filename]
    subprocess.check_call(args)

# Use bookbook to combine all the notebooks into a latex file:
#     see https://github.com/takluyver/bookbook

os.chdir(build_dir)
os.system('python3 -m bookbook.latex --output-file riemann --template riemann.tplx')

# Apply some fixes to the latex file:
os.system('python3 ../files_for_latex_pdf/fix_latex_file.py')

# Use pdflatex and bibtex as usual to build the pdf file
os.system('pdflatex riemann')
os.system('bibtex riemann')
os.system('pdflatex riemann')
os.system('pdflatex riemann')

os.chdir('..')
print('The pdf and latex files are in build_pdf directory')

if 1:
    # copy and rename pdf file if desired:
    pdf_name = 'RiemannProblemsJupyterSolutions.pdf'
    os.system('cp build_pdf/riemann.pdf %s' % pdf_name)
    print('The pdf file has also been copied to %s' % pdf_name)
