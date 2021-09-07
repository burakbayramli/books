# Instructions for building the pdf file

 - Edit the file `make_pdf.py` to set the list of chapters that should be included. Setting
   ```
     chapters = part0 + part1 + part2
   ```   
   should build the pdf file for the printed book.
   For testing you might want to select a smaller subset.
   
 - Remove the directory `build_pdf` to get rid of
   old versions:
   ```  
    rm -rf build_pdf
   ```    
 - Run the python script to build the pdf:
   ```
    python make_pdf.py
   ```
 - This will create a directory `build_pdf` that contains, among other things:
   
    - `riemann.tex`  - The latex file
    - `riemann.pdf`  - The resulting pdf file
    - Style files and bibtex files

   
 - You can edit the latex file if desired and remake the pdf using 
   ```
    pdflatex riemann
   ```
   
### What is done by this script:
    
The `build_pdf.py` script creates the `build_pdf` directory and copies the notebooks and various files into it, including the `riemann.bib` bibtex file, and the files found in `files_for_latex_pdf`.

It then does some editing of the notebooks and the python code in the `exact_solvers` subdirectory, to replace the interactive widgets used in the notebooks by static versions that look ok in the pdf.  This is done by replacing lines of the form
   ```
   from ipywidgets import interact
   ```
with 
   ```  
  from utils.snapshot_widgets import interact
   ```  
This uses the function defined in the file `utils/snapshot_widgets.py` that we designed for this purpose.  In particular, interactive widgets with time sliders to show the evolution of a Riemann solution are generally replaced by a pair of images, the initial conditions at t=0 and the solution at some later time, e.g. t=0.2.

After modifying  the notebooks, the script proceeds to run all the notebooks and then combines them into a single latex file `riemann.tex` using the `bookbook` package from https://github.com/takluyver/bookbook

The script in `files_for_latex_pdf/fix_latex_file.py` is then used to fix up the latex files.  Additional fixes could be added to this script if desired.

Finally, pdflatex and bibtex are used as usual to convert `riemann.tex` into `riemann.pdf` (in the `build_pdf` directory).

