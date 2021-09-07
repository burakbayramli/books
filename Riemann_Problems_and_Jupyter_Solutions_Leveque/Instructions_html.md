# Instructions for building the html files

- If `riemann.bib` has changed run the shell script `make_html_bib.sh` to convert 
  `riemann.bib` into `riemann.html` (the bibliography) and 
  `riemann_bib.html` (bibtex version):
  
  ```
   source make_html_bib.sh
  ```
  
  Note that this requires `bibtex2html` from 
  https://www.lri.fr/~filliatr/bibtex2html
  
- Edit the file `make_html.py` to set the list of chapters that should be included. Setting
  ```
    chapters = part0 + part1 + part2
  ```
  
  should build the html files for chapters corresponding 
  to the printed book.
  For testing you might want to select a smaller subset.
  
- Remove the directory `build_html` to get rid of
  old versions:
  ```
   rm -rf build_html
  ```
  
- Run the python script to build the pdf:
  ```
   python make_html.py
  ```
     
- This will create a directory `build_html` that contains, among other things:

   - `Index.html`  - The main index

## For the authors:

#### To copy a new version to the website:

- Create new `build_html` directory as above and confirm it contains the right stuff, 
by opening `build_html/Index.html`.  Build the html files with the `master` branch checked out (or whatever branch has the notebooks you want to build), not the `gh-pages` branch.

- In particular, ideally go through every notebook to
  make sure all the JSAnimations showed up
  properly, since sometimes this fails.

- Update `./index.html` in the top directory if needed. 
  This is the landing page
    http://www.clawpack.org/riemann_book/index.html

- Push to the webpage via:

  ```
    git checkout gh-pages
    
    #cp -r html html_old  # if you want to, for comparison
    
    # remove old version of html files:
    rm -rf html/*
    
    # copy over all new files:
    cp -r build_html/* html/ 
    
    # open html/Index.html and check things look ok
    
    git status  # check that it looks ok
    git add html
    git add index.html  # if you changed landing page
    git status  # check that it looks ok
    git commit -m "describe your changes"
    git push origin_push gh-pages
  ```
    
  Here `origin_push` must be set up as a remote that allows pushing directly to `git@github.com:clawpack/riemann_book`

    
    