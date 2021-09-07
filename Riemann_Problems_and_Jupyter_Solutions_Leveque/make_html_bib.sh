
# This must be done whenever new entries are added to riemann.bib
# to create new html versions of the bibliography, pointed to both by the
# notebooks and by the versions rendered as html.

# First install bibtex2html, downloadable from
# https://www.lri.fr/~filliatr/bibtex2html/

export TMPDIR=.
bibtex2html --sort-as-bibtex \
            --style files_for_latex_pdf/siamplain \
            --header '<h1>Bibliography</h1>' \
            riemann.bib
