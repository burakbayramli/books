(TeX-add-style-hook "lect08"
 (lambda ()
    (LaTeX-add-environments
     '("titledslide" 1))
    (TeX-add-symbols
     '("ci" 3)
     '("slidehead" 1)
     "lecnum"
     "variables"
     "variable"
     "cell"
     "table"
     "values"
     "reals"
     "hg"
     "gr")
    (TeX-run-style-hooks
     "graphicx"
     "pdftex"
     "amssymb"
     "amsmath"
     "latex2e"
     "slides10"
     "slides"
     "landscape")))

