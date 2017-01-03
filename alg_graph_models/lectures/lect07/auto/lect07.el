(TeX-add-style-hook "lect07"
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
     "amssymb"
     "graphicx"
     "amsmath"
     "latex2e"
     "slides10"
     "slides"
     "landscape")))

