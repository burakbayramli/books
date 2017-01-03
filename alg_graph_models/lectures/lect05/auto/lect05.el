(TeX-add-style-hook "lect05"
 (lambda ()
    (LaTeX-add-environments
     '("titledslide" 1))
    (TeX-add-symbols
     '("slidehead" 1)
     "lecnum"
     "variables"
     "variable"
     "cell"
     "table"
     "values"
     "reals")
    (TeX-run-style-hooks
     "amssymb"
     "graphicx"
     "amsmath"
     "latex2e"
     "slides10"
     "slides"
     "landscape")))

