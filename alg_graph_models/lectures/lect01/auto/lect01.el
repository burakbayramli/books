(TeX-add-style-hook "lect01"
 (lambda ()
    (LaTeX-add-environments
     '("titledslide" 1))
    (TeX-add-symbols
     '("slidehead" 1)
     "lecnum")
    (TeX-run-style-hooks
     "amssymb"
     "graphicx"
     "amsmath"
     "latex2e"
     "slides10"
     "slides"
     "landscape")))

