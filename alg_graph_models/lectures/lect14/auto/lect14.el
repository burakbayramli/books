(TeX-add-style-hook "lect14"
 (lambda ()
    (LaTeX-add-environments
     '("titledslide" 1))
    (TeX-add-symbols
     '("slidehead" 1)
     "lecnum"
     "betafn"
     "dirchfn")
    (TeX-run-style-hooks
     "graphicx"
     "pdftex"
     "latex2e"
     "slides10"
     "slides"
     "landscape")))

