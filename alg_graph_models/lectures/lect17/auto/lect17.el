(TeX-add-style-hook "lect17"
 (lambda ()
    (LaTeX-add-environments
     '("titledslide" 1))
    (TeX-add-symbols
     '("slidehead" 1)
     "lecnum"
     "betafn"
     "dirchfn"
     "hg")
    (TeX-run-style-hooks
     "url"
     "graphicx"
     "pdftex"
     "latex2e"
     "slides10"
     "slides"
     "landscape")))

