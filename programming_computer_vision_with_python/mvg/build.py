import os, sys

if len(sys.argv) == 1 or sys.argv[1] == 'tex':
    os.system("pdflatex -shell-escape mvg*.tex")
    os.system("evince mvg*.pdf")
    exit()
