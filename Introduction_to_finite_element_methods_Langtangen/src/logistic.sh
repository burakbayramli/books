#!/bin/sh
python logistic.py 0.9 0.05 1   # insufficient tolerance
python logistic.py 0.9 0.001 1  # sufficient tolerance
python logistic.py 0.45 0.001 1  # sufficient tolerance
python logistic.py 0.09 0.0001 1  # sufficient tolerance
doconce combine_images logistic_N10_eps-01_u.pdf logistic_N10_eps-03_u.pdf logistic_N20_eps-03_u.pdf logistic_N100_eps-04_u.pdf logistic_u.pdf
doconce combine_images logistic_N10_eps-01_u.png logistic_N10_eps-03_u.png logistic_N20_eps-03_u.png logistic_N100_eps-04_u.png logistic_u.png
doconce combine_images logistic_N10_eps-01_iter.pdf logistic_N10_eps-03_iter.pdf logistic_N20_eps-03_iter.pdf logistic_N100_eps-04_iter.pdf logistic_iter.pdf
doconce combine_images logistic_N10_eps-01_iter.png logistic_N10_eps-03_iter.png logistic_N20_eps-03_iter.png logistic_N100_eps-04_iter.png logistic_iter.png
