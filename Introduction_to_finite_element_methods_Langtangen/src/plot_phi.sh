#!/bin/sh
python plot_phi.py phi --SCITOOLS_easyviz_backend gnuplot
scitools rename fe_basis gpl_fe_basis fe_basis*.png fe_basis*.pdf
scitools rename fe_dbasis gpl_fe_dbasis fe_dbasis*.png fe_dbasis*.pdf

python plot_phi.py phi --SCITOOLS_easyviz_backend matplotlib
scitools rename fe_basis mpl_fe_basis fe_basis*.png fe_basis*.pdf
scitools rename fe_dbasis mpl_fe_dbasis fe_dbasis*.png fe_dbasis*.pdf
