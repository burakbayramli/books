export MATLABPATH=./HMMAll/HMM:./HMMAll/KPMstats:./HMMAll/KPMtools:./HMMAll/netlab3.3
if [ `uname` = "Linux" ]
then
    $HOME/matlab/bin/matlab  -nodisplay -nosplash -nodesktop -r  $1
else
    /Applications/MATLAB_R2014b.app/bin/matlab  -nodisplay -nosplash -nodesktop -r  $1
fi
