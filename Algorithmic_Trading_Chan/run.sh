export MATLABPATH=./jplv7/util:./jplv7/regress:./jplv7/distrib:./jplv7/coint
if [ `uname` = "Linux" ]
then
    $HOME/matlab/bin/matlab  -nodisplay -nosplash -nodesktop -r  $1
else
    /Applications/MATLAB_R2014b.app/bin/matlab  -nodisplay -nosplash -nodesktop -r  $1
fi
