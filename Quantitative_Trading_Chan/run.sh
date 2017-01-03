export MATLABPATH=../Algorithmic_Trading_Chan/jplv7/util:../Algorithmic_Trading_Chan/jplv7/regress:../Algorithmic_Trading_Chan/jplv7/distrib:../Algorithmic_Trading_Chan/jplv7/coint
if [ `uname` = "Linux" ]
then
    $HOME/matlab/bin/matlab  -nodisplay -nosplash -nodesktop -r  $1
else
    /Applications/MATLAB_R2014b.app/bin/matlab  -nodisplay -nosplash -nodesktop -r  $1
fi
