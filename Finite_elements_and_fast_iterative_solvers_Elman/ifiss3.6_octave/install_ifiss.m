%INSTALL_IFISS installation instructions
%IFISS scriptfile: DJS; 23 January 2019.
%Copyright (c) 2014 D.J. Silvester, H.C. Elman, A. Ramage (see readme.m)
%
fprintf(' To install the toolbox, the script-file\n');
fprintf(' gohome.m must be edited to reflect the correct path\n');
fprintf(' to the ifiss home directory on the installed computer: \n')
fprintf(' \n %s\n\n',pwd);
%currentdir=pwd; fprintf(['\n ',currentdir,' \n\n']);
fprintf(' This step only needs to be done once. Once IFISS is installed,\n');
fprintf(' for all subsequent uses the MATLAB search path must include\n');
fprintf(' the IFISS subdirectories. This can be done by running the\n');
fprintf(' script-file setpath.m\n');
fprintf(' \n');
pause
fprintf('To test the installation regenerate the saved session \n')
fprintf('in the diary file "guide36.txt" (see edit window ...)\n\n')
edit guide36.txt
