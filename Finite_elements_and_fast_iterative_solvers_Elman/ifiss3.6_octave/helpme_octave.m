%HELPME_OCTAVE  Octave specific issues 
%   IFISS scriptfile: DJS; 1 February 2019.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage

fprintf(' \n');
fprintf(' IFISS 3.6 \n')
fprintf(' The Octave package is identical to the MATLAB package\n');
fprintf(' with the exception of the functions in the top level directory,\n');
fprintf(' and those in the compatibility directory /octave382/ \n');
fprintf(' \n');
fprintf(' Here is a list of minor "issues" with the Octave 4.0.3 implementation.\n');
fprintf(' *  The nondefault setting "more off" is preset in setpath.m: otherwise\n') 
fprintf('    screen and the graphical output are NOT synchronised correctly\n');
fprintf(' *  Some of the solver options give different results to those in MATLAB:\n');
fprintf(' *  The graphical output is imperfect: \n');
fprintf('    o interpetation of TeX symbols in labels/legends is always not well defined\n');
fprintf('    o plotting can also be a little slow ... please be patient! \n');
fprintf(' *  Generation of avi movies fails (no "getframe" in Octave)\n');
fprintf(' \n');
fprintf(' Please report any other compatibility problems that you encounter\n') 
fprintf(' to the Octave Bug Report website.\n\n');
