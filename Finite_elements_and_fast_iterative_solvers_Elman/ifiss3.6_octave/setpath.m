%SETPATH sets IFISS search path for Octave implementation
%   IFISS scriptfile: DJS; 17 March 2020.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage

% initilise matlab path for ./ifiss_octave/ directories
gohome,
warning('off', 'all')
addpath(genpath(pwd),'-end')

% ensure that the output is NOT buffered
more off

% adjust font sizes for retina display
set(0, "defaulttextfontsize", 16)  % title
set(0, "defaultaxesfontsize", 12)  % axes labels
set(0, "defaulttextfontname", "Courier")
set(0, "defaultaxesfontname", "Courier")
set(0, "defaultlinelinewidth", 1.5)
