function [XOBS,TIME,VRBL] = SDE_getdata(DATAFILE)

% Loads the n x 3 data-matrix from an ASCII tab-delimied files and checks for missing values:
% returns the observation values XOBS sampled at times TIME and
% corresponding to the label-variables VRBL. See manual.pdf for details.
%
% IN:  DATAFILE; the name of the ASCII tab-delimited file containing the data
% OUT: XOBS; the matrix-shaped observed data
%      TIME; the array of unique observation times
%      VRBL; the array of unique label-variables 
%
% example: [XOBS,TIME,VRBL] = SDE_getdata('sampledata1')

% Copyright (C) 2007, Umberto Picchini  
% umberto.picchini@biomatematica.it
% http://www.biomatematica.it/Pages/Picchini.html
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

error(nargchk(1, 1, nargin));

dataname = sprintf('%s.dat',DATAFILE);
A = load(dataname);  % Read data matrix
A = sortrows(A,1);  % sort the data according to the time values (column 1)

if(size(A,2)~=3)
    error('The DATAFILE matrix should have exactly three columns.');
end

% Raw Arrays
TIMEREP = A(:,1);      % presume ODE-like structure, call independent variable "time" (there may appear repetitions)
XOBSRAW = A(:,2);      % X observed
VRBLREP = A(:,3);      % the variable to which the XOBS refers (there may appear repetitions)

% prepare standard arrays 
[label,TIME,VRBL] = SDE_makelabel(TIMEREP,VRBLREP);  % creates the standard TIME and VRBL arrays (without repetitions)
XOBS = SDE_xobsmatrix(label,TIME,VRBL,VRBLREP,XOBSRAW);  % creates the matrix-shaped XOBS data from XOBSRAW

% check for missing values
[row,col]=find(isnan(XOBS));
if(~isempty(row) && ~isempty(col))
    fprintf('\n\n')
    disp('SDE Toolbox does not allow for datafile with missing values:');
    fprintf('the missing value correspond to the state variable %d at time %d',col(1),TIME(row(1)));
    fprintf('\n')
    error('check your datafile');
end



