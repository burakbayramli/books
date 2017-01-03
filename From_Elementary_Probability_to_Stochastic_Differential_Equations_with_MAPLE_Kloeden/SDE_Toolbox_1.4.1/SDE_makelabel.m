function [label,TIME,VRBL] = SDE_makelabel(TIMEREP,VRBLREP)

% SDE_makelabel constructs an array TIME of different times (the array TIMEREP without repetitions) 
% and an array VRBL of different variables (the array VRBLREP without repetitions) and returns a 
% length(TIME) * length(VRBL) label matrix defined as label(i,j) = 1 if the
% variable VRBL(j) has been measured at time TIME(i).
%
% usage: [label,TIME,VRBL] = SDE_makelabel(TIMEREP,VRBLREP)
%
% IN:   TIMEREP; the array of times as contained in the .dat datafile
%       VRBLREP; the array of variables as contained in the .dat datafile
% OUT:  label: the label matrix as defined above
%       TIME; the array of unique observation times
%       VRBL; the array of unique label-variables 

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

error(nargchk(2, 2, nargin));

TIME = unique(TIMEREP);  % TIMEREP without repeated values
VRBL = unique(VRBLREP);  % VRBLREP without repeated values

nvars = length(VRBL);
n = length(TIME);
label = zeros(n,nvars);
for(j=1:nvars)
    index = find(VRBLREP == VRBL(j));
    timevrbl = TIMEREP(index);
    for(i=1:length(TIME))
       stopnow = 0;
       for(k=1:length(timevrbl) & (stopnow == 0))
           if(TIME(i) == timevrbl(k))
              label(i,j) = 1;
              stopnow = 1;
              timevrbl = timevrbl(k+1:end);
           end
       end
    end
end

