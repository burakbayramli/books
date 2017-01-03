function y = SDE_split_sdeinput(x,NUMDEPVARS)

% Split the (NUMSIM x NUMDEPVARS) sdefile input state-variables into NUMDEPVARS slots.
%
% usage: y = SDE_split_sdeinput(x,NUMDEPVARS);
%
% IN:           x; working value of dependent variable 
%      NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
% OUT:          y; the splitted values of x

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

y = cell(1,NUMDEPVARS);

for i=1:NUMDEPVARS
    y{i} = x(i:NUMDEPVARS:end);
end
