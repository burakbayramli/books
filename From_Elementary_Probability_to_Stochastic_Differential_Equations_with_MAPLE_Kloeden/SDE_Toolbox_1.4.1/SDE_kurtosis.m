function kurt = SDE_kurtosis(x)

% Computes the sample kurtosis of the x array
%
% kurt = SDE_kurtosis(x)
%
% IN:     x, an array of numbers
% OUT:    kurt, the kurtosis

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

sd = std(x);
m  = mean(x);
kurt = mean((x-m).^4)/sd^4;