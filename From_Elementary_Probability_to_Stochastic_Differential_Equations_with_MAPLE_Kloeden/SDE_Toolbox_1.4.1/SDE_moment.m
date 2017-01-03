function moment = SDE_moment(x,p)

% Computes the raw moment of order p for the x array
%
% usage: moment = SDE_moment(x,p)
%
% IN:     x, an array of numbers
%         p, a positive integer
% OUT:    moment, the raw moment of order p

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

if  (p - floor(p)) > 0 || p < 1
    error('Requires a positive integer second argument.');
end

if p==1
    moment = 0;
    return
end
m = mean(x);
moment = mean((x - m).^p);