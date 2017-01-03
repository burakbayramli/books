function xobsmatrix = SDE_xobsmatrix(label,TIME,VRBL,VRBLREP,XOBSRAW)

% xobsmatrix = SDE_xobsmatrix(label,TIME,VRBL,VRBLREP,XOBSRAW) uses the label matrix produced in SDE_makelabel.m
% and returns the observed data in matrix form: i.e. xobsmatrix is a length(TIME) * length(VRBL) matrix such that 
% xobsmatrix(i,j) contains the observed (measured) value of  the X process at time TIME(i) with respect to the 
% VRBL(j)-th variable.
%
% IN:  label: the label matrix created by SDE_makelabel
%      TIME; the array of unique observation times
%      VRBL; the array of unique label-variables 
%      VRBLREP; the array of variables as contained in the .dat datafile
%      XOBSRAW; the array of observations as contained in the .dat datafile
% OUT: xobsmatrix; the observed data in matrix form
%
% See also SDE_MAKELABEL

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

error(nargchk(5, 5, nargin));

xobsmatrix = zeros(length(TIME),length(VRBL));
for(j=1:length(VRBL))
    indexvrbl = find(VRBLREP==VRBL(j));
    xobs = XOBSRAW(indexvrbl);
    for(i=1:length(TIME))
        if(label(i,j)==0)
            xobsmatrix(i,j) = NaN;
        end
    end
    i=1;
    z=1;
    while((i<=length(TIME)) && (z<=length(xobs)))
        if(isnan(xobsmatrix(i,j)))
            i=i+1;
        else
            xobsmatrix(i,j)=xobs(z);
            i=i+1;
            z=z+1;
        end
    end
end

% VRBL = VRBL(ones(1,length(TIME)),:);
% VRBL = VRBL(:).';
% Now replicate the values in VRBL depending on the TIME value (useful for future versions of SDE_Toolbox)
VRBL = repmat(VRBL,length(unique(TIME)),1);