function smallT = margTable(bigT, bigdom, bigsz, onto, maximize)
% Marginalize a table

% This file is from pmtk3.googlecode.com


if nargin < 5, maximize = 0; end

smallT = reshapePMTK(bigT, bigsz);        % make sure it is a multi-dim array
sum_over = setdiffPMTK(bigdom, onto);
ndx = lookupIndices(sum_over, bigdom);
if maximize
    for i=1:length(ndx)
        smallT = max(smallT, [], ndx(i));
    end
else
    for i=1:length(ndx)
        smallT = sum(smallT, ndx(i));
    end
end

ns = zeros(1, max(bigdom));
ns(bigdom) = bigsz;
smallT = squeeze(smallT);               % remove all dimensions of size 1
smallT = reshapePMTK(smallT, ns(onto)); % put back relevant dims of size 1

 % If onto is not in sorted order, permute columns
 % so dimensions match what user requested
 % KPM 15 march 2011
 shrunkenBigdom = bigdom;
 shrunkenBigdom(ndx) = []; % id's of variables left over, in order
 ndx2 = lookupIndices(onto, shrunkenBigdom);
 if ~isequal(ndx2, sort(ndx2)) % not in order
   smallT = permute(smallT, ndx2);
 end
 
end
