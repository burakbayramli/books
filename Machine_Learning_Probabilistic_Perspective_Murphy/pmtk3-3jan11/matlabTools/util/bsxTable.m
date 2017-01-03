function Tbig = bsxTable(fn, Tbig, Tsmall, bigdom, smalldom)
%% Apply a binary function to two multidimensional vectors using bsxfun
% reshaping and virtually expanding the smaller table as needed
%
% The tables are matched up according to smalldom and bigdom
% smalldom must be a subset (but not necessarily a subsequence) of bigdom
%
%
%%


% This file is from pmtk3.googlecode.com

smallsz = sizePMTK(Tsmall);
if isequal(bigdom, smalldom)
    Tbig = fn(Tbig, Tsmall);
else
    nbig    = numel(bigdom);
    ndx     = lookupIndices(smalldom, bigdom);
    
     % If the domains are not in the same order, permute columns of small to match
     % big's order
     % KPM 15 march 2011
     [sndx, perm] = sort(ndx); % same order as bigdom
    if ~isequal(ndx, sndx) % not in order
      Tsmall = permute(Tsmall, perm);
      smalldom = smalldom(perm);
      ndx     = lookupIndices(smalldom, bigdom);
    end
    
     % ensure Tsmall has same num dimensionas as Tbig
    sz      = ones(1, nbig);
    sz(ndx) = smallsz; % may have 1's in middle
    Tsmall  = reshape(Tsmall, [sz 1]);
    
    % Elementwise operate  
    Tbig    = bsxfun(fn, Tbig, Tsmall);
end
end
