function d = lpcsynth(a,g,e,h,ov)
% d = lpcsynth(a,g,e,h,ov)   Resynthesize from LPC representation
%    Each row of a is an LPC fit to a h-point (non-overlapping) 
%    frame of data.  g gives the overall gains for each frame and 
%    e is an excitation signal (if e is empty, white noise is used; 
%    if e is a scalar, a pulse train is used with that period).
%    ov nonzero selects overlap-add of reconstructed 
%    windows, else e is assumed to consist of independent hop-sized 
%    segments that will line up correctly without cross-fading
%    (matching the ov option to lpcfit; default is ov = 1).
%    Return d as the resulting LPC resynthesis.
% 2001-02-25 dpwe@ee.columbia.edu $Header: /homes/dpwe/matlab/columbiafns/RCS/lpcsynth.m,v 1.1 2004/03/30 20:56:04 dpwe Exp $

if nargin < 3
  e = [];
end
if nargin < 4
  h = 128;
end
if nargin < 5
  ov = 1;
end

w = 2*h;

[nhops,p] = size(a);

npts = nhops*h;
% Excitation needs extra half-window at the end if in overlap mode
nepts = npts + ov*(w-h);

if length(e) == 0
  e = randn(1,nepts);
elseif length(e) == 1
  pd = e;
  e = sqrt(pd) * (rem(1:nepts,pd) == 0);
else
  nepts = length(e);
  npts = nepts - ov*(w-h);
end

% Try to make sure we don't run out of e (in ov mode)
e = [e, zeros(1, w)];

d = zeros(1,npts);

for hop = 1:nhops
  
  hbase = (hop-1)*h;
  
  oldbit = d(hbase + [1:h]);
  aa = a(hop,:);
  G = g(hop);
  if ov == 0 
    newbit = G*filter(1, aa, e(hbase + [1:h]));
  else
    newbit = G*filter(1, aa, e(hbase + [1:w]));
  end
  if ov == 0
    d(hbase + [1:h]) = newbit;
  else
    d(hbase + [1:w]) = [oldbit, zeros(1,(w-h))] + (hanning(w)'.*newbit); 
  end
  
end

% De-emphasis (must match pre-emphasis in lpcfit)
pre = [1 -0.9];
d = filter(1,pre,d);
