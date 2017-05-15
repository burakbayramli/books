function [lo,hi]=D_afilt(x,af1,aff)

% 2D analysis filtering

%columns filtering
[l,h]=D_1af(x,af1,1);

%rows filtering
[lo, hi{1}]=D_1af(l,aff,2);
[hi{2}, hi{3}]=D_1af(h,aff,2);