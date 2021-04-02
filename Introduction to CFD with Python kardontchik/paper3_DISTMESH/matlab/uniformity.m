function u=uniformity(p,t,fh,varargin)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

[pc,r]=circumcenter(p,t);
hc=feval(fh,pc,varargin{:});

sz=r./hc;
u=std(sz)/mean(sz);
