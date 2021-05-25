function h=hmatrix(p,xx,yy,dd,hh,varargin)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

h=interp2(xx,yy,hh,p(:,1),p(:,2),'*linear');
