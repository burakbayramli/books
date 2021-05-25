function d=dmatrix(p,xx,yy,dd,varargin)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

d=interp2(xx,yy,dd,p(:,1),p(:,2),'*linear');
