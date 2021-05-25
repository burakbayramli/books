function d=dmatrix3d(p,xx,yy,zz,dd,varargin)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

d=interpn(xx,yy,zz,dd,p(:,1),p(:,2),p(:,3),'*linear');
