function h=hmatrix3d(p,xx,yy,zz,dd,hh,varargin)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

h=interpn(xx,yy,zz,hh,p(:,1),p(:,2),p(:,3),'*linear');
