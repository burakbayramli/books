function d=dcircle(p,xc,yc,r)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

d=sqrt((p(:,1)-xc).^2+(p(:,2)-yc).^2)-r;
