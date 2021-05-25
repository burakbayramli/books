function p=bndproj(p,t,fd,varargin)
%BNDPROJ Project boundary points to true boundary
%   P=BNDPROJ(P,T,FD,FDARGS)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

deps=sqrt(eps)*max(max(p)-min(p));

if size(p,2)==2
  e=boundedges(p,t);
  e=unique(e(:));
  
  d=feval(fd,p(e,:),varargin{:});
  dgradx=(feval(fd,[p(e,1)+deps,p(e,2)],varargin{:})-d)/deps;
  dgrady=(feval(fd,[p(e,1),p(e,2)+deps],varargin{:})-d)/deps;
  dgrad2=dgradx.^2+dgrady.^2;
  dgrad2(dgrad2==0)=1;
  p(e,:)=p(e,:)-[d.*dgradx./dgrad2,d.*dgrady./dgrad2];
elseif size(p,2)==3
  if size(t,2)==3
    tri=t;
  else
    tri=surftri(p,t);
  end
  tri=unique(tri(:));
  
  d=feval(fd,p(tri,:),varargin{:});
  dgradx=(feval(fd,[p(tri,1)+deps,p(tri,2),p(tri,3)],varargin{:})-d)/deps;
  dgrady=(feval(fd,[p(tri,1),p(tri,2)+deps,p(tri,3)],varargin{:})-d)/deps;
  dgradz=(feval(fd,[p(tri,1),p(tri,2),p(tri,3)+deps],varargin{:})-d)/deps;
  dgrad2=dgradx.^2+dgrady.^2+dgradz.^2;
  dgrad2(dgrad2==0)=1;
  p(tri,:)=p(tri,:)-[d.*dgradx./dgrad2,d.*dgrady./dgrad2,d.*dgradz./dgrad2];
end
