function d=dpoly(p,pv)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

np=size(p,1);
nvs=size(pv,1)-1;

ds=dsegment(p,pv);
%ds=zeros(np,nvs);
%for iv=1:nvs
%  ds(:,iv)=donesegment(p,pv(iv:iv+1,:));
%end
d=min(ds,[],2);

d=(-1).^(inpolygon(p(:,1),p(:,2),pv(:,1),pv(:,2))).*d;

% MEXED

%function ds=donesegment(p,pv)
%
%e=ones(size(p,1),1);
%
%v=diff(pv,1);
%w=p-e*pv(1,:);
%
%c1=sum(w.*v(e,:),2);
%c2=sum(v(e,:).^2,2);
%
%ds=0*e;
%
%ix=c1<=0;
%ds(ix)=sqrt(sum((p(ix,:)-pv(1*ones(sum(ix),1),:)).^2,2));
%
%ix=c1>=c2;
%ds(ix)=sqrt(sum((p(ix,:)-pv(2*ones(sum(ix),1),:)).^2,2));
%
%ix=c1>0 & c2>c1;
%nix=sum(ix);
%if nix>0
%  Pb=ones(nix,1)*pv(1,:)+c1(ix)./c2(ix)*v;
%  ds(ix)=sqrt(sum((p(ix,:)-Pb).^2,2));
%end
