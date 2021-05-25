function [p,t,pix]=fixmesh(p,t,ptol)
%FIXMESH  Remove duplicated/unused nodes and fix element orientation.
%   [P,T]=FIXMESH(P,T)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

if nargin<3, ptol=1024*eps; end
if nargin>=2 & (isempty(p) | isempty(t)), pix=1:size(p,1); return; end

snap=max(max(p,[],1)-min(p,[],1),[],2)*ptol;
[foo,ix,jx]=unique(round(p/snap)*snap,'rows');
p=p(ix,:);

if nargin>=2
    t=reshape(jx(t),size(t));
    
    [pix,ix1,jx1]=unique(t);
    t=reshape(jx1,size(t));
    p=p(pix,:);
    pix=ix(pix);
    
    if size(t,2)==size(p,2)+1
        flip=simpvol(p,t)<0;
        t(flip,[1,2])=t(flip,[2,1]);
    end
end
