function [t2t,t2n]=mkt2t(t)
%MKT2T  Compute element connectivities from element indices.
%   [T2T,T2N]=MKT2T(T)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

nt=size(t,1);
dim=size(t,2)-1;

switch dim
 case 1
  edges=[t(:,2)
         t(:,1)];
 case 2
  edges=[t(:,[2,3])
         t(:,[3,1])
         t(:,[1,2])];
 case 3
  edges=[t(:,[2,3,4])
         t(:,[3,4,1])
         t(:,[4,1,2])
         t(:,[1,2,3])];
end

ts=[repmat(int32(1:nt),1,dim+1); kron(int32(1:(dim+1)),ones(1,nt,'int32'))]';

edges=sort(edges,2);
[foo,foo,jx]=unique(edges,'rows');

[jx,ix]=sort(jx);
ts=ts(ix,:);

ix=find(diff(jx)==0);
ts1=ts(ix,:);
ts2=ts(ix+1,:);

t2t=zeros(nt,dim+1,'int32');
t2t(ts1(:,1)+nt*(ts1(:,2)-1))=ts2(:,1);
t2t(ts2(:,1)+nt*(ts2(:,2)-1))=ts1(:,1);

if nargout>=2
  t2n=zeros(nt,dim+1,'int32');
  t2n(ts1(:,1)+nt*(ts1(:,2)-1))=ts2(:,2);
  t2n(ts2(:,1)+nt*(ts2(:,2)-1))=ts1(:,2);  
end
