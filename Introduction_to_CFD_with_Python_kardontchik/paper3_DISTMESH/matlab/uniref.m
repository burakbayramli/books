function [p,t]=uniref(p,t,nref,fd,varargin)
%UNIREF Uniform mesh refinement
%   [P,T]=UNIREF(P,T,NREF,FD,FDARGS)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

if nargin<3, nref=1; end
if nargin<4, fd={}; end

dim=size(t,2)-1;

for iref=1:nref
  np=size(p,1);
  nt=size(t,1);
  switch dim
   case 1
    pmid=(p(t(:,1),:)+p(t(:,2),:))/2;
    t1=t(:,1);
    t2=t(:,2);
    t12=(1:nt)'+np;
    t=[t1,t12;
       t12,t2];
    p=[p;pmid];
   case 2
    pair=[t(:,[1,2]);t(:,[1,3]);t(:,[2,3])];
    [pair,pairi,pairj]=unique(sort(pair,2),'rows');
    pmid=(p(pair(:,1),:)+p(pair(:,2),:))/2;
    t1=t(:,1);
    t2=t(:,2);
    t3=t(:,3);
    t12=pairj(1:nt)+np;
    t13=pairj(nt+1:2*nt)+np;
    t23=pairj(2*nt+1:3*nt)+np;
    
    t=[t1,t12,t13;
       t12,t23,t13;
       t2,t23,t12;
       t3,t13,t23];
    p=[p;pmid];
   case 3
    error('Not implemented.');
  end
  
  if ~isempty(fd)
      for iproj=1:5
          p=bndproj(p,t,fd,varargin{:});
      end
  end
end
