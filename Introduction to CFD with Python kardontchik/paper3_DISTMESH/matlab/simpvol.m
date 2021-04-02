function v=simpvol(p,t)
%SIMPVOL Simplex volume.
%   V=SIMPVOL(P,T)

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

switch size(p,2)
 case 1
  d12=p(t(:,2),:)-p(t(:,1),:);
  v=d12;
 case 2
  d12=p(t(:,2),:)-p(t(:,1),:);
  d13=p(t(:,3),:)-p(t(:,1),:);
  v=(d12(:,1).*d13(:,2)-d12(:,2).*d13(:,1))/2;
 case 3
  d12=p(t(:,2),:)-p(t(:,1),:);
  d13=p(t(:,3),:)-p(t(:,1),:);
  d14=p(t(:,4),:)-p(t(:,1),:);
  v=dot(cross(d12,d13,2),d14,2)/6;
 otherwise
  v=zeros(size(t,1),1);
  for ii=1:size(t,1)
    A=zeros(size(p,2)+1);
    A(:,1)=1;
    for jj=1:size(p,2)+1
      A(jj,2:end)=p(t(ii,jj),:);
    end
    v(ii)=det(A);
  end
  v=v/factorial(size(p,2));
end
