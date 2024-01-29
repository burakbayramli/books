load nodes.dat
load elements.dat;
n=size(nodes,1);
ne=size(elements,1);

tmp=zeros(n,2);
k=1;
for i=1:n
  x=nodes(i,2);
  if x==0.0
    tmp(k,1)=i;
    tmp(k,2)=80;
    k=k+1;
  end
end
nodalTemp=tmp(1:k-1,:);


tmp=zeros(ne*4,3);
k=1;
% vertical edges
for i=1:ne
  for j=2:5
    x1=nodes(elements(i,j),2);
    y1=nodes(elements(i,j),3);
    if j==5
      x2=nodes(elements(i,2),2);
      y2=nodes(elements(i,2),3);
    else
      x2=nodes(elements(i,j+1),2);
      y2=nodes(elements(i,j+1),3); 
    end
    if x1==2.0  && x2==2.0
      tmp(k,1)=i;
      tmp(k,2)=j;
      tmp(k,3)=-1000;
      k=k+1;
    elseif y1==0  && y2==0
      tmp(k,1)=i;
      tmp(k,2)=j;
      tmp(k,3)=-1500;
      k=k+1;
    elseif y1==2.0  && y2==2.0
      tmp(k,1)=i;
      tmp(k,2)=j;
      tmp(k,3)=-500;
      k=k+1;  
    end
  end
end
edgeFlux=tmp(1:k-1,:);


dlmwrite('nodalTemp.dat',nodalTemp,'delimiter','\t','precision','%.6f');
dlmwrite('edgeFlux.dat',edgeFlux,'delimiter','\t','precision','%.6f');

