function [A11,B1,B2,areas] = NCAssembler(p,t2e,t)
nt=size(t,2);
ne=max(t2e(:));
A11=sparse(ne,ne);
B1=sparse(nt,ne);
B2=sparse(nt,ne);
areas=zeros(nt,1);
for i=1:nt
vertex=t(1:3,i);
x=p(1,vertex);
y=p(2,vertex);
[area,Sx,Sy]=CRGradients(x,y);
edges=t2e(i,:);
A11(edges,edges)=A11(edges,edges)+(Sx*Sx'+Sy*Sy')*area;
B1(i,edges)=-Sx'*area;
B2(i,edges)=-Sy'*area;
areas(i)=area;
end
