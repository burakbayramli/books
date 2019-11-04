x=-1:.1:1;
nx=length(x);
zx=zeros(nx,nx);
for j=1:nx
    for i=1:nx
      xv=[x(i),x(j)]';
      z(i,j)=f_easy(xv);
    end
end
mesh(x,x,z);
