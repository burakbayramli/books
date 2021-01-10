function[rho,u,v]=ruv(nx,ny,f)

rho=sum (f,3);

for i=1:nx

rho(i,ny)=f(i,ny,9)+f(i,ny,1)+f(i,ny,3)+2.*(f(i,ny,2)+f(i,ny,6)+f(i,ny,5));

end


%calculate velocity compnents

u = ( sum(f(:,:,[1 5 8]),3) - sum(f(:,:,[3 6 7]),3) )./rho;

v = ( sum(f(:,:,[2 5 6]),3) - sum(f(:,:,[4 7 8]),3) )./rho;

end
