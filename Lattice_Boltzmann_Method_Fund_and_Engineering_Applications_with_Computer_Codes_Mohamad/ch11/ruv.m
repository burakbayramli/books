function[rho,u,v]=ruv(nx,ny,f)
rho=sum (f,3);

%calculate velocity compnents

u = ( sum(f(:,:,[1 5 8]),3) - sum(f(:,:,[3 6 7]),3) )./rho;

v = ( sum(f(:,:,[2 5 6]),3) - sum(f(:,:,[4 7 8]),3) )./rho;

end
