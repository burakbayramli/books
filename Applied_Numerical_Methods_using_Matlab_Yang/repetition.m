function y=repetition(x,M,m)
if m==1
  MNx=ones(M,1)*x; y=MNx(:)';
 else
  Nx=length(x); N=ceil(Nx/m);
  x=[x zeros(1,N*m-Nx)];
  MNx=ones(M,1)*x; 
  y=[];
  for n=1:N
    tmp= MNx(:,(n-1)*m+[1:m]).';
    y= [y tmp(:).'];
 end
end 
