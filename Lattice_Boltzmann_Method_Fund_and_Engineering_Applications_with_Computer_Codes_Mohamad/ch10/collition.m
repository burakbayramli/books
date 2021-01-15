function [f]=collition(nx,ny,u,v,cx,cy,omega,f,rho,w,mm,mminv,msm)
% Calculate the fmeq
fmeq(:,:,1)= rho.*(-2.+3.*rho.*(u.*u+v.*v));
fmeq(:,:,2)= rho.*( 1.-3.*rho.*(u.*u+v.*v));
fmeq(:,:,3)= rho.* u;
fmeq(:,:,4)=-rho.* u;
fmeq(:,:,5)= rho.* v;
fmeq(:,:,6)=-rho.* v;
fmeq(:,:,7)= rho.*(u.*u-v.*v);
fmeq(:,:,8)= rho.* u.*v;
fmeq(:,:,9)= rho;

for i=1:nx

for j=1:ny

for k=1:9
smf=0.0;
for N=1:9

smf=smf+mm(k,N)*f(i,j,N);

end
fmom(i,j,k)=smf;
end

end

end

for j=1:ny

for i=1:nx

for k=1:9

ssmb=0.0;
for N=1:9

ssmb=ssmb+msm(k,N)*(fmom(i,j,N)-fmeq(i,j,N)); %saleh

end

f(i,j,k)=f(i,j,k)+ssmb;

end

end

end

end
