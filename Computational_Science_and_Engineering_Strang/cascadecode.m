
%4.7  cascadecode.m

% CASCADE.M - Wavelet cascade algorithm, Daubechies 4

h=[1+sqrt(3),3+sqrt(3),3-sqrt(3),1-sqrt(3)]/8; n=length(h)-1;
tsplit=100; tt=0:1/tsplit:n; ntt=length(tt); phi=double(tt<1);

while 1     % Iterate until convergence or divergence
   phinew=0*phi;
   for j=1:ntt
     for k=0:n
       index=2*j-k*tsplit+1;
       if index>=1 & index<=n*tsplit+1
         phinew(j)=phinew(j)+2*h(k+1)*phi(index);
       end
     end
   end
   plot(tt,phinew),pause(1e-1)

   if max(abs(phinew))>100, error('divergence'); end
   if max(abs(phinew-phi))<1e-3, break; end
   phi=phinew;
end
