%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
error=[ ];N=[ ];
for n=10:10:100
   A=FEM_ConvecDiffAP1(eps,lambda,n);    
   b=FEM_ConvecDiffbP1(n,f);             
   u=A\b;             
   u=[0;u;0];                       
   x=(0:n+1)'/(n+1);                 
   uexa=FEM_ConvecDiffSolExa(eps,lambda,1,x);  
   N=[N;n];error=[error; norm(uexa-u,'inf')];
end
plot(log(N),log(error),'+-');

