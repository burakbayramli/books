function ddraw = dirich_rnd(a)
% PURPOSE: a matrix of random draws from the Dirichlet distribution
%---------------------------------------------------

kdim=size(a,1);
a1=zeros(kdim,1);
for i = 1:kdim
    a1(i,1)=gamm_rnd(1,1,a(i,1),1);
end
ddraw=a1./sum(a1);


    

