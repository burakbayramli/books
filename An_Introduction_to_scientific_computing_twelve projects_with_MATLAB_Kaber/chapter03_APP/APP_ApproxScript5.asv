%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%The uniform case
ind=[];lebE=[];
for n=10:5:30
    x=(-n/2:n/2)/n*2; %equispaced points
    l=APP_Lebesgue(x)
    ind=[ind;n];lebE=[lebE;l];
end;
figure(1);plot(ind,log(lebE),'+-')
title('Lebesgue constant for equispaced points')
xlabel('n')
ylabel('log(\Lambda)')
% Chebyshev points
ind=[];lebT=[];
for n=10:5:30
    x=cos(pi*(.5+n:-1:0)/(n+1)); %Chebyshev points
    l=APP_Lebesgue(x);
    ind=[ind;n];lebT=[lebT;l];
end;
figure(2);plot(ind,exp(lebT),'+-')
title('Lebesgue constant for Tchebyschev points')
xlabel('n')
ylabel('e^\Lambda')
