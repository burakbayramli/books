%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Condition number of a Vandermonde matrix
%% as a fuction of the size of the matrix
%% 
N=2:2:20;cd=[];
for n=N
    cd=[cd APP_condVanderMonde(n)];
end;
plot(N,log(cd),'+-')
title('Condition number of VanDerMonde matrix')
xlabel('n')
ylabel('log(cond)')
