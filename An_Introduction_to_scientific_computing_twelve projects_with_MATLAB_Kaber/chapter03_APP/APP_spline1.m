%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
n0=10;E=[];N=[];
for i=1:10, 
    n=i*n0;E=[E;APP_errorS1(n)];N=[N;n];
end;
loglog(N,E,'-+','MarkerSize',10,'LineWidth',3);
set(gca,'FontSize',24);
xlabel('log n');ylabel('log Error');
fprintf('slope of the straight line = %g \n',log(E(end)/(E(1)))/log(N(end)/N(1)));
title('piecewise linear approximation')
