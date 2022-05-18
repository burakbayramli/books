%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Exercise 2.5
%%  Main script to solve the characteristic equation providing the
%%  linearized solution of the delayed system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear
td=4;
alpha=0.0005;
for  I=0:20
 bz=1/(1+8*alpha*I^3);
 funtext='(x+1)^2*(x+0.5)*(x+bz)+12 *alpha*I^3*bz*x*exp(-td*x)';
 funequi=inline(funtext,'x','bz','I','alpha','td');
 guess=i/2;
 x0=fsolve(funequi,guess,optimset('Display','off'),bz,I,alpha,td);
 fprintf('I=%f x0=%f+i%f\n',I,real(x0),imag(x0))
end
