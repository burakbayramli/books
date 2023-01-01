%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function v=FEM_ConvecDiffbP2(n,f)
%computation of the right hand side of the linear system
%by the Simpson rule
h=1/(n+1);tab=.5*(1:2*n+1)'*h;
ftab=feval(f,tab); 
v=h*ftab/3;v(1:2:end)=2*v(1:2:end);


