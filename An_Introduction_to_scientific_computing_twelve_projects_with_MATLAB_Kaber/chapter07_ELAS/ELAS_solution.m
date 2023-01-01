%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      function u=ELAS_solution(x,y); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%    function u=ELAS_solution(x,y)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%   Definition of the solution at point M(x,y) 
%%   in order to solve exercise 1 of project 7
%%   Solution of the plate problem (linear equation)
%%   ELAS: elastic deformation of a thin plate
%%
%%
%%   Input  : x, y coordinates of point M
%%
%%   Output : u  solution value in this point
%%               (see also  procedures rhs.m and rhs2.m)
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
      ax=3.7*pi;by=5.4*pi;coef=1.e+02;;c0=1.e+01;
      u0=(ax*x-by*y);
%     u=coef*cos(ax*x)*sin(by*y);          %% 1)
      u=coef*sin(ax*x)*sin(by*y)+c0*u0;    %% 2)
%     u=(1.-x)*(1.-x)+(.5-y)*(.5-y);       %% 3)
