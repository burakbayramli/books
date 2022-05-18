%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      function rhslap=ELAS_lap_rhs(x,y); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%    function rhslap=ELAS_lap_rhs(x,y)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
%%   Right-hand side definition at point M(x,y) 
%%   in order to solve exercise 1 of project 7
%%   Solution of the plate problem (linear equation)
%%   ELAS: elastic deformation of a thin plate
%%
%%   Input  : x, y coordinates of point M
%%
%%   Output : rhslap right-hand side for the Laplacian equation
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
      ax=3.7*pi;by=5.4*pi;coef=1.e+02;
      k=coef*(ax*ax+by*by);
%     rhslap=k*cos(ax*x)*sin(by*y);   %% 1)
      rhslap=k*sin(ax*x)*sin(by*y);   %% 2)
%     rhslap=0.;                      %% 3)
 