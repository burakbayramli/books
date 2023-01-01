%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      function rhsbilap=ELAS_bilap_rhs(x,y); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%    function rhsbilap=ELAS_bilap_rhs(x,y)
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
%%   Output : rhsbilap right-hand side for the bilaplacian equation
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
      ax=3.7*pi;by=5.4*pi;coef=1.e+02;
      ax2=ax*ax;by2=by*by;
      k=coef*(ax2*ax2+by2*by2+2.*ax2*by2);
%     rhsbilap=k*cos(ax*x)*sin(by*y);   %% 1)
      rhsbilap=k*sin(ax*x)*sin(by*y);   %% 2)
%     rhsbilap=0.;                      %% 3)
 