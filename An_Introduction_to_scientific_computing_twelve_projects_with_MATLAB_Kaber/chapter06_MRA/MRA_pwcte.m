%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
       function [f,xx]=MRA_pwcte(nbni);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [f,xx]=MRA_pwcte(nbni)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   MRA: Multi Resolution Analysis
%%   Approximation of a function by its mean values 
%%   on intervals [2^{-j}k,2^{-j}(k+1)]
%%   
%%   Input :  nbni level number in MRA
%%
%%   Ouput : f    sampling of function MRA_function
%%           xx   sampling points
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      nbp=MRA_pntniv(nbni);
      fm=zeros(1,nbp);
      fc=zeros(1,nbp+1);
      f=zeros(1,nbp);
      xx=zeros(1,nbp);    
%     domain size
      xl=1.d0;
%     mesh step size
      h=xl/nbp;
      h2=h/2.d0; 
%     sampling 
%     MRA_function is the name of the 
%     function to be sampled (see MRA_function.m)
      x=h2;    
      for i=1:nbp
         xx(i)=x; 
         fc(i)=MRA_function(x-h2);
         fm(i)=MRA_function(x);
         x=x+h;
      end
      fc(nbp+1)=MRA_function(x-h2);
%     h * f(i) = int_{i}^{i+1} f(t) dt
%     mean value by Simpson rule
      h6=1./6.d0;
      for i=1:nbp
         f(i)=(fc(i)+fc(i+1)+4.d0*fm(i))*h6;
      end
