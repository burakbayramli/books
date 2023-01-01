%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
       function [f,xx]=MRA_sample(nbni);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [f,xx]=MRA_sample(nbni)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   MRA: Multi Resolution Analysis
%%   Plot a sample of a function 
%%   
%%   Input :  nbni level number in MRA
%%
%%   Ouput : f    sampling of function MRA_function
%%           xx   sampling points
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      nbp=MRA_pntniv(nbni);
      f=zeros(1,nbp);
      xx=zeros(1,nbp); 
%     domain size
      xl=1.d0;
%     mesh step size
      h=xl/nbp;
      x=0.;
      for i=1:nbp
         xx(i)=x; 
         x=x+h;
      end
%     sampling
%     MRA_function is the name of the 
%     function to be sampled (see MRA_function.m)
      f=MRA_function(xx); 
      nf=5; figure(nf);fs=18;
      hold on
%     plotting the signal
      title('Sample','FontSize',fs);
      plot(xx,f,'b'); hold off
