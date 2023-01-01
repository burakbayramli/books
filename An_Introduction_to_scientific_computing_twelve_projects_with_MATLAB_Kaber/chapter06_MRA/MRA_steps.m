%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
     function MRA_steps(x,fc);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function MRA_steps(x,fc)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   MRA: Multi Resolution Analysis
%%   Plot a piecewise constant approximation
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      nf=5; figure(nf);fs=18;
      hold on
      nn=size(fc,2);
      h=x(2)-x(1);h2=h/2.d0;
%     plotting the signal
      title('Piecewise constant approximation','FontSize',fs);
      for i=1:nn
          xx(1)=x(i)-h2;xx(2)=x(i)+h2;
          fcp(1)=fc(i);fcp(2)=fc(i);
          plot(xx,fcp,'b');
      end
%     MRA_function is the name of the 
%     function to be sampled (see MRA_function.m)
      f=MRA_function(x); 
      plot(x,f,'r'); hold off
