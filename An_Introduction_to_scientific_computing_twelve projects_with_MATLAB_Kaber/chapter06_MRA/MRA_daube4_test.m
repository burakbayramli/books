%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   MRA: Multi Resolution Analysis
%%   Looking for the Daubechies wavelet
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      clear all; close all;
%     MRA levels   
      nbni=10;
%     sampling points 
      nbp=MRA_pntniv(nbni);   
      fprintf('\n MRA levels  %d',nbni);
      fprintf('\n Sampling points   %d',nbp);
%     mean value interpolation                
      [ue,x]=MRA_sample(nbni);uo=zeros(1,nbp);
%     recomposition by Daubechies wavelets
      is=-1;ur=uo;uo(20)=1.;
      [ur,uw]=MRA_daube4(ur,uo,nbp,nbni,is);
%     plotting the signal 
      nf=10; figure(nf);fs=18;
      xm=[0.,1.];ym1=[0.5,0.5];ym2=[-0.5,-0.5];
      title('Daubechies wavelet','FontSize',fs);
      plot(x,ur,'b',xm,ym1,'white',xm,ym2,'white');
%     recomposition by Daubechies wavelets
      uo=zeros(1,nbp);
      is=-1;ur2=uo;uo(20)=1.;uo(24)=1.;
      [ur2,uw]=MRA_daube4(ur2,uo,nbp,nbni,is);
      uo=zeros(1,nbp);
      is=-1;ur3=uo;uo(120)=1.;uo(124)=1.;
      [ur3,uw]=MRA_daube4(ur3,uo,nbp,nbni,is);
%     plotting the signals 
      nf=11; figure(nf);fs=18;
      plot(x,ur2,'b',x,ur3,'r',xm,ym1,'white',xm,ym2,'white'); 
      title('Daubechies wavelets','FontSize',fs);
