%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Matlab Solution of exercise 3 - project 6
%%   MRA: Multi Resolution Analysis
%%   Signal decomposition - recomposition : Schauder wavelet
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
%     sample                
      [ue,x]=MRA_sample(nbni);uo=zeros(1,nbp);
%     decomposition by Schauder wavelets
      is=1;[ur,uw]=MRA_schauder(ue,uo,nbp,nbni,is);
%     recomposition by Schauder wavelets
      is=-1;uo=uw;[ur,uw]=MRA_schauder(ur,uo,nbp,nbni,is);
%     plotting both signals
      nf=10; figure(nf);fs=18;
      plot(x,ue,'r',x,ur,'b');
      legend('Original Signal','Reconstructed Signal');
      title('Schauder wavelet','FontSize',fs);
