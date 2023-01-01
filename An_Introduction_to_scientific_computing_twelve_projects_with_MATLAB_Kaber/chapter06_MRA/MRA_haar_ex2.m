%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Matlab Solution of exercise 2 - project 6
%%   MRA: Multi Resolution Analysis
%%   Signal decomposition - recomposition : Haar wavelet
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
      [ue,x]=MRA_pwcte(nbni);uo=zeros(1,nbp);MRA_steps(x,ue);
%     decomposition by Haar wavelets
      is=1;[ur,uw]=MRA_haar(ue,uo,nbp,nbni,is);
%     compression by getting rid of small coefficients
      seuil=1.d-04;[uo,nbc]=MRA_tri1(uw,seuil);
      fprintf('\n Coefficients elimination <   %12.8f',seuil);
      fprintf('\n Significant coefficients     %d',nbc);
%     recomposition by Haar wavelets
      is=-1;[ur,uw]=MRA_haar(ur,uo,nbp,nbni,is);
%     comparison to original signal
      e=norm(ur-ue,2);eu=norm(ue,2);er=e/eu;
      fprintf('\n Error on recomposed signal %12.8f',er);
%     plotting both signals
      nf=10; figure(nf);fs=18;
      plot(x,ue,'r',x,ur,'b');
      legend('Original Signal','Reconstructed Signal');
      title('Haar wavelet','FontSize',fs);
