%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
        function [ur,uw]=MRA_haar(u,uo,nbp,nbni,is);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [ur,uw]=MRA_haar(u,uo,nbp,nbni,is)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Signal decomposition - recomposition : Haar wavelet
%%   MRA: Multi Resolution Analysis
%%   
%%   Input : nbp  sampling points number
%%           nbni MRA levels number
%%           is   flag
%%           u    signal  sampling (when is = 1)
%%           uo   wavelet coefficients (when is = -1)
%%
%%   Output : uw  wavelet coefficients (when is = 1) 
%%            ur  signal  sampling (when is = -1)
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
      sqr2=sqrt(2.d0);
      c1=sqr2/2.d0;c2=sqr2/2.d0;
      d1=sqr2/2.d0;d2=-sqr2/2.d0;
%      
      if (is==1)
%
%         Decomposition
%         =============
%
%         interpolation
          uw=u;ur=u;
          nj=nbp; 
%         coefficients computation
          for ni=1:nbni
             for i=1:nj
                ur(i)=uw(i);
             end
             nj=nj/2;
             for i=1:nj
                i2=2*i;
%               Vj coefficients
                uw(i)=ur(i2-1)*c1+ur(i2)*c2;
%               Wj coefficients (wavelets)
                uw(nj+i)=ur(i2-1)*d1+ur(i2)*d2;
             end
          end
%
      else
%
%         Reconstruction
%         =============
%
          ur=uo;uw=uo;
          nj=1;
%         coefficients computation
          for ni=1:nbni
             nj2=nj*2;
             for i=1:nj2
                uw(i)=ur(i);
             end
             for i=1:nj
                i2=2*i;
%               Vj+1 coefficients 
                ur(i2-1)=uw(i)*c1+uw(nj+i)*c2;
                ur(i2)=uw(i)*d1+uw(nj+i)*d2;
             end
             nj=nj2;
          end
%
      end