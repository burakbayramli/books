%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
        function [ur,uw]=MRA_schauder(u,uo,nbp,nbni,is);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [ur,uw]=MRA_schauder(u,uo,nbp,nbni,is)
%%   MRA: Multi Resolution Analysis
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Signal decomposition - recomposition : Schauder wavelet
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
      c1=sqrt(2.d0);
      c2=sqrt(2.d0)/2.d0;
      c3=-sqrt(2.d0)/2.d0;
      c4=sqrt(2.d0)/4.d0;
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
                uw(i)=ur(i2-1)*c1;
%               Wj coefficients (wavelets)
                i2p1=i2+1;
                if (i==nj) i2p1=1 ; end
                uw(nj+i)=ur(i2)*c1+(ur(i2p1)+ur(i2-1))*c3;
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
             for i=1:nj-1
                i2=2*i;
%               Vj+1 coefficients 
                ur(i2-1)=uw(i)*c2;
                ur(i2)=uw(nj+i)*c2+(uw(i)+uw(i+1))*c4;
             end
             ur(2*nj-1)=uw(nj)*c2;
             ur(2*nj)=uw(2*nj)*c2+(uw(nj)+uw(1))*c4;
             nj=nj2;
          end
%
      end