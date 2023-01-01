%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
        function [ur,uw]=MRA_daube4(u,uo,nbp,nbni,is);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   MRA: Multi Resolution Analysis
%%   function [ur,uw]=MRA_daube4(u,uo,nbp,nbni,is)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Signal decomposition - recomposition : Daubechies D4 wavelet
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
         c=4.d0*sqrt(2.d0);
         c0=(1.d0+sqrt(3.d0))/c;
         c1=(3.d0+sqrt(3.d0))/c;
         c2=(3.d0-sqrt(3.d0))/c;
         c3=(1.d0-sqrt(3.d0))/c;
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
             njp1=nj+1;
             if (ni==1) njp1=1 ; end       
             ur(njp1)=ur(1);
             nj2=nj;
             nj=nj/2;
             for i=1:nj-1
                i2=2*i;
%               Vj coefficients
                uw(i)=c0*ur(i2-1)+c1*ur(i2)+c2*ur(i2+1)+c3*ur(i2+2);
%               Wj coefficients (wavelets)
                uw(nj+i)=c3*ur(i2-1)-c2*ur(i2)+c1*ur(i2+1)-c0*ur(i2+2);
             end
             uw(nj)=c0*ur(nj2-1)+c1*ur(nj2)+c2*ur(1)+c3*ur(2);
             uw(nj2)=c3*ur(nj2-1)-c2*ur(nj2)+c1*ur(1)-c0*ur(2);
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
             ur(1)=c2*uw(nj)+c1*uw(nj2)+c0*uw(1)+c3*uw(nj+1);
             ur(2)=c3*uw(nj)-c0*uw(nj2)+c1*uw(1)-c2*uw(nj+1);
             for i=1:nj-1
                i2=2*i;nji=nj+i;
%               Vj+1 coefficients
                ur(i2+1)=c2*uw(i)+c1*uw(nji)+c0*uw(i+1)+c3*uw(nji+1);
                ur(i2+2)=c3*uw(i)-c0*uw(nji)+c1*uw(i+1)-c2*uw(nji+1);
             end
             nj=nj2;
          end
%
      end