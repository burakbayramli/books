%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Matlab Solution of exercise 7 - project 6
%%   MRA: Multi Resolution Analysis
%%   Image decomposition - recomposition : Haar wavelet
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
      clear all; close all; format long e ;
%     read image file
      [U,MAP]=imread('lenna.jpg','jpg');
%     plot original image
      nf=20;figure(nf);colormap('gray');
%     change of grey levelset
      U=double(U);Uscaled=(U+1)/4;
      image(Uscaled);
      title('Original Image');
%     MRA levels and sampling points 
      [n,m]=size(U);
      nbp=n;nbni=MRA_nivpnt(nbp);
      fprintf('\n MRA levels  %d',nbni);
      fprintf('\n Sampling points   %d',nbp);
%     decomposition by Haar wavelets
      V=zeros(nbp);W=zeros(nbp);V=U;
      ur=zeros(nbp,1);uo=zeros(nbp,1);uw=zeros(nbp,1);
%     two dimensional transformation 
      is=1;
      for i=1:nbp
         for j=1:nbp
            ur(j)=V(j,i);
         end
         [ur,uw]=MRA_haar(ur,uo,nbp,nbni,is);
         for j=1:nbp
            V(j,i)=uw(j);
         end
      end
      for i=1:nbp
         for j=1:nbp
            ur(j)=V(i,j);
         end
         [ur,uw]=MRA_haar(ur,uo,nbp,nbni,is);
         for j=1:nbp
            W(i,j)=uw(j);
         end
      end
%     compression by getting rid of small coefficients
      seuil=1.d-03;[W0,nbc]=MRA_tri2(W,seuil);
      fprintf('\n Coefficients elimination <   %12.8f',seuil);
      nbct=nbp*nbp;
      fprintf('\n Total number of coefficients  %d',nbct);
      fprintf('\n Significant coefficients      %d',nbc);
%     recomposition by Haar wavelets
      V=W0;UU=zeros(nbp);
%     two dimensional transformation 
      is=-1;
      for i=1:nbp
         for j=1:nbp
            uo(j)=V(i,j);
         end
         [ur,uw]=MRA_haar(ur,uo,nbp,nbni,is);
         for j=1:nbp
            V(i,j)=ur(j);
         end
      end
      for i=1:nbp
         for j=1:nbp
            uo(j)=V(j,i);
         end
         [ur,uw]=MRA_haar(ur,uo,nbp,nbni,is);
         for j=1:nbp
            UU(j,i)=ur(j);
         end
      end
%     comparison to original image
      e=norm(U-UU,2);eu=norm(U,2);er=e/eu;
      fprintf('\n Error on recomposed signal %12.8f',er);
%     plot the recomposed signal
      nf=21;figure(nf);fs=18;
      colormap('gray');
      UUscaled=(UU+1)/4;
      image(UUscaled);
      title('Reconstructed Image (Haar)','FontSize',fs);