function [bi,FBI]=stp_BI(FIMG,N,W0,W1,ALPHA)
% Band-pass oriented image filtering in 2D Fourier domain
% FIMG-the 2D FFT of the image (NxN size)

FBI=zeros(N,N); %space for filtered image
ny=0; cs=1;
for wy=-pi:2*pi/(N-1):pi,
   ny=ny+1;  nx=0;
   for wx=-pi:2*pi/(N-1):pi,
      nx=nx+1;
      w=sqrt(wx^2+wy^2);
      if w<W0, R=0; 
      else
         if w>W1, R=0;
         else
            aux=cos(pi*(w-W0)/(W1-W0));
            theta=atan2(wy,wx);
            cs=cos(theta-ALPHA)^2;
            R=sqrt(0.5*(1-aux));
         end;
      end;
      FBI(ny,nx)=FIMG(ny,nx)*R*cs; %filtering in Fourier domain
   end;
end;   

bi=ifft2(FBI); %inverse Fourier transform of filtered image