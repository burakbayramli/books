function [hi,FHI]=stp_HI(FIMG,N,W1,WMAX)
% High-pass image filtering in 2D Fourier domain
% FIMG-the 2D FFT of the image (NxN size)

FHI=zeros(N,N); %space for filtered image
ny=0;
for wy=-pi:2*pi/(N-1):pi,
   ny=ny+1;  nx=0;
   for wx=-pi:2*pi/(N-1):pi,
      nx=nx+1;
      w=sqrt(wx^2+wy^2);
      if w<W1, H=0; 
      else
         if w>WMAX, H=1;
         else
            aux=cos(pi*(w-W1)/(WMAX-W1));
            H=sqrt(0.5*(1-aux));
         end;
      end;
      FHI(ny,nx)=FIMG(ny,nx)*H; %filtering in Fourier domain
   end;
end;   

hi=ifft2(FHI); %inverse Fourier transform of filtered image