function [li,FLI]=stp_LI(FIMG,N,W1,WMAX)
% Low-pass image filtering in 2D Fourier domain
% FIMG-the 2D FFT of the image (NxN size)

FLI=zeros(N,N); %space for filtered image
ny=0;
for wy=-pi:2*pi/(N-1):pi,
   ny=ny+1;  nx=0;
   for wx=-pi:2*pi/(N-1):pi,
      nx=nx+1;
      w=sqrt(wx^2+wy^2);
      if w<W1, L=1; 
      else
         if w>WMAX, L=0;
         else
            aux=cos(pi*(w-W1)/(WMAX-W1));
            L=sqrt(0.5*(1+aux));
         end;
      end;
      FLI(ny,nx)=FIMG(ny,nx)*L; %filtering in Fourier domain
   end;
end;   

li=ifft2(FLI); %inverse Fourier transform of filtered image