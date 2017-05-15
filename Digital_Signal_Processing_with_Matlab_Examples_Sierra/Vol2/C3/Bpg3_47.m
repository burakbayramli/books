% Impulse response of the circular filter

%the circular filter in 2-D Fourier domain
R=4; %radius
rs=R^2;

Ffilt=zeros(256,256);
for nx=1:256,
   for ny=1:256,      
      pr=((nx-128)^2)+((ny-128)^2);
      if pr<=rs,
         Ffilt(nx,ny)=1;
      end;
   end;
end;

%inverse transform
hfilt=ifftshift(ifft2(Ffilt));
ah=abs(hfilt);

figure(1)
mesh(ah);
title('Impulse response');


