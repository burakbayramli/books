function y=resamp(x,Ly)
% y=resamp(x,Ly)
% 
% Inputs: x(1:Lx) =real vector of length Lx.
%         Ly =length of resampled version of x.
%
% Output: y(1:Ly) =resampled version of x.       
%
% There are (Lx-1) steps from beginning to end of x(1:Lx), and
%           (Ly-1) steps from beginning to end of y(1:Ly).
% Therefore the original time step, Tx, is changed to Ty=Tx*(Lx-1)/(Ly-1).
% For instance, if Ty=Tx/c is desired, then Ly=c*(Lx-1)+1.
%
% Interpolation is done in the frequency domain using fft's.
% Ly can be < Lx.  In this case x should have no spectral content at or
%   above 0.5/Ty Hz, assuming Ty is in seconds.
%
% See also: reconst, resamp_log, resamp_r, resamp_m

% Check for errors.
if length(x)~=numel(x),             %assure x is a vector
    error('In resamp, x must be a vector.');
elseif Ly-fix(Ly) ~= 0
    error('Ly (length of y) must be an integer');
end

%prepare to resample: remove a ramp and create an odd periodic signal, x2
x=x(:);                                 %make x a col. vector
Lx=length(x);
x1=x-linspace(x(1),x(Lx),Lx)';          %x1(1)=x1(Lx)=0
x2=[x1; -x1(Lx-1:-1:2)];               	%odd vector of length Lx2=2*Lx-2
X2=fft(x2);                             %X2 is imaginary; Lx2 is even
Lx2=length(X2);                         %Nyquist: X2(Lx2/2+1)=0

%create Y2, the spectrum of y2
if Ly>Lx                                %upsampling: Ly > Lx
    z=2*(Ly-Lx);                       	%#zeros to insert in Y2
    Y2=[X2(1:Lx2/2); zeros(z,1); X2(Lx2/2+1:Lx2)];
else                                    %downsampling: Ly <= Lx
    d=Lx-Ly;                            %delete 2d components of X2
    Y2=[X2(1:Lx2/2-d); 0; X2(Lx2/2+d+2:Lx2)];
end
Ly2=length(Y2);                         %Ly2=Lx2+2d
y2=(Ly2/Lx2)*real(ifft(Y2));            %scaled inv. DFT

%y is the first Ly elements of y2 with the ramp added back
y=y2(1:Ly)+linspace(x(1),x(Lx),Ly)';	%add back the ramp
return
%*******************************************************************
sp_fig(1);
kx=0:Lx-1;
ky=(0:Ly-1)*(Lx-1)/(Ly-1);
plot(kx,x,'o',ky,y,'*'); grid;


