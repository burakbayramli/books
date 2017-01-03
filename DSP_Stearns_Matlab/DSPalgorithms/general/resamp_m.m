function y=resamp_m(x,m)
% y=resamp_m(x,m)
% 
% Inputs: x(1:Lx) =real vector of length x.
%         m       =INTEGER multiplier such that the sampling rate of y
%                  is m times the sampling rate of x.
%
% Output: y       =resampled version of x with length(y)=m*length(x).
%
% The original sampling frequency, fx,is changed to fy=m*fx.
% The original time step, Tx, is changed to Ty=Tx/m.
%
% Interpolation is done in the frequency domain using fft's.
%
% See also: resamp, resconst, resamp_log, resamp_r

% Check for errors.
if m<=1,                                %assure m>1
    error('factor m must be >1.');
elseif length(x)~=numel(x),             %assure x is a vector
    error('x must be a vector.');
end

%prepare to upsample
x=x(:);                                 %make x a col. vector
Lx=length(x);
sx=(x(Lx)-x(1))/(Lx-1);                 %slope of ramp function
rx=x(1)+sx*(0:Lx-1)';
x1=x-rx;                                %remove the ramp
x2=[x1;-x1(Lx-1:-1:2)];                 %x1 is appended for sine fit
X2=fft(x2);                             %length(X2)=2*Lx-2

%upsampling: Ly>Lx. See (3.27) in the text. Sampling rate is increased.
H=X2(Lx)/2;                                 %(Nyquist component)/2
Nz=2*(m-1)*(Lx-1)-1;                        %# zeros to insert
z=zeros(Nz,1);
Y2=[X2(1:Lx-1); H; z; H; X2(Lx+1:2*Lx-2)];  %length(Y2)=2*Lx+Nz-1
y2=m*real(ifft(Y2));
y1=y2(1:m*(Lx-1)+1);                        %ignore the end of y2

%add the ramp back to y2
sy=sx/m;
ry=x(1)+sy*(0:m*(Lx-1))';
y=y1+ry;

return
%************************************************************************
%plots
tx=0:Lx-1;
tx2=0:2*Lx-3;
ty=(0:m*(Lx-1))/m;
ty2=(0:length(y2)-1)/m;
sp_fig(1); plot(tx,x,'-o',ty,y,'-^',tx,rx,'-x'); grid;
xlabel('x index'); ylabel('signals & ramp');
title('test of resamp\_m');
legend('x','y','ramp');
