function [y,iy]=resamp_log(x,Ly)
%[y,iy]=resamp_log(x,Ly)
% 
%Similar to resamp, but with logarithmicly spaced output samples.
%Inputs:  x  =input vector
%         Ly =length of output vector y
%
%Outputs: y  =vector with Ly log-spaced, linearly-interpolated samples of x
%         iy =Ly log-spaced indices of x, such that min(iy)=1, max(iy)=Lx,
%             and the interval, iy(k)-iy(k-1), increases exponentially 
%             with k.
%
%See also: resamp, reconst

%check for errors.
if Ly<3,                                    %assure Ly>1
    error('Ly must be >=3.');
elseif length(x)~=numel(x),                 %assure x is a vector
    error('x must be a vector.');
end

x=x(:);                                     %make x a col. vector
Lx=length(x);
d=x(Lx)-x(Lx-1);
x=[x; x(Lx)+d];                             %extend x with 1 point

%iy=vector with Ly points and exponentially increasing spacing
iy=exp((0:Ly-1)'*log(Lx)/(Ly-1));           %iymin=1; iymax=logLx

%interpolate linearly in x to get Ly log-spaced samples
y=zeros(Ly,1);
for k=1:Ly
    i0=floor(iy(k));
    if i0==iy(k)                            %if iy(k) is an index of x,
        y(k)=x(i0);                         %y(k)=x at i0
    else                                    %or if iy(k) > i0,
        dx=x(i0+1)-x(i0);
        di=iy(k)-i0;
        y(k)=x(i0)+di*dx;                   %interpolated value
    end
end
