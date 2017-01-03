function k=kernel(x,xp,pref,lambda,noise,varargin)
%KERNEL A kernel evaluated at two points
% k=kernel(x,xp,pref,lambda,noise,<kernel>)
% A kernel evaluated at two points x and xp
% By default the kernal is the Squared Exponential Kernel
% kernel:  1 = Squared Exponential, 2 = OU, 3= Linear
delx = x-xp;
if isempty(varargin)
    kerneltype=1;
else
    kerneltype=varargin{1};
end
switch kerneltype
    case 1
        k=pref*exp(-lambda*(delx'*delx)); % Squared Exponential kernel
    case 2
        k=pref*exp(-lambda*sqrt(delx'*delx)); %Ornsten Uhelnbeck kernel
    case 3
        k=pref*(x'*xp);  % linear kernel
end
k=k+noise*eye(size(k)); % add on the noise