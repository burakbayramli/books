function play(y,fs,t)
% play(y,fs,t)
% 
% Same as wavplay except range of y is adjusted to +-1.
%
% y  =sample vector.
% fs =sampling rate (8000,11025,...etc.).
%
% You may add a third argument, t, >0 and <length(y)/fs.
% Play is then terminated at t seconds.
if nargin<2
    error('Not enough arguments.');
elseif nargin>3
    error('Too many argurments.');
elseif max(y)-min(y)<=0
    error('Zero signal vector.');
end
scale=2/(max(y)-min(y));
ys=(y-min(y))*scale-1;
K=length(ys);
if(nargin==3)
    K=min(K,max(1,fix(t*fs)));
end
wavplay(ys(1:K),fs);
