function [H,v]=gain(b,a,N)
% [H,v]=gain(b,a,N)
%
% H  =N values of complex gain of a linear digital system.
% v  =N frequencies in [0,0.5] Hz-s at which gain values are computed.
%     N is optional and is set internally to 513 unless specified.
%     H and v are both column vectors of length N.
% b  =numerator weights of linear system transfer function.
% a  =denominator weights.  a(1) must not be 0. Usually, a(1)=1.
%
% b and a can be arrays. If so, each row contains the weights of a
% section of a single overall digital transfer function in cascade form.
% See also gain_f, power_gain, pds

% Check for 3rd argument and for errors.
if(nargin<3),
   N=513;
elseif(length(N)~=1),
  error('The third argument (N) must be a scalar.');
end
[nr,ncb]=size(b);
[nra,nca]=size(a);
if(nra==1 & nca==1)
   b=row_vec(b);
   ncb=length(b); nr=1;
end
if nr~=nra,
   error('Number of rows in b and a must be the same.')
elseif(min(abs(a(:,1)))==0)
   error('a cannot have a zero in its first column.');
elseif(N<max(nca,ncb)),
   error('N cannot be < the length of a weight vector.');
end
% Compute the gain using fft's of length N2.
N2=2*(N-1);
a=[a zeros(nr,N2-nca)];
b=[b zeros(nr,N2-ncb)];
A=fft(a');
B=fft(b');
if(min(abs(A))==0),
   error('Gain is infinite at some point.');
end
if(nr>1),
	H=prod((B./A)');
else
	H=(B./A)';
end
% Specify v and H(v).
v=linspace(0,.5,N)';
H=H(1:N)';
