function [y,b,a]=comb(Nzeros,subset,r,x)
% [y,b,a]=comb(Nzeros,subset,r,x)
%
% Comb filter function.
% Inputs:
% Nzeros =# zeros at radius r on the z-plane.
%         0 to Nyquist range is [0:Nzeros/2]/Nzeros Hz-s.
% subset =subset of indices where poles exist.
%         subset must be in the form [k:n:m], so that there
%         are resonant frequencies at v=[k:n:m]/Nzeros Hz-s.
%         k must be >0 and m must be <=Nzeros/2.
% r      =pole & zero radius. Must be in range [0.9,1.0).
% x      =input signal vector. Must have length > Nzeros+100.
% Outputs:
% y     =output signal array, dimensioned length(v) x length(x),
%        The nth row of y is the filter output for the tooth
%        with resonance at v(n) Hz-s.
% [b,a] =weight arrays for the comb filter. Each array has
%        length(v) rows, one for each pole.
%
% See also: filter, filters, imp_resp

% Check for errors.
if(nargin<4),
   error('Four arguments are required.')
elseif(length(x)<=Nzeros+100),
   error('Length of x must be > N+100.');
elseif(r<.9 | r>=1),
   error('r must be at least 0.9 and less than 1.');
elseif(min(subset)<=0 | max(subset)>Nzeros/2)
   error('Subset must be in range (0,N/2]');
end
x=row_vec(x); N=Nzeros; Nc=length(subset);
% Do each column of y. b is set for max. gain =1. Nzeros=# zeros.
y=zeros(length(x),Nc);
b=ones(Nc,1)*[1 zeros(1,N-1) -r^N];
a=zeros(Nc,3);
for n=1:Nc,
   v=subset(n)/N;
   a(n,:)=[1 -2*r*cos(2*pi*v) r^2];
   Hrmax=1/abs(a(n,:)*[exp(j*2*pi*v) 1 exp(-j*2*pi*v)]');
   Hzmax=abs(1-(r^N)*exp(-j*2*pi*N*v));
   b(n,:)=b(n,:)/(Hrmax*Hzmax);
   y(:,n)=filter(b(n,:),a(n,:),x);
end
