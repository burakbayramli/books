function [y,Nbits]=a_encode(x,ipr)
% [y,Nbits]=a_encode(x)
% Arithmetic encoding using a fixed frequency table computed
% by freq(x).
%
% x     =input vector of integers.
%        NOTE: the minimum element in x must be zero.
%
% y     =encoded version of x. Each element of y has one byte 
%        (8 bits) of the arithmetic-coded version of x.
%
% Nbits =Total number of bits in y. Let Ny = length(y).
%        Then 8*(Ny-1) < Nbits <= 8*Ny.
%
% Note: You may include a second argument, i.e., a_encode(x,ipr).
%       If ipr=1 encoding info is printed. If ipr=2, more is printed.
%
% See also: a_decode, freq
if nargin==1,
	ipr=0;
end
Nx=length(x(:));
% Frequencies (f) are scaled so sum(f)=Nx=length(x).
[f,x0]=freq(x);
if x0~=0,
   error('Before using a_encode, x must be translated so min(x)=0');
elseif Nx<2,
   error('Length(x) must be >1.');
end
f=f*Nx;
N=length(f);
if N>=2^16
	error('Number of symbols exceeds 2^16-1.');
end
% Set word partitions and define y. W =unsigned word size in bits.
y=zeros(Nx,1);
W=44;
% Initialize on the basis of W.
t=2^(W/2)-1;
t1=(t+1)/4;
t2=2*t1;
t3=3*t1;
% cf(1)=0, cf(n)=cf(n-1)+f(n), cf(N+1)=Nx.
M=triu(ones(N,N));
cf=[0 f*M];
% Initialize for encoding.
L=0;
H=t;
if ipr==1 | ipr==2,
    fprintf('cf:\n');
    fprintf('%7.0f%7.0f%7.0f%7.0f%7.0f%7.0f%7.0f%7.0f\n',cf);
    fprintf('\nL,H: %12.1f%12.1f \n',[L,H]);
end
Nbits=0;
Nbtf=0;
y=zeros(Nx,1);
% Encode loop starts here. sym range is [1,N].
for ix=1:Nx;
   sym=x(ix)+1;
% Update range limits L and H.
	if ipr==1 | ipr==2,
	   fprintf('k,x,L,H,cflow:%8d%5d%12d%12d%6d\n',...
               ix,sym-1,L,H,cf(sym))
	end
   R=H-L;
   H=fix(L+(R*cf(sym+1))/Nx-1);
   L=fix(L+(R*cf(sym))/Nx);
% Ensure L < t2 <= H, and (L,H) not in [t1,t3).
   while H<t2 | L>=t2 | (L>=t1 & H<t3),
      if H<t2
         [y,Nbits,Nbtf]=a_output(y,Nbits,0,Nbtf,ipr);
      elseif L>=t2
         [y,Nbits,Nbtf]=a_output(y,Nbits,1,Nbtf,ipr);
         L=L-t2;
         H=H-t2;
      else
         Nbtf=Nbtf+1;
         L=L-t1;
         H=H-t1;
      end
      L=2*L;
      H=2*H+1;
   end
end 
% End of encode loop. Append termination bits.
Nbtf=Nbtf+1;
if L<t1
   [y,Nbits,Nbtf]=a_output(y,Nbits,0,Nbtf,ipr);
elseif L>=t1
   [y,Nbits,Nbtf]=a_output(y,Nbits,1,Nbtf,ipr);
end
% Since Nbits began at zero, Nbits is now the no. bits in y.
Ny=fix((Nbits-1)/8)+1;
y=y(1:Ny);
