function [H,Ht]=entropy(x);
% function [H,Ht]=entropy(x).
%
% x =any INTEGER vector or array. (Real values are rounded.)
%
% H = Average entropy in vector x in bits per symbol.
% Ht = Total entropy in vector x = H * length(x).
% If x is a matrix, H and Ht are vectors with each element
% equal to the entropy of the corresponding column of x.
% See also: freq
[Lx,Ncols]=size(x);
if Lx==1 & Ncols==1,
	   error('Input cannot be a scalar.');
elseif Lx==1 & Ncols>1,
	   x=x';
		[Lx,Ncols]=size(x);
end
H=zeros(1,Ncols);
Ht=zeros(1,Ncols);
for col=1:Ncols,
    f=freq(x(:,col));
    H(col)=-(f)*log2(max(1/Lx,f'));
    Ht(col)=H(col)*Lx;
end