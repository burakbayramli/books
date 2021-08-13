function vrot=givapp(c,s,vin,k)
%GIVAPP apply a sequence of Givens rotations
%   input
%          c, s      vectors of length k-1 defining rotations
%          vin       vector of length k to which rotations are applied
%          k         k-1 = number of rotations
%   output
%          tranformed vector after rotations are applied
% called by gmres_r
%   IFISS function: HCE; 15 March 2005.

% 
%  C. T. Kelley, July 10, 1994
%Copyright 1994 C. T. Kelley.  
%Reproduced and distributed with permission of the copyright holder.
%
% This code comes with no guarantee or warranty of any kind.
%
%  function vrot=givapp(c, s, vin, k)
%
vrot=vin;
for i=1:k
    w1=c(i)*vrot(i)-s(i)*vrot(i+1);        % Change on next line, 6/3/97
    w2=s(i)*vrot(i)+conj(c(i))*vrot(i+1);  % w2=s(i)*vrot(i)+c(i)*vrot(i+1);
    vrot(i:i+1)=[w1,w2];
end
