function xw=windowing(x,w)
N= length(x);
if nargin<2|w=='rt'|isempty(w), xw=x;
 elseif w=='bt', xw= x.*bartlett(N)';
  elseif w=='bk', xw= x.*blackman(N)'; 
  elseif w=='hm', xw= x.*hamming(N)';
end     
