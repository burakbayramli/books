function [b,a]=bilin(d,c)
% [b,a]=bilin_weights(d,c)
%
% Bilinear trans. of analog weights to digital weights.
% [b,a]=weights of IIR digital filter in cascade form with 
% 2-pole sections; H(z)=H(z,1)H(z,2)...H(z,L/2) where
% L=# poles and each section is a ratio of quadratics.
%
% Inputs: d =numerator weights of analog filter in 1-pole
%            sections. d is dimensioned (L/2 x 2).
%         c =denominator weights, dimensioned same as d.
%            [d,c] are combined with [d',c'] and transformed
%            to produce the L/2 2-pole digital filter sections.
%
% Outputs: b =digital numerator weights, dimensioned (L/2 x 3).
%          a =digital denominator weights, dimensioned the same.
[L2,ncd]=size(d);
[nr,ncc]=size(c);
% Check for errors.
if(nr~=L2 | ncd~=2 | ncc~=2),
   error('Inputs d and c must both be L/2 x 2 arrays.');
end
% Bilinear transformation of H(s) to H(z) using z and p vectors.
a(:,1)=abs(c(:,1)+c(:,2)).^2;
if(min(a(:,1))==0),
   error('"c" should not have a row of zeros.');
end
a(:,2)=2*real((c(:,1)+c(:,2)).*conj(c(:,2)-c(:,1)));
a(:,3)=abs(c(:,2)-c(:,1)).^2;
b(:,1)=abs(d(:,1)+d(:,2)).^2;
b(:,2)=2*real((d(:,1)+d(:,2)).*conj(d(:,2)-d(:,1)));
b(:,3)=abs(d(:,2)-d(:,1)).^2;
% Scale H(z) so a(:,1)=1.
sa=a(:,1)*ones(1,3);
a=a./sa;
b=b./sa;