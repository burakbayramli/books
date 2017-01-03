function [b,a]=iir_weights(z,p)
% [b,a]=iir_weights(z,p)
%
% [b,a]=weights of IIR digital filter in cascade form with 
% 2-pole sections; that is, H(z)=H(z,1)H(z,2)...H(z,L/2)'
% where L=# poles.
%
% Inputs: z =vector of L/2 analog zeros, i.e., zeros of H(s).
%            Exception: if z=0 (scalar), H(s) has no zeros.
%         p =vector of L/2 analog poles, i.e., poles of H(s).
%
% Outputs: b =digital numerator weight array, dimensioned (L/2 x 3).
%          a =digital denominator weight array, dimensioned the same.
z=[row_vec(z)]';
p=[row_vec(p)]';
L2=length(p);
% Check for errors.
if(length(z)~=0 & length(z)~=L2),
   error('Input z must be zero or have the same length as p.');
end
% Bilinear transformation of H(s) to H(z) using z and p vectors.
a(:,1)=(1-p).*(1-conj(p));
a(:,2)=-2*(1-p.*conj(p));
a(:,3)=(1+p).*(1+conj(p));
if(z==0),
   b=p.*conj(p)*[1 2 1];
else
   b(:,1)=z.*conj(z).*(1-z).*(1-conj(z));
   b(:,2)=-2*z.*conj(z).*(1-z.*conj(z));
   b(:,3)=z.*conj(z).*(1+z).*(1+conj(z));
end
% Scale H(z) so a(:,1)=1.
s=a(:,1)*ones(1,3);
a=a./s;
b=b./s;
