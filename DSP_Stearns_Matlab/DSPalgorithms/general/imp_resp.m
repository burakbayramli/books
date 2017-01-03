function h=imp_resp(b,a,K);
% h=imp_resp(b,a,K)
%
% Impulse response of a linear system in direct or cascade form.
% Inputs:
%   b,a =numerator and denominator weights of H(z). The weights
%        on the nth row are for the nth section in cascade.
%   K   =desired length of impulse response.
% Output:
%   h(1:K) =impulse response (row vector).
% See also: filter, filters

% Check for errors.
[Na,Ma]=size(a);
[Nb,Mb]=size(b);
if(Na~=Nb);
   error('b and a vectors must have the same number of rows.');
elseif(min(abs(a(:,1)))==0),
   error('a(1) cannot be zero in any filter section.');
end

% Note: we could do this using the following 2 expressions:
% x=[1,zeros(1,K-1)];
% h=filters(b,a,x);
% These in turn use function "deconv", which is much faster.
% The code below is used only to demonstrate specifically
% the division of B(z) by A(z) followed by FFT filtering.
% Polynomial division is done explicitly in function "poly_div".

N=2^nextpow2(2*K);
h=[1,zeros(1,N-1)];
for i=1:Na,                         % For each section in cascade,
   c=poly_div(b(i,:),a(i,:),K);     % Divide B(z) by A(z).
   h=ifft(fft(h,N).*fft(c,N));      % Filter in the frequency domain.
end
h=real(h(1:K));