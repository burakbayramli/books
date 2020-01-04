
function afwig = WVDist_AF(sig)
% WVDist_AF -- Alias-Free Wigner-Ville Distribution
%  Usage
%    afwig = WVDist_AF(sig)
%  Inputs
%    sig     1-d signal
%  Outputs
%    afwig   complex-valued matrix representing the alias-free
%            Wigner-Ville distribution of zero-extended signal with
%            rows corresponding to frequencies and columns corresponding
%            to times.
%
%  Side Effects
%    Image Plot of the alias-free Wigner-Ville distribution
%
%  See Also
%    WVDist, WVDist_AF, ImagePhasePlane
%
%  References
%   Jechang Jeong and William J. Williams,
%   "Alias-Free Generalized Discrete-Time Time-Frequency Distribution,"
%   IEEE Transactions on Signal Processing, vol. 40, pp. 2757-2765.
%
sig = sig(:);
n   = length(sig);
f   = [zeros(n,1); sig; zeros(n,1)];
afwig = zeros(n, n);
ix  = 0:(n/2-1);
zerosn = zeros(n,1);

for t=1:n,
    tplus    = n + t + ix;
    tminus   = n + t - ix;
    x = zerosn;
    %even indices
    x(1:2:(n)) = f(tplus) .* f(tminus);
    %odd indices
    x(2:2:(n)) = (f(tplus+1).*f(tminus) + f(tplus).*f(tminus-1))/2;
    temp = 2* (fft(x));
    lgh2 = length(temp)/2;
    afwig(1:lgh2, t) = temp(1:lgh2);
end
temp2 = afwig;
clear afwig;
afwig = temp2(1:lgh2,:); 


abstf = abs(afwig);
tfmax = max(max(abstf));
tfmin = min(min(abstf));
colormap(1-gray(256))
imagesc(0:.001:.10, 0:1000:5000, log10(abs(afwig)));axis('xy')
%image(linspace(0,1,n/2),linspace(0,1,n/2),256*(abstf-tfmin)/(tfmax-tfmin));
%image(linspace(0,1,n),linspace(0,1,n),256*(abstf-tfmin)/(tfmax-tfmin));
axis('xy')
title('Alias Free Wigner Distribution');
xlabel('Time')
ylabel('Frequency')

%
% Copyright (c) 1994-5, Shaobing Chen
%
    
    
%   
% Part of WaveLab Version .701
% Built Tuesday, January 30, 1996 8:25:59 PM
% This is Copyrighted Material
% For Copying permissions see COPYING.m
% Comments? e-mail wavelab@playfair.stanford.edu
%   
    

