function aq = quant(a,qtype,B);
% Synopsis: aq = quant(a,qtype,B).
% Quantizes a number.
% Input parameters:
% a: the input number, assumed a fraction.
% qtype: 't': truncation, 'r': rounding,
%        'm': magnitude truncation
% B:     number of bits

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

fs = 2^(B-1);
aq = a*fs;
if (qtype == 't'),
   aq = floor(aq)/fs;
elseif (qtype == 'r'),
   aq = round(aq)/fs;
elseif (qtype == 'm'),
   aq = (sign(aq)*floor(abs(aq)))/fs;
else
   error('Unrecognized qtype in QUANT')
end

