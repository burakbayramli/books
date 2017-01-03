function x = istft(d, ftsize, w, h)
% X = istft(D, F, W, H)                   Inverse short-time Fourier transform.
%	Performs overlap-add resynthesis from the short-time Fourier transform 
%	data in D.  Each column of D is taken as the result of an F-point 
%	fft; each successive frame was offset by H points. Data is 
%	hamm-windowed at W pts.  
%       W = 0 gives a rectangular window; W as a vector uses that as window.
% dpwe 1994may24.  Uses built-in 'ifft' etc.
% $Header: /homes/dpwe/public_html/resources/matlab/pvoc/RCS/istft.m,v 1.4 2009/01/07 04:20:00 dpwe Exp $

s = size(d);
%if s(1) != (ftsize/2)+1
%  error('number of rows should be fftsize/2+1')
%end
 
cols = s(2);
xlen = ftsize + (cols-1)*h;
x = zeros(1,xlen);

if length(w) == 1
  if w == 0
    % special case: rectangular window
    win = ones(1,ftsize);
  else
    if rem(w, 2) == 0   % force window to be odd-len
      w = w + 1;
    end
    halflen = (w-1)/2;
    halff = ftsize/2;
    halfwin = 0.5 * ( 1 + cos( pi * (0:halflen)/halflen));
    win = zeros(1, ftsize);
    acthalflen = min(halff, halflen);
    win((halff+1):(halff+acthalflen)) = halfwin(1:acthalflen);
    win((halff+1):-1:(halff-acthalflen+2)) = halfwin(1:acthalflen);
    % 2009-01-06: Make stft-istft loop be identity
    win = 2/3*win;
  end
else
  win = w;
  w = length(win);
end
   
for b = 0:h:(h*(cols-1))
  ft = d(:,1+b/h)';
  ft = [ft, conj(ft([((ftsize/2)):-1:2]))];
  px = real(ifft(ft));
  x((b+1):(b+ftsize)) = x((b+1):(b+ftsize))+px.*win;
end;
