function[T,L] = labreadlab(F,K,SR)
% [T,L] = labreadlab(F,K,SR)   Read a (speech transcription) labfile
%     F is the name of a TIMIT-format label file.  K is a symbol mapping 
%     matrix from labreadkey.m.  Return T as a Nx2 matrix of start, end 
%     times (in seconds), and L as a vector of the associated label
%     (indexed into K).  SR is optional sampling rate of TIMIT times; 
%     default is 16000.
% 2001-03-27 dpwe@ee.columbia.edu

if nargin < 3
  SR = 16000;
end

fid = fopen(F,'rt');

[nsym,symlen] = size(K);

eof = 0;

T = [];
L = [];

while eof == 0

  line = fgetl(fid);
  
  [times,count,err,next] = sscanf(line, '%f', 2);
  times = times';
  sym = sscanf(line(next:end), '%s', 1); 

  % disp([num2str(times),' ', sym]);

  if length(times) > 0 & length(sym) > 0

    ls = length(sym);
    if ls < symlen
      sym = [sym, ' '*ones(1,symlen - ls)];
    end

    % Look up the symbol
    symix = find(sum( (K == ones(nsym,1)*sym(1:symlen))' ) == symlen);
    if length(symix) == 0
      disp(['Warn: symbol ', sym,' (at ',num2str(times(1)),') not known']);
      symix = 0;   % 0 means unknown
    end
    T = [T; times/SR];
    L = [L; symix(1)];

  else
    % Warn unless it's a comment
    if line(1) ~= '#'
      disp(['Skipped line:', line]);
    end
    %eof = 1;
  end
  eof = feof(fid);

end

fclose(fid);
