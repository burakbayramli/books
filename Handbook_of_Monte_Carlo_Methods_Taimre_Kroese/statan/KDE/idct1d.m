function out = idct1d(data)
% computes the inverse discrete cosine transform of a vector
data=data(:); % make data a column vector
nrows=length(data);
% Compute weights
weights = exp(i*(0:nrows-1)*pi/(2*nrows)).';
data = real(ifft(weights.*data));
% Reorder elements
out = zeros(nrows,1);
out(1:2:nrows) = data(1:nrows/2);
out(2:2:nrows) = data(nrows:-1:nrows/2+1);
