function data=dct1d(data)
% computes the discrete cosine transform of a  vector
data=data(:); % make data a column vector
nrows=length(data);
% Compute weights to multiply DFT coefficients
weight = [1;2*(exp(-i*(1:nrows-1)*pi/(2*nrows))).'];
% Reorder the elements of the columns of x
data = [ data(1:2:end); data(end:-2:2) ];
% Multiply FFT by weights:
data= real(weight.* fft(data));
