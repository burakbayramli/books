function z = circonv(x,y);
% Synopsis: z = circonv(x,y).
% Performs circular convolution by means of linear convolution.
% Input parameters:
% x, y; the two vectors to be convolved.
% Output parameter:
% z: the result of the circular convolution.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N = length(x);
if (length(y) ~= N),
   error('Vectors of unequal lengths in circonv');
end
z = conv(reshape(x,1,N),reshape(y,1,N));
z = z(1:N) + [z(N+1:2*N-1),0];

