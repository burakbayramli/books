
% 
% u = exp(i*k*2*pi*x)
% v = fft(u)
% 
% n = 10
% 
% k = -6 --> v(5) = n
% k = -5 --> v(6) = n
% ---------------------
% k = -4 --> v(7) = n
% k = -3 --> v(8) = n
% k = -2 --> v(9) = n
% k = -1 --> v(10) = n
% k = 0 --> v(1) = n
% k = 1 --> v(2) = n
% k = 2 --> v(3) = n
% k = 3 --> v(4) = n
% k = 4 --> v(5) = n
% ------------------
% k = 5 --> v(6) = n
% k = 6 --> v(7) = n
% 
% so given n, I can get info on floor(n/2)-1 modes.  
%
function v = ftrunc(u,N)

n = length(u);
nyq = floor(n/2)-1;

u = fft(u);
v = zeros(size(u));
for j=1:N+1
    % take care of the positive wave numbers:
    v(j) = u(j);
end
for j=1:N
    v(n-j+1) = u(n-j+1);
end
v = ifft(v);

    


