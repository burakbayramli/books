function [output] = my_sinc_filter_low(order, fc1, fs)
Fc1 = fc1 / fs ; % Normierung auf fn (Fc1 = 0...0.5 fs)
M  = order;
output = zeros(M+1, 1); % Init
window = hamm(M+1);
for i = 0:M 
  if 2 * i == M
    B(i+1) = 2*pi*Fc1; 
  else
    tmp1 = 2*pi*Fc1 * (i-(M/2))
    tmp2 = (i-(M/2))
    B(i+1) = sin(tmp1) / tmp2; 
  end
  B(i+1) = B(i+1) * window(i+1); 
end
B      = B./sum(B);
output = B;
