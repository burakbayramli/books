function [output] = my_sinc_filter_band(order, fc1, fc2, fs)
Fc1 = fc1 / fs ; % Normierung auf fn (Fc1 = 0...0.5 fs)
Fc2 = fc2 / fs ; % Normierung auf fn (Fc2 = 0...0.5 fs)
M  = order;
output = zeros(M+1, 1); % Init
window = hamm(M+1);
for i = 0:M 
    if 2 * i == M  
        A(i+1) = 2*pi*Fc1; 
    else 
        A(i+1) = sin(2*pi*Fc1 * (i-(M/2))) / (i-(M/2)); 
    end
    A(i+1) = A(i+1) * window(i+1); 
end
for i = 0:M 
    if 2 * i == M 
        B(i+1) = 2*pi*Fc2; 
    else 
        B(i+1) = sin(2*pi*Fc2 * (i-(M/2))) / (i-(M/2)); 
    end
    B(i+1) = B(i+1) * window(i+1); 
end
A = A./sum(A);
B = B./sum(B);
B          = - B;
B((M/2)+1) = B((M/2)+1) + 1;
output     = A + B;
output          = - output;
output((M/2)+1) = output((M/2)+1) + 1;

