function [output] = my_sinc_filter_high(order, fc1, fs)
Fc1 = fc1 / fs ; 
M  = order;
output = zeros(M+1, 1);
window = hamm(M+1);
for i = 0:M 
    if 2 * i == M 
        B(i+1) = 2*pi*Fc1; 
    else
        B(i+1) = sin(2*pi*Fc1 * (i-(M/2))) / (i-(M/2)); 
    end
    B(i+1) = B(i+1) * window(i+1); 
end                
B = B./sum(B);
output      = - B;
output((M/2)+1) = output((M/2)+1) + 1;

