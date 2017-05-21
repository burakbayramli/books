function [output] = my_sinc_filter_low(order, fc1, fs, analyse_plot)
  
if (fs < 1) || (~isreal(fs)) 
    error(['Wrong sampling frequency fs [Hz]: ', sprintf('%8.1f',fs),' ...must be a real positiv number']);
end  
Fc1 = fc1 / fs ; % Normierung auf fn (Fc1 = 0...0.5 fs)
if (Fc1 <= 0) || (Fc1 >= 0.5)
    error(['Wrong cutoff frequency fc1 [Hz]: ', sprintf('%8.1f',fc1),' ...choose: 0 < fc1 < ',sprintf('%8.1f',fs/2),' = fs/2']);
end
M  = order;
if (mod(M, 2) ~= 0) % Filterordnung gerade?
    error(['Wrong Filterorder: ', sprintf('%8.1f',order),' ...must be even']);
end
        
output = zeros(M+1, 1); % Init
window = hamm(M+1);

for i = 0:M 
    if 2 * i == M 
        B(i+1) = 2*pi*Fc1; 
    else 
        B(i+1) = sin(2*pi*Fc1 * (i-(M/2))) / (i-(M/2)); 
    end
    B(i+1) = B(i+1) * window(i+1); 
end
B      = B./sum(B);
output = B;
