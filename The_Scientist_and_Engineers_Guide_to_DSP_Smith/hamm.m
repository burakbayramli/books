function output = hamm(window_size) 
N = window_size;
output = zeros(N, 1);
if mod(N, 2) == 0 
    m = fix(N / 2); 
    n = m; 
else
    m = fix(N / 2)+1; 
    n = m-1; 
end     
window = 0.54 - 0.46 * cos(2*pi*(0:m) / (N-1));
output = transpose([window(1:m),window(n:-1:1)]);
