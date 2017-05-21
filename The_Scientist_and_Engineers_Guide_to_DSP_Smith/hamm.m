function output = hamm(window_size) 
% Input der Funktion: 
% window_size = Fensterlänge 
% window_typ = 'Hamming', 'Hann', 'Blackman', 'Blackman-Harris'

N = window_size; % Fensterlänge 
output = zeros(N, 1); % Init 
if mod(N, 2) == 0 % N gerade 
    m = fix(N / 2); 
    n = m; 
else % N ungerade 
    m = fix(N / 2)+1; 
    n = m-1; 
end 
    
window = 0.54 - 0.46 * cos(2*pi*(0:m) / (N-1)); 

% Ergebnisvektor 
output = transpose([window(1:m),window(n:-1:1)]);
