function output = Fenster(window_size) 
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
    
switch lower(window_type) 
   case 'hamming' 
      window = 0.54 - 0.46 * cos(2*pi*(0:m) / (N-1)); 
   case 'hann' 
      window = 0.50 - 0.50 * cos(2*pi*(0:m) / (N-1)); 
   case 'blackman' 
      window = 0.42 - 0.50 * cos(2*pi*(0:m) / (N-1)) +  ... 
                   0.08 * cos(4*pi* (0:m) / (N-1));
   case 'blackmanharris' 
      window = 0.35875 - 0.48829 * cos(2*pi*(0:m) / (N-1)) +  ... 
                   0.14128 * cos(4*pi* (0:m) / (N-1)) - ...
                   0.01168 * cos(6*pi* (0:m) / (N-1));
   otherwise 
      error(['Unknown window type: ', window_type]); 
end 

% Ergebnisvektor 
output = transpose([window(1:m),window(n:-1:1)]);
