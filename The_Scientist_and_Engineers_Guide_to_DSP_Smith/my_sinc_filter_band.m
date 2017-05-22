function [output] = my_sinc_filter_band(order, fc1, fc2, fs)
if (fs < 1) || (~isreal(fs)) 
    error(['Wrong sampling frequency fs [Hz]: ', sprintf('%8.1f',fs),' ...must be a real positiv number']);
end  
% Grenzfrequenz 1 überprüfen
Fc1 = fc1 / fs ; % Normierung auf fn (Fc1 = 0...0.5 fs)
if (Fc1 <= 0) || (Fc1 >= 0.5)
    error(['Wrong cutoff frequency fc1 [Hz]: ', sprintf('%8.1f',fc1),' ...choose: 0 < fc1 < ',sprintf('%8.1f',fs/2),' = fs/2']);
end
Fc2 = fc2 / fs ; % Normierung auf fn (Fc2 = 0...0.5 fs)
if ((Fc2 <= 0) || (Fc2 >= 0.5) || (Fc2 <= Fc1))
  error(['Wrong cutoff frequency fc2 [Hz]: ', sprintf('%8.1f',fc2),' ...choose: ',sprintf('%8.1f',fc1) ' < fc2 < ',sprintf('%8.1f',fs/2),' = fs/2']);
end
% Filterordnung überprüfen
M  = order;
if (mod(M, 2) ~= 0) % Filterordnung gerade?
    error(['Wrong Filterorder: ', sprintf('%8.1f',order),' ...must be even']);
end

output = zeros(M+1, 1); % Init
% Fensterfunktion erstellen
window = hamm(M+1);

% 1. Filterstufe: Berechnung der Filterkoeffizienten
for i = 0:M 
    if 2 * i == M   % Multiplikation ist schneller als Division 
        A(i+1) = 2*pi*Fc1; 
    else 
        A(i+1) = sin(2*pi*Fc1 * (i-(M/2))) / (i-(M/2)); 
    end
    A(i+1) = A(i+1) * window(i+1); 
end

% 2. Filterstufe: Berechnung der Filterkoeffizienten
for i = 0:M 
    if 2 * i == M 
        B(i+1) = 2*pi*Fc2; 
    else 
        B(i+1) = sin(2*pi*Fc2 * (i-(M/2))) / (i-(M/2)); 
    end
    B(i+1) = B(i+1) * window(i+1); 
end

% Verstärkungsfaktor des Filters auf 1 normieren
A = A./sum(A);
B = B./sum(B);
% Tiefpass in Hochpass durch Inversion des Spektrums wandeln
B          = - B;
B((M/2)+1) = B((M/2)+1) + 1;
output     = A + B;
% Filterkoeef. übergeben
% Bandsperre in Bandpass durch Inversion des Spektrums wandeln
output          = - output;
output((M/2)+1) = output((M/2)+1) + 1;

