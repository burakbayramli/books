function filter_analyse(h, fs, filter_type)
%
% Die Funktion erstellt das Bodediagramm der Impulsantwort des Filters h
% Die reale/en Grenzfrequenz/en fc1 und fc2 [Hz] werden grafisch bestimmt
% 
% Input:
% h           = Impulsantwort des Filters
% fs [Hz]     = Sampling frequency = Abtastfrequenz
% filter_type = 'low' = Tiefpass, 'high' = Hochpass,, 'bandpass' = Bandpass
%               und 'bandreject' = Bandsperre
%
% Output:
% Darstellung der realen Grenzfrequenz 1 und 2 (2 - wenn benutzt) im command window
% Genauigkeit 0.1 Hz
% -------------------------------------------------------------------------

% Anzahl Messwerte
N = 2^15; % 32768 Messwerte
% Frequenzspektrum mit FFT berechnen
H = fft(h, N);
% Berechnung des Amplitudengangs aus dem komplexen Frequenzvektor H:
mag = abs(H(2:(N/2)+1)); % Darstellung von df...fn [Hz]
mag = 20*log10(mag); % Umrechnung in dB
% Auflösung des Spektrums
df = fs/N;
% Nyquistfrequenz
fn = fs/2; 
% Frequenzvektor df...fn [Hz]
f = df : df : fn;
% Berechnung der Phase in Grad
phase = angledim(unwrap(angle(H(2:(N/2)+1))),'radians','degrees');

% Ausgabe des Bodediagramms
figure('Name','Bodediagram - Filter Frequency response','NumberTitle','off')
% Darstellung Betrag 
subplot(211); 
semilogx(f,mag,'b','LineWidth',2);

% 3dB Grenzfrequenz/-en grafisch bestimmen
% Grenzfrequenz = Abfall der Verstärkung um 1/sqrt(2) = - 3.0103 dB
dbwert = 20*log10(1/sqrt(2));

switch lower(filter_type)
    case 'high' % Hochpass
        % Grenzfrequenz suchen
        index1 = find(mag>=dbwert,1,'first');
        disp(sprintf('\n\t %s %8.2f \n',['real filter cutoff frequency fc1: '],f(index1-1)));
        hold on;
        plot(f(index1-1),mag(index1-1),'ro','LineWidth',2); % Grenzfrequenz fc1 markieren
        hold off;
        text_fc1 = sprintf(['real cutoff frequency fc1 = ' num2str(f(index1-1),'%8.2f'),' Hz']);
        if f(index1-1) > 0.5*fn
            legend('filter frequency response',text_fc1,'Location','NorthWest');
        else
            legend('filter frequency response',text_fc1,'Location','SouthEast');
        end
        
    case 'low' % Tiefpass
        % Grenzfrequenz suchen
        index1 = find(mag<=dbwert,1,'first');
        disp(sprintf('\n\t %s %8.2f \n',['real filter cutoff frequency fc1: '],f(index1-1)));
        hold on;
        plot(f(index1-1),mag(index1-1),'ro','LineWidth',2); % Grenzfrequenz fc1 markieren
        hold off;
        text_fc1 = sprintf(['real cutoff frequency fc1 = ' num2str(f(index1-1),'%8.2f'),' Hz']);
        if f(index1-1) > 0.5*fn
            legend('filter frequency response',text_fc1,'Location','SouthWest');
        else
            legend('filter frequency response',text_fc1,'Location','NorthEast');
        end
    
    case 'bandpass'
        % 1. Grenzfrequenz suchen
        index1 = find(mag>=dbwert,1,'first');
        disp(sprintf('\n\t %s %8.2f \n',['real filter cutoff frequency fc1: '],f(index1-1)));
        hold on;
        plot(f(index1-1),mag(index1-1),'ro','LineWidth',2); % Grenzfrequenz fc1 markieren
        if (strcmp(filter_type, 'bandpass')) || (strcmp(filter_type, 'bandreject'))
            % 2. Grenzfrequenz suchen       
            index2 = find(mag(index1:end)<=dbwert,1,'first');
            disp(sprintf('\n\t %s %8.2f \n',['real filter cutoff frequency fc2: '],f(index1+index2-2)));
            plot(f(index1+index2-2),mag(index1+index2-2),'go','LineWidth',2); % Grenzfrequenz fc1 markieren
            hold off;
            text_fc1 = sprintf(['real cutoff frequency fc1 = ' num2str(f(index1-1),'%8.2f'),' Hz']);
            text_fc2 = sprintf(['real cutoff frequency fc2 = ' num2str(f(index1+index2-2),'%8.2f'),' Hz']);        
            legend('filter frequency response',text_fc1, text_fc2,'Location','NorthWest');
        end
    
    case 'bandreject'
        % 1. Grenzfrequenz suchen
        index1 = find(mag<=dbwert,1,'first');
        disp(sprintf('\n\t %s %8.2f \n',['real filter cutoff frequency fc1: '],f(index1-1)));
        hold on;
        plot(f(index1-1),mag(index1-1),'ro','LineWidth',2); % Grenzfrequenz fc1 markieren
        % 2. Grenzfrequenz suchen
        index2 = find(mag(index1:end)>=dbwert,1,'first');
        disp(sprintf('\n\t %s %8.2f \n',['real filter cutoff frequency fc2: '],f(index1+index2-2)));
        plot(f(index1+index2-2),mag(index1+index2-2),'go','LineWidth',2); % Grenzfrequenz fc1 markieren
        hold off;
        text_fc1 = sprintf(['real cutoff frequency fc1 = ' num2str(f(index1-1),'%8.2f'),' Hz']);
        text_fc2 = sprintf(['real cutoff frequency fc2 = ' num2str(f(index1+index2-2),'%8.2f'),' Hz']);        
        legend('filter frequency response',text_fc1, text_fc2,'Location','SouthWest');
    otherwise 
        error(['Unknown filter type in bode diagram: ', filter_type]);    
end % end switch

axis([10^-1 fn -200 20]);
title('Bode Diagram - Filter frequency response','FontSize',18); 
xlabel('Frequency [Hz]','FontSize',14); 
ylabel('Magnitude [dB]','FontSize',14); 
grid on; 
% Darstellung Phase 
subplot(212);
semilogx(f,phase,'LineWidth',2);
hold on;
plot(f(index1-1),phase(index1-1),'ro','LineWidth',2); % Grenzfrequenz fc1 markieren
if (strcmp(filter_type, 'bandpass')) || (strcmp(filter_type, 'bandreject'))
    plot(f(index1+index2-2),phase(index1+index2-2),'go','LineWidth',2); % Grenzfrequenz fc2 markieren
end
hold off;
axis([10^-1 fn 1.01*min(phase) 1000]);
xlabel('Frequency [Hz]','FontSize',14); 
ylabel('Phase [deg]','FontSize',14); 
grid on;

% Lineare Darstellung
figure('Name','Filter Frequency response','NumberTitle','off')
f = 0 : df : fn;
plot(f,abs(H(1:(N/2)+1)),'LineWidth',2);
if (strcmp(filter_type, 'low')) || (strcmp(filter_type, 'high'))
    %nur eine Grenzfrequenz
    if 2*f(index1-1) > fn
        f_end = fn;
    else
        f_end = 2*f(index1-1);
    end
    axis([0 f_end 0 1.2])    
else
    if 2*f(index1+index2-2) > fn
        f_end = fn;
    else
        f_end = 2*f(index1+index2-2);
    end
    axis([0.5*f(index1-1) f_end 0 1.2])
end
title('Filter frequency response','FontSize',18); 
xlabel('Frequency [Hz]','FontSize',14); 
ylabel('Amplitude','FontSize',14); 
grid on;

% Sprungantwort für Tiefpass - Filter
if (strcmp(filter_type, 'low'))
    figure('Name','Filter step response','NumberTitle','off')
    % Länge der Sprungfunktion abhängig von der Länge des Filters 
    if (length(h) < 1024)
        N = 1024;
    else
        N = 2^nextpow2(length(h));
    end
    % Sprungfunktion
    stepfunction = [ones(N,1)]; 
    % Sprungantwort erstellen
    step_response = FFT_Faltung(stepfunction,h);     
    plot(step_response(1:N),'LineWidth',2);
    title('Filter step response','FontSize',18); 
    xlabel('Sample number','FontSize',14); 
    ylabel('Amplitude','FontSize',14);
    grid on;
end