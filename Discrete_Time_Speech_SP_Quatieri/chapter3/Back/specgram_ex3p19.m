

function [spec] = specgram_hw3p20(x, winlgh, frmlgh, sampling_rate); 

% specgram_hw3p20(x, winlgh, frmlgh, sampling_rate)
%
% function to compute a spectrogram 
%
% x = input waveform 
% winlgh  = window length in samples
% frmlgh  = frame length in samples
% sampling_rate = samples/sec


			% PARAMETERS

%sampling_rate = 10000;	% sampling rate
lfft = 1024; 		% FFT length	
lfft2 = lfft/2;
%winlgh = 200; 		% (128) window length (in samples)
%frmlgh = 10; 		% frame interval (in samples)
noverlap = winlgh - frmlgh;

% x = x(1:4500);
 x = 2.0*x/max(abs(x));

 etime = length(x)/sampling_rate;

%---------------------------------------------------------------------------%

   	spec = abs(specgram(x, lfft, sampling_rate, winlgh, noverlap));

	subplot(211);
	plot((1:length(x))/sampling_rate,x)
	xlabel('Time (s)');
	title('SPEECH');
	axis([0 etime -2.5 2.5]);
	grid;
	subplot(212)
	imagesc(0:.010:etime, 0:1000:(sampling_rate/2), log10(abs(spec)));axis('xy')
	xlabel('Time  (ms)'),ylabel('Frequency  (Hz)');
	title('SPECTROGRAM');



