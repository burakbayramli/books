

function [spec] = specgram_hw6p5(x); 


			% PARAMETERS

sampling_rate = 10000;	% sampling rate
lfft = 1024; 		% FFT length	
lfft2 = lfft/2;
winlgh = 200; 		% (128) window length (in samples)
frmlgh = 10; 		% frame interval (in samples)
noverlap = winlgh - frmlgh;

x = x(1:4500);

%---------------------------------------------------------------------------%

   	spec = abs(specgram(x, lfft, sampling_rate, winlgh, noverlap));

	subplot(211);
	plot((1:length(x))/sampling_rate,x)
	xlabel('Time (s)');
	title('SPEECH');
	axis([0 0.45 -2.5 2.5]);
	subplot(212)
	imagesc(0:.010:.45, 0:1000:5000, log10(abs(spec)));axis('xy')
	xlabel('Time  (ms)'),ylabel('Frequency  (Hz)');
	title('SPECTROGRAM');



