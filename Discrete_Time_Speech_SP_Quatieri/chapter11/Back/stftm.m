

function [freq] = stftm(x, winlgh); 

% DISPLAY STFTM AND MAKE FREQUENCY TRACK via |STFT| ridge
%
% [freq] = stftm(x, winlgh); 
%
% x = input
% winlgh:  window length
% 
% freq = frequency track
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

			% OPTIONS
freq_est = 3;		% 1: max; 2:weighted avgerage; 3:peaks; 
limit_search = 0;

display = 1;

			% PARAMETERS

sampling_rate = 10000;	% sampling rate
lfft = 1024; 		% FFT length	
lfft2 = lfft/2;
frmlgh = 1; 		% frame interval (in samples)
noverlap = winlgh - frmlgh;
l1 = round((1/(sampling_rate/2))*lfft2);
l1 = 1;
l2 = round((5000/(sampling_rate/2))*lfft2);
search_range = l1:l2;
[limit_min,e] = min(search_range);
limit_min = 0;

%---------------------------------------------------------------------------%

   spec = abs(specgram(x, lfft, sampling_rate, winlgh, noverlap));

   size_spec = size(spec);
   for n = 1:size_spec(2);

	if freq_est == 1;
		[a, b] = max(spec(search_range,n));
		freq_track(n) = b + limit_min;
	elseif freq_est == 2
    freq_track(n)=sum((1:lfft2+1).*(spec(:,n).^2)')/sum(spec(:,n).^2);
	elseif freq_est == 3
		max_array = zeros(1,lfft2);
		peak_locations = pp(spec(search_range,n));
		peak_locations = peak_locations + limit_min;
		max_array(peak_locations) = spec(peak_locations, n);
		maxval = max(max_array);
		index = find(max_array > maxval/2);

		%subplot(311);
		%plot(spec(:,n));
		%hold;
		%plot(max_array, 'r');
		%hold;
		%pause;

		freq_track(n) = index(1);
		if length(index) > 1
			freq_track2(n) = index(2);
		else
			freq_track2(n) = index(1);
		end;
	end;
   end;

	lgh_freq = length(freq_track(:));
	freq = zeros(1,lgh_freq)'; 

if display == 1;

	figure(1);
	clf;
	subplot(311);
	temp = freq;
	plot((1:length(temp))/sampling_rate, temp)

	clear temp;
	temp = [freq_track(:)];
	track_display = (temp/lfft2)*(sampling_rate/2);
	subplot(312)
	plot((1:length(temp))*(frmlgh/sampling_rate), track_display);

	subplot(211);
	plot((1:length(x))/sampling_rate,x)
	xlabel('Time (ms)');
	axis([0 0.10 -2.5 2.5]);
	subplot(212)
	imagesc(0:.001:.10, 0:1000:5000, log10(abs(spec)));axis('xy')
	%imagesc(0:.001:.02, 0:1000:12000, log10(abs(spec)));axis('xy')
	xlabel('Time  (ms)'),ylabel('Frequency  (Hz)');
	pause(1)


	figure(2);
	clf;
	clear temp;
	temp = [freq_track];
	freq = (pi*temp/lfft2);
	subplot(211)
	plot((1:length(freq))*(0.10/length(freq)),freq);
	hold;
	temp = [freq_track2];
	freq = (pi*temp/lfft2);
	plot((1:length(freq))*(0.10/length(freq)),freq);
	%plot(freq);
	hold;
	xlabel('Time (ms)');
	ylabel('Frequency (rad)')
	axis([0 0.10 0.80 2.0]);

	%plot((1:length(temp))*(frmlgh/sampling_rate), freq);

end; 
