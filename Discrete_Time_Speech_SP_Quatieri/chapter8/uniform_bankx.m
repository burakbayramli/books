
function y = uniform_bankx(spacing, prototype_filter, plot_factor, sampling_rate, splot)
% UNIFORM_BANKX: design a set of uniformly spaced complex bandpass filters
%
%       y = uniform_bankx(spacing, prototype_filter, plot_factor, sampling_rate)
%
%       spacing is the filter spacing in Hz
%       prototype_filter is the "analysis filter"
% 	plot_factor is a normalization factor for plotting 
%       sampling_rate
% 	y is output

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


						% set up some parameters


if (splot == 1)
	clf;
	subplot(211);
	test = [plot_factor, zeros(1,511)];
	plot (test);
	pause(1);
	hold on;
end;

	number_filters = (sampling_rate/spacing);
	lengthp = length(prototype_filter);
						% do "end" filters
							
		n = number_filters
	        b =  prototype_filter;
		length_fil = length(b)
		%delay = 1; 
		delay = (length_fil/2) + 1.0; 
		test = b;
		y(:, 1) = b.';            

if (splot == 1)
		ft = fft(b, 1024);
		mft = abs(ft(1:512));
		plot(mft(1:512));
		pause(1);
end;

	for n = 2:number_filters

						% modulate prototype_filter 

		modfreq = 2.0*pi*(n-1)*(spacing/sampling_rate);
	        b =  prototype_filter.*exp( j * ( modfreq*([1:lengthp]-delay)));
		test = test + b;

		y(:, n) = b.';            

						% plot
	if (splot == 1)
		ft = fft(b, 1024);
		mft = abs(ft(1:512));
		plot(mft(1:512));
		title('FREQUENCY RESPONSE');
		xlabel('Frequency (DFT samples)');
		ylabel('Amplitude');
		axis([0 512 0 120])
		pause(1);
	end;

	end

	maxval = max(abs(test));
	test = test/maxval;
	y = y/maxval;

	%find( real(test) > 0);
	[a,b] = max(real(test))
	[a,b] = max(imag(test))

if (splot ==1)
	hold off;
end;


if splot == 2;
	clf;
	subplot(311);
	plot(real(test));
	title('REAL PART OF COMPOSITE IMPULSE RESPONSE');
	xlabel('Time (samples)')
	ylabel('Amplitude');
	subplot(312);
	plot(imag(test));
	title('IMAG PART OF COMPOSITE IMPULSE RESPONSE');
	xlabel('Time (samples)');
	ylabel('Amplitude');
	subplot(313);
	ft = fft(test, 1024);
	plot(abs(ft(1:512)));
	axis([0 512 0 2])
	title('FREQUENCY RESPONSE');
	xlabel('Frequency (DFT samples)');
	ylabel('Amplitude');
	pause(1);
end; 
		
