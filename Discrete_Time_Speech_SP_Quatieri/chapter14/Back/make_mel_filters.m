
function mel_fr = make_mel_filters(bandwidth);

% make mel-filter bank

% mel_fr = frequency response

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				% specs

	sampling_rate = 2*bandwidth;
	lfft = 1024;
	lfft2= lfft/2;
	lfft2p1= lfft2+1;

	resp_lgh = 200;        % response length
	resp_lgh2 = resp_lgh/2;


				% Mel filterbank
	Fs = sampling_rate;
	fft_len = lfft;
	[FB,DCT] = init_filtbank(Fs,fft_len);
	size_FB = size(FB)
	nfilters = size_FB(1);

	mel_fr = FB;

	figure(1);
	subplot(311);
	scale0 = sum(FB(1,(1:lfft2p1)));
	scalen = max(FB(1,(1:lfft2p1))/scale0);
	plot(bandwidth*(1:lfft2p1)/lfft2p1,FB(1,(1:lfft2p1))/scale0/scalen);
	hold;
	for (k = 2:nfilters)
		%plot(FB(k,(1:lfft2p1)));
		scalex = sum(FB(k,(1:lfft2p1)));
		plot(bandwidth*(1:lfft2p1)/lfft2p1,(FB(k,(1:lfft2p1)))/scalex/scalen);
    end;
    
	hold;
	axis([0 bandwidth 0 1.25]);
	xlabel ('Frequency (Hz)');
	ylabel ('Amplitude');
	grid;
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
