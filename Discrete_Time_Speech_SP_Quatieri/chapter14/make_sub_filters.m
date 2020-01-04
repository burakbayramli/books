function sub_imp = make_sub_filters(bandwidth);

% make sub-filter bank

% sub_fil = frequency response


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				% specs

	sampling_rate = 10000;
	lfft = 1024;
	lfft2= lfft/2;
	lfft2p1= lfft2+1;

	phase_type = 0;

	resp_lgh = 200;        % response length
	resp_lgh2 = resp_lgh/2;


				% Mel filterbank
	Fs = sampling_rate;
	fft_len = lfft;
	[FB,DCT] = init_filtbank(Fs,fft_len);
	size_FB = size(FB)

	nfilters = size_FB(1);

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
    
    hmel = zeros(nfilters,200);

	for (k = 1:nfilters)

		mel = FB(k,(1:lfft2p1));
		mags = mel(1:lfft2p1);
		mags(lfft:-1: lfft2p1 + 1) = mel(2:lfft2);
        
        
        if phase_type ==  0 		% zero-phase

	czero = real(ifft(mags, lfft));
	phase_zero = zeros(1,lfft);

	hzero = zeros(1,resp_lgh);
	hzero(resp_lgh2+1:resp_lgh) = czero(1:resp_lgh2);
	hzero(1:resp_lgh2) = czero(lfft-(resp_lgh2-1):lfft);
    
	h =  hilbertq(hzero);
    	hmel(k,:) = h;

	h = hzero;
	subplot(312);
	plot(real(h));
	subplot(313)
	mh = abs(fft(hmel(k,:),lfft));
	plot(mh(1:lfft2));
    	pause(0.10);
    
    


elseif phase_type == 1 		% minimum-phase

				% generate min-phase impulse response
	%[cmin, hmin] = rceps(h);
	%phase_min = imag(fft(cmin, lfft));

	cmin = ifft(log(mags+.0000001), lfft);
	cmin(2:lfft2) = 2.0*cmin(2:lfft2);
	cmin(lfft2p1:lfft) = zeros(1,lfft2);
	phase_min = imag(fft(cmin, lfft));
	aux = real(ifft(exp(fft(cmin, lfft))));

	hmin = zeros(1,resp_lgh);
	hmin(resp_lgh2+1:resp_lgh) = aux(1:resp_lgh2);
	hmin(1:resp_lgh2) = aux(lfft-(resp_lgh2-1):lfft);
	subplot(313);

	h =  hilbertq(hmin);
	plot(real(h));
    
    	hmel(k,:) = h;
    
	
end;

	end;


	sub_imp = hmel;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
