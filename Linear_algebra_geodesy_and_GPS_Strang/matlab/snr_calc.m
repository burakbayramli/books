% please run proj6par4.m before running this

% SNR eq. for the K signal. 

SNR_K= Ns(2,:)./(sqrt(Ns(2,:)+Nray(K_lay_start_ind:K_lay_end_ind-1)+Nb(K_lay_start_ind:K_lay_end_ind-1)));
SNR_K_db = 20*log10(SNR_K);

% SNR eq. considering Lord Rayleigh's signal responce
SNR_ray = Nray./(sqrt(Nray+Nb));
SNR_ray_db = 20*log10(SNR_ray);

figure
subplot(2,1,1);
plot(alt_arr(K_lay_start_ind:K_lay_end_ind-1)./1000, SNR_K)
xlabel('Altitude [km]')
ylabel('SNR_{K}')
axis tight; grid
title('SNR K layer')
subplot(2,1,2);
plot(alt_arr(K_lay_start_ind:K_lay_end_ind-1)./1000, SNR_K_db)
xlabel('Altitude [km]')
ylabel('SNR_{K}in db')
axis tight; grid

figure
subplot(2,1,1);
plot(alt_arr./1000, SNR_ray)
xlabel('Altitude [km]')
ylabel('SNR_{Ray}')
axis tight; grid
title('SNR Rayleigh')
subplot(2,1,2);
plot(alt_arr./1000, SNR_ray_db)
xlabel('Altitude [km]')
ylabel('SNR_{Ray} in db')
axis tight; grid
