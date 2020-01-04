
% SENERSEP Smooth ENERgy SEParation algorithm, amplitude and frequency tracking
%         [am, fm, energy] = amfm_sep(sig,f1,f2), applies the energy operator 
%         on the sig and its first derivative in order to compute the
%         the sig's envelope and  instantaneous frequency. f1 and f2 
%         determine the amount of smoothing for the energy signals (f1 for the
%         energy of the signal and f2 for the energy of the difference of 
%         the signal): for f1 = 3 the filter (1,2,1) is applied three times
%         on the energy output, for f1=f2=0 no smoothing is done.
%

function [am, fm, energy]= amfm_sep(sig,flag1,flag2);

samples = length(sig);

sh_l_sig = sig(2:samples);
sh_l_sig(samples) = 2*sig(samples);
sh_r_sig(1) = 2*sig(1);
sh_r_sig(2:samples) = sig(1:samples-1);

% ENERGY OF THE SIGNAL
eo_sig = sig.*sig - sh_l_sig.*sh_r_sig;

% SMOOTH ENEGRY SIGNAL
for i=1:flag1;
eo_sig = conv(eo_sig,[0.25 0.5 0.25]);
eo_sig = eo_sig(2:samples+1);
end;

eo = eo_sig;

sig_diff1 = (sig - sh_r_sig) ;
sig_diff2 = (sh_l_sig - sig) ;

sh_l_sig_diff1 = sig_diff1(2:samples);
sh_l_sig_diff1(samples) = 2*sig_diff1(samples);
sh_r_sig_diff1(1) = 2*sig_diff1(1);
sh_r_sig_diff1(2:samples) = sig_diff1(1:samples-1);

sh_l_sig_diff2 = sig_diff2(2:samples);
sh_l_sig_diff2(samples) = 2*sig_diff2(samples);
sh_r_sig_diff2(1) = 2*sig_diff2(1);
sh_r_sig_diff2(2:samples) = sig_diff2(1:samples-1);


% ENERGY OF THE BACKWARD DIFFERENCE OF THE SIGNAL
temp1 = sig_diff1.*sig_diff1;
eo_sig_diff1 = temp1 - sh_l_sig_diff1.*sh_r_sig_diff1;

% ENERGY OF THE FORWARD DIFFERENCE OF THE SIGNAL
temp2 = sig_diff2.*sig_diff2;
eo_sig_diff2 = temp2 - sh_l_sig_diff2.*sh_r_sig_diff2;

% SMOOTH ENERGY SIGNALS OF THE SIG DIFFERENCES
for i=1:flag2;
eo_sig_diff1 = conv(eo_sig_diff1,[0.25 0.5 0.25]);
eo_sig_diff1 = eo_sig_diff1(2:samples+1);
eo_sig_diff2 = conv(eo_sig_diff2,[0.25 0.5 0.25]);
eo_sig_diff2 = eo_sig_diff2(2:samples+1);
end;


cos_ifreq = 1 - (eo_sig_diff1 + eo_sig_diff2)./(4*eo_sig);
sin_ifreq = sqrt(abs(1 - cos_ifreq.*cos_ifreq)) ;

% SEPARATION ALGORITHM 1   
ifreq = atan2(sin_ifreq, cos_ifreq);
env = sqrt(abs(eo_sig./(1 - cos_ifreq.*cos_ifreq)));


% BOUNDARY CONDITIONS
bounds = max(flag1,flag2)+2;
for i = 1:bounds
ifreq(i)= ifreq(bounds+1);
env(i) = env(bounds+1);
eo(i) = eo(bounds+1);
ifreq(samples-i+1)=ifreq(samples-bounds);
env(samples-i+1)=env(samples-bounds);
eo(samples-i+1)=eo(samples-bounds);
end;

am = env;
fm = ifreq;
energy = eo;
