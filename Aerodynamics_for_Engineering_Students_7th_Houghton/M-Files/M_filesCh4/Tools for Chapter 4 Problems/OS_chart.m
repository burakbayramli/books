% OS_chart
%
M=2;
wv = asin(1/M)*(180/pi)+[0:1000]*0.001*(90-asin(1/M)*(180/pi));
turn = OS_mw(M, wv);
plot(turn, wv)
%
tn = [0:0.25:24];
for i = 1:length(tn)
  wave(i) = OS_mt(M, tn(i));
  wavestr(i) = OS_mt_str(M, tn(i));
end
hold on
plot(tn, wave, '--r')
plot(tn, wavestr, '--g')
hold off
