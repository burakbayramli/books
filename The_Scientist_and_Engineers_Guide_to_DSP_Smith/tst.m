pkg load signal

x = 1:100;

order = 32
fc1 = 0.1
fc2 = 0.3
fs = 20
filter_type = 'low'
window_type = 'hamming'
analyse_plot = 'n'
f1 = window_sinc_filter(order, fc1, fc2, fs, filter_type, window_type, analyse_plot);

size(f1)
f1

f2 = fir1(32, fc1, "low", "hamming", "noscale");
size(f2)
f2

#help fir1

res1 = conv(x, f1);

res2 = conv(x, f2);

sum(res1)

sum(res2)

