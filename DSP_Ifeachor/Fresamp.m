function Fresamp
%Design arbitrary FIR filter by frequency sampling method

n=15;
m = [1 1 1 1 0.5571 0.0841 0 0];
f = [0 1/7 2/7 3/7 4/7 5/7 6/7 1];
window = [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]; % n+1 elements long
b = fir2(n,f,m,window); % get filter coefficients

%Output the results
disp('The coefficients are:'); disp(b);
[h,w] = freqz(b,1,128);
plot(f,m,w/pi,abs(h)); title('Desired and Actual Frequency Responses');
