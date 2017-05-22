%% Nest Interview
% Spencer Barton
% 9/13/14

clear all;
close all;
pkg load signal;

FILENAME = 'accel.csv';
Fs = 20; % Hz
STEPS = 20;

%% Basic peek at the data - plot x, y, z
% It appears that the data is normalized to 1 for gravity

data = csvread(FILENAME); % x,y,z

nSamples = size(data,1);
time = linspace(0, nSamples / Fs, nSamples);

%% Plot 2-norm of x,y,z
% This will likely be a much more useful measurment because the
% accelermeter can be rotated.

normData = sqrt(sum(data.^2, 2));

%% Plot freq to get a sense of frequencies in play
% Very string DC value - expected because gravity is always constant
% May want to remove effects of gravity

%% Subtract effects of gravity and replot freq
% This is nicer to look at

% Subtract gavity
gravNormData = normData-1;

%% BPF
% Thinking about the fundamental step frequency
% Around .17*2*pi
% About 1.3 m/s walking speed, .67/.76 m step size (women/men)
% About 1.9 step/sec (women) or 1.7 step/sec (men)
% Since sampled at 20Hz, -> looking for about 1.7 - 1.9 Hz


%% Peak detection with band pass filtered data
% Step starts with upward motion so this will appear as neg accel in data
% Want to look at pair distance for min followed by max
% There is a huge difference between distances

b = fir1(32, [.1 .3]);
size(b)
b
bpData = filter(b,1,gravNormData);
size(bpData)
bpData
figure; plot(time, bpData);
print ('out3.png')

% Find max peaks
minPeakDist = floor(.67 * Fs / 2); % half of estimated step time distance

minPeakDist

