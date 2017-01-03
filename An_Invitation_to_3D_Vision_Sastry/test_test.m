oim = pgmRead('einstein.pgm');
tic; corrDn(oim,[1 1; 1 1]/4,'reflect1',[2 2]); time = toc;
imSubSample = min(max(floor(log2(time)/2+3),0),2);
im = blurDn(oim, imSubSample,'qmf9');
clear oim;
clf; showIm(im, 'auto2', 'auto', 'im');

binom5 = binomialFilter(5);
lo_filt = binom5*binom5';
% convolve with a low pass filter
blurred = rconv2(im,lo_filt);
subplot(1,2,1); showIm(im, 'auto2', 'auto', 'im');
subplot(1,2,2); showIm(blurred, 'auto2', 'auto', 'blurred');
