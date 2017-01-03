%Chapter 3 Basic Image Processing Operations: CHAPTER3.M
%Written by: Mark S. Nixon

disp ('Welcome to the Chapter3 script')
disp ('This worksheet is the companion to Chapter 3 and is an introduction.')
disp ('The worksheet follows the text directly and allows you to process basic images.')

%Let's initialise the display colour
colormap(gray);

%And let's start with empty memory
clear

disp (' ')
disp ('Let us use the image of an eye.')
%read in the image
eye=imread('eye_orig.jpg','jpg');
%images are stored as integers, so we need to double them for Matlab
%we also need to ensure we have a greyscale, not three colour planes 
eye=double(eye(:,:,1));
disp ('Let us see the top left hand corner')
%print out top left corner
eye(1:10,1:10)
%and display it as an image
%we need to scale for the early section of this chapter only, using a minimum
mini=min(min(eye));
%and a maximum
maxi=max(max(eye));
subplot (1,1,1), imagesc(eye, [mini maxi])
plotedit on, title ('Original image of an eye'), plotedit off
disp ('When you are ready to move on, press RETURN') 
pause;

%Let's show its dimensions
disp (' ')
disp ('The size of this image is')
size(eye)

disp (' ')
disp ('Let us have a look at its histogram. This is a count of all pixels with a ')
disp ('specified brightness level, plotted against brightness level.') 
%We can calculate it by invoking the function histogram.m
hist_eye=histogram(eye);
%now we shall plot it
plot(hist_eye);
plotedit on, title ('Histogram'), xlabel ('Brightness'),ylabel ('Number'),
plotedit off pause

disp (' ')
disp ('The most common point operator replaces each pixel by a scaled version of ')
disp ('the original value. We therefore multiply each pixel by a number (like a ')
disp ('gain), by specifying a function scale which is fed the picture and the ')
disp ('gain, or a level shift (upwards or downwards). The function scale takes ')
disp ('a picture pic and multiplies it by gain -here 1.1 - and adds a level -')
disp ('here 10. After viewing the image press RETURN') 
%We'll call the function scale.m
brighter=scale(eye,1.1,10);
disp ('Let us see the top left hand corner')
brighter(1:10,1:10)
subplot(2,2,1), imagesc(eye, [mini maxi])
plotedit on, title ('Original image'), plotedit off
subplot(2,2,2), plot(histogram(eye))
plotedit on, title ('Original histogram'), plotedit off
subplot(2,2,3), imagesc(brighter, [mini maxi])
plotedit on, title ('Brighter image'), plotedit off
subplot(2,2,4), plot(histogram(brighter))
plotedit on, title ('Brighter histogram'), plotedit off
pause

mult=input('Now choose a new value for the scale (e.g. 0.8) ');
add=input('And a new value for the offset (e.g. 5) ');
nbrighter=scale(eye,mult,add);
disp ('Now see the result')
subplot(2,2,1), imagesc(eye, [mini maxi])
plotedit on, title ('Original image'), plotedit off
subplot(2,2,2), imagesc(nbrighter, [mini maxi])
plotedit on, title ('New Image'), plotedit off
subplot(2,2,3), imagesc(brighter, [mini maxi])
plotedit on, title ('Brighter image'), plotedit off
subplot(2,2,4), plot(histogram(nbrighter))
plotedit on, title ('New histogram'), plotedit off
pause;

disp (' ')
disp ('We shall now take a mathematical function of an image, here the logarithm')
disp ('This is often used to compress components of the Fourier transform, for display')
disp ('Its effect here is to make the numbers so small, you can see very litte!')
%We'll call the function logn.m
log_eye=logn(eye);
log_eye(1:10,1:10)
subplot(1,1,1), imagesc(log_eye);
plotedit on, title ('Logarithm of eye'), plotedit off
pause;

disp (' ')
disp ('For guaranteed viewing, we normalise images to stretch their histogram to ')
disp ('occupy all available grey levels. This is what appears to be done by ')
disp ('Matlabs imagesc function')
%We'll call the function normalise.m
norm_eye=normalise(eye);
disp ('Let us see the top left hand corner')
norm_eye(1:10,1:10)
imagesc(norm_eye);
plotedit on, title ('Normalised image'), plotedit off 
pause;
disp ('The histogram of the new image shows how all the grey levels have been used')
hist_norm=histogram(norm_eye);
plot(hist_norm);
plotedit on, title ('Normalised Histogram'), plotedit off 
pause;

disp (' ')
disp ('The display process optimised for human viewing is called histogram equalisation ')
disp ('Here, unlike intensity normalisation, black and white are not given the same weight')
disp ('This stretches image intensity in a manner particularly suited to human viewing')
%We'll call the function equalise
equa_eye=equalise(eye);
equa_eye(1:10,1:10)
imagesc(equa_eye);
plotedit on, title ('Equalised eye'), plotedit off 
pause;
disp ('The histogram of the new image shows how all the grey levels have been used')
equa_hist=histogram(equa_eye);
plot(equa_hist);
plotedit on, title ('Equalised histogram'), plotedit off 
pause;

disp (' ')
disp ('We shall now consider the thresholding function that sets points above a ')
disp ('specified level to white and all others to black. Using a threshold of 161, we get')
%We'll call the function threshold
thre_eye=threshold(eye,161);
thre_eye(1:10,1:10)
imagesc(thre_eye)
plotedit on, title ('Eye thresholded at 161'), plotedit off 
pause;
thresh=input('Now choose a threshold (e.g. 180) ');
nthre_eye=threshold(eye,thresh);
subplot(1,2,1), imagesc(thre_eye)
plotedit on, title ('Eye thresholded at '), plotedit off 
subplot(1,2,2), imagesc(nthre_eye)
plotedit on, title ('Eye thresholded at new value'), plotedit off 
disp ('If your threshold was bigger than 161, fewer points should be set')
disp ('to white')
pause;

disp (' ')
disp ('We shall now move on to group operators where the new pixel values are the ')
disp ('result of analysing points in a region, rather than point operators which') 
disp ('operate on single points. First, we have a template which describes the region') 
disp ('of interest and we then convolve this by summing up, over the region of the ') 
disp ('template, the result of multiplying  pixel values by the respective template ')
disp ('(weighting) coefficient.')

disp (' ')
disp ('The generalised operator is template convolution, but we shall first look')
disp ('at direct implementation of direct averaging. Here, points in a new image')
disp ('are the average of a 3*3 region, centred at the point of interest, in the')
disp ('old image. The result is shown as an image, and as points.')
%and we'll invoke the function ave33.m
ave33_eye=ave33(eye);
ave33_eye(1:10,1:10)
subplot(1,1,1), imagesc(ave33_eye)
plotedit on, title ('Averaged eye'), plotedit off 
pause;

disp (' ')
disp ('Now we shall use a larger operator, 5*5, using the general function ave')
disp ('This takes a little longer - it has more data to process')
%it's to ave we go
ave55_eye=ave(eye,5);
disp ('There is a greater amount of smoothin with a bigger operator')
disp ('but the noise is much more reduced')
disp ('Notice that the border gets wider as the window size increases. It')
disp ('has been set to black (0) here.') ave55_eye(1:10,1:10)
imagesc(ave55_eye);
plotedit on, title ('Eye averaged by 5*5 operator'), plotedit off 
pause;

disp ('The template for this is one where each point is the reciprocal of the ')
disp ('number of points in it')
ave_temp=ave_template(3)
disp ('This can be used within template convolution as shown by an image equalling the earlier result')
ave_eye=convolve(eye,ave_temp);
imagesc(ave_eye);
pause;

disp (' ')
disp ('In image processing, we often use a Gaussian smoothing filter which can ')
disp ('give a better smoothing performance than direct averaging. In this the ') 
disp ('template coefficients are set according to the Gaussian distribution. ')
disp ('For a 19*19 template, with variance of 4, this is shown as surface.')
disp ('Zoom into the top left corner to view the bell-shaped surface.')
%and the function is gaussian_template
gaussian_temp=gaussian_template(19,4);
surface(gaussian_temp);
pause;

disp (' ')
disp ('For speed, we often use smaller window sizes in convolution. Here we')
disp ('shall use a 5*5 operator, with variance 0.9, to filter the image.')
disp ('The template coefficients are:')
gauss_temp=gaussian_template(5,0.9)
gauss_eye=convolve(eye,gauss_temp);
imagesc(gauss_eye);
plotedit on, title ('Eye averaged by 5*5 Gaussian operator'), plotedit off 
disp ('Notice how features are better preserved in Gaussian smoothing, but with similar noise reduction to direct averaging')
pause;

