%Chapter 2 Images, Sampling and the Frequency Domain CHAPTER2.M

%Written by: Mark S. Nixon

disp ('Welcome to the Chapter2 script')
disp ('This worksheet is the companion to Chapter 2 and is an introduction.')

disp (' ')
disp ('The worksheet follows the text as close as Matlab allows and allows')
disp ('you to process basic images.')
disp ('Let us start with a sampled pulse which is 1 for 5 sampling instants')
disp ('and zero for the last five')

%first, let us empty the memory
clear

for i=1:10
  if i<6 p(i)=1;
         else p(i)=0;
  end
end

disp ('Let us have a look at it')
disp ('When you are ready to move on, press RETURN')
subplot(1,1,1), plot(p)
plotedit on
title ('A pulse of duration 5 samples')
xlabel ('sample number')
ylabel ('amplitude')
plotedit off

%Let's hold awhile so we can view it
pause;

disp ('Note that it is a set of points, as opposed to a continuous function')
disp ('This is because it is a sampled signal, sampled at regular times')
disp (' ')
disp ('We shall calculate its Fourier transform by invoking Matlabs Fourier')
disp ('transform routine. The Fourier transform tells us how much there is of')
disp ('a collection of sinewaves of increasing frequecy in the original')
disp ('signal. It is a bit like the graphic equaliser on some stereo sets:')
disp ('you can see how much low, middle and high frequency content in the')
disp ('channel you are listening to.')

Fp=fft(p);

disp ('This is actually a complex number, Fp=real+j*imaginary so we need')
disp (' to look at its magnitude and phase separately')

plot(abs(Fp))
plotedit on
title ('Fourier transform of pulse')
xlabel ('frequency')
ylabel ('magnitude')
plotedit off
%Let's hold awhile so we can view it
pause;

disp ('This tells us how much of the original frequencies make up the')
disp ('original signal (the proportion in which they are added). The')
disp ('phase gives us timing information.')

plot(angle(Fp))
plotedit on
title ('Fourier transform of pulse')
xlabel ('frequency')
ylabel ('phase')
plotedit off
%Let's hold awhile so we can view it
pause;

disp ('In fact, let us look at the frequencies that made up the original')
disp ('signal. We can see that the is a lot of the first harmonic, but')
disp ('little of the third.')
for i=1:10
h1(i)=real(Fp(1)*exp(complex(0,i*pi/10)));
h2(i)=real(Fp(2)*exp(complex(0,2*i*pi/10)));
h3(i)=real(Fp(3)*exp(complex(0,3*i*pi/10)));
h4(i)=real(Fp(4)*exp(complex(0,4*i*pi/10)));
end
subplot(2,2,1), plot(h1)
plotedit on, title ('First harmonic'), xlabel ('Time'), plotedit off
subplot(2,2,2), plot(h2)
plotedit on, title ('Second harmonic'), xlabel('Time'), plotedit off
subplot(2,2,3), plot(h3)
plotedit on, title ('Third harmonic'), xlabel ('Time'), plotedit off
subplot(2,2,4), plot(h4)
plotedit on, title ('Fourth harmonic'), xlabel('Time'), plotedit off
pause


disp (' ')
disp ('Since we know how much of each sinewave we need, and when they')
disp ('happened then we can reconstruct the original signal. This is')
disp ('the inverse Fourier transform')

newp=ifft(Fp);

subplot (1,1,1)
plot(real(newp))
plotedit on
title ('Reconstructed pulse')
xlabel ('sample number')
ylabel ('amplitude')
plotedit off
%Let's hold awhile so we can view it
pause;

disp ('We are there! We can decompose a signal into its frequency components,')
disp ('and not lose any information in the process. We shall later see how')
disp ('the frequency domain representation allows us to process the image ')
disp ('in ways we cannot see intuitively.')
disp (' ')
disp ('We shall first look at some transform pairs. First, the transform')
disp ('of a sinewave is a single value in the frequency domain, at the')
disp ('at the frequency of the sinwave')

for i=1:200
  p(i)=cos(i*pi/10);
end

plot(p)
plotedit on
title ('10 cylces of a sinewave')
xlabel ('sample number')
ylabel ('amplitude')
plotedit off
pause

disp ('And for its Fourier transform')

Fp=fft(p);
plot(abs(Fp))
plotedit on
title ('Fourier transform of sinewave')
xlabel ('frequency')
ylabel ('magnitude')
plotedit off
pause
disp ('So there is a peak at a frequency of 10 and this fits nicely with')
disp ('the original sinal. There is also a peak at 190 - this is actually')
disp ('a peak at -10, and negative frequency is a mathematical nicety')
disp ('In fact, the spectrum (frequency response) exists between -100 and')
disp ('100 units of frequency')

sigma=10;
disp ('A Gaussian waveform is')
for i=1:200
  g(i)=exp(-((i-100)*(i-100))/(2*sigma*sigma));
end

plot(g)
plotedit on
title ('Gaussian waveform')
xlabel ('sample number')
ylabel ('amplitude')
plotedit off
pause

disp (' ')
disp ('The Fourier transform of a Gaussian is')
Fg=fft(g);
plot(abs(Fg))
plotedit on
title ('Fourier transform of Gaussian')
xlabel ('frequency')
ylabel ('magnitude')
plotedit off
pause
disp ('In fact, as the Gaussian waveform becomes flatter, with increasing')
disp ('variance, the transform becomes thinner. This is the inverse')
disp ('relationship between time and frequency.')
disp (' ')
sigma = input('Currently, the standard deviation=10. Try a new value (e.g.1)');
for i=1:200   newg(i)=exp(-((i-100)^2)/(2*sigma*sigma));
end
disp ('Do you expect the result you get?')
subplot(2,2,1), plot(g)
plotedit on, title ('Original Gaussian'), xlabel ('Time'), plotedit off
subplot(2,2,2), plot(abs(fft(g)))
plotedit on, title ('Original transform'), xlabel('Frequency'), plotedit off
subplot(2,2,3), plot(newg)
plotedit on, title ('Your Gaussian'), xlabel ('Time'), plotedit off
subplot(2,2,4), plot(abs(fft(newg)))
plotedit on, title ('Your transform'), xlabel('Frequency'), plotedit off
pause

disp (' ')
disp ('Let us look at some transforms of images. First, we shall use an')
disp ('image of vertical lines')

pic=[0  100  0  0  100  0  0  0;...
     0  100  0  0  100  0  0  0;...
     0  100  0  0  100  0  0  0;...
     0  100  0  0  100  0  0  0;...
     0  100  0  0  100  0  0  0;...
     0  100  0  0  100  0  0  0;...
     0  100  0  0  100  0  0  0;...
     0  100  0  0  100  0  0  0]

%As a reminder, pixels are addressed in row-column format. 
%Using x for the horizontal axis (a column count), and y for the vertical axis (a row count)
% then picture points are addressed as pic(y,x).

%We'll set the output display to black and white
colormap(gray);

disp ('We shall now view the array as an image')
subplot(1,1,1), imagesc(pic);
plotedit on, title ('Image of vertical line'), plotedit off
 
pause;

disp ('so what is its transform?')

Fpic=fft2(pic);
imagesc(abs(Fpic))
plotedit on, title ('Transform of image of lines'), plotedit off
pause

disp ('This tells us that things only change along the horizontal axis')
disp ('That is of course, perfectly right. It is constant along the')
disp ('vertical axis. If we swap the image round, so it is horizontal')

tpic=transpose(pic);
imagesc(tpic)
plotedit on, title ('Transposed lines'), plotedit off
pause

disp ('Then the transform just shows up vertical change, not horizontal')
Fpic=fft2(tpic);
subplot(1,2,1), imagesc(tpic)
plotedit on, title ('Image of horizontal lines'), plotedit off
subplot(1,2,2), imagesc(abs(Fpic))
plotedit on, title ('Transform of horizontal lines'), plotedit off
pause

disp ('Let us use the earlier image of a square.')
pic=[1  2  3  4  1  1  2  1;...
     2  2  3  2  1  2  2  1;...
     3  1 38 39 37 36  3  1;...
     4  1 45 44 41 42  2  1;...
     1  2 43 44 40 39  1  3;...
     2  1 39 41 42 40  2  1;...
     1  2  1  2  2  3  1  1;...
     1  2  1  3  1  1  4  2]
disp ('Then the transform shows change along the vertical and horizontal axes')

Fpic=fft2(pic);
subplot(1,2,1), imagesc(pic)
plotedit on, title ('Image of square'), plotedit off
subplot(1,2,2), imagesc(abs(Fpic))
plotedit on, title ('Transform of square'), plotedit off
pause

disp (' ')
disp ('Unfortunately, the d.c. component has been placed at the corners of')
disp ('the image, by the Fourier process. It is usually easier to visualise')
disp ('the 2D transform if it is in the middle. We can acieve this by using')
disp ('the function rearrange')
disp ('Now you will see the low frequency at the centre and high frequency')
disp ('at the edges')
Fpic=fft2(rearrange(pic));
subplot(1,2,1), imagesc(pic)
plotedit on, title ('Image of square'), plotedit off
subplot(1,2,2), imagesc(log(abs(Fpic)))
%we take the log to compress the range so that we can see it
plotedit on, title ('Rearranged transform of square'), plotedit off
pause

disp ('Now let us look at a proper image and its Fourier transform')
colormap(gray) 
pic=imread('eye.jpg','jpg');
%images are stored as integers, so we need to double them for Matlab
%we also need to ensure we have a greyscale, not three colour planes 
pic=double(pic(:,:,1));

Fpic=fft2(rearrange(pic));
subplot(1,2,1), imagesc(pic)
plotedit on, title ('Image'), plotedit off
subplot(1,2,2), imagesc(log(abs(Fpic)))
plotedit on, title ('Rearranged Fourier transform'), plotedit off
pause

disp (' ')
disp ('Apart from showing how much frequency content there is in an image')
disp ('the Fourier transform also has some important properties. The first')
disp ('of these is called shift invariance. The magnitude spectrum does')
disp ('not change when the image is shifted. So let us look')

shifted=shift_y(pic,50);
subplot(2,2,1), imagesc(pic)
plotedit on, title ('Image'), plotedit off
subplot(2,2,2), imagesc(shifted)
plotedit on, title ('Image shifted and wrapped'), plotedit off
subplot(2,2,3), imagesc(log(abs(fft2(rearrange(pic)))))
plotedit on, title ('Transformed original'), plotedit off
subplot(2,2,4), imagesc(log(abs(fft2(rearrange(shifted)))))
plotedit on, title ('Transformed shifted'), plotedit off
pause

disp (' ')
disp ('That is not entirely convincing. Let us find the maximum difference')
disp ('between the two images. It is')

%check it out, check it out: why don't we need to rearrange it?
max(max(log(abs(fft2(shifted)))-log(abs(fft2(pic)))))
%you got it: we ain't displaying it!

disp ('That is exactly right, very close to nothing. The difference is in')
disp ('the phase. This is to be expected, as the phase tells us when')
disp ('things happen')

subplot(2,2,1), imagesc(angle(fft2(rearrange(pic))))
plotedit on, title ('Phase of original'), plotedit off
subplot(2,2,2), imagesc(angle(fft2(rearrange(shifted))))
plotedit on, title ('Phase of shifted and wrapped'), plotedit off
subplot(2,2,3), imagesc(log(abs(fft2(rearrange(pic)))))
plotedit on, title ('Transformed original'), plotedit off
subplot(2,2,4), imagesc(log(abs(fft2(rearrange(shifted)))))
plotedit on, title ('Transformed shifted'), plotedit off
pause

disp (' ')
disp ('Let us check the minimum difference in phase (in radians)')
max(max(angle(fft2(shifted))-angle(fft2(pic))))
disp ('That is enough: it is clearly not zero! (It is nearly 2*pi!)')

disp (' ')
disp ('So let us consider rotation. The transpose is a 90 degree rotation')
subplot(2,2,1), imagesc(pic)
plotedit on, title ('Original'), plotedit off
subplot(2,2,2), imagesc(transpose(pic))
plotedit on, title ('Transposed original'), plotedit off
subplot(2,2,3), imagesc(log(abs(fft2(rearrange(pic)))))
plotedit on, title ('Transformed original'), plotedit off
subplot(2,2,4), imagesc(log(abs(fft2(rearrange(transpose(pic))))))
plotedit on, title ('Transformed transposed'), plotedit off
pause

disp ('Yes, the transform rotates when the original image is rotated.')
disp ('The transform also scales (inversely) with distance, but we shall')
disp ('show that here')

disp (' ')
disp ('There are other transforms, an infinite number in fact. That keeps')
disp ('the mathematicians happy! It also gives us the discrete cosine')
disp ('transform (DCT) which is used widely in coding (MPEG, JPEG etc.)')
disp ('It is widely used as the transform information is most compact')
disp ('There is also a Hartley transform, which omits the complex')
disp ('arithmetic of the Fourier transform. The (real) images are:')

subplot(2,2,1), imagesc(pic)
plotedit on, title ('Original'), plotedit off
subplot(2,2,2), imagesc(log(abs(fft2(rearrange(pic)))))
plotedit on, title ('Fourier transform'), plotedit off
d=DCTP(transpose(pic))
subplot(2,2,3), imagesc(log(d-min(min(d))+0.0001))
plotedit on, title ('Discrete cosine transform'), plotedit off
h=Hartley(rearrange(transpose(pic)));
subplot(2,2,4), imagesc(log(h-min(min(h))+0.0001))
plotedit on, title ('Hartley transform'), plotedit off
pause

disp (' ')
disp ('One of the advantages of a fequency domain calculation is access to')
disp ('frequency components. That means we can selectively filter the')
disp ('transform and the invert it to get a filtered image. By filtering')
disp ('out the high frequencies, we keep the low ones. This is a low-pass')
disp ('filter. We shall use a routine that keeps points within a circle')
disp ('centred on the origin.')

subplot (1,1,1)
filtered=low_filter(fft2(rearrange(pic)),5);
imagesc(log(abs(filtered)+0.0001))
plotedit on, title ('Low-pass filtered Fourier transform'), plotedit off
pause

disp ('And when we inverse Fourier transform, we get the low frequencies')
disp ('which give us a blurred image.')

imagesc(abs(ifft2(filtered)));
plotedit on, title ('Low-pass filtered image'), plotedit off
pause

disp (' ')
disp ('Alternatively, we can do high-pass filtering which removes')
disp (' the low frequencies to leave just the high ones. These ')
disp ('are where the image changes quickly, at the edges of features, like')
disp ('the eyes.')

filtered=high_filter(fft2(rearrange(pic)),5);
subplot(1,2,1), imagesc(abs(filtered))
plotedit on, title ('High pass filtered transform'), plotedit off
subplot(1,2,2), imagesc(abs(ifft2(filtered)))
plotedit on, title ('High pass filtered image'), plotedit off
pause


