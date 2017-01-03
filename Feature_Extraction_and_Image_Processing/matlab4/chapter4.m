%Chapter 4 Low-Level Feature Extraction and Edge Detection: CHAPTER4.M
%Written by: Mark S. Nixon

disp ('Welcome to the Chapter4 script')
disp ('This worksheet is the companion to Chapter 4 and is an introduction.')
disp ('The worksheet follows the text directly and allows you to process basic images.')

%Let's first empty the memory
clear

%Let's initialise the display colour
colormap(gray);

disp (' ')
disp ('Let us use the image of an eye.')
disp ('When you are ready to move on, press RETURN')
%read in the image
eye=imread('eye.jpg','jpg');
%images are stored as integers, so we need to double them for Matlab
%we also need to ensure we have a greyscale, not three colour planes
eye=double(eye(:,:,1));
%so let's display it
subplot(1,1,1), imagesc(eye);
plotedit on, title ('Image of an eye'), plotedit off
pause;

disp(' ')
disp ('We detect vertical edges by differencing horizontally adjacent')
disp ('points. Note how clearly the edge of the face appears')
%so we'll call the edge_x operator. 
vertical=edge_x(eye);
imagesc(vertical);
plotedit on, title ('Vertical edges of an eye'), plotedit off 
pause;

disp (' ')
disp ('We detect horizontal edges by differencing vertically adjacent points')
disp ('Notice how the side of the face now disappears, wheras the')
disp ('eyebrows appear')
%so we'll call the edge_y operator
subplot(1,2,2), horizontal=edge_y(eye);
subplot(1,2,1), imagesc(horizontal);
plotedit on, title ('Horizontal edges of an eye'), plotedit off 
subplot(1,2,2), imagesc(vertical);
plotedit on, title ('Vertical edges of an eye'), plotedit off 
pause;

disp (' ')
disp ('We detect all edges by combining the vertical and horizontal edges')
%so we'll call the edge operator
all_edges=edge(eye);
subplot(1,1,1), imagesc(all_edges);
plotedit on, title ('All edges of an eye'), plotedit off 
pause;

disp (' ')
disp ('The Roberts operator is actually one of the oldest edge detection')
disp ('operators. The edges are the maximum of the difference between')
disp ('points on the two diagonals.')
%so we'll call the Roberts cross operator
roberts_edges=roberts(eye);
imagesc(roberts_edges);
plotedit on, title ('Eye edges by Roberts cross operator'), plotedit off 
pause;

disp (' ')
disp ('The Prewitt operator includes smoothing in horizontal and vertical')
disp ('templates')  
prewitt_edges=prewitt(eye); 
disp ('From these, we calculate the magnitude and direction. The magnitude')
disp ('shows the amount of contrast, as revealed by its image')
pmagnitude=prewitt_edges(:,:,1); 
subplot (1,2,1), imagesc(pmagnitude); 
plotedit on, title ('Magnitude of eye edges by Prewitt operator'), plotedit off
subplot(1,2,2), imagesc(roberts_edges);
plotedit on, title ('Eye edges by Roberts cross operator'), plotedit off 
disp ('We can see that the effect of smoothing is to reduce noise in')
disp ('the edge detection process')
pause;
disp ('The direction is how the edge is changing, but this is much less')
disp ('easy to see in the displayed image.')
direction=prewitt_edges(:,:,2);
imagesc(direction);
plotedit on, title ('Direction of eye edges by Prewitt operator'), plotedit off
pause;

disp (' ')
disp ('The Sobel operator includes better smoothing than the Prewitt')

disp ('operator.It is harder to see here, but is gereally experienced')
sobel_edges=sobel33(eye); 
disp ('Again, we calculate the magnitude and direction. Again, The')
disp ('magnitude shows the amount of contrast, as shown in the image')
disp ('for a 3*3 Sobel operator.') 
smagnitude=sobel_edges(:,:,1);

subplot(1,2,1), imagesc(smagnitude);
plotedit on, title ('Magnitude of eye edges by Sobel'), plotedit off
subplot(1,2,2), imagesc(pmagnitude);
plotedit on, title ('Magnitude of eye edges by Prewitt'), plotedit off
pause;
disp ('The direction is still much less easy to see!')
subplot(1,1,1), direction=sobel_edges(:,:,2);
imagesc(direction);
plotedit on, title ('Direction of eye edges by Sobel'), plotedit off
pause;
