%Chapter 1 Introduction (Hello Matlab) CHAPTER1.M

%Written by: Mark S. Nixon

disp ('Welcome to the Chapter1 script')
disp ('This worksheet is the companion to Chapter 1 and is an introduction.')
disp ('It is the source of Section 1.4.3 Hello Matlab.')
disp ('The worksheet follows the text directly and allows you to process basic images.')

disp ('Let us define a matrix of pixels (picture elements), a synthetic computer image called pic.')
pic=[1  2  3  4  1  1  2  1;...
     2  2  3  2  1  2  2  1;...
     3  1 38 39 37 36  3  1;...
     4  1 45 44 41 42  2  1;...
     1  2 43 44 40 39  1  3;...
     2  1 39 41 42 40  2  1;...
     1  2  1  2  2  3  1  1;...
     1  2  1  3  1  1  4  2]
%Pixels are addressed in row-column format. 
%Using x for the horizontal axis (a column count), and y for the vertical axis (a row count)
% then picture points are addressed as pic(y,x). The origin is at co-ordinates (1,1), so 
% the point pic(3,3) is on the third row and third column; the point pic(4,3) is on the 
% fourth row, at the third column.  Let's print them:
disp ('The element pic(3,3) is')
pic(3,3)
disp ('The element pic(4,3) is')
pic(4,3)

%We'll set the output display to black and white
colormap(gray);

%We can view the matrix as a surface plot
disp ('We shall now view the array as a surface plot - as a figure (play with the controls to see it in relief)')
disp ('When you are ready to move on, press RETURN')
surface(pic);
%Let's hold awhile so we can view it
pause;
%Or as an image
disp ('We shall now view the array as an image')
disp ('When you are ready to move on, press RETURN')
imagesc(pic);
%Let's hold awhile so we can view it
pause;

%Let's look at its dimensions
disp ('The dimensions of the array are')
size(pic)

%now let's invoke a routine that inverts the image
inverted_pic = invert(pic);
%Let's print it out to check it
disp ('When we invert it by subtracting each point from the maximum, we get')
inverted_pic
%And view it
disp ('And when viewed as an image, we see')
disp ('When you are ready to move on, press RETURN')
imagesc(inverted_pic);
%Let's hold awhile so we can view it
pause;

disp ('We shall now read in a bitmap image, and view it')
disp ('When you are ready to move on, press RETURN')
face=imread('rhdark.bmp','bmp');
imagesc(face);
pause;

%We need to change from unsigned integer format (uint8) to double precision so we can process it
face=double(face);

disp ('Now we shall invert it, and view the inverted image')
inverted_face=invert(face);
imagesc(inverted_face);
disp ('So we now know how to process images in Matlab. We shall be using this later!')






