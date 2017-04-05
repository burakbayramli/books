function I = imreadbw(file)
I=im2double(imread(file)) ;

if(size(I,3) > 1)
  I = rgb2gray( I ) ;
end


