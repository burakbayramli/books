function image_h(x,M,N);
%image_h(x,M,N);

%Creates an image from a vector (x) which represents a horizontally-
%    scanned image with M rows and N columns of grey or color pixels.
%
%For a grey-scale image, the  vector x is the complete set of horizontal
%    scans, row by row, of the grey-scale image plane.
%
%For a color image, x=[red scan; green scan; blue scan]; that is, x is
%    a vector with concatenatd scans of the 3 color planes in order.
%
%An image is created from x and displayed using the Matlab "image" 
%    function.

x=double(x);
Lx=length(x);
L=M*N;
if Lx~=numel(x) | (Lx~=L & Lx~=3*L),
    error('x must be a vector of length M*N or 3*M*N');
end
P=Lx/L;                                         %P =# image planes
if min(x)<0,
    x=x-min(x);                                 %x must be non-negative
end
if max(x)==0,
    error('image is all black')
end
x=x/max(x);                                     %x is in [0,1]
yt=zeros(N,M,P);
yt(:)=[x(1:L),x(L+1:2*L),x(2*L+1:3*L)];
y=zeros(M,N,P);
y(:)=[yt(:,:,1)',yt(:,:,2)',yt(:,:,3)'];        %y is the recovered image
image(y);
