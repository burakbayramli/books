function d=image_distr(x)
% d=image_distr(x)
%
% This function computes the color distributions of an image.
%
% x is the image array, dimensioned (Nrows,Ncolumns,Ncolors).
%
% d is the color distribution array, dimensioned (256,Ncolors).
%   Ncolors=# colors. When Ncolors=3, col. 1 of d is the red
%   intensity histogram, col. 2 is the green, and col. 3 the blue.
%
%   The first bin in each col. of d represents intensity 0.
%   The last bin in each col. represents intensity 255.
[Ny,Nx,Nc]=size(x);
if double(min(x(:)))<0,
   error('Minimum pixel value <0.');      % Stop if negative pixel value.
end
d=zeros(256,Nc);
for i=1:Nc,
   dx=row_vec(double(x(:,:,i)));          % Convert to double if necessary.
   d(:,i)=histc(dx,[0:255])';             % Intensity histogram for color i.
%   fprintf('color%2.0d finished.\n',i);  % Notify progress if desired.
end