function T = TranslateImage(I, dx, dy)
%T = TranslateImage(I, dx, dy) translates image I by (dx, dy) pixels.  The
%shift parameters dx and dy are integers.  The borders are handled by
%copying values.  The resultant image T is the same size as I.

[ny, nx] = size(I);
T = I;
if (dx > 0)
    T = [repmat(T(:, 1), 1, dx) T(:, 1:nx - dx)];
elseif (dx < 0)
    T = [T(:, -dx+1:nx) repmat(T(:, nx), 1, -dx)];
end    
    
if (dy > 0)
    T = [repmat(T(1, :), dy, 1); T(1:ny - dy,:)];
elseif (dy < 0)
    T = [T(-dy+1:ny,:); repmat(T(ny, :), -dy, 1)];
end    
    
