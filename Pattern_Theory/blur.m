y=imread('InkaAlligator.jpg','jpeg');
[i,j,k]=size(y);
a=.1;
for c=1:3
    yhat(:,:,c) = double(dct2(y(:,:,c)));
end
for k=1:10
    a = a/2;
    z = zeros(i,j,3);
    freq=((0:i-1).^2)'*ones(1,j) + ones(i,1)*((0:j-1).^2);
    factor = exp(-a*freq);
    for c=1:3
        z(:,:,c) = idct2(yhat(:,:,c).*factor);
    end
    figure(1), imagesc(uint8(z)), axis equal, axis off
    title(sprintf('a=%1.5f',a))
    pause
    %name = sprintf('InkaAll%d.jpg',k);
    %print('-djpeg', name)
end
