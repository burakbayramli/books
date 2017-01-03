% Jana Kosecka, (c) George Mason University, Fall 99
% these 3-tap (and others) filters are decribed in the paper
% E. Simmoncelli: Bayesian multi-scale differential optical flow. 
% Different sizes of filters they are described in the paper. 

prefilt = [0.223755 0.552490 0.223755];
derivfilt = [-0.453014 0 0.45301];

% serve as weighted sum weighting the middle pixel most
blur	= [1 6 15 20 15 6 1];
blur	= blur / sum(blur);

% must match the filter size for computation of temporal derivatives
nframes	= 3; 

% Get image sequence
[dimy,dimx]=size(imread('yos.10.pnm'));
seq(1).im = zeros(dimy,dimx);
for i=1:nframes
   filename = sprintf('yos.%d.pnm',9+i);
   disp(['Reading image ',filename]);
   seq(i).im = imread(filename);
end

% view motion sequence
if( 1 ) 
	clear M;
	for i =1 : nframes
		%imshow( seq(i).im,256 );
                imagesc( seq(i).im);
                colormap('gray');
		M(:,i) = getframe;
	end
	movie(M,-10,30);
end

%%% SPATIAL AND TEMPORAL DERIVATIVE
f	= zeros(dimy,dimx);

% prefilter in the temporal diection
for i = 1 : nframes
	f = f + prefilt(i)*seq(i).im;
end

% compute derivatives in x and y, prefilter in the other dimension
fx	= conv2( conv2( f, prefilt', 'same' ), derivfilt, 'same' );
fy	= conv2( conv2( f, prefilt, 'same' ), derivfilt', 'same' );

ft	= zeros(dimy,dimx);
% compute derivative in time 
for i = 1 : nframes
	ft = ft + derivfilt(i)*seq(i).im;
end

% and prefilter in the other directions 
ft	= conv2( conv2( ft, prefilt', 'same' ), prefilt, 'same' );

blur    = [1 6 15 20 15 6 1];
blur    = blur / sum(blur);
fx2     = conv2( conv2( fx .* fx, blur', 'same' ), blur, 'same'  );
fy2     = conv2( conv2( fy .* fy, blur', 'same'  ), blur, 'same'  );
fxy     = conv2( conv2( fx .* fy, blur', 'same'  ), blur, 'same'  );
fxt     = conv2( conv2( fx .* ft, blur', 'same'  ), blur, 'same'  );
fyt     = conv2( conv2( fy .* ft, blur', 'same'  ), blur, 'same'  );
s       = 4;

[ydim,xdim] = size( fx );
Vx      = zeros(ydim/s, ceil(xdim/s));
Vy      = zeros(ydim/s, ceil(xdim/s));
cx      = 1; 

for x = 1 : s : xdim
        cy = 1;
        for y = 1 : s : ydim
                M = [fx2(y,x) fxy(y,x) ; fxy(y,x) fy2(y,x)];
                b = [fxt(y,x) ; fyt(y,x)];
                if( rank(M) < 2 )
                        Vx(cy,cx) = 0;
                        Vy(cy,cx) = 0;
                else
                        v = inv(M) * b; 
                        Vx(cy,cx) = v(1);
                        Vy(cy,cx) = v(2);
                end
                cy = cy + 1;
        end
        cx = cx + 1;
end


%%% SHOW FLOW FIELD
%%%

figure(1); imagesc(seq(1).im); colormap gray; hold on;
[xramp,yramp] = meshgrid(s:s:xdim,s:s:ydim);
quiver( xramp, yramp, Vx, Vy, 5 );
axis equal;
hold on;

