% the LOT matrix Po
N=8; %number of rows of DCT matrix (edit this)
D=idct(eye(N));
De=D(1:2:N,:);
Do=D(2:2:N,:);
R=(De-Do);
J=flipud(eye(N));
Po=0.5*[R,R*J;R, -R*J];
