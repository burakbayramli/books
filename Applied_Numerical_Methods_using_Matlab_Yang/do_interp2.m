%do_interp2.m
% 2-dimensional interpolation
x=-2:2:2;  y=-2:2:2;
[X,Y] = meshgrid(x,y);
Z =X.^2 + Y.^2;
clf, mesh(X,Y,Z)
pause
xi=-2:2;  yi=-2:2;
[Xi,Yi]= meshgrid(xi,yi);
Zi= interp2(X,Y,Z,Xi,Yi); %built-in routine
%Zi= interp2(x,y,Z,Xi,Yi); %built-in routine
mesh(Xi,Yi,Zi)
pause
Zi= intrp2(x,y,Z,xi,yi); %our own routine
mesh(xi,yi,Zi)
clf
figure(2), clf
xi=-2:0.1:2;  yi=-2:0.1:2;
[Xi,Yi] = meshgrid(xi,yi);
Z0 =Xi.^2 + Yi.^2;
subplot(131), mesh(Xi,Yi,Z0) %true function
x=-2:0.5:2;  y=-2:0.5:2;
[X,Y] = meshgrid(x,y); %on sample grid
Z =X.^2 + Y.^2;
subplot(132), mesh(X,Y,Z)
Zi= interp2(x,y,Z,Xi,Yi); %built-in routine
subplot(133), mesh(xi,yi,Zi)
Zi= intrp2(x,y,Z,xi,yi); %our own  routine
pause, mesh(xi,yi,Zi)
relative_error=norm(Z0-Zi)/norm(Z0)