function test07
% some auxiliary calculations for the Taylor-Hood element
% and postprocessor after Sohn, convection term
clc, format short, format compact
syms x y
f = [(1-x-y)*(1-2*x - 2*y), x*(2*x-1),y*(2*y-1),4*x*(1-x-y),4*x*y,4*y*(1-x-y)];

%fx = diff(f,x)
%fy = diff(f,y)

fx = [-3+4*x+4*y, 4*x-1,   0, 4-8*x-4*y, 4*y, -4*y]; %df/dx
fy = [-3+4*x+4*y,   0, 4*y-1,      -4*x, 4*x,  4-4*x-8*y]; % df/dy

qx = fx.';
qy = fy.'; 

qx_mal_u = qx*f;
x_qx_mal_u = x*qx*f;
y_qx_mal_u = y*qx*f;

qy_mal_u = qy*f;
x_qy_mal_u = x*qy*f;
y_qy_mal_u = y*qy*f;


aux = int(qx_mal_u,x,0,1-y);
int_qx_u = int(aux,y,0,1);
int_qx_u = int_qx_u*30;

aux = int(x_qx_mal_u,x,0,1-y);
int_x_qx_u = int(aux,y,0,1);
int_x_qx_u = int_x_qx_u*360;

aux = int(y_qx_mal_u,x,0,1-y);
int_y_qx_u = int(aux,y,0,1);
int_y_qx_u = int_y_qx_u*360;
% -----------------------------
aux = int(qy_mal_u,x,0,1-y);
int_qy_u = int(aux,y,0,1);
int_qy_u = int_qy_u*30

aux = int(x_qy_mal_u,x,0,1-y);
int_x_qy_u = int(aux,y,0,1);
int_x_qy_u = int_x_qy_u*360

aux = int(y_qy_mal_u,x,0,1-y);
int_y_qy_u = int(aux,y,0,1);
int_y_qy_u = int_y_qy_u*360





