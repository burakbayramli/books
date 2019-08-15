function test04
% some auxiliary calculations for the Taylor-Hood element
% and postprocessor after Sohn
clc, format short, format compact
syms x y
f = [(1-x-y)*(1-2*x - 2*y), x*(2*x-1),y*(2*y-1),4*x*(1-x-y),4*x*y,4*y*(1-x-y)];

dfx = diff(f,x);
dfy = diff(f,y);

dfx = [-3+4*x+4*y, 4*x-1,   0, 4-8*x-4*y, 4*y, -4*y]; %df/dx
dfy = [-3+4*x+4*y,   0, 4*y-1,      -4*x, 4*x,  4-4*x-8*y]; % df/dy
dqx = dfx.';
dqy = dfy.'; 
%dfxx = diff(dfx,x)
%dfxy = diff(dfx,y)
%dfyy = diff(dfy,y)

dfxx = [  4,  4,  0, -8,  0,  0];
dfxy = [  4,  0,  0, -4,  4, -4];
dfyy = [  4,  0,  4,  0,  0, -8];

qx_mal_uxx = dqx*dfxx;
qx_mal_uxy = dqx*dfxy;
qx_mal_uyy = dqx*dfyy;

qy_mal_uxx = dqy*dfxx;
qy_mal_uxy = dqy*dfxy;
qy_mal_uyy = dqy*dfyy;

aux = int(qx_mal_uxx,x,0,1-y);
int_qx_uxx = int(aux,y,0,1);
int_qx_uxx = int_qx_uxx*3

aux = int(qx_mal_uxy,x,0,1-y);
int_qx_uxy = int(aux,y,0,1);
int_qx_uxy = int_qx_uxy*3

aux = int(qx_mal_uyy,x,0,1-y);
int_qx_uyy = int(aux,y,0,1);
int_qx_uyy = int_qx_uyy*3

% ---------------------------------
aux = int(qy_mal_uxx,x,0,1-y);
int_qy_uxx = int(aux,y,0,1);
int_qy_uxx = int_qy_uxx*3

aux = int(qy_mal_uxy,x,0,1-y);
int_qy_uxy = int(aux,y,0,1);
int_qy_uxy = int_qy_uxy*3

aux = int(qy_mal_uyy,x,0,1-y);
int_qy_uyy = int(aux,y,0,1);
int_qy_uyy = int_qy_uyy*3
