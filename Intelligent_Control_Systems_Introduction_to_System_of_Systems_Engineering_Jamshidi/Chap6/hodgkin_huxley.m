Cm = 1;
Cn = 1;
Ch = 1;
C = 1;
alpha = 1;
g_K = 36;
g_L = 0.3;
g_Na = 120;
v_Na = 115;
v_K = -12;
v_L = 10.6;
h = 0;m = 0;n = 0;
T = 0.0001;

vh = 0;ih = 0;
vm = 0;im = 0;
vn = 0;in = 0;
v = 0; 

iK = 0;
iNa = 0;
iL = 0;

data = [];
ii = 10;
fp = fopen('hh.txt','w');
v = 0;
for i = 1:1000000
    
    v = v + T*(ii - (iK + iNa + iL))/C;
    alpha_n = (0.1 - 0.01*v)/(exp(1-0.1*v) - 1);
    alpha_m = (2.5 - 0.1*v)/(exp(2.5 - 0.1*v) - 1);
    alpha_h = 0.07*exp(-v/20);
    
    beta_n = 0.125*exp(-v/80);
    beta_m = 4*exp(-v/18);
    beta_h = 1/(exp(3 - 0.1*v) + 1);
    
    m = m + T*(alpha_m*(1-m)-beta_m*m);
    n = n + T*(alpha_n*(1-n)-beta_n*n);
    h = h + T*(alpha_h*(1-h)-beta_h*h);
        
    iNa = g_Na*m^3*h*(v - v_Na);
    iK = g_K*n^4*(v - v_K);
    iL = g_L*(v - v_L);
    
    data = [T*i v];
    fprintf(fp,'%6.2f %6.2f\n',data');
   
end
fclose(fp);
data = load('hh.txt');
time = data(:,1);
pp = data(:,2);
plot(time,pp);
