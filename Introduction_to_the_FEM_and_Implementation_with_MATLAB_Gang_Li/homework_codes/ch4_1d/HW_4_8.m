clear all;
start_x=0;
end_x=1;

length= end_x - start_x;
center=(start_x + end_x)/2;

for ng=2:3
  [gauss_x, gauss_w]=Get1DGauss(ng);
  I=0;                    
  for g=1:ng
    I=I+ 4/(1+(gauss_x(g)*length/2+center)^2)*gauss_w(g)*length/2;
  end
  I                       % integration result
  error=abs((I-pi)/pi)    % relative error
end