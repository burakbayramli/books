function [bandwidth,density,xmesh]=kde(data,n,MIN,MAX)
% set up the grid over which the density estimate is computed;
data=data(:);
R=MAX-MIN; dx=R/(n-1); xmesh=MIN+[0:dx:R]; N=length(data);
%bin the data uniformly using the grid define above;
initial_data=histc(data,xmesh)/N;
a=dct1d(initial_data); % discrete cosine transform of initial data
% now compute the optimal bandwidth^2 using the referenced method
I=[1:n-1]'.^2; 
% use  fzero to solve the equation t=zeta*gamma^[l](t)
t_star=fzero(@(t)fixed_point(t,N,I,a(2:end).^2),[0,.01]);
% smooth the discrete cosine transform of initial data using t_star
a_t=a.*exp(-[0:n-1]'.^2*pi^2*t_star/2);
% now apply the inverse discrete cosine transform
density=idct1d(a_t)/dx;
% take the rescaling of the data into account
bandwidth=sqrt(t_star)*R;