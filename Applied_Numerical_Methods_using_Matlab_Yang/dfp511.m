function df= dfp511(x,h) % numerical derivative of (P5.11-2)
if nargin<2,  h=0.001;  end
df= (fp511(x+h)-fp511(x-h))/2/h; %Eq.(5.1-8)
