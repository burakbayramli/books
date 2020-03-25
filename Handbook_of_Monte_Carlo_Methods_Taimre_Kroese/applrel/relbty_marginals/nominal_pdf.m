function [x,Score]=nominal_pdf
global  GRAPH

sig=GRAPH.sig;
m=length(sig);

%#######################################################

x=randn(1,m).*sig;
if nargout>1
Score=S(x);
end

