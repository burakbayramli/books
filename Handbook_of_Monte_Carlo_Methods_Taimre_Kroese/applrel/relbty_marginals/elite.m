function Indic=elite(x,gam)
global GRAPH
cuts=GRAPH.cuts; 
paths=GRAPH.paths;
if  max(min(x(GRAPH.cuts),[],2))>=gam
    Indic=1; %active_lower_bound=1;
elseif min(max(x(GRAPH.paths),[],2))<gam
    Indic=0;
else
    
    Indic=S(x)>=gam;
end