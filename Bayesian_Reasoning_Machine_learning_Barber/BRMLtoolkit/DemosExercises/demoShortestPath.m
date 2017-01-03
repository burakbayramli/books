function demoShortestPath
%DEMOSHORTESTPATH demo of finding the shortest weigthed path

A=inf(8); % path weight is infinite if no link i->j
A(1,2)=1;A(1,5)=1; A(2,5)=1; A(2,3)=5; A(3,4)=-1; A(3,5)=2; A(3,6)=1;
A(4,6)=1; A(4,7)=2; A(4,8)=1; A(5,6)=4;  A(6,7)=-3; A(7,8)=2.5; A(8,7)=2; A(5,3)=1;
draw_layout(~isinf(A),{'1','2','3','4','5','6','7','8'},ones(8,1));
[a b]=assign([1 8]); % start and end states
[optpath pathweight]=mostprobablepath(-A',a,b); % use negative since we want shortest path
% transpose is required since mostprobablepath assumes a transition p(sink|source)
fprintf('shortest path has weight %g\n',-pathweight); optpath
for i=1:length(optpath)-1; fprintf('%g+',A(optpath(i),optpath(i+1))); end;