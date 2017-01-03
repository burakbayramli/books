function demoShortestPathMult
%DEMOSHORTESTPATH demo of finding the shortest weigthed path

A=inf(8); % path weight is infinite if no link i->j
A(1,2)=1;A(1,5)=1; A(2,5)=1; A(2,3)=5; A(3,4)=-1; A(3,5)=2; A(3,6)=1;
A(4,6)=1; A(4,7)=2; A(4,8)=1; A(5,6)=4;  A(6,7)=-3; A(7,8)=2.5; A(8,7)=2; A(5,3)=1;
draw_layout(~isinf(A),{'1','2','3','4','5','6','7','8'},ones(8,1));
[a b]=assign([1 8]); % start and end states
[optpath pathweight]=mostprobablepath(-A',a,b); % use negative since we want shortest path
% transpose is required since mostprobable path assumes a transition p(sink|source)
fprintf('shortest path has weight %g\n',-pathweight); optpath
for i=1:length(optpath)-1; fprintf('%g+',A(optpath(i),optpath(i+1))); end;

[optpathmult pathweightmult]=mostprobablepathmult(-A');
N=size(A,1);
for startstate=1:N
    for endstate=[1:startstate-1:startstate+1:N]
        [optpath logprob]=mostprobablepath(-A',startstate,endstate);
        if isfinite(logprob)
            fprintf('[%d->%d]:\n',startstate,endstate)
            fprintf('single   source method: path weight= %g, path: ',logprob)
            fprintf('%s\n',num2str(optpath))
            fprintf('multiple source method: path weight= %g, path: ',pathweightmult(startstate,endstate))
            s1= noselfpath(squeeze(optpathmult(startstate,endstate,:))');
            fprintf('%s\n',num2str(s1))
        end
    end
end

