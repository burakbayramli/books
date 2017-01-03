function demoMostProbablePathMult
%DEMOMOSTPROBABLEPATHMULT demo: most probable path multi-source, multi-sink
A=zeros(9);
A(8,1)=1;A(2,1)=1;A(4,2)=1;A(3,2)=1;A(6,2)=1;A(5,2)=1;A(7,2)=1;A(7,9)=1;A(9,8)=1;
draw_layout(A',{'1','2','3','4','5','6','7','8','9'},ones(9,1));
p=A./repmat(sum(A),size(A,1),1);
N=size(A,1);
[optpathmult,logprobmult]=mostprobablepathmult(log(p)); % multiple source multiple sink method O(N^3)
%[optpathmult,logprobmult]=mostprobablepathmult(log(A)); % multiple source multiple sink method O(N^3) -- shortest path
% check using all single source single sink pairs:
for startstate=1:N
    for endstate=1:N
        [optpath logprob]=mostprobablepath(log(p),startstate,endstate);
        %[optpath logprob]=mostprobablepath(log(A),startstate,endstate); % shortest path
        if isfinite(logprob)
            fprintf('[%d->%d]:\n',startstate,endstate)
            fprintf('single   source method: log probability = %g, path: ',logprob)
            fprintf('%s\n',num2str(optpath))
            fprintf('multiple source method: log probability = %g, path: ',logprobmult(startstate,endstate))
            s1= noselfpath(squeeze(optpathmult(startstate,endstate,:))');
            fprintf('%s\n',num2str(s1))
        end
    end
end