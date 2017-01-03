function p = setstate(pot,vars,state,val)
%SETSTATE set a potential's specified joint state to a specified value
% p = setstate(pot,vars,states,val)
% All states of the potential that match the given (sub)state are set to val
p=pot; [dum iperm]=ismember(vars,pot.variables);
nstates=numstates(pot); permstates=zeros(1,length(nstates));
permstates(iperm)=state;
if all(permstates>0) % if the state is unique
    p.table(subv2ind(nstates,permstates))=val;
else % set all states that match the given substate to the given value
    sub=find(permstates>0);
    st=ind2subv(nstates,1:prod(nstates));
    p.table(find(all((st(:,sub)==repmat(permstates(sub),prod(nstates),1))')))=val;
end