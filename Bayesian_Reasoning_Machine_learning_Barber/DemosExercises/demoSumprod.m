function demoSumprod
%DEMOSUMPROD Sum-Product algorithm test :
figure
% Variable order is arbitary
variables=1:5; [a b c d e]=assign(variables);
nstates=ceil(3*rand(1,5)+1); % random number of states for each variable

pot(1).variables=[a b]; pot(1).table=rand(nstates(pot(1).variables));
pot(2).variables=[b c d];pot(2).table=rand(nstates(pot(2).variables));
pot(3).variables=[c]; pot(3).table=rand(nstates(pot(3).variables),1);
pot(4).variables=[e d]; pot(4).table=rand(nstates(pot(4).variables));
pot(5).variables=[d]; pot(5).table=rand(nstates(pot(5).variables),1);

subplot(1,2,1); drawNet(markov(pot)); title('Markov Network');
A = FactorGraph(pot); subplot(1,2,2); drawFG(A); title('Factor Graph');
[marg mess]=sumprodFG(pot,A);

% check if this is correct:
jointpot = multpots(pot); V=length(pot);
for i=1:V
    margpot(i)= sumpot(jointpot,setdiff(1:V,i));
    margpot(i).table=margpot(i).table/sum(margpot(i).table);
    str1=disptable(marg(i),[]);
    str2=disptable(margpot(i),[]);
    disp([' marginal of variable ',num2str(i),' FG (left) Raw summation (right):']);
    disp([char(str1) char(str2)]); 
end

% FG on reduced variables (as a demonstation of changing variables):
disp('compute p(c|a=1) by FG on reduced variables (column 1) and raw summation (column 2):')
[pot2 newvars oldvars]=squeezepots(setpot(pot,a,1));
[marg2 mess2]=sumprodFG(pot2,FactorGraph(pot2));
str1=disptable(marg2(newvars(oldvars==c)),[]);
str2=disptable(condpot(multpots(setpot(pot,a,1)),c),[]);
disp([char(str1) char(str2)])

fprintf(1,'\n\nCompute the marginals for the variables connected to each potential:\n')
% compute marginals for the factors:
for f=1:length(pot)
    potmarg(f)=normpot(multpots([pot(f) mess(mess2fact(f,A))])); % marginal is the potential multiplied by incoming messages
    potmargcheck(f)=normpot(sumpot(jointpot,pot(f).variables,0));
    str1=disptable(potmarg(f));str2=disptable(potmargcheck(f));
    fprintf(1,'marginal table on potential %d (left sumprod, right check)\n',f); disp([char(str1) char(str2)])
end