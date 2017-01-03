function demoHMMbigram
%DEMOHMMBIGRAM demo of HHM for the bigram typing scenario
load freq % http://www.data-compression.com/english.shtml
l = {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',' '};
load typing % get the A transition and B emission matrices
figure(1); imagesc(A); set(gca,'xtick',1:27); set(gca,'xticklabel',l); set(gca,'ytick',1:27); set(gca,'yticklabel',l)
colorbar; colormap hot; title('transition')
figure(2); imagesc(B); set(gca,'xtick',1:27); set(gca,'xticklabel',l); set(gca,'ytick',1:27); set(gca,'yticklabel',l)
colorbar; colormap hot; title('emission')
ph1=condp(ones(27,1)); % uniform first hidden state distribution

s = 'kezrninh'; % observed sequence
v=double(s)-96; v=replace(v,-64,27); % convert to numbers

% find the most likely hidden sequences by defining a Factor Graph:
T = length(s);
hh=1:T; vv=T+1:2*T;
empot.variables=[vv(1) hh(1)]; empot.table=B;
prior.variables=hh(1); prior.table=ph1;
pot(1) = multpots([setpot(empot,vv(1),v(1)) prior]);
for t=2:T
    tranpot.variables=[hh(t) hh(t-1)]; tranpot.table=A;
    empot.variables=[vv(t) hh(t)]; empot.table=B;
    pot(t) = multpots([setpot(empot,vv(t),v(t)) tranpot]);
end
FG = FactorGraph(pot);
Nmax=200;
[maxstate maxval mess]=maxNprodFG(pot,FG,[],Nmax);
strs = char(replace(maxstate+96,123,32)) % make strings from the decodings
fid=fopen('brit-a-z.txt','r'); % see http://www.curlewcommunications.co.uk/wordlist.html for Disclaimer and Copyright
w=textscan(fid,'%s'); w=w{1}; % get the words from the dictionary

% discard those decodings that are not in the dictionary:
for t=1:Nmax
    str = strs(t,:); % current string
    spac = strfind(str,' '); % chop the string into words
    spac = [spac length(str)+1]; % find the spaces
    start=1; val=1;
    for i=1:length(spac) % go through all the words in the string
        wd{i} = str(start:(spac(i)-1));
        start=spac(i)+1;
        if isempty(find(strcmp(wd{i},w))) % check if word is in the dictionary
            val=0; break
        end
    end
    if val; disp([num2str(t) ':' str]);end
end