%raffledraw.m
%scriptfile for EFR 4.4

K = input('Enter number of players:   ');
N=zeros(K,26); %this allows up to 26 characters for each players name.
n=input('Enter IN SINGLE QUOTES first player name:   ');
len(1)=length(n);
N(1,1:len(1))=n;
W(1)=input('Enter weight of first player:  ');
for i=2:K-1
    n=input('Enter IN SINGLE QUOTES next player name:  ');
    len(i)=length(n);
    N(i,1:len(i))=n;
    W(i)=input('Enter weight of this player:  ');
end
n=input('Enter IN SINGLE QUOTES last player name:  ');
len(K)=length(n);
N(K,1:len(K))=n;
W(K)=input('Enter weight of last player:  ');

totW = sum(W); %total weight of all players (=# of raffle tickets)

%the next four commands are optional, they only add suspense and 
%drama to the raffle drawing which the computer can do in lightning time
fprintf('\r \r RANDOM SELECTION PROCESS INITIATED \r \r ...')
pause(1) %creates a 1 second pause
fprintf('\r \r ...SHUFFLING....\r \r')
pause(5) %creates a 5 second pause
%%%%%%%%%%%%%%%%%%%%%%%

rand('state',sum(100*clock)) 
magic = floor(totW*rand); %this will be a random number between 0 and totW
count =W(1); %number of raffle tickets of player 1
if magic<=count
    fprintf('WINNER IS %s \r \r', char(N(1,1:len(1))))
    fprintf('CONGRATULATIONS %s!!!!!!!!!!!!', char(N(1,1:len(1))))
    return
else count = count + W(2);, k=2;
    while 1
        if magic <=count
         fprintf('WINNER IS %s \r \r', char(N(k,1:len(k))))
    fprintf('CONGRATULATIONS %s!!!!!!!!!!!!', char(N(k,1:len(k))))
    return
    end
    k=k+1;, count = count +W(k);
    end
end


