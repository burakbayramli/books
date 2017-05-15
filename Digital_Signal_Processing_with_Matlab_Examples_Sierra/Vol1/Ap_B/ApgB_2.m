% Example of HMM (synthetic speech)
% with 2 states
% each state has 4 emission alternatives

%transition matrix (probabilities)
T=[0.3 0.7;
   0.5 0.5];
%emissions from state 1 (probabilities)
E1=[0.2 0.3 0.2 0.3];
%emissions from state 2 (probabilities)
E2=[0.3 0.3 0.2 0.2];

%initial probabilities
pC=0.6; %consonant (X=1)
pW=0.4; %wovel (X=2)

%initial state
rand('state',sum(100*clock));
u=rand(1);
X=2;
if u<pC, X=1; end; 

%initial emission
u=rand(1);
if X==1,
  if u<E1(1), EM=1; R='W';
   elseif u<(E1(1)+E1(2)), EM=2; R='H';
   elseif u<(E1(1)+E1(2)+E1(3)), EM=3; R='T';
   else EM=4; R='_';
  end;
end;
if X==2,
  if u<E2(1), EM=1; R='A';
   elseif u<(E2(1)+E2(2)), EM=2; R='E';
   elseif u<(E2(1)+E2(2)+E2(3)), EM=3; R='O';
   else EM=4; R='U';
  end;
end;

rX=zeros(1,60); %for state record
rE=zeros(1,60); %for emission record
rX(1)=X;
rE(1)=EM;

%run the process-------------------
for nn=2:60,
  u=rand(1); 
  %state transitions
  if X==1, 
      X=2;
      if u<T(1,1), X=1; end;		    
  end;    
  if X==2,
      X=1;
      if u<T(2,2), X=2; end;
  end;   
  
  %emission
  u=rand(1);
  if X==1,
    if u<E1(1), EM=1; R=[R,'W'];
     elseif u<(E1(1)+E1(2)), EM=2; R=[R,'H'];
     elseif u<(E1(1)+E1(2)+E1(3)), EM=3; R=[R,'T'];
     else EM=4; R=[R,'_'];
    end; 
  end;
  if X==2,
    if u<E2(1), EM=1; R=[R,'A'];
     elseif u<(E2(1)+E2(2)), EM=2; R=[R,'E'];
     elseif u<(E2(1)+E2(2)+E2(3)), EM=3; R=[R,'O'];
     else EM=4; R=[R,'U'];
     end; 
  end;
      
  rX(nn)=X; %store result
  rE(nn)=EM;
  
end;

disp(R); %print result 

%display
subplot(2,1,1)
plot(rX,'k');
title('HMM (synthetic speech): the hidden states');
xlabel('n');
axis([0 60 0.8 2.2]);

subplot(2,1,2)
plot(rE,'k');
axis([0 60 0.5 4.5]);
title('the emissions');
xlabel('n');

