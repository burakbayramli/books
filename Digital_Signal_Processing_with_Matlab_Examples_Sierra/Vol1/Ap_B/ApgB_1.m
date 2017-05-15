% Example of Markov Chain
% with 3 states

%transition matrix
T=[0.65 0.20 0.15;
   0.30 0.24 0.46;
   0.52 0.12 0.36];

%initial probabilities
pC=0.5; pS=0.4; pR=0.1;

%initial state
rand('state',sum(100*clock));
u=rand(1);
if u<pC, X=1; 
elseif u<pC+pS, X=2;
else X=3;
end;   

%initialize result R
if X==1, R='C'; end; %clouds
if X==2, R='S'; end; %sunny
if X==3, R='R'; end; %rain  

rX=zeros(1,60); %for state historic
rX(1)=X;

%run the process---------------
for nn=2:60,
   u=rand(1); 
  %state transitions
  if X==1, 
      if u<T(1,1), X=1; 
		elseif u<(T(1,1)+T(1,2)), X=2;
      else X=3;
      end; 
  end;    
  if X==2,
      if u<T(2,1), X=1; 
		elseif u<(T(2,1)+T(2,2)), X=2;
      else X=3;
      end;
   end;   
   if X==3,
      if u<T(3,1), X=1; 
		elseif u<(T(3,1)+T(3,2)), X=2;
      else X=3;
      end;     
   end;
      
rX(nn)=X; %store result
%concatenation     
if X==1, R=[R,'C']; end; %clouds
if X==2, R=[R,'S']; end; %sunny
if X==3, R=[R,'R']; end; %rain  
  
end;

disp(R);      
plot(rX,'k');
axis([0 60 0.8 3.2]);
title('3 states Markov Chain: transitions');
xlabel('n');
