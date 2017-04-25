 clear all; close all;clc

 %Reference points
P37 =[	27.297	-4.953	1.47]';
P31 =[	20.693	-4.849	1.93]';
P102 =[	22.590	0.524	1.2]';
P43 =[	17.113	-3.003	2.17]';
P208 =[	22.554	4.727	1.77]';
P101 =[	22.447	-7.880	1.6]';


%True solution (Unknown)
N=[26.759	-1.342	1.13]';

s037=norm(P37-N);
s031=norm(P31-N);
s0102=norm(P102-N);
s043=norm(P43-N);
s0208=norm(P208-N);
s0101=norm(P101-N);
%Distances
fprintf('\n  True distances \n');
Ssoll =    [s037  s031  s0102 s043   s0208  s0101];
disp(Ssoll);
fprintf('\n Measured distances: \n');
S38=       [3.652 7.036 4.586 9.960 7.542   7.883];
disp(S38);

% corrected distances
fprintf('\n Corrected distances based on excess delay: \n');
S38= S38 - [ 0    0      0    0.12      0.14   0 ];
P = [P37 P31 P102 P43 P208 P101];
disp(S38);

fprintf('\n ---Trilateration: Direct solution --- \n');
S = S38;
W = diag(ones(1,length(S)));
[N1 N2] = Trilateration(P,S, W);
Nsol1 = N1(2:4,1);
disp([Nsol1])

fprintf('\n\n\n');

fprintf('\n ---Trilateration: Recursive least square--- \n');
Nmat0 = RecTrilateration(P,S,W);
Nmat = Nmat0(:,1:5);
fprintf('\n Recursive solutions \n');
disp(Nmat(2:4,:))
[n_mat m_mat]=size(Nmat);
for i1=1:m_mat
    Nn = Nmat(:,i1);
    Nn = Nn(2:4);
    [Sn(i1,:) , F(i1)] = distanzen(Nn,P,S);
    Diff(i1)= Nmat(1,i1) - (norm(Nn))^2;
end
[Fmin Imin] = min(F);
Nrec = Nmat(:,Imin);
fprintf('\n The solution which minimizes the error square sum \n');
Nrec = Nrec(2:4);
disp(Nrec);

Nrec_r = sign(real(Nrec)).*abs(Nrec);
[Sneu Fneu ] = distanzen(Nrec,P,S);
fprintf('\n Calculated distances (Distances to solution)\n');
disp(Sneu )
fprintf('\n Error norm\n');
disp(Fneu)


