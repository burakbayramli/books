clear
N = 2
x = zeros(N,1); P = 100*eye(N,N);

b = [[3.0],
     [4.0],
     [6.0],
     [3.0],
     [8.0],
     [7.0],
     [5.0]]
	
for k = 1:7
  [x,K,P] = rlse_online([k 1],b(k,:),x,P);
end
x

A = ones(7,2);
A(:,1) = 1:7;
A\b % for comparison with the off-line processing (batch job)
