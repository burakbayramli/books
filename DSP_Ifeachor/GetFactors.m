function  DecimationFactors = GetFactors(M,n)

% The function GetFactors finds all possible decimation factors for
% 2-, 3-, and 4-stage decimation. M is the
% overall decimation factor and n is the number of stages

p = floor(M/(2^(n-1)));
m = 1;
for i=2:p
	for j=2:p
		if n==2&i*j==M
			R(m,1) = i;	% get the 2-stage decimator factors
			R(m,2) = j;
			m = m + 1;
		elseif n>2
			for k=2:p
				if n==3&i*j*k==M
					R(m,1) = i; % get the 3-stage
					R(m,2) = j; % decimator factors
					R(m,3) = k;
					m = m + 1;
				elseif n>3
					for l=2:p
						if i*j*k*l==M
							R(m,1) = i; % get the 4-stage 
							R(m,2) = j; % decimator factors
							R(m,3) = k;
							R(m,4) = l;
							m = m + 1;
						end
					end
				end
			end
		end
	end
end
R = fliplr(sort(R')');	% sort the decimation factor vectors
z = zeros(1,size(R,2));
k = 1;
for i=1:size(R,1)		% reject the redundancies
	for j=i+1:size(R,1)
		if R(i,:)==R(j,:)
			R(j,:) = z;
		end
	end
	if R(i,:)~=z
		DecimationFactors(k,:) = R(i,:);
		k = k + 1;
	end
end
