function V = sobmat(d,r)
polys=[1,3,7,11,13,19,25,37,59,47,61,55,41,67,97,91,109,103,...
115,131,193,137,145,143,241,157,185,167,229,171,213,191,...
253,203,211,239,247,285,369,299,425,301,361,333,357,351,...
501,355,397,391,451,463,487];
ivals=[1 1 1 1 1 1 1 1;        %1
       1 3 5 15 17 51 85 255;  %2
       1 1 7 11 13 61 67 79;   %3
       1 3 7 5 7 43 49 147;    %4
       1 1 5 3 15 51 125 141;  %5
       1 3 1 1 9 59 25 89;     %6
       1 1 3 7 31 47 109 173;  %7
       1 3 3 9 9 57 43 43;     %8
       1 3 7 13 3 35 89 9;     %9
       1 1 5 11 27 53 69 25;  %10
       1 3 5 1 15 19 113 115; %11
       1 1 7 3 29 51 47 97;   %12
       1 3 7 7 21 61 55 19;   %13
       1 1 1 9 23 39 97 97;   %14
       1 3 3 5 19 33 3 197;   %15
       1 1 3 13 11 7 37 101;  %16
       1 1 7 13 25 5 83 255;  %17
       1 3 5 11 7 11 103 29;  %18
       1 1 1 3 13 39 27 203;  %19
       1 3 1 15 17 63 13 65]; %20
       
       m = zeros(1,r);
       V = zeros(r,r,d);
       V(:,:,1) = eye(r);
       
       for k=2:d
       	       ppn=polys(k); %polynomial number
       	       m=ivals(k,:); %initial values
       	       c= cbe(ppn,2); 
       	       c = c(2:end); % coefficients of the primitive polynomial
       	       deg = numel(c); %degree of the polynomial
       	       for i=9:r %first 8 values already given
	   	       s = 0;
	   	       for j = 1:deg
	       		       s = bitxor(s,2^j*c(j)*m(i-j));
	end
	m(i)= bitxor(s,m(i-deg));
end

for j=1:r
	    h = cbe(m(j),2); % binary representation
	    numdigs = numel(h);
	    V(j- numdigs+1:j,j,k)= h';
    end
end

