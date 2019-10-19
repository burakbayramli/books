function H = totbl(A,b,p,alpha,xlab,zlab,conslab)
% syntax: H = totbl(A,b,p,alpha,xlab,zlab,conslab);
% store the matrices A,b,p,alpha as a tableau H
% row and column labels stored in last two rows and columns

if nargin < 1
  error('require at least one argument for totbl');
else 
  [m,n] = size(A);
  if (m > 99 | n > 99) 
    error('too large for labeled tableau');
  end
  if nargin < 2
    H.val = A;
    H.bas = writelbl('y',1:m);
    H.nonbas = writelbl('x',1:n);
    H.obj = [];
  else
    b = b(:);
    if length(b) ~= m
      error('wrong dimension on rhs');
    end
    if nargin < 3
      H.val = [A -b];
      H.bas = writelbl('y',1:m);
      H.nonbas = writelbl('x',1:n,'1');
      H.obj = [];
    else
      if isempty(p)
	H.val = [A -b];
	H.bas = writelbl('x',n+1:n+m);
	H.nonbas = writelbl('x',1:n,'1');
	H.obj = [];
      else
	p = p(:);
        if length(p) ~= n
          error('wrong dimension on cost');
        end
	if nargin < 4
	  H.val = [A -b; p' 0.];
	  H.bas = writelbl('x',n+1:n+m,'z');
	  H.nonbas = writelbl('x',1:n,'1');
	  H.obj = m+1;
	else 
	  if nargin < 7
            conslab = '1';
	    if nargin < 6
	      zlab = 'z';
	      if nargin < 5
	        xlab = 'x';
	      end
	    end
	  end
	  if (length(conslab) > 3 | length(zlab) > 3 | length(xlab) > 1)
	    error('labels too long');
	  end
	  H.val = [A -b; p' alpha];
	  H.bas = writelbl(xlab,n+1:n+m,zlab);
	  H.nonbas = writelbl(xlab,1:n,conslab);
	  H.obj = m+1;
	end
      end
    end
  end
end

tbl(H);

return;
