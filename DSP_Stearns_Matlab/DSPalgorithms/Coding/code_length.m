function codlen=code_length(x)
% codlen =code_length(x)
% Produces an ordered list of Huffman code lengths.
% x     =input vector or array. The elements in x are assumed
%        to be in the range [0,xmax].
% codlen =col. vector of Huffman code lengths.  1 x xmax+1.
%         codlen(j)= length of Huffman code for symbol (j-1).
%
% See also freq, h_codes, h_encode
[f,xmin,xmax]=freq(x);
f=row_vec(32*f);
if xmin~=0,
   error('x must be translated so min(x)=0');
elseif length(x)<2
   error('Length(x) must be >1.');
elseif xmax+1>=2^16
	error('Number of symbols cannot exceed 2^16-1.');
elseif range(x)==0
    error('Range must be >0 for Huffman coding.');
end
Ncodes=xmax+1;
L=Ncodes;
maxnode=Ncodes;
node=[1:2*maxnode]';
next_node=zeros(2*maxnode,1);
% Create the binary tree.
while L>1;
   % Sort the [node,f] vector.
   [fsort,indx]=sort(f);
   new_node=node(indx);
   node=new_node;
   f=fsort;   
  % Combine 1st 2 nodes into a new node at end.
   maxnode=maxnode+1;
   next_node(node(1))=maxnode;
   next_node(node(2))=maxnode;
   node=[node(3:L);maxnode];
   f=[f(3:L) f(1)+f(2)];
   L=L-1;
end
% Compute the code lengths using the tree.
codlen=zeros(Ncodes,1);
for j=1:Ncodes;
   dist=1;
   next=next_node(j);
   while next<maxnode;
      dist=dist+1;
      next=next_node(next);
   end
   codlen(j)=dist;
end