function [ord noparents_vars]=ancestralorder(A)
%ANCESTRALORDER Return the ancestral order or the DAG A (oldest first)
% ord=ancestralorder(A)
N=size(A,1); AA=A; ord=[]; noparents_vars=[];
done=zeros(1,N);
while length(ord)<N
	for i=1:N
		nochildren = isempty(find(AA(i,:)));
		noparents = isempty(find(AA(:,i)));
		noparentsA = isempty(find(A(:,i)));
		if noparentsA; noparents_vars = unique([noparents_vars i]); end
		if ~(noparents & nochildren)
			if nochildren % i has no children
				AA(:,i)=0;
				ord=[ord i];
				done(i)=1;
			end
		end
	end
	if all(done); break; end
	rest=setdiff(1:N,find(done));
	if isempty(find(AA(:,rest)))
		break;
	end
end
ord=fliplr([ord rest]);