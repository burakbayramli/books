function [A Atriangulated cl]=makeThinJT(N,Width,varargin)
%makeThinJT make a thin junction tree
% [A Atriangulated cl]=makeThinJT(N,Width,<opts>)
%
% N is the number of nodes in the graph
% Width is the maximal clique size
% opts.Acand is a binary adjacency matrix containing the possible edges for the tree
% opts.Ainit is the initial guess for the tree
% opts.plotprogress (0/1)
% opts.showlinksleft (0/1)
%
% The routine starts from some Ainit adjancecy matrix (default the
% identity) and then looks at each candidate edge in turn. If adding a
% candidate edge makes a triangulated graph with a clique <= Width, then
% the candidate is accepted, otherwise rejected. The routine greedily trys
% to add edges to nodes with the smallest number of neighbours.
opts=[];if nargin==3; opts=varargin{1}; end
opts=setfields(opts,'Acand',ones(N,N),'Ainit',eye(N),'plotprogress',1,'showlinksleft',1);% default options
candedges=edges(opts.Acand);
A=opts.Ainit;
while ~isempty(candedges)
    ns=neighboursize(A);
    [val ind]=sort(ns); % try to add links to nodes with the smallest number of neighbours
    k=0; done=0;
    while k<length(ind) && done==0
        k=k+1;
        for ed=1:size(candedges,1)
            if candedges(ed,1)==ind(k) || candedges(ed,2)==ind(k)
                done=1; break;
            end
        end
    end
    i=candedges(ed,1); j=candedges(ed,2);
    Atry=A; Atry(i,j)=1; Atry(j,i)=1; % add in the link
    cols=find(sum(A));
    [Atriangulated cl toowide] = triangulateComponent(Atry(cols,cols),Width); % get a triangulation for this candidate
    sz=zeros(1,length(cl));for c=1:length(cl); sz(1,c)=length(cl(c).variables); end
    if opts.showlinksleft;
        fprintf(1,'number of links left %d\n',size(candedges,1));
    end
    if all(sz<Width+1) && ~toowide
        A(i,j)=1; A(j,i)=1; % accept the candidate
        if opts.plotprogress
            subplot(1,3,1); imagesc(A); title('A');
            subplot(1,3,2); imagesc(Atriangulated); title('A triangulated');
            subplot(1,3,3); imagesc(edges2adj(candedges,N)); title('candidates left'); colormap(bone); drawnow
        end
    else
        A(i,j)=0; A(j,i)=0; % reject the candidate
    end
    candedges(ed,:)=[];
end
A=double(A); Atriangulated=full(double(Atriangulated));