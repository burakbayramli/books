
NFrames = 400;
mx = 2000;
dim = 1;
qxt = zeros(mx,NFrames+1);
txt = zeros(1,NFrames+1);
for Frame=0:NFrames
   [qdata,t] = readamrdata(dim,Frame);
    % store data for x-t plot:
    qq = qdata(1).data;
    qxt(:,Frame+1) = qq(1,:)';
    txt(:,Frame+1) = t;
    end

xlim = [0 1];
tlim = [0 max(txt)];
figure(1)
clf
qmax = max(max(qxt));

dx = qdata(1).dx;
x = dx/2 : dx : 1-dx/2;

% for pressure:
% cline = [0 2:4:100 110:20:1000];

% for density:
cline = [.04:.04:.96 1:.5:22];

contour(x,txt,qxt',cline)
colormap([0 0 1])
axis([xlim tlim])


   

