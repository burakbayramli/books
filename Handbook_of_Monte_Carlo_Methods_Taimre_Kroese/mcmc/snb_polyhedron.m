%snb_polyhedron.m
A=[0,0,-1;-1,0,0;1,-2,0;1,2,1];
A(4,:)=A(4,:)/sqrt(6);
A(3,:)=A(3,:)/sqrt(5);
b=[0;0;0;2/sqrt(6)];
x=[1/2,1/2,0]'; %initial starting point
k=1;          %which constraint is active at x
T=10^3;       %run shake-and-bake 10^3  times
a=A(k,:); data=nan(T,length(x)); %preallocate memory
for t=1:T
    Y1=randn(1,3);
    Y1=Y1./sqrt(sum(Y1.^2));
    Y2=Y1- (a*Y1')*a;
    Y3=Y2/sqrt(sum(Y2.^2));
    R=rand^(1/2);
    d=R*Y3-sqrt(1-R^2)*a;
    lam=(b-A*x)./(A*d');
    % next compute lam_s=lambda_star and update k
    lam(k)=inf; lam(lam<0)=inf; [lam_s,k]=min(lam);
    x=x+lam_s*d'; % update to new hit point
    a=A(k,:);        % new active constraint
    data(t,:)=x; % accumulate data
    Index(t)=k;
end
% display data
plot3(data(:,1),data(:,2),data(:,3),'k.','MarkerSize',10)

