%nonhommjp.m
q12 = @(t) sin(t)^2;
q21 = @(t) 1 + sin(t);
q23 = @(t) 1 - cos(t);
q2 = @(t) q21(t) + q23(t);
f1 = @(t,Tn) exp(-t/2 + (-sin(2*Tn) + sin(2*(t+Tn)))/4)*sin(t+Tn)^2;
f2 = @(t,Tn) exp(-2*t-cos(Tn)+cos(t+Tn)-sin(Tn)+sin(t+Tn))*...
    (2-cos(t+Tn)+sin(t+Tn));
y=1; tn=0; n=0; yy=[y]; tt=[tn];
while y ~= 3
    if y==1
        accept=false;
        while ~accept
            A = -log(rand)*2;
            accept = rand < f1(A,tn)/exp(-(A-1)/2);
        end
        tn = tn + A; y = 2;
    else %y==2
        accept =false;
        while ~accept
            A = -log(rand)/2;
            accept=rand<f2(A,tn)/(exp(-2*(A-sqrt(2)))*(2+sqrt(2)));
        end
        tn = tn + A;
        if rand < q21(tn)/q2(tn)
            y =1;
        else
            y=3;
        end
    end
    yy = [yy,y]; tt = [tt,tn]; n = n+1;
end
% for plotting
for i=1:n
    line([tt(i),tt(i+1)],[yy(i),yy(i)],'Linewidth',3);
    line([tt(i+1),tt(i+1)],[yy(i),yy(i+1)],'Marker','.','LineStyle',':');
end
