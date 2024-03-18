function [y,dt] = bwe1(fun,y0,dt,tol,ntim,itmax,S)
% Solve fun(t,y)=0 by
% time-stepping (yn+1-yn)=Dt fun(yn+1)

global ImpVar
a=ImpVar.a;

warning('off');

n  = length(y0);
t  = 0;
y  = y0;
f0 = feval(fun,0,y);
thresh = 1e-8;
fac = [];
g = [];
vectorized = 0;

rhomin = 1e-3;
pmin   = 10;
gamma  = 1.4;
invalid =1;
for k = 1:ntim;
    disp('new step')
    dt
    while invalid
        f0 = feval(fun,0,y);
        [jac,fac,g,nfevals] = numjacJO(fun,t,y,f0,thresh,fac,vectorized,S,g);
        jac = eye(n)-dt*jac;
        [l,u] = lu(jac);
        rat = 0.5;
        it  = 0;
        newjac = 1;
        gjord = 0;
        res0 = norm(y - y0 - dt*f0,2)/sqrt(n);
        nc  = 2*tol;
        ncold = 2*rat*nc;
        while (nc > tol) && (it < itmax)
            f0   = feval(fun,0,y);
            res  = y - y0 - dt*f0;
            corr = u\(l\res);
            nc   = norm(corr,2)/sqrt(n);
            it   = it + 1;
            disp([it log10(nc)])
            if (nc<rat*ncold) || (it == 1) || newjac
                y = y - corr;
                W=reshape(y,length(y)/3,3);
                W(:,1)=max(W(:,1),rhomin.*a);
                W(:,3)=max(W(:,3),(pmin+(gamma-1)*((W(:,2)./a).^2).*(a./W(:,1))).*a);
                y=W(:);
                ncold  = nc;
                newjac = 0;
            else
                if gjord == 3;%round(sqrt((length(y0)/3))/2)
                    it=itmax;
                    %disp('Hoppsan nu gick det inte med en ny');
                    break
                end
                disp('newjac')
                newjac = 1;
                gjord  = gjord+1;
                [jac,fac,g,nfevals] = numjacJO(fun,t,y,f0,thresh,fac,vectorized,S,g);
                jac   = eye(n)-dt*jac;
                [l,u] = lu(jac);
                ncold = nc;
            end
        end
        if it == itmax
            disp('red dt');
            dt=dt/2;
            y=y0;
            invalid=1;
            %disp(['Timestep reduced to: ', num2str(dt), 'sec']);
        elseif it < 0.7*itmax
            dt=dt*2;
            invalid=0;
            %disp(['Timestep increased to: ', num2str(dt), 'sec']);
        end
    end
end
writejac = 1;
if writejac
    jacw = eye(n)-jac;
    save jacfile jacw
end
