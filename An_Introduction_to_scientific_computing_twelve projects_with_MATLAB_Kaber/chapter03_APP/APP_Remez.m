%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise 3.11
function APP_remez(n)
%Computation by the Remez algorithm
%of the best uniform polynomial approximation
%in P_n of the function on I=[0 1]
%
%Initialization 
I=linspace(0,1,100);tol =1.e-8;iteraMax=100;
x=.5*(1-cos(pi*(0:n+1)'/(n+1)));  %Chebyshev points 
%x=(0:n+1)'/(n+1);                %uniform case
%x=sort(rand(n+2,1));             %random case
itera=1;converge=0;
while (itera<iteraMax)&(~converge)
    cfp=APP_equiosc(x);
    cfp=cfp(end:-1:1);%ordering of the coefficients
    %Is it the right polynomial ?
    fminuspI=f(I)-polyval(cfp,I);
    [lemax,imax]=max(abs(fminuspI));
    fminusp=f(x)-polyval(cfp,x);
    if abs(lemax-max(fminusp))< tol
       fprintf('The Remez Algorithm converges in %i iterations\n',itera)
       converge=1;
       hold off;figure(1);plot(I,f(I)-polyval(cfp,I),'g')
       hold on;plot(x,polyval(cfp,x)-f(x),'-+')
       hold off;figure(2);plot(I,f(I),I,polyval(cfp,I),'+','LineWidth',2,'MarkerSize',10)
       set(gca,'XTick',0:.2:1,'FontSize',24)
    else 
       %replace one of the points $x_i$ by the point I(imax)
       xnew=I(imax);
       [m,j]=max(x>xnew);%x(j-1)<= xnew < x(j)
       fminuspxnew=fminuspI(imax);
       if (m==0)    
          if fminusp(end)*fminuspxnew <0
            x(1:end-1)=x(2:end);x(end)=xnew;
          else
            x(end)=xnew;
          end;
       else
          if j==1
             if fminusp(1)*fminuspxnew <0
                x(2:end)=x(1:end-1);x(1)=xnew;
             else
                x(1)=xnew;
             end;
          else 
             if fminusp(j-1)*fminuspxnew <0
                x(j)=xnew;
             else
                x(j-1)=xnew;
             end; 
          end;
       end;
    end;
    itera=itera+1;
end;
if itera==iteraMax 
   fprintf('No convergence after  %i iterations\n',itera)
end;
