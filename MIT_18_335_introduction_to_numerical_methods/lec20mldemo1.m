% MIT 18.335 - Lecture 20 MATLAB Demo 1
% Elimination movie
% Per-Olof Persson, November 26, 2007

clf
pos=get(gcf,'pos');
pos(3)=pos(4)*2;
set(gcf,'pos',pos);

pausetime=0.5;

for method=1:5
  N=10;
  A=sparse(toeplitz([2,-1,zeros(1,N-2)]));
  I=speye(N,N);
  B=kron(A,I)+kron(I,A);
  [x,y]=ndgrid(1:N,1:N);
  B0=B;
  L0=chol(B0)';
  
  switch method
   case 1
    p=1:N^2;
    methodstr='Natural Ordering';
   case 2
    p=symrcm(B);
    methodstr='Reverse Cuthill-McKee';
   case 3
    p=realmmd(B);
    methodstr='Minimum Degree';
   case 4
    p=symamd(B);
    methodstr='SYMAMD (MATLAB Default)';
   case 5
%    %    NEEDS MESHPART FOR GENERAL CASE!!!
%    p=ndperm('coordpart',8,B,[x(:),y(:)]);

%    %    PRECOMPUTED (FOR 10-BY-10 ONLY)
    p=[13 12 11 1 2 31 41 42 32 21 22 4 15 25 24 35 34 44 45 3 14 23 33 43 61 62 72 73 81 91 92 71 82 64 65 75 85 95 94 84 63 74 83 93 51 52 53 54 55 6 7 18 27 37 48 17 28 38 9 10 20 30 40 50 8 19 29 39 49 57 67 78 87 97 98 77 88 69 70 80 90 100 68 79 89 99 47 58 59 60 5 16 26 36 46 56 66 76 86 96];

    methodstr='Nested Dissection';
  end

  B=B(p,p);
  R=chol(B);
  xy=[x(p);y(p)]';
  Bplt=B;
  
  labelpars={'fontname','helvetica','fontsize',16};
  
  nflops=0;
  n=size(B,1);
  L=zeros(n);
  lpars={'marker','.','linestyle','none','markersize',64};
  tpars={'fontname','helvetica','fontsize',16,'horiz','center','col','g'};
  for i=1:n
    subplot(1,2,2)
    spy(Bplt,16)
    %  xlabel(['nz = ' int2str(nnz(Bplt))],labelpars{:});
    xlabel(['nz = ' int2str(nnz(Bplt))],'fontname','helvetica','fontsize',25);
    
    subplot(1,2,1)
    cla
    gplot(B,xy);
    line(xy(i:N^2,1),xy(i:N^2,2),'color','k',lpars{:});
    for j=i:n
      degree=length(find(B(:,j)))-1;
      text(xy(j,1),xy(j,2),int2str(degree),tpars{:});
    end
    axis equal,axis off,axis([1,N,1,N])
    
    texthandle=text(5.5,-.25,['flops = ',int2str(nflops)],'fontname','helvetica', ...
                    'fontsize',25,'horiz','center');
    
    titlehandle=text(11.5,11.2,methodstr,'fontname','helvetica', ...
                     'horiz','center','fontsize',32);
    
    pause(pausetime);
    if i==1
      pause
    end
    
    subplot(1,2,2)
    Btmp=sparse(N^2,N^2);
    Btmp(i,i:N^2)=Bplt(i,i:N^2);
    Btmp(i:N^2,i)=Bplt(i:N^2,i);
    %  hold on, spy(Btmp,'r',16); hold off
    hold on, spy(Btmp,'r',25); hold off
    if i==n
      xlabel(['nz = ' int2str(nnz(Bplt))],'fontname','helvetica','fontsize',25); 
    else
      xlabel(['nz = ' int2str(nnz(Bplt))],labelpars{:});
    end
    
    subplot(1,2,1)
    line(xy(i,1),xy(i,2),'color','r',lpars{:});
    
    delete(texthandle);
    if i==n
      texthandle=text(5.5,-.25,['flops = ',int2str(nflops)],'fontname','helvetica', ...
                      'fontsize',25,'horiz','center');
    else
      texthandle=text(5.5,-.1,['flops = ',int2str(nflops)],'fontname','helvetica', ...
                      'fontsize',16,'horiz','center');
    end
    
    pause(pausetime);
    
    degree=length(find(B(:,i)))-1;
    nflops=nflops+2*degree^2;
    
    L(i,i)=sqrt(B(i,i));
    L(i+1:n,i)=B(i+1:n,i)/L(i,i);
    B(i+1:n,i+1:n)=B(i+1:n,i+1:n)-L(i+1:n,i)*L(i+1:n,i)';
    Bplt(i+1:n,i+1:n)=Bplt(i+1:n,i+1:n)-L(i+1:n,i)*L(i+1:n,i)';
    B(i, i:n)=0;
    B(i:n, i)=0;
    Bplt(i+1:n,i)=0;
  end
  pause
end
