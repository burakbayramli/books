%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%===============================================
% P1 interpolation on finite elements mesh
% Visualization of the iso-contours
%===============================================

function Interp_P1(ftest,nisol,nfig)

global Ns Nt XYs Refs I123 Reft Nomfic

% Computes the values for the iso-contours
%umax=max(ftest);umin=min(ftest);
%u=umin+[0:nisol-1]*(umax-umin)/(nisol-1);u=u';
updatej(['===>Click ' num2str(nisol) ' times in the domain']);
updatej( '    to choose the location of iso-contours');

for i=1:nisol
   [xx,yy]=ginput(1);
   ik=Find_triang([xx yy]);
   u(i)=sum(ftest(I123(ik,:)'))/3;
end
u=u';u=sort(u);nisol=length(u);
H=[];

% Computes the lines corresponding to iso-contours
figure(nfig);
colormap('default');cols=colormap;ncolor=size(cols,1);
      
for i=1:nisol          % loop on the requested values
   colorloc=cols(1+i*fix((ncolor-1)/nisol),:);
   uloc=u(i);
   updatej(['Iso-value = ' num2str(uloc)]);
   for k=1:Nt          % loop on the triangles
      Iplot=[I123(k,:)';I123(k,1)];
      kplot=0;clear xiso yiso;
      for is=1:3       % loop on the vertices of the triangle
         i1=Iplot(is);i2=Iplot(is+1);a=2;
         if(ftest(i1)-ftest(i2) ~= 0) 
            a=(uloc-ftest(i2))/(ftest(i1)-ftest(i2));
         else
            if(uloc == ftest(i2));a=0;end;
         end
         
         if(a>=0 & a<= 1)
            kplot=kplot+1;
            xiso(kplot)=a*XYs(i1,1)+(1-a)*XYs(i2,1);
            yiso(kplot)=a*XYs(i1,2)+(1-a)*XYs(i2,2);
         end   
      end
      
      if(kplot > 1); h=plot(xiso,yiso);set(h,'Color',colorloc); hold on;end;   
   end
   H=[H;h];
end
legend(H,num2str(u))
hold off
updatej('===>Continue the computation');
updatej('    following the last message before');
updatej('    displaying the iso-lines');
updatej(' ***********************')
