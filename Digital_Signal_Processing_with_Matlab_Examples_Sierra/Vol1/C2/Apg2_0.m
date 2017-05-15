% Normal PDF with shaded zone
v=-1.3:0.01:1.3;
mu=0; sigma=0.4;
ypdf=normpdf(v,mu,sigma);
plot(v,ypdf); hold on;
axis([-1.5 1.5 0 1.1]);
xlabel('values'); title('normal PDF');
for n=1:2:50,
   plot([v(150+n) v(150+n)],[0 ypdf(150+n)],'g','linewidth',2);
end;

