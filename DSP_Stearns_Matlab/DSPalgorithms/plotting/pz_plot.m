function pz_plot(b,a)
% pz_plot(b,a)
% Plots poles and zeros on the z plane.
% b and a are row-vector coefficients in
%
%        b(1)+b(2)*z^(-1)+...+b(N)*z^(-(N-1))
% H(z) = ------------------------------------
%        a(1)+a(2)*z^(-1)+...+a(M)*z^(-(M-1))
%
% b and a can have multiple rows if H(z) is in cascade form.
% zeros ('o') and poles ('x') are plotted on the z plane.
% Modified - thanks to Peter Deng.

[Nsect,N]=size(b);
[Nsecta,M]=size(a);
if(Nsect~=Nsecta)
   error('pz_plot error: b and a must have same no. rows.')
end

% Plot the coordinates.
circle=exp(j*2*pi*linspace(0,1,1000));
cla; plot(circle,'k'); hold on;
n=linspace(-1,1,1000);
Ncircles=5;
for c=2:Ncircles
   plot(circle*(c-1)/Ncircles, ':k')
end
Nlines=4;
for q=1:Nlines
   plot(n*cos(pi*q/Nlines),n*sin(pi*q/Nlines),':k')
end

% Plot the zeroes. lim= outer limit of plot axes.
lim=1;
for s=1:Nsect
   if (N>1 & abs(b(s,1))>0),
      z=roots(b(s,:)).';
      lim=max([lim,abs(real(z)),abs(imag(z))]);
      for n=1:N-1
         hz(n)=plot(real(z(n)),imag(z(n)),'ok');
         set(hz(n),'MarkerSize',12);
         set(hz(n),'LineWidth',1.5);   
      end
   end
end

% Plot the poles.
for s=1:Nsect
   if (M>1 & abs(a(s,1))>0),
      p=roots(a(s,:)).';
      lim=max([lim,abs(real(p)),abs(imag(p))]);
      for n=1:M-1
         hp=plot(real(p(n)),imag(p(n)),'xk');
         set(hp,'MarkerSize',12);
         set(hp,'LineWidth',1.5);   
      end
   end
end

% Label the axes.
set(gca,'fontname','times','fontsize',14)
xlabel('Re({\itz})');
ylabel('Im({\itz})');

% Adjust the axes so |z|=1 is a circle.
axis([-lim,lim,-lim,lim]);
axis square
hold off;
