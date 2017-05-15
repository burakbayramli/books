% Image approximation (compression)
keaton=imread('keaton1bw.tif'); %read the image file into a matrix
D=im2double(keaton);

[U,S,V]=svd(D); %singular value decomposition

%display of approximated photo
figure(1)
nn=1;
for k=[10 30 50 100];
   Dk=U(:,1:k)*S(1:k,1:k)*V(:,1:k)';
   subplot(2,2,nn);
   nn=nn+1;
   imshow(Dk); %display
   title(['k=' num2str(k)]);
end;

%scree plot
figure(2)
for n=2:50, %from second eigenvalue
   plot(n,S(n,n),'ko'); hold on;
end
title('scree plot (excluding first eigenvalue)');
xlabel('n'); ylabel('eigenvalue');

%print first eigenvalue
S(1,1) %first eigenvalue

