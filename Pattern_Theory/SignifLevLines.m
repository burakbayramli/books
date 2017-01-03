I=imread('INRIA3.tif'); %load your image
ss = size(I);
J = double(I);
% If needed, first filter the image with a Gaussian kernel:
%J = imfilter(J,fspecial('gaussian',[15 15],min(ss)/250),'replicate'); 

% calculate centered gradients, their size, size-ranks and orientation
gradx=([J(3,:); J(3:end,:); J(end,:)]-[J(1,:); J(1:end-2,:); J(end-2,:)])/2;
grady=([J(:,3) J(:,3:end) J(:,end)]-[J(:,1) J(:,1:end-2) J(:,end-2)])/2;
grad = sqrt(gradx.^2+grady.^2);
[g1,p]=sort(grad(:),'descend'); [p1,rk]=sort(p); rk = reshape(rk,size(grad));
energy = -log(rk/(ss(1)*ss(2)));
% angle = atan2(gradx, grady);
disp(sprintf('\n1.Pre-computations on image gradients done'))

greystep=50;
%extract level lines with the greystep discretization of grey levels
C=contourc(double(J),10:greystep:240); 
seglen = sqrt(diff(C(1,:)).^2 + diff(C(2,:)).^2); 

% Find all open and closed level lines and 
% put min(grad) at beginning of closed level lines
 
LL=0;  % carries total number of possible segments
ll = []; % carries starting indices of components of level line
a=1; 
while (a <= size(C,2))
    len = C(2,a);
    ll = [ll a];
    if C(:,a+1) == C(:,a+len) % closed level line
        LL = LL+len^2-3*len+3;
        ind = (a+1):(a+len);
        g = grad(sub2ind(ss, round(C(2,ind)), round(C(1,ind))));
        k = min(find(g==min(g)));
        C(:,ind) = C(:,[(a+k):(a+len) (a+2):(a+k)]);
    else
        LL = LL+len*(len-1)/2;  % open level line
    end
    a=a+len+1;
end
thresh = log(1000*LL);
disp(sprintf('2. Level lines checked, LL=%d, thresh = %f', LL, thresh))

% Find their optimal pieces of level lines
a=1; S = [];
for a = ll
    len = abs(C(2,a)); 
    reallen = seglen((a+1):(a+len-1));
    ind=sub2ind(ss,round(C(2,(a+1):(a+len))), round(C(1,(a+1):(a+len))));
    myenergy = energy(ind); 
    bb = bestblocks(myenergy, reallen);
    pass = find(bb(3,:)>thresh); 
%     tmp = abs(diff(angle(ind2))); 
%     maxcurv = max(min([tmp;2*pi-tmp]));
    n = size(pass,2);
    S = [S [a*ones(1,n); a+bb(1:2,pass); bb(3,pass); zeros(1,n)]];
%     if len > 5
%         figure(2), hold off,
%         imagesc(I), colormap gray, axis equal, hold on
%         plot(C(1,(a+1):(a+len)), C(2,(a+1):(a+len)),'b','LineWidth',1)
%         plot(C(1,a+1),C(2,a+1),'*r')
%         for k=1:size(bb,2)
%             plot(C(1,(a+bb(1,k)):(a+bb(2,k))), C(2,(a+bb(1,k)):(a+bb(2,k))),'y','LineWidth',2)
%         end
%         pause
%     end
end
[s1,q] = sort(S(4,:),'descend');
S = S(:,q);
disp(sprintf('3. Bestblocks done, found %d segments', size(S,2)))

k=1; K = zeros(size(I));
elmt = strel('disk', round(max(ss)/100));
while k <= size(S,2)
    a = S(:,k);
    ind = a(2):a(3);
    ind2 = sub2ind(ss,round(C(2,ind)), round(C(1,ind)));
    K1 = zeros(ss); K1(ind2)=1;
    if K(ind2)==1
        S(5,k)=1;
    else
        K = K | imdilate(K1, elmt);
    end
    k = k+1;
end
disp(sprintf('4. Morphological filtering I done, %d redundant segments eliminated',sum(S(5,:))))
S = S(:,S(5,:)==0); 
kk = size(S,2);

k=1; K = zeros(ss);
while k <= kk
    a = S(:,k);
    ind = a(2):a(3);
    ind2 = sub2ind(ss,round(C(2,ind)), round(C(1,ind)));
    K1 = zeros(ss); K1(ind2)=1;
    K1 = imdilate(K1, elmt);
    indbeg = S(2,1:kk); indend = S(3,1:kk);
    ind2beg = sub2ind(ss,round(C(2,indbeg)), round(C(1,indbeg)));
    ind2end = sub2ind(ss,round(C(2,indend)), round(C(1,indend)));
    for n = find(K1(ind2beg)==1 & K1(ind2end)==1)
        b = S(:,n);
        ind = b(2):b(3);
        ind2 = sub2ind(ss,round(C(2,ind)), round(C(1,ind)));
        if (sum(K1(ind2)==0)==0) & (n ~=k)
            S(5,n) = 1;
        end
    end
    k = k+1;
end
disp(sprintf('5. Morphological filtering II done, %d redundant segments eliminated',sum(S(5,:))))
S = S(:,S(5,:)==0);
disp(sprintf('There are %d segments of level lines left',size(S,2)))

%Plot the significant segments of level lines in a sequential way 
%and print their log-significance 
figure(1), hold off, imagesc(I), colormap gray, axis equal, hold on
for k=1:size(S,2)
    a = S(:,k);
    cont = a(2):a(3); len = abs(C(2,a(1)));
    ind2 = sub2ind(ss,round(C(2,cont)), round(C(1,cont)));
    plot(C(1,cont), C(2,cont),'y','LineWidth',1)
    title(sprintf('log-significance %f, segnum %d',a(4),k))
    pause
end


