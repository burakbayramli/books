function [confide, moms]=fdet_mc3(d, total_order, iter, oxact, draw_tolerance, asize, randstate, alphafine)
%
%[confide, moms]=fdet_mc3(d, total_order, iter, oxact, draw_tolerance, asize, randstate, alphafine)
%
%This function estimates the Log-determinant of (I-a*d) and provides confidence bounds on the estimated log-determinant
%(a is a scalar, and d is an n by n weight matrix). While the estimated log-determinant is valid for a in (-1/min(eig(d)),1), the 
%lower bound computation requires a in (-1,1).
%
%
%INPUT:
%
%The required n by n matrix d can be a symmetric or asymmetric weight matrix. However, maximum modulus of the eigenvalue of d must be 1 or less. 
%A row-stochastic, doubly stochastic scaling will do this. Matrices similar to stochastic matrices will satisfy this as well.
%Finally, one can find the maximum eigenvalue, and scale a candidate weight matrix by this to yield a weight
%maxtrix with maximum eigenvalue of 1. Note, one can find the maximum eigenvalue of a sparse d using the command 
%eigs in a reasonable time for moderately large matrices. 
%
%The optional scalar integer total_order specifies the highest term estimated in the Taylor series (e.g., E(tr(D^total_order)) ). 
%Please specify even order. The default equals 50.
%
%The optional scalar integer iter specifies how many independent realizations of u to use in the estimation of E(tr(D^total_order)).
%The default equals 32.
%
%The optional scalar integer oexact specifies how many exact moments are
%computed. Users can choose 2 or 4. The default is 4 if the number of
%non-zero elements in d is less than 500,000 and 2 if it is above that.
%Users with memory restricted machines may wish to specify oxact=2.
%
%The optional scalar draw_tolerance gives how close (in terms of standard
%deviations) a random normal draw should be so that the error on the estimated first
%trace which equals n(u'du)/(u'u) since tr(d)=0 is relatively low. The
%default is 0.15 standard deviations. This gives about a 12% chance of
%accepting a draw. Lower values of draw_tolerance result in fewer accepted
%draws and longer running times, but increase accuracy. However, past a
%certain point it is better to increase accuracy via iter. This is inspired
%by Zhang, Y. and W.E. Leithead, "Approximate Implementation of Logarithm
%of Matrix Determinant in Gaussian Processes," Journal of Statistical
%Computation and Simulation, forthcoming. This work uses some of the
%structure and results of Barry, Ronald, and R. Kelley Pace, 
%"A Monte Carlo Estimator of the Log Determinant of Large Sparse Matrices," 
%Linear Algebra and its Applications, Volume 289, Number 1-3, 1999, p.
%41-54.
%
%The optional scalar asize gives the size of the confidence limit desired in decimal units. Default is 0.05 (5%). This variable asize
%lie in (0,1). This is a Chebyshev interval, and the actual size is very conservative.
%
%The optional non-negative integer scalar randstate sets the state of the random number generator. Use this if you wish the same
%result each invocation, otherwise the results will vary somewhat each time. This variation should provide some idea of the 
%sensitivity of the results to the approximation.
%
%The optional aiter by 1 vector alphafine gives the evaluation points a(i) for i=1...aiter. The default is an linearly spaced vector
%going from -0.999 to 0.999 (aiter=1,999).  At the moment, the lower bound computation only works for
%a in the interval (-1,1).
%
%
%OUTPUT:
%
%The aiter by 4 matrix confide gives a, lower bound of ln|I-a*d|, an estimated ln|I-a*d|, and upper bound of ln|I-a*d|, 
%where aiter is specified internally (can be edited in this file) or has the same length of the
%optionally specified alphafine. Essentially, the matrix confide supplies the points used in the creation of the grid of 
%stimated log-determinant values along with their confidence limits. 
%
%confide(:,1) gives the evaluation points (values of a).
%
%confide(:,2) gives the lower confidence limit on the log-determinant
%
%confide(:,2) gives the estimated log-determinant
%
%confide(:,4) gives the upper confidence limit on the log-determinant
%
%The optional total_order by 1 vector moms gives the estimated moments
%scaled by n.

%NOTES:
%
%If you use this function, please cite:
%
%Barry, Ronald, and R. Kelley Pace, 
%"A Monte Carlo Estimator of the Log Determinant of Large Sparse Matrices," 
%Linear Algebra and its Applications, Volume 289, Number 1-3, 1999, p. 41-54.
%
%Written by Kelley Pace, www.spatial-statistics.com, on 11/27/99, revised 1/1/03, revised 3/1/07.


%%%%%%%%%%%%%% Default setting %%%%%%%%%%%%%

if (nargin<8)
%alpha vector default
alphafine=(-0.999:0.001:0.999)'; %negative and positive a
%alphafine=(0:0.001:0.999)';%positive a
end

if (nargin<7)
    %default random number seed
    randstate=round(sum(100*clock));
    %randstate=2007;
    rand('state',randstate);
end

if nargin<6
    %default size of test
    asize=0.05;
end


if nargin<5
    %default size of draw_tolerance
    draw_tolerance=0.15;
end


if nargin<4
    %number of exact moments
    if nnz(d)<500000
        oxact=4;
    else
        oxact=2
    end
end

if nargin<3
%iter default
iter=32;
end

if nargin<2
    %total order default
total_order=50;
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%finding the size of the weight matrix
[n,ncols]=size(d);
%finding number of evaluation points
aiter=length(alphafine);

%%%% block of partial input checking %%%%%%%%%%%%%%%

%making sure alphafine is a column vector
      if aiter==1
       error('alphafine needs to be a column vector') 
      end

      if draw_tolerance<0.001 | ~isreal(draw_tolerance)
          error('draw_tolerance should be sufficiently positive real scalar to allow for draws')
      end
    
%transforming a row into a column vector (when necessary)   
      if size(alphafine,2)>1
        alphafine=alphafine';
    end
 
 if length(iter)>1 | rem(iter,1)
        error('iter should be a scalar integer')
    end
    
 if length(total_order)>1 | rem(total_order,1)
       error('total_order should be a scalar integer')
   end
   
   if length(asize)>1 |~isreal(asize)
       error('asize should be a real scalar')
   end
       
 if n~=ncols
     error('weight matrix needs to be n by n')
 end
   
   if length(oxact)>1 | ~((oxact==2)|(oxact==4))
       error('oxact should be a scalar integer equal to 2 or 4')
   end
   
       
   if max(abs(alphafine))>=1 | ~isreal(alphafine)
       error('the real vector alphafine should have a maximum element of less than 1 and a minimum element of greater than -1.0')
   end
   
   if (asize>=1)|(asize<=0)
       error('asize must lie in (0,1)')
   end
   
    if ((randstate<0)|logical( rem(randstate,1)~=0 )|( length(randstate)>1 ) );
       error('randstate must be a non-negative integer scalar');
   end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if oxact==2
% Calculating exact traces of 1st two moments. td=[tr(D) tr(D*D)/2]. Note, tr(D) should be equal to 0.
t1=0;
t2=sum(sum(d.*(d')));
tvec=[t1;t2];
td=full([t1;t2/2]);
end

if oxact==4
    % Calculating exact traces of 1st four moments. td=[tr(D) tr(D*D)/2 tr(D^3)/3 tr(D^4)/4]. Note, tr(D) should be equal to 0.
t1=0;
t2=sum(sum(d.*(d')));
d2=d*d;
t3=sum(sum(d.*(d2')));
t4=sum(sum(d2.*(d2')));
tvec=[t1;t2;t3;t4];
td=full([t1;t2/2;t3/3;t4/4]);
end
    

%The scalar total_order specifies total number of moments and is comprised of 2 exact moments, tr(D),tr(D*D)
%and o-2 stochastic moments u'(D^j)u/(u'u).
%This block accumulates stochastic moments from order 1 to total order in columns
%and independent realiztions of the stochastic moments in rows.
mavmomi=zeros(total_order, iter);
momis=zeros(total_order, iter);
for j=1:iter;

%This code segment selects draws that come close to giving the correct answer for the known first moment (tr(d)=0).    
std_m1=sqrt(2*t2/(n*n));%standard deviation of first moment
stand=std_m1*draw_tolerance;%setting standard of  draw_tolerance standard deviations 
delta1v=100;%setting a value above stand to get loop started
u=randn(n,1);%random draw that occurs in case stand is so high that no loops occur
while abs(delta1v/n)>stand;%allowing only draws that are within stand standard deviations of correct value
u=randn(n,1);%random draw
utu=u'*u;%inner product
du=d*u;%du
delta1v=n*u'*du/utu;%estimated first moment
end    
    
    
v=u;
utu=u'*u;
for i=1:total_order;
v=d*v;
momis(i,j)=n*((u'*v)/(utu));
mavmomi(i,j)=momis(i,j)/i;
end;
end;

%this substitutes in the first two exact moments for the first two stochastic moments
mavmomi(1:length(td),:)=td(:,ones(iter,1));

%averages across iterations
avmomi=mean(mavmomi')';

%averages across iterations
momsall=mean(momis')';
momsall(1:length(tvec))=tvec;
momsout=momsall/n;

clear u,v; %minor memory savings

%weighting moments by respective powers of a
seq_order=(1:total_order);
oiter=ones(total_order,1)./seq_order';
even_ind=((-1).^seq_order>0)'./seq_order';
srvs=zeros(iter, aiter);
sum_all_weights=zeros(aiter, 1);
sum_even_weights=zeros(aiter,1);
for ii=1:aiter
    wvec=(alphafine(ii).^seq_order);
srvs(:,ii)=-(wvec*mavmomi)'; 
sum_all_weights(ii)=wvec*oiter;
sum_eve_weights(ii)=wvec*even_ind;
end

%Estimated ln|I-aD| using mixture of exact, stochastic moments
%exact from 1 to oexact, stochastic from (oexact+1) to total_order
lndetmat=mean(srvs)'; %average of independent realizations of Krylov space
sderr=(std(srvs)/sqrt(iter))'; %standard error of independent realizations of Krylov space

%lower bound computation
%%The Linear Algebra and its Applications paper uses the following:
%fbound=((n*alpha.^(total_order+1))./((total_order+1)*(1-alpha)))';

%However, for matrices with max(abs(eigenvalue)) equal to or less than 1,
%and an even order_total, the following bound is tighter. 

last_moment=avmomi(end);
posind=(alphafine>=0);

altlowpos=last_moment*(log(1-alphafine)+sum_all_weights);
altlowneg=last_moment*0.5*(log(1-alphafine.*alphafine)+sum_even_weights);
fbound=posind.*altlowpos+(1-posind).*altlowneg;

%confidence limits, with lower limit biased downward (more conservative)
%The paper used a normal approximation or a t for small iter.
%Using Chebyshev's Theorem should yield a very conservative asize% confidence interval. 
cfactor=sqrt(1/asize);

%confidence limits of size asize
low_asize=(lndetmat-cfactor*sderr+fbound);
high_asize=(lndetmat+cfactor*sderr);

%AR parameter, lower confidence limit, estimated log-det, upper confidence limit
confide=[alphafine low_asize lndetmat high_asize];

if (nargout>1)
    moms=momsout;
end
    

