


%                       Program fseries
%
%       This program sums the real Fourier series on [-2,2] for several
%   different functions.  User enters the choice of function f (and 
%   sometimes the choice of sine or cosine series), and the number of
%   terms 'n' to be summed up.  The program lists the coefficients in 
%   expansion, and plots the nth partial sum against the function f.
%   It also computes the mean square error sigma = (1/2L) ||f - f_n||^2.
%   In our case, L = 2.
%       The choices of functions are 
%
%   1) f(x) given on [0,2] is f(x) = x for 0 < x < 1, f(x) = 2-x for 1 < x < 2.
%      User then has choice of making an even or odd extension of f to [-2,2].
%   2) f(x) given on [0,2] is f(x) = 1-x/2.  User has choice of making even
%      or odd extension of f to [-2,2].
%   3) f(x) given on [-2,2] is f(x) = 1 for |x| < 1, f = 0 for 1 < |x| <2.
%   4) f(x) given on [-2,2] is f(x) = x(x+2)(x-2).


m = input('Enter choice of data 1,2,3,or 4.  ')
n = input('Enter the number of terms to be summed.  ') 

N = 1:1:n;

x = -2:.02:2;

%  first choice of data
if m == 1
   disp(' 1 for the odd extension to [-2,2] (sine series)  ')
   disp(' 2 for the even extension to [-2,2] (cosine series)'  )
   mm = input('Enter 1 or 2     ')

%  odd extension of first choice of data
   if mm  == 1
       sumsig = 0
       b = zeros(size(N));
       for k = 1:n
          b(k) = (8.0/(k*pi)^2)*.5*(1-(-1)^k)*(-1)^((k-1)/2);
          sumsig = sumsig +b(k)^2;
       end


        fprintf( 'These are the coefficients b(k) \n\n')
        fprintf( '         k            b(k)       \n\n')
        fprintf( '       %2.0f       %.8f  \n', [N;b]  );

       sigma = 1/3 - .5*sumsig;
   fprintf( ' \n')
   fprintf(' For n = %2.0f, the mean square error sigma = %1.8f\n', n, sigma)
                
        partialsum = 0;
        for k = 1:n
            partialsum = partialsum + b(k)*sin(k*x*pi/2);
        end
        for i = 1:201
           if x(i) < -1
               f(i) = -(2+x(i));
           elseif x(i) < 1
               f(i) = x(i);
           else 
               f(i) = 2-x(i);
           end
         end

         y = zeros(size(x));
         plot(x,f,x,partialsum,'g',x,y,'r')

%    even extension of first choice of data 
     elseif mm == 2
       sumsig = .5;
       a = zeros(size(N));
       for k = 1:n
          a(k) = (8.0/(k*pi)^2)*((-1)^(k/2)-1)*.5*(1+(-1)^k);
          sumsig = sumsig + a(k)^2;
       end
    
        aa = [1,a];

        N = [0,N];

        fprintf('These are the coefficients a(k) \n\n')
        fprintf( '         k            a(k)       \n\n')
        fprintf( '       %2.0f       %.8f  \n', [N;aa]  )
        fprintf( ' \n')
       

        sigma = 1/3 -.5*sumsig;

   fprintf(' For n = %2.0f, the mean square error sigma = %1.8f\n', n,sigma)

       partialsum = .5*ones(size(x));
       for k = 1:n
          partialsum = partialsum +a(k)*cos(k*x*pi/2);
       end


       for i = 1:201
          if x(i) < -1
              f(i) = x(i) + 2;
          elseif x(i) < 0
              f(i) = -x(i);
          elseif x(i) < 1
              f(i) = x(i);
          else 
              f(i) = 2-x(i);
          end
       end
       plot(x,f,x,partialsum,'g')

   end

% begin second choice of data

elseif m == 2 

   disp(' 1 for the odd extension on [-2,2] (sine series) ' )
   disp(' 2 for even extension [-2,2] (cosine series) ')
   mm = input(' Enter 1 or 2  ')

%  odd extension of second choice of data 
   if mm == 1 
      sumsig = 0;
      b = zeros(size(N));
      for k = 1:n
         b(k) = 2.0/(k*pi);
         sumsig = sumsig +b(k)^2;
      end

      fprintf('These are the coefficients b(k)\n \n')
      fprintf('       k           b(k)          \n\n')
      fprintf('     %2.0f        %.8f    \n', [N;b]  )
      fprintf('\n')

      sigma = 1/3 - .5*sumsig;

      fprintf('For n = %2.0f, the mean square error sigma = %1.8f\n', n,sigma)


      partialsum = 0;
      for k = 1:n
         partialsum = partialsum + b(k)*sin(k*x*pi/2);
      end

      for i = 1:201
         if x(i) < 0
             f(i) = -1 -.5*x(i);
         else
             f(i) = 1 - .5*x(i);
         end
      end
      plot(x,f,x, partialsum, 'g')

%  even extension of second choice of data
   elseif mm == 2

       sumsig = .5;
       a = zeros(size(N));
       for k = 1:n
          a(k) = 2*(1-(-1)^k)/(k*pi)^2;
          sumsig = sumsig + a(k)^2;
       end 

       aa = [1.0, a];
       N = [0,N];

       fprintf('These are the coefficients a(k)  \n \n')
       fprintf('       k              a(k)        \n \n')
       fprintf('     %2.0f          %.8f  \n', [N;aa] )
       fprintf('\n')


       sigma = 1/3 - .5*sumsig;

       fprintf('For n = %2.0f the mean square error sigma = %.8f\n', n, sigma)

       partialsum = .5*ones(size(x));
       for k = 1:n
           partialsum = partialsum +a(k)*cos(k*x*pi/2);
       end

       for i = 1:201;
           if x(i) < 0
               f(i) = .5*(x(i)+2);
           else
               f(i) =  1 -.5*x(i);
           end
        end
        plot(x,f,x,partialsum,'g')
% end the logical choice of mm = 1 or mm = 2 for second choice of data
   end

% third choice of data (an even function )
elseif m == 3

    sumsig = .5;
    a = zeros(size(N));
    for k = 1:n
        a(k) = (1/(k*pi))* (-1)^((k-1)/2)*(1-(-1)^k);
        sumsig = sumsig + a(k)^2;
    end


    N = [0,N];
    aa = [1,a];

    fprintf('These are the coefficients a(k)   \n\n')
    fprintf('          k             a(k)      \n\n')
    fprintf('        %2.0f          %.8f       \n', [N;aa])
    fprintf('\n')


    sigma = .5 - .5*sumsig;

    fprintf('For n = %2.0f, the mean square error sigma = %.8f \n', n, sigma)

    partialsum = .5*ones(size(x));
    for k = 1:n
        partialsum = partialsum + a(k)*cos(k*x*pi/2);
    end

    for i = 1:201
       if x(i) < -1
          f(i) = 0;
       elseif x(i) < 1
          f(i) = 1;
       else
          f(i) = 0;
       end
    end
    plot(x,f,x,partialsum,'g')

% begin fourth choice of data (an odd function)

else 

    b = zeros(size(N));
    sumsig = 0;
    for k = 1:n
        b(k) = -12*(2/(k*pi))^3 *(-1)^k;
        sumsig = sumsig + b(k)^2;
    end

    fprintf('These are the coefficients b(k)  \n\n')
    fprintf('         k             b(k)          \n\n')
    fprintf('       %2.0f          %.8f          \n', [N;b])
    fprintf('\n')

    sigma = 512/105 - .5*sumsig;

    fprintf('For n = %2.0f, the mean sqaure error sigma = %.8f\n', n, sigma)
    partialsum = 0;
    for k = 1:n
       partialsum = partialsum + b(k)*sin(k*x*pi/2);
    end

    for i = 1:201
        f(i) = x(i)*(x(i)+2)*(2-x(i));
    end

    plot(x,f,x,partialsum,'g')

end


