function [X partA partb] = carp(A,b,p,K,x0)
%CARP Component Averaging Row Projection
%
%   [X info restart] = cav(A,b,K)
%   [X info restart] = cav(A,b,K,x0)
%   [X info restart] = cav(A,b,K,x0,options)
%
% Implements the CAV methodfor the linear system Ax = b:
%
%       x^{k+1} = x^k + lambda_k*A^T*M*(b-A*x^k)
%
% where M = diag(w_i/||a^i||_S^2, S = diag(s_j), s_j denotes the number
% of nonzero elements in column j, and w_i are weights (default: w_i = 1).
%
% Input:
%   A          m times n matrix.
%   b          m times 1 vector containing the right-hand side.
%   K          Number of iterations. If K is a scalar, then K is the maximum
%              number of iterations and only the last iterate is saved.
%              If K is a vector, then the largest value in K is the maximum
%              number of iterations and only iterates corresponding to the
%              values in K are saved, together with the last iterate.
%              If K is empty then a stopping criterion must be specified.
%   x0         n times 1 starting vector. Default: x0 = 0.
%   s           The number of sources in the right side of the domain.
%   p           The number of receivers (seismographs) equally spaced on
%   options    Struct with the following fields:
if nargin < 3
    error('Too few input arguments')
end

[m n] = size(A);

% Check that the sizes of A and b match.
if size(b,1) ~= m || size(b,2) ~= 1
    error('The size of A and b do not match')
end

if nargin < 5
    % Default value for x0.
    x0 = zeros(n,1);
end

% Check if x0 is empty.
if isempty(x0)
    x0 = zeros(n,1);
elseif size(x0,1) ~= n || size(x0,2) ~= 1
    % Check the size of x0.
    error('The size of X0 does not match the problem')
end

% AT = A';
% rxk = b - A*x0;

%Should add options and stopping criteria later
if (mod(m,p) ~= 0)
    error('Matrix cannot be partitioned')    
end

%partitioning the matrix

[partA partb sindx] = partition(A,b,p);



%check for which coloumn is completley zeros.
% indx = find(all(A == 0));
% j = 1;
% X = A(1:m1:m,:); 
kk = 1;
x_temp = zeros(n,1);
X = zeros(n,max(K));
x_final = zeros(n,p);
while(kk < max(K))
    x0 = x_temp;
   
    for j = 1:p
         tempS=sparse(squeeze(partA(j,:,:)));
    ind = find(all(tempS == 0));
%        x0(ind)=0;
        xKacz = kaczmarz(sparse(squeeze(partA(j,:,:))),partb(j,:)',2,x0);
        x_final(:,j) = xKacz;
%          x_final(ind,j) = 0;
    end
    
    
    tempsum = (sum(x_final,2))/p;
     x_temp = tempsum;
%     count = 0;
%     for ii =  1:n
%         if(sindx(ii) > 1)
%             x_temp(ii,1) = tempsum(ii)/sindx(ii);
%             
%         elseif(sindx(ii) == 1)
%             x_temp(ii,1) = tempsum(ii);
%             x_final(1,:);
%             ii;
%         else
%             x_temp(ii,1) = 0;
%             count = count +1;
%         end
%         
%     end
    kk = kk+1;
%    norm(x_temp-x);
% norm(A*x_temp - b)
X(:,kk) = x_temp';
end
end
