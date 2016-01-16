function [partA partb sindx] = partition(A,b,p)
[m,n] = size(A);
m1 = m/p;
sindx = p*ones(1,n);
partA = zeros(p,m1,n);
partb = zeros(p,m1);


% Partitioning the rays based on station.
for j = 1:p
    partA(j,:,:)=A(j:p:m,:);
    partb(j,:) = b(j:p:m);
    tempS=A(j:p:m,:);
    indx = find(all(tempS == 0));
    for ii = 1:size(indx,2)
        sindx(indx(ii)) = sindx(indx(ii))-1;
    end    
end

end