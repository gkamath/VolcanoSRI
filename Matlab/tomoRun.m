
% In this program a seismic tomography is generated using 
% AIR Tools http://www2.compute.dtu.dk/~pcha/AIRtools/
% and later solved using ART methods.

clear all
close all
clc

% Initialization

N = 32; % Scalar denoting the number of discretization intervals
s = 32; % Number of source in the right side of the domain
p = 32; %Number of reciever in the left side of the domain

isDisp = 0; %pause second between generation

%Generate the tomography
[A b x s p] = seismictomo(N,s,p,isDisp);
disp('Generation of Ax=b done')

%Plotting the magma i.e x
figure;imagesc(reshape(x,32,32));

disp('Plotting the ground-truth magma or fault region');
%% Solving for x using different methods
%Below are Simulation for Different Methods.

%Test for convergence

%% Kaczmarz
maxIter = 200; %maximum number of iteration
iteration = 1:maxIter; %vector sequence for all the iteration needs to be saved
x0 = zeros(N*N,1);
options.lambda = 1;
%Solving for x using ART
xKacz = kaczmarz(A,b,iteration,x0,options);

magma = reshape(xKacz(:,maxIter),N,N);
pcolor(magma);

resKacz = zeros(1,maxIter-1);
relKacz = zeros(1,maxIter-1);
errKacz = zeros(1,maxIter-1);
for i=1:maxIter-1
    relKacz(i) = norm(xKacz(:,i)-xKacz(:,i+1))/norm(xKacz(:,i));
    resKacz(i) = norm(A*xKacz(:,i)-b);
    errKacz(i) = norm(x-xKacz(:,i));
end

disp('Kaczmarz Done');
%% RandKaczmarz
maxIter = 200; %maximum number of iteration
iteration = 1:maxIter; %vector sequence for all the iteration needs to be saved

%Solving for x using ART
xRandkacz = randkaczmarz(A,b,iteration);

magma = reshape(xRandkacz(:,maxIter),N,N);
pcolor(magma);

resRandkacz = zeros(1,maxIter-1);
relRandkacz = zeros(1,maxIter-1);
errRandkacz = zeros(1,maxIter-1);
for i=1:maxIter-1
    relRandkacz(i) = norm(xRandkacz(:,i)-xRandkacz(:,i+1))/norm(xRandkacz(:,i));
    resRandkacz(i) = norm(A*xRandkacz(:,i)-b);
    errRandkacz(i) = norm(x-xRandkacz(:,i));
end

disp('RandKaczmarz Done');
%% CAV
maxIter = 200; %maximum number of iteration
iteration = 1:maxIter; %vector sequence for all the iteration needs to be saved

%Solving for x using ART
xCav = cav(A,b,iteration);

magma = reshape(xCav(:,maxIter),N,N);
pcolor(magma);

resCav = zeros(1,maxIter-1);
relCav = zeros(1,maxIter-1);
errCav = zeros(1,maxIter-1);
for i=1:maxIter-1
    relCav(i) = norm(xCav(:,i+1))/norm(xCav(:,i));
    resCav(i) = norm(A*xCav(:,i)-b);
    errCav(i) = norm(x-xCav(:,i));
end

disp('CAV Done');
%% Cimmino's
maxIter = 200; %maximum number of iteration
iteration = 1:maxIter; %vector sequence for all the iteration needs to be saved

%Solving for x using ART
xCimmino = cimmino(A,b,iteration);

magma = reshape(xCimmino(:,maxIter),N,N);
pcolor(magma);

resCimmino = zeros(1,maxIter-1);
relCimmino = zeros(1,maxIter-1);
errCimmino = zeros(1,maxIter-1);
for i=1:maxIter-1
    relCimmino(i) = norm(xCimmino(:,i+1))/norm(xCimmino(:,i));
    resCimmino(i) = norm(A*xCimmino(:,i)-b);
    errCimmino(i) = norm(x-xCimmino(:,i));
end

disp('Cimmino Done');
%% SART
maxIter = 200; %maximum number of iteration
iteration = 1:maxIter; %vector sequence for all the iteration needs to be saved

%Solving for x using ART
xSart = sart(A,b,iteration);


resSart = zeros(1,maxIter-1);
relSart = zeros(1,maxIter-1);
errSart = zeros(1,maxIter-1);
for i=1:maxIter-1
    relSart(i) = norm(xSart(:,i+1))/norm(xSart(:,i));
    resSart(i) = norm(A*xSart(:,i)-b);
    errSart(i) = norm(x-xSart(:,i));
end

disp('Sart Done');
%% 


figure;
semilogy(resKacz,'bd-','LineWidth',2, 'MarkerSize',6);
hold on
semilogy(resRandkacz,'gs-','LineWidth',2, 'MarkerSize',6);
hold on 
semilogy(resCav,'rx-','LineWidth',2, 'MarkerSize',6);
hold on
semilogy(resCimmino,'cs-','LineWidth',2, 'MarkerSize',6);
hold on
semilogy(resSart,'ms-','LineWidth',2, 'MarkerSize',6);
legend('Kaczmarz','RandKaczmarz','Cav','Cimmino','Sart');
xlabel('Iteration','fontsize',24,'fontname','Times New Roman');

ylabel('Residual','fontsize',24,'fontname','Times New Roman');
% set(gca,'FontSize',24,'fontname','Times New Roman');

figure;
plot(relKacz,'bd-','LineWidth',2, 'MarkerSize',6);
hold on
plot(relRandkacz,'gs-','LineWidth',2, 'MarkerSize',6);
hold on
plot(relCav,'rs-','LineWidth',2, 'MarkerSize',6);
hold on
plot(relCimmino,'cs-','LineWidth',2, 'MarkerSize',6);
hold on
plot(relSart,'ms-','LineWidth',2, 'MarkerSize',6);
legend('Kaczmarz','RandKaczmarz','Cav','Cimmino','Sart');
xlabel('Iteration','fontsize',24,'fontname','Times New Roman');

ylabel('Relative Updates','fontsize',24,'fontname','Times New Roman');

figure;
semilogy(errKacz,'bd-','LineWidth',2, 'MarkerSize',6);
hold on
semilogy(errRandkacz,'gs-','LineWidth',2, 'MarkerSize',6);
hold on 
semilogy(errCav,'rx-','LineWidth',2, 'MarkerSize',6);
hold on
semilogy(errCimmino,'cs-','LineWidth',2, 'MarkerSize',6);
hold on
semilogy(errSart,'ms-','LineWidth',2, 'MarkerSize',6);
legend('Kaczmarz','RandKaczmarz','Cav','Cimmino','Sart');
xlabel('Iteration','fontsize',24,'fontname','Times New Roman');

ylabel('Error','fontsize',24,'fontname','Times New Roman');

figure
for i = 1:100
    imagesc(reshape(xKacz(:,i),32,32));
    pause(0.1)
end
%% Multigrid

