% =========================================================================
% The role of size distribution
% A hypothetical world where the two equipments are the same.
clear all;
load('./Results/benchmark_new.mat');
aggyd = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc = sum(yc(indzhatc:end).*zprob(indzhatc:end));
aggy  = mud*aggyd + (1-mud)*aggyc;
ksid_0 = -3.4114;
ksid_1 = -0.3636;
ez_nt = exp(ksid_0)*yd(indzhatd:end).^(1+ksid_1);
agge_nt_bench = sum(ez_nt.*zprob(indzhatd:end));
aggi_nt_bench = agge_nt_bench/aggyd;
clearvars -except ez_nt agge_nt_bench aggi_nt_bench 

% load('./Results/notax_new.mat');
load('./Results/regulation_new.mat');
aggyd = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc = sum(yc(indzhatc:end).*zprob(indzhatc:end));
aggy  = mud*aggyd + (1-mud)*aggyc;
ksid_0 = -3.4114;
ksid_1 = -0.3636;
ez_nt = exp(ksid_0)*yd(indzhatd:end).^(1+ksid_1);
agge_nt_notax = sum(ez_nt.*zprob(indzhatd:end));
aggi_nt_notax = agge_nt_notax/aggyd;

ez_dirty = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d = sum(ez_dirty.*zprob(indzhatd:indztilded-1));
agge_c = sum(ez_clean.*zprob(indztilded:end));
agge = agge_d + agge_c;
aggi = agge/aggyd;

totrede = agge/agge_nt_bench;
prodrede = agge_nt_notax/agge_nt_bench;

totredi = aggi/aggi_nt_bench;
prodredi = aggi_nt_notax/aggi_nt_bench;
disp('The Role of Technology')
disp(1-(1-prodredi)/(1-totredi))

% Plot the tax function
clear all ;
load('./Results/benchmark_new.mat');
figure(1)
plot(log(zgrid(indzhatc:end)),tauzc(indzhatc:end),'LineWidth',2);
title('The Generic Tax Function');
xlabel('Log Productivity');
ylim([0.04 0.28]);
saveas(gcf,'./Results/FigureD2.eps','epsc');