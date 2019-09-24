clear all;
load('testplot.mat');
figure(1)
plot(log(zgrid(indzhatc:end)),tauzc(indzhatc:end),...
    'LineWidth',2);
title('The Generic Tax Function');
xlabel('Log Productivity');
ylim([0.04 0.28]);
saveas(gcf,'./Results/FigureD2.eps','epsc');