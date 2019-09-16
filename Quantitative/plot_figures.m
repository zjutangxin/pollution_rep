% -------------------------------------------------------------------------
%                      Program Description
% -------------------------------------------------------------------------
%   
% Purpose:
%     - Plot Figures 4 and 5.
%     - Two Sectors Model
%     - The Size Distribution of Firms and Industrial Water Pollution: A
%       Quantitative Analysis of China.
%  
% Author:
%     - Xin Tang @ International Monetary Fund
% 
% File Dependence:
%     - benchmark_new.mat
%     - notax_new.mat
%     - regulation_new.mat
%  
% Record of Revisions:
%         Date:                 Description of Changes
%     ============        =================================
%      04/19/2019                 Original Version
% =========================================================================
clc;
clear all;

% Benchmark Distributions
load('./Results/benchmark_new.mat');
% ================== Plot Figure 5 ======================
% Bench vs No-tax: firm
figure(1)
bar([firmmodel,fsdata'],1,'group');
colormap summer ;
legend('Model','China','Location','NorthEast');
set(gca,'XTick',1:5);
set(gca,'XTickLabel',{'1-19','20-49','50-99','100-399','400+'});
ylim([0 1]);
xlabel('Firm Size Groups');
ylabel('Fraction of Firms');
title('Firm Size Distribution: Benchmark','FontWeight','Normal');
saveas(gcf,'./Results/Figure5_Left.eps','epsc');

% Bench vs No-tax: employment
figure(2)
bar([esmodel,esdata'],1,'group');
colormap summer ;
legend('Model','China','Location','NorthEast');
set(gca,'XTick',1:5);
set(gca,'XTickLabel',{'1-19','20-49','50-99','100-399','400+'});
ylim([0 1]);
xlabel('Firm Size Groups');
ylabel('Fraction of Workers');
title('Employment Distribution: Benchmark','FontWeight','Normal');
saveas(gcf,'./Results/Figure5_Right.eps','epsc');

% ============== Compute Data for Figure 6 ==================
totfirmd_bench = sum(zprob(indzhatd:end));
tote_bench = sum(ldd(indzhatd:end).*zprob(indzhatd:end));
indmd = pid>wss;
firmmodeld_bench = zeros(5,1) ;
esmodeld_bench = zeros(5,1) ;
% Polluting Sector
for indbin = 1:1:5
    seld = find(ldd>=cutleft(indbin) & ldd<cutleft(indbin+1));
    if isempty(seld) == 1
        continue ;
    end
    firmmodeld_bench(indbin) = sum(zprob(seld).*indmd(seld)) ;
    esmodeld_bench(indbin) =...
        sum(zprob(seld).*ldd(seld).*indmd(seld)) ;    
end
firmmodeld_bench = firmmodeld_bench/totfirmd_bench;
esmodeld_bench = esmodeld_bench/tote_bench;

clearvars -except firmmodeld_bench esmodeld_bench;

% No-tax Distributions
load('./Results/notax_new.mat');
totfirmd_notax = sum(zprob(indzhatd:end));
tote_notax = sum(ldd(indzhatd:end).*zprob(indzhatd:end));
indmd = pid>wss;
firmmodeld_notax = zeros(5,1) ;
esmodeld_notax = zeros(5,1) ;
% Polluting Sector
for indbin = 1:1:5
    seld = find(ldd>=cutleft(indbin) & ldd<cutleft(indbin+1));
    if isempty(seld) == 1
        continue ;
    end
    firmmodeld_notax(indbin) = sum(zprob(seld).*indmd(seld)) ;
    esmodeld_notax(indbin) =...
        sum(zprob(seld).*ldd(seld).*indmd(seld)) ;    
end
firmmodeld_notax = firmmodeld_notax/totfirmd_notax;
esmodeld_notax = esmodeld_notax/tote_notax;

clearvars -except firmmodeld_bench esmodeld_bench ...
    firmmodeld_notax esmodeld_notax;

% Regulation Distributions
load('./Results/regulation_new.mat');
totfirmd_reg = sum(zprob(indzhatd:end));
tote_reg = sum(ldd(indzhatd:end).*zprob(indzhatd:end));
indmd = pid>wss;
firmmodeld_reg = zeros(5,1) ;
esmodeld_reg = zeros(5,1) ;
% Polluting Sector
for indbin = 1:1:5
    seld = find(ldd>=cutleft(indbin) & ldd<cutleft(indbin+1));
    if isempty(seld) == 1
        continue ;
    end
    firmmodeld_reg(indbin) = sum(zprob(seld).*indmd(seld)) ;
    esmodeld_reg(indbin) =...
        sum(zprob(seld).*ldd(seld).*indmd(seld)) ;    
end
firmmodeld_reg = firmmodeld_reg/totfirmd_reg;
esmodeld_reg = esmodeld_reg/tote_reg;

clearvars -except firmmodeld_bench esmodeld_bench ...
    firmmodeld_notax esmodeld_notax ...
    firmmodeld_reg esmodeld_reg ;

% ================== Plot Figure 5 ======================
% Bench vs No-tax: firm
figure(3)
bar([firmmodeld_bench,firmmodeld_notax],1,'group');
colormap summer ;
legend('Benchmark','No Tax','Location','NorthEast');
set(gca,'XTick',1:5);
set(gca,'XTickLabel',{'1-19','20-49','50-99','100-399','400+'});
ylim([0 1]);
xlabel('Firm Size Groups');
ylabel('Fraction of Firms');
title('Firm Size Distribution: Benchmark vs. No Tax',...
    'FontWeight','Normal');
saveas(gcf,'./Results/Figure6_TopLeft.eps','epsc');

% Bench vs No-tax: employment
figure(4)
bar([esmodeld_bench,esmodeld_notax],1,'group');
colormap summer ;
legend('Benchmark','No Tax','Location','NorthEast');
set(gca,'XTick',1:5);
set(gca,'XTickLabel',{'1-19','20-49','50-99','100-399','400+'});
ylim([0 1]);
xlabel('Firm Size Groups');
ylabel('Fraction of Workers');
title('Employment Distribution: Benchmark vs. No Tax',...
    'FontWeight','Normal');
saveas(gcf,'./Results/Figure6_BottomLeft.eps','epsc');

% Bench vs Regulation: firm
figure(5)
bar([firmmodeld_bench,firmmodeld_reg],1,'group');
colormap summer ;
legend('Benchmark','Regulation','Location','NorthEast');
set(gca,'XTick',1:5);
set(gca,'XTickLabel',{'1-19','20-49','50-99','100-399','400+'});
ylim([0 1]);
xlabel('Firm Size Groups');
ylabel('Fraction of Firms');
title('Firm Size Distribution: Benchmark vs. Regulation',...
    'FontWeight','Normal');
saveas(gcf,'./Results/Figure6_TopRight.eps','epsc');

% Bench vs Regulation: employment
figure(6)
bar([esmodeld_bench,esmodeld_reg],1,'group');
colormap summer ;
legend('Benchmark','Regulation','Location','NorthEast');
set(gca,'XTick',1:5);
set(gca,'XTickLabel',{'1-19','20-49','50-99','100-399','400+'});
ylim([0 1]);
xlabel('Firm Size Groups');
ylabel('Fraction of Workers');
title('Employment Distribution: Benchmark vs. Regulation',...
    'FontWeight','Normal');
saveas(gcf,'./Results/Figure6_BotRight.eps','epsc');