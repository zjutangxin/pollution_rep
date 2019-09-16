% -------------------------------------------------------------------------
%                      Program Description
% -------------------------------------------------------------------------
%   
% Purpose:
%     - Compute Entries in the Tables for the Online Appendix.
%     - Two Sectors Model with CES aggregation.
%     - The Size Distribution of Firms and Industrial Water Pollution: A
%       Quantitative Analysis of China.
%  
% Author:
%     - Xin Tang @ International Monetary Fund
% 
% File Dependence:
%     - fcn.m: evaluates all the allocations at a given wage and returns 
%       the excess demand in the labor market.
%  
% Record of Revisions:
%         Date:                 Description of Changes
%     ============        =================================
%      04/19/2019                 Original Version
% =========================================================================
clc;
clear all;

% ================ Load Data ======================
% load('./Results/benchmark_ces.mat');
% disp('Print Results for the CES Benchmark')
% load('./Results/nodirty_ces.mat');
% disp('Print Results for the CES No Dirty Tax')
% load('./Results/notax_ces.mat');
% disp('Print Results for the CES No Tax')
% load('./Results/benchmark_ces3.mat');
% disp('Print Results for the CES 3 Benchmark')
% load('./Results/notax_ces3.mat');
% disp('Print Results for the CES 3 No Tax')
load('./Results/nodirty_ces3.mat');
disp('Print Results for the CES 3 No Dirty Tax')

% =========================================================================
%                               Table 5
%                          Aggregate Impacts
% =========================================================================
% ================ Compute the Results ======================
% --------------- Panel A -------------------
% Aggregate Output
aggyd = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc = sum(yc(indzhatc:end).*zprob(indzhatc:end));
aggypd = priced*aggyd;
aggypc = pricec*aggyc;
aggyp = aggypd+aggypc;
aggy = (varces*aggyd^((sigmaces-1)/sigmaces)+...
    (1-varces)*aggyc^((sigmaces-1)/sigmaces))^(sigmaces/(sigmaces-1));

% --------------- Panel B -------------------
totfirmd = sum(zprob(indzhatd:end));
totfirmc = sum(zprob(indzhatc:end));
totfirm = mud*totfirmd + (1-mud)*totfirmc;
% Mean Size
meansized = sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd;
meansizec = sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc;
meansize  = mud*meansized + (1-mud)*meansizec;

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d = sum(ez_dirty.*zprob(indzhatd:indztilded-1));
agge_c = sum(ez_clean.*zprob(indztilded:end));
agge = agge_d + agge_c;

cleanshare = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));

% ================ Print to Terminal ======================
disp('===================================================================')
disp('                   Table 4: Aggregate Impacts                      ')
disp('===================================================================')
disp('                     Polluting                  Non-polluting      ')
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Output                '   num2str(aggyd,'%10.6f') '        '...
     '            ' num2str(aggyc,'%10.6f') ] ;
disp(tmpstring)
tmpstring = ...
    ['Price                 '   num2str(priced,'%10.6f') '        '...
     '            ' num2str(pricec,'%10.6f') ] ;
disp(tmpstring)
tmpstring = ...
    ['Output Value          '   num2str(aggypd,'%10.6f') '        '...
     '            ' num2str(aggypc,'%10.6f') ] ;
disp(tmpstring)
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Number of Firms       '   num2str(totfirmd,'%10.6f') '        '...
     '            ' num2str(totfirmc,'%10.6f') ] ;
disp(tmpstring)
tmpstring = ...
    ['Mean Size             '   num2str(meansized,'%10.6f') '        '...
     '          ' num2str(meansizec,'%10.6f') ] ;
disp(tmpstring)
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Aggregate Pollution   '   num2str(agge,'%10.6f')];
disp(tmpstring)
tmpstring = ...
    ['Aggregate Intensity   '   num2str(agge/aggyd,'%10.6f')];
disp(tmpstring)
tmpstring = ...
    ['Clean Share           '   num2str(cleanshare,'%10.6f')];
disp(tmpstring)
disp('===================================================================')