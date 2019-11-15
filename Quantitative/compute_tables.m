% -------------------------------------------------------------------------
%                      Program Description
% -------------------------------------------------------------------------
%   
% Purpose:
%     - Compute Entries in the Tables
%     - Two Sectors Model
%     - The Size Distribution of Firms and Industrial Water Pollution: A
%       Quantitative Analysis of China.
%  
% Author:
%     - Xin Tang @ International Monetary Fund
% 
% Record of Revisions:
%         Date:                 Description of Changes
%     ============        =================================
%      04/19/2019                 Original Version
% =========================================================================
clc;
clear all;

% ================ Load Data ======================
% load('./Results/benchmark_new.mat');
% disp('Print Results for the Benchmark')
% load('./Results/notax_new.mat');
% disp('Print Results for the No-tax case')
% load('./Results/regulation_new.mat');
% disp('Print Results for the Regulation case')
load('./Results/flattax_new.mat');
disp('Print Results for the Flat Tax')

% =========================================================================
%                               Table 5
%                          Aggregate Impacts
% =========================================================================
% ================ Compute the Results ======================
% --------------- Panel A -------------------
% Aggregate Output
aggyd = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc = sum(yc(indzhatc:end).*zprob(indzhatc:end));
aggy  = mud*aggyd + (1-mud)*aggyc;
% Aggregate Capital
aggkd = sum(kdd(indzhatd:end).*zprob(indzhatd:end));
aggkd = aggkd + ke*sum(zprob(indztilded:end));
aggkc = sum(kdc(indzhatc:end).*zprob(indzhatc:end));
aggk = mud*aggkd + (1-mud)*aggkc ;
% Aggregate Consumption
aggc = aggy - delta*aggk ;
% Labor Market
aggldd = sum(ldd(indzhatd:end).*zprob(indzhatd:end));
aggldc = sum(ldc(indzhatc:end).*zprob(indzhatc:end));
agglsd = sum(zprob(1:indzhatd-1));
agglsc = sum(zprob(1:indzhatc-1));
aggld = mud*aggldd+(1-mud)*aggldc ;
aggls = mud*agglsd+(1-mud)*agglsc ;
% Output per Worker
apld = aggyd/aggldd;
aplc = aggyc/aggldc;
apl  = aggy/aggld;
% Output per Firm
totfirmd = sum(zprob(indzhatd:end));
totfirmc = sum(zprob(indzhatc:end));
totfirm = mud*totfirmd + (1-mud)*totfirmc;
apfd = aggyd/totfirmd;
apfc = aggyc/totfirmc;
apf  = aggy/totfirm;

% Average Firm TFP
%%%
meanfd = zgrid(indzhatd:end)'*zprob(indzhatd:end)/totfirmd ;
meanfc = zgrid(indzhatc:end)'*zprob(indzhatc:end)/totfirmc ;
meanf  = mud*meanfd + (1-mud)*meanfc ;
% --------------- Panel B -------------------
% Mean Size
meansized = sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd;
meansizec = sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc;
meansize  = mud*meansized + (1-mud)*meansizec;
% Median Size
distd = zeros(zn,1);
distc = zeros(zn,1);
distd(indzhatd:end) = zprob(indzhatd:end)/totfirmd;
distc(indzhatc:end) = zprob(indzhatc:end)/totfirmc;
dist = mud*distd+(1-mud)*distc;
cdist = cumsum(dist) ;
medsize_ind = find(cdist>0.5,1);
medsize = ldc(medsize_ind);
cdistd = cumsum(distd) ;
medsized_ind = find(cdistd>0.5,1);
medsized = ldd(medsized_ind);
cdistc = cumsum(distc) ;
medsizec_ind = find(cdistc>0.5,1);
medsizec = ldc(medsizec_ind);

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
    ['Aggregate Output      '   num2str(aggyd,'%10.6f') '        '...
     '            ' num2str(aggyc,'%10.6f') ] ;
disp(tmpstring)
tmpstring = ...
    ['Aggregate Capital     '   num2str(aggkd,'%10.6f') '        '...
     '            ' num2str(aggkc,'%10.6f') ] ;
disp(tmpstring)
disp(['Aggregate Consumption ' num2str(aggc,'%10.6f')]);
tmpstring = ...
    ['Output per Worker     '   num2str(apld,'%10.6f') '        '...
     '            ' num2str(aplc,'%10.6f') ] ;
disp(tmpstring)
tmpstring = ...
    ['Output per Firm       '   num2str(apfd,'%10.6f') '        '...
     '        ' num2str(apfc,'%10.6f') ] ;
disp(tmpstring)
tmpstring = ...
    ['Average Productivity  '   num2str(meanfd,'%10.6f') '        '...
     '   ' num2str(meanfc,'%10.6f') ] ;
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
tmpstring = ...
    ['Median Size           '   num2str(medsized,'%10.6f') '        '...
     '          ' num2str(medsizec,'%10.6f') ] ;
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


% =========================================================================
%                               Table 5
%                        Distributional Impacts
% =========================================================================
% ================ Compute the Results ======================
cutz = [0.2 0.4 0.6 0.8] ;
indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistd>cutz(iz),1) ;
end
indcz(1,1) = indzhatd ;
% Output accounted for by firms of different productivity
yshared = zeros(1,5) ;
for iz = 1:1:4
    yshared(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yd(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        yshared(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yd(indcz(iz+1):end)) ;
    end
end
yshared = yshared/aggyd ;

indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistc>cutz(iz),1) ;
end
indcz(1,1) = indzhatc ;
% Output accounted for by firms of different productivity
ysharec = zeros(1,5) ;
for iz = 1:1:4
    ysharec(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yc(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        ysharec(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yc(indcz(iz+1):end)) ;
    end
end
ysharec = ysharec/aggyc ;

% ================ Print to Terminal ======================
disp('===================================================================')
disp('                  Table 5: Distributional Impacts                  ')
disp('===================================================================')
disp(['Polluting         ' num2str(yshared)])
disp(['Non-Polluting     ' num2str(ysharec)])

















