% -------------------------------------------------------------------------
%                      Program Description
% -------------------------------------------------------------------------
%   
% Purpose:
%     - Generate Appendix Table J.1
%     - Two Sectors Model with CES Aggregation
%     - The Size Distribution of Firms and Industrial Water Pollution: A
%       Quantitative Analysis of China.
%  
% Author:
%     - Xin Tang @ International Monetary Fund
%  
% Record of Revisions:
%         Date:                 Description of Changes
%     ============        =================================
%      11/15/2019                 Original Version
% =========================================================================
clc;
clear all;

% ========================= CES = 1.5 =====================================
% ============= Compute the raw numbers for Benchmark =====================
load('./Results/benchmark_ces.mat');
% --------------- Panel A -------------------
% Aggregate Output
aggyd_bench = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_bench = sum(yc(indzhatc:end).*zprob(indzhatc:end));
priced_bench = priced ;
pricec_bench = pricec ;
aggypd_bench = priced_bench*aggyd_bench;
aggypc_bench = pricec_bench*aggyc_bench;
aggyp_bench = aggypd_bench+aggypc_bench;
aggy_bench = (varces*aggyd_bench^((sigmaces-1)/sigmaces)+...
    (1-varces)*aggyc_bench^((sigmaces-1)/sigmaces))...
    ^(sigmaces/(sigmaces-1));

% --------------- Panel B -------------------
totfirmd_bench = sum(zprob(indzhatd:end));
totfirmc_bench = sum(zprob(indzhatc:end));
totfirm_bench = mud*totfirmd_bench + (1-mud)*totfirmc_bench;
% Mean Size
meansized_bench = ...
    sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_bench;
meansizec_bench = ...
    sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_bench;
meansize_bench  = mud*meansized_bench + (1-mud)*meansizec_bench;

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_bench = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_bench = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_bench = sum(ez_dirty_bench.*zprob(indzhatd:indztilded-1));
agge_c_bench = sum(ez_clean_bench.*zprob(indztilded:end));
agge_bench = agge_d_bench + agge_c_bench;

cleanshare_bench = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));

clearvars -except aggyd_bench aggyc_bench priced_bench pricec_bench ...
    aggypd_bench aggypc_bench totfirmd_bench totfirmc_bench ...
    meansized_bench meansizec_bench ...
    agge_bench cleanshare_bench ;
save('./results/gentab_bench_ces.mat')

% ============= Compute the raw numbers for Case (i) ======================
load('./Results/notax_ces.mat');
% --------------- Panel A -------------------
% Aggregate Output
aggyd_c1 = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_c1 = sum(yc(indzhatc:end).*zprob(indzhatc:end));
priced_c1 = priced ;
pricec_c1 = pricec ;
aggypd_c1 = priced_c1*aggyd_c1;
aggypc_c1 = pricec_c1*aggyc_c1;
aggyp_c1 = aggypd_c1+aggypc_c1;
aggy_c1 = (varces*aggyd_c1^((sigmaces-1)/sigmaces)+...
    (1-varces)*aggyc_c1^((sigmaces-1)/sigmaces))...
    ^(sigmaces/(sigmaces-1));

% --------------- Panel B -------------------
totfirmd_c1 = sum(zprob(indzhatd:end));
totfirmc_c1 = sum(zprob(indzhatc:end));
totfirm_c1 = mud*totfirmd_c1 + (1-mud)*totfirmc_c1;
% Mean Size
meansized_c1 = ...
    sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_c1;
meansizec_c1 = ...
    sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_c1;
meansize_c1  = mud*meansized_c1 + (1-mud)*meansizec_c1;

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_c1 = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_c1 = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_c1 = sum(ez_dirty_c1.*zprob(indzhatd:indztilded-1));
agge_c_c1 = sum(ez_clean_c1.*zprob(indztilded:end));
agge_c1 = agge_d_c1 + agge_c_c1;

cleanshare_c1 = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));

clearvars -except aggyd_c1 aggyc_c1 priced_c1 pricec_c1 ...
    aggypd_c1 aggypc_c1 totfirmd_c1 totfirmc_c1 ...
    meansized_c1 meansizec_c1 ...
    agge_c1 cleanshare_c1 ;
save('./results/gentab_c1_ces.mat')

% ============= Compute the raw numbers for Case (ii) =====================
load('./Results/nodirty_ces.mat');
% --------------- Panel A -------------------
% Aggregate Output
aggyd_c2 = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_c2 = sum(yc(indzhatc:end).*zprob(indzhatc:end));
priced_c2 = priced ;
pricec_c2 = pricec ;
aggypd_c2 = priced_c2*aggyd_c2;
aggypc_c2 = pricec_c2*aggyc_c2;
aggyp_c2 = aggypd_c2+aggypc_c2;
aggy_c2 = (varces*aggyd_c2^((sigmaces-1)/sigmaces)+...
    (1-varces)*aggyc_c2^((sigmaces-1)/sigmaces))...
    ^(sigmaces/(sigmaces-1));

% --------------- Panel B -------------------
totfirmd_c2 = sum(zprob(indzhatd:end));
totfirmc_c2 = sum(zprob(indzhatc:end));
totfirm_c2 = mud*totfirmd_c2 + (1-mud)*totfirmc_c2;
% Mean Size
meansized_c2 = ...
    sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_c2;
meansizec_c2 = ...
    sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_c2;
meansize_c2  = mud*meansized_c2 + (1-mud)*meansizec_c2;

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_c2 = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_c2 = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_c2 = sum(ez_dirty_c2.*zprob(indzhatd:indztilded-1));
agge_c_c2 = sum(ez_clean_c2.*zprob(indztilded:end));
agge_c2 = agge_d_c2 + agge_c_c2;

cleanshare_c2 = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));

clearvars -except aggyd_c2 aggyc_c2 priced_c2 pricec_c2 ...
    aggypd_c2 aggypc_c2 totfirmd_c2 totfirmc_c2 ...
    meansized_c2 meansizec_c2 ...
    agge_c2 cleanshare_c2 ;
save('./results/gentab_c2_ces.mat')

% ========================= CES = 3.0 =====================================
% ============= Compute the raw numbers for Benchmark =====================
load('./Results/benchmark_ces3.mat');
% --------------- Panel A -------------------
% Aggregate Output
aggyd_bench3 = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_bench3 = sum(yc(indzhatc:end).*zprob(indzhatc:end));
priced_bench3 = priced ;
pricec_bench3 = pricec ;
aggypd_bench3 = priced_bench3*aggyd_bench3;
aggypc_bench3 = pricec_bench3*aggyc_bench3;
aggyp_bench3 = aggypd_bench3+aggypc_bench3;
aggy_bench3 = (varces*aggyd_bench3^((sigmaces-1)/sigmaces)+...
    (1-varces)*aggyc_bench3^((sigmaces-1)/sigmaces))...
    ^(sigmaces/(sigmaces-1));

% --------------- Panel B -------------------
totfirmd_bench3 = sum(zprob(indzhatd:end));
totfirmc_bench3 = sum(zprob(indzhatc:end));
totfirm_bench3 = mud*totfirmd_bench3 + (1-mud)*totfirmc_bench3;
% Mean Size
meansized_bench3 = ...
    sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_bench3;
meansizec_bench3 = ...
    sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_bench3;
meansize_bench3  = mud*meansized_bench3 + (1-mud)*meansizec_bench3;

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_bench3 = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_bench3 = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_bench3 = sum(ez_dirty_bench3.*zprob(indzhatd:indztilded-1));
agge_c_bench3 = sum(ez_clean_bench3.*zprob(indztilded:end));
agge_bench3 = agge_d_bench3 + agge_c_bench3;

cleanshare_bench3 = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));

clearvars -except aggyd_bench3 aggyc_bench3 priced_bench3 pricec_bench3 ...
    aggypd_bench3 aggypc_bench3 totfirmd_bench3 totfirmc_bench3 ...
    meansized_bench3 meansizec_bench3 ...
    agge_bench3 cleanshare_bench3 ;
save('./results/gentab_bench_ces3.mat')

% ============= Compute the raw numbers for Case (i) ======================
load('./Results/notax_ces3.mat');
% --------------- Panel A -------------------
% Aggregate Output
aggyd_c13 = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_c13 = sum(yc(indzhatc:end).*zprob(indzhatc:end));
priced_c13 = priced ;
pricec_c13 = pricec ;
aggypd_c13 = priced_c13*aggyd_c13;
aggypc_c13 = pricec_c13*aggyc_c13;
aggyp_c13 = aggypd_c13+aggypc_c13;
aggy_c13 = (varces*aggyd_c13^((sigmaces-1)/sigmaces)+...
    (1-varces)*aggyc_c13^((sigmaces-1)/sigmaces))...
    ^(sigmaces/(sigmaces-1));

% --------------- Panel B -------------------
totfirmd_c13 = sum(zprob(indzhatd:end));
totfirmc_c13 = sum(zprob(indzhatc:end));
totfirm_c13 = mud*totfirmd_c13 + (1-mud)*totfirmc_c13;
% Mean Size
meansized_c13 = ...
    sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_c13;
meansizec_c13 = ...
    sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_c13;
meansize_c13  = mud*meansized_c13 + (1-mud)*meansizec_c13;

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_c13 = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_c13 = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_c13 = sum(ez_dirty_c13.*zprob(indzhatd:indztilded-1));
agge_c_c13 = sum(ez_clean_c13.*zprob(indztilded:end));
agge_c13 = agge_d_c13 + agge_c_c13;

cleanshare_c13 = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));

clearvars -except aggyd_c13 aggyc_c13 priced_c13 pricec_c13 ...
    aggypd_c13 aggypc_c13 totfirmd_c13 totfirmc_c13 ...
    meansized_c13 meansizec_c13 ...
    agge_c13 cleanshare_c13 ;
save('./results/gentab_c1_ces3.mat')

% ============= Compute the raw numbers for Case (ii) =====================
load('./Results/nodirty_ces3.mat');
% --------------- Panel A -------------------
% Aggregate Output
aggyd_c23 = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_c23 = sum(yc(indzhatc:end).*zprob(indzhatc:end));
priced_c23 = priced ;
pricec_c23 = pricec ;
aggypd_c23 = priced_c23*aggyd_c23;
aggypc_c23 = pricec_c23*aggyc_c23;
aggyp_c23 = aggypd_c23+aggypc_c23;
aggy_c23 = (varces*aggyd_c23^((sigmaces-1)/sigmaces)+...
    (1-varces)*aggyc_c23^((sigmaces-1)/sigmaces))...
    ^(sigmaces/(sigmaces-1));

% --------------- Panel B -------------------
totfirmd_c23 = sum(zprob(indzhatd:end));
totfirmc_c23 = sum(zprob(indzhatc:end));
totfirm_c23 = mud*totfirmd_c23 + (1-mud)*totfirmc_c23;
% Mean Size
meansized_c23 = ...
    sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_c23;
meansizec_c23 = ...
    sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_c23;
meansize_c23  = mud*meansized_c23 + (1-mud)*meansizec_c23;

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_c23 = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_c23 = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_c23 = sum(ez_dirty_c23.*zprob(indzhatd:indztilded-1));
agge_c_c23 = sum(ez_clean_c23.*zprob(indztilded:end));
agge_c23 = agge_d_c23 + agge_c_c23;

cleanshare_c23 = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));

clearvars -except aggyd_c23 aggyc_c23 priced_c23 pricec_c23 ...
    aggypd_c23 aggypc_c23 totfirmd_c23 totfirmc_c23 ...
    meansized_c23 meansizec_c23 ...
    agge_c23 cleanshare_c23 ;
save('./results/gentab_c2_ces3.mat')

% =================== Print the table to terminal =========================
clear all ;
load('./results/gentab_bench_ces.mat')
load('./results/gentab_c1_ces.mat')
load('./results/gentab_c2_ces.mat')
load('./results/gentab_bench_ces3.mat')
load('./results/gentab_c1_ces3.mat')
load('./results/gentab_c2_ces3.mat')

disp('===================================================================')
disp('           Table 4: Aggregate Impacts Upper (CES = 1.5)            ')
disp('===================================================================')
disp('                     Polluting                  Non-polluting      ')
disp('            -------------------------------------------------------')
disp('Statistics  Benchmark    (i)     (ii)    Benchmark     (i)     (ii)')
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Output        ' '100.00' '    ' ...
        num2str(100*aggyd_c1/aggyd_bench,'%8.2f') ...
     '  ' num2str(100*aggyd_c2/aggyd_bench,'%8.2f')  ...
     '    ' '100.00' '      ' ...
      num2str(100*aggyc_c1/aggyc_bench,'%8.2f') ...
     '  ' num2str(100*aggyc_c2/aggyc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Price         ' '100.00' '    ' ...
        num2str(100*priced_c1/priced_bench,'%8.2f') ...
     '   ' num2str(100*priced_c2/priced_bench,'%8.2f')  ...
     '     ' '100.00' '      ' ...
      num2str(100*pricec_c1/pricec_bench,'%8.2f') ...
     '  ' num2str(100*pricec_c2/pricec_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Revenue       ' '100.00' '    ' ...
        num2str(100*aggypd_c1/aggypd_bench,'%8.2f') ...
     '  ' num2str(100*aggypd_c2/aggypd_bench,'%8.2f')  ...
     '    ' '100.00' '      ' ...
      num2str(100*aggypc_c1/aggypc_bench,'%8.2f') ...
     '  ' num2str(100*aggypc_c2/aggypc_bench,'%8.2f')] ;
disp(tmpstring)
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['#firms        ' '100.00' '     ' ...
        num2str(100*totfirmd_c1/totfirmd_bench,'%8.2f') ...
     '   ' num2str(100*totfirmd_c2/totfirmd_bench,'%8.2f')  ...
     '    ' '100.00' '       ' ...
      num2str(100*totfirmc_c1/totfirmc_bench,'%8.2f') ...
     '  ' num2str(100*totfirmc_c2/totfirmc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Mean Size     ' num2str(meansized_bench,'%8.2f') '     '...
    num2str(meansized_c1,'%8.2f') ...
     '   ' num2str(meansized_c2,'%8.2f')  ...
     '   ' num2str(meansizec_bench,'%8.2f') '       ' ...
      num2str(meansizec_c1,'%8.2f') ...
     '  ' num2str(meansizec_c2,'%8.2f')] ;
disp(tmpstring)
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Pollution     ' '100.00' '     ' ...
    num2str(100*agge_c1/agge_bench,'%8.2f') '   '...
    num2str(100*agge_c2/agge_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Intensity     ' '100.00' '     ' ...
    num2str(100*(agge_c1/aggyd_c1)/(agge_bench/aggyd_bench),'%8.2f') ...
    '   '...
    num2str(100*(agge_c2/aggyd_c2)/(agge_bench/aggyd_bench),'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Clean Share   ' num2str(100*cleanshare_bench,'%8.2f') '      ' ...
    num2str(100*cleanshare_c1,'%8.2f') '   '...
    num2str(100*cleanshare_c2,'%8.2f')] ;
disp(tmpstring)
disp('===================================================================')

disp('===================================================================')
disp('           Table 4: Aggregate Impacts Lower (CES = 3.0)            ')
disp('===================================================================')
disp('                     Polluting                  Non-polluting      ')
disp('            -------------------------------------------------------')
disp('Statistics  Benchmark    (i)     (ii)    Benchmark     (i)     (ii)')
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Output        ' '100.00' '    ' ...
        num2str(100*aggyd_c13/aggyd_bench3,'%8.2f') ...
     '  ' num2str(100*aggyd_c23/aggyd_bench3,'%8.2f')  ...
     '    ' '100.00' '      ' ...
      num2str(100*aggyc_c13/aggyc_bench3,'%8.2f') ...
     '  ' num2str(100*aggyc_c23/aggyc_bench3,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Price         ' '100.00' '    ' ...
        num2str(100*priced_c13/priced_bench3,'%8.2f') ...
     '   ' num2str(100*priced_c23/priced_bench3,'%8.2f')  ...
     '    ' '100.00' '      ' ...
      num2str(100*pricec_c13/pricec_bench3,'%8.2f') ...
     '  ' num2str(100*pricec_c23/pricec_bench3,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Revenue       ' '100.00' '    ' ...
        num2str(100*aggypd_c13/aggypd_bench3,'%8.2f') ...
     '  ' num2str(100*aggypd_c23/aggypd_bench3,'%8.2f')  ...
     '    ' '100.00' '      ' ...
      num2str(100*aggypc_c13/aggypc_bench3,'%8.2f') ...
     '  ' num2str(100*aggypc_c23/aggypc_bench3,'%8.2f')] ;
disp(tmpstring)
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['#firms        ' '100.00' '     ' ...
        num2str(100*totfirmd_c13/totfirmd_bench3,'%8.2f') ...
     '   ' num2str(100*totfirmd_c23/totfirmd_bench3,'%8.2f')  ...
     '    ' '100.00' '       ' ...
      num2str(100*totfirmc_c13/totfirmc_bench3,'%8.2f') ...
     '  ' num2str(100*totfirmc_c23/totfirmc_bench3,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Mean Size     ' num2str(meansized_bench3,'%8.2f') '     '...
    num2str(meansized_c13,'%8.2f') ...
     '   ' num2str(meansized_c23,'%8.2f')  ...
     '   ' num2str(meansizec_bench3,'%8.2f') '       ' ...
      num2str(meansizec_c13,'%8.2f') ...
     '  ' num2str(meansizec_c23,'%8.2f')] ;
disp(tmpstring)
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Pollution     ' '100.00' '     ' ...
    num2str(100*agge_c13/agge_bench3,'%8.2f') '   '...
    num2str(100*agge_c23/agge_bench3,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Intensity     ' '100.00' '     ' ...
    num2str(100*(agge_c13/aggyd_c13)/(agge_bench3/aggyd_bench3),'%8.2f') ...
    '   '...
    num2str(100*(agge_c23/aggyd_c23)/(agge_bench3/aggyd_bench3),'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Clean Share   ' num2str(100*cleanshare_bench3,'%8.2f') '      ' ...
    num2str(100*cleanshare_c13,'%8.2f') '   '...
    num2str(100*cleanshare_c23,'%8.2f')] ;
disp(tmpstring)
disp('===================================================================')