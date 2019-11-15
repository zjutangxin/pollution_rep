% -------------------------------------------------------------------------
%                      Program Description
% -------------------------------------------------------------------------
%   
% Purpose:
%     - Generate Tables 4, 5 and 6
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
%      11/15/2019                 Original Version
% =========================================================================
clc;
clear all;

% ============= Compute the raw numbers for Benchmark =====================
load('./Results/benchmark_new.mat');
% =========================================================================
%                               Table 4
%                          Aggregate Impacts
% =========================================================================
% --------------- Panel A -------------------
% Aggregate Output
aggyd_bench = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_bench = sum(yc(indzhatc:end).*zprob(indzhatc:end));
aggy_bench  = mud*aggyd_bench + (1-mud)*aggyc_bench;
aggydshare_bench = mud*aggyd_bench/aggy_bench;
aggycshare_bench = 1-aggydshare_bench;
% Aggregate Capital
aggkd_bench = sum(kdd(indzhatd:end).*zprob(indzhatd:end));
aggkd_bench = aggkd_bench + ke*sum(zprob(indztilded:end));
aggkc_bench = sum(kdc(indzhatc:end).*zprob(indzhatc:end));
aggk_bench = mud*aggkd_bench + (1-mud)*aggkc_bench ;
% Aggregate Consumption
aggc_bench = aggy_bench - delta*aggk_bench ;
% Labor Market
aggldd_bench = sum(ldd(indzhatd:end).*zprob(indzhatd:end));
aggldc_bench = sum(ldc(indzhatc:end).*zprob(indzhatc:end));
agglsd_bench = sum(zprob(1:indzhatd-1));
agglsc_bench = sum(zprob(1:indzhatc-1));
aggld_bench = mud*aggldd_bench+(1-mud)*aggldc_bench ;
aggls_bench = mud*agglsd_bench+(1-mud)*agglsc_bench ;
% Output per Worker
apld_bench = aggyd_bench/aggldd_bench;
aplc_bench = aggyc_bench/aggldc_bench;
apl_bench  = aggy_bench/aggld_bench;
% Output per Firm
totfirmd_bench = sum(zprob(indzhatd:end));
totfirmc_bench = sum(zprob(indzhatc:end));
totfirm_bench = mud*totfirmd_bench + (1-mud)*totfirmc_bench;
apfd_bench = aggyd_bench/totfirmd_bench;
apfc_bench = aggyc_bench/totfirmc_bench;
apf_bench  = aggy_bench/totfirm_bench;

% Average Firm TFP
%%%
meanfd_bench = zgrid(indzhatd:end)'*zprob(indzhatd:end)/totfirmd_bench ;
meanfc_bench = zgrid(indzhatc:end)'*zprob(indzhatc:end)/totfirmc_bench ;
meanf_bench  = mud*meanfd_bench + (1-mud)*meanfc_bench ;

% --------------- Panel B -------------------
% Mean Size
meansized_bench = ...
    sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_bench;
meansizec_bench = ...
    sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_bench;
meansize_bench  = mud*meansized_bench + (1-mud)*meansizec_bench;
% Median Size
distd_bench = zeros(zn,1);
distc_bench = zeros(zn,1);
distd_bench(indzhatd:end) = zprob(indzhatd:end)/totfirmd_bench;
distc_bench(indzhatc:end) = zprob(indzhatc:end)/totfirmc_bench;
dist_bench = mud*distd_bench+(1-mud)*distc_bench;
cdist_bench = cumsum(dist_bench) ;
medsize_ind_bench = find(cdist_bench>0.5,1);
medsize_bench = ldc(medsize_ind_bench);
cdistd_bench = cumsum(distd_bench) ;
medsized_ind_bench = find(cdistd_bench>0.5,1);
medsized_bench = ldd(medsized_ind_bench);
cdistc_bench = cumsum(distc_bench) ;
medsizec_ind_bench = find(cdistc_bench>0.5,1);
medsizec_bench = ldc(medsizec_ind_bench);

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_bench = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_bench = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_bench = sum(ez_dirty_bench.*zprob(indzhatd:indztilded-1));
agge_c_bench = sum(ez_clean_bench.*zprob(indztilded:end));
agge_bench = agge_d_bench + agge_c_bench;

cleanshare_bench = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));
xi_bench = xi;
wss_bench = wss;

% =========================================================================
%                               Table 5
%                        Distributional Impacts
% =========================================================================
% ================ Compute the Results ======================
cutz = [0.2 0.4 0.6 0.8] ;
indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistd_bench>cutz(iz),1) ;
end
indcz(1,1) = indzhatd ;
% Output accounted for by firms of different productivity
yshared_bench = zeros(1,5) ;
for iz = 1:1:4
    yshared_bench(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yd(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        yshared_bench(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yd(indcz(iz+1):end)) ;
    end
end
yshared_bench = yshared_bench/aggyd_bench ;

indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistc_bench>cutz(iz),1) ;
end
indcz(1,1) = indzhatc ;
% Output accounted for by firms of different productivity
ysharec_bench = zeros(1,5) ;
for iz = 1:1:4
    ysharec_bench(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yc(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        ysharec_bench(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yc(indcz(iz+1):end)) ;
    end
end
ysharec_bench = ysharec_bench/aggyc_bench ;

clearvars -except aggyd_bench aggyc_bench aggkd_bench ...
    aggydshare_bench aggycshare_bench ...
    aggkc_bench aggc_bench apld_bench aplc_bench ...
    totfirmd_bench totfirmc_bench meanfd_bench meanfc_bench...
    apfd_bench apfc_bench apf_bench wss_bench ...
    meansized_bench meansizec_bench medsized_bench medsizec_bench ...
    agge_bench cleanshare_bench yshared_bench ysharec_bench xi_bench;
save('./results/gentab_bench.mat');

% ============= Compute the raw numbers for Case (i) ======================
load('./Results/notax_new.mat');
% =========================================================================
%                               Table 4
%                          Aggregate Impacts
% =========================================================================
% --------------- Panel A -------------------
% Aggregate Output
aggyd_c1 = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_c1 = sum(yc(indzhatc:end).*zprob(indzhatc:end));
aggy_c1  = mud*aggyd_c1 + (1-mud)*aggyc_c1;
aggydshare_c1 = mud*aggyd_c1/aggy_c1;
aggycshare_c1 = 1-aggydshare_c1;
% Aggregate Capital
aggkd_c1 = sum(kdd(indzhatd:end).*zprob(indzhatd:end));
aggkd_c1 = aggkd_c1 + ke*sum(zprob(indztilded:end));
aggkc_c1 = sum(kdc(indzhatc:end).*zprob(indzhatc:end));
aggk_c1 = mud*aggkd_c1 + (1-mud)*aggkc_c1 ;
% Aggregate Consumption
aggc_c1 = aggy_c1 - delta*aggk_c1 ;
% Labor Market
aggldd_c1 = sum(ldd(indzhatd:end).*zprob(indzhatd:end));
aggldc_c1 = sum(ldc(indzhatc:end).*zprob(indzhatc:end));
agglsd_c1 = sum(zprob(1:indzhatd-1));
agglsc_c1 = sum(zprob(1:indzhatc-1));
aggld_c1 = mud*aggldd_c1+(1-mud)*aggldc_c1 ;
aggls_c1 = mud*agglsd_c1+(1-mud)*agglsc_c1 ;
% Output per Worker
apld_c1 = aggyd_c1/aggldd_c1;
aplc_c1 = aggyc_c1/aggldc_c1;
apl_c1  = aggy_c1/aggld_c1;
% Output per Firm
totfirmd_c1 = sum(zprob(indzhatd:end));
totfirmc_c1 = sum(zprob(indzhatc:end));
totfirm_c1 = mud*totfirmd_c1 + (1-mud)*totfirmc_c1;
apfd_c1 = aggyd_c1/totfirmd_c1;
apfc_c1 = aggyc_c1/totfirmc_c1;
apf_c1  = aggy_c1/totfirm_c1;

% Average Firm TFP
%%%
meanfd_c1 = zgrid(indzhatd:end)'*zprob(indzhatd:end)/totfirmd_c1 ;
meanfc_c1 = zgrid(indzhatc:end)'*zprob(indzhatc:end)/totfirmc_c1 ;
meanf_c1  = mud*meanfd_c1 + (1-mud)*meanfc_c1 ;

% --------------- Panel B -------------------
% Mean Size
meansized_c1 = sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_c1;
meansizec_c1 = sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_c1;
meansize_c1  = mud*meansized_c1 + (1-mud)*meansizec_c1;
% Median Size
distd_c1 = zeros(zn,1);
distc_c1 = zeros(zn,1);
distd_c1(indzhatd:end) = zprob(indzhatd:end)/totfirmd_c1;
distc_c1(indzhatc:end) = zprob(indzhatc:end)/totfirmc_c1;
dist_c1 = mud*distd_c1+(1-mud)*distc_c1;
cdist_c1 = cumsum(dist_c1) ;
medsize_ind_c1 = find(cdist_c1>0.5,1);
medsize_c1 = ldc(medsize_ind_c1);
cdistd_c1 = cumsum(distd_c1) ;
medsized_ind_c1 = find(cdistd_c1>0.5,1);
medsized_c1 = ldd(medsized_ind_c1);
cdistc_c1 = cumsum(distc_c1) ;
medsizec_ind_c1 = find(cdistc_c1>0.5,1);
medsizec_c1 = ldc(medsizec_ind_c1);

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_c1 = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_c1 = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_c1 = sum(ez_dirty_c1.*zprob(indzhatd:indztilded-1));
agge_c_c1 = sum(ez_clean_c1.*zprob(indztilded:end));
agge_c1 = agge_d_c1 + agge_c_c1;

cleanshare_c1 = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));
xi_c1 = xi;
wss_c1 = wss;

% =========================================================================
%                               Table 5
%                        Distributional Impacts
% =========================================================================
% ================ Compute the Results ======================
cutz = [0.2 0.4 0.6 0.8] ;
indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistd_c1>cutz(iz),1) ;
end
indcz(1,1) = indzhatd ;
% Output accounted for by firms of different productivity
yshared_c1 = zeros(1,5) ;
for iz = 1:1:4
    yshared_c1(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yd(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        yshared_c1(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yd(indcz(iz+1):end)) ;
    end
end
yshared_c1 = yshared_c1/aggyd_c1 ;

indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistc_c1>cutz(iz),1) ;
end
indcz(1,1) = indzhatc ;
% Output accounted for by firms of different productivity
ysharec_c1 = zeros(1,5) ;
for iz = 1:1:4
    ysharec_c1(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yc(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        ysharec_c1(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yc(indcz(iz+1):end)) ;
    end
end
ysharec_c1 = ysharec_c1/aggyc_c1 ;

clearvars -except aggyd_c1 aggyc_c1 aggkd_c1 ...
    aggydshare_c1 aggycshare_c1 ...
    aggkc_c1 aggc_c1 apld_c1 aplc_c1 ...
    totfirmd_c1 totfirmc_c1 meanfd_c1 meanfc_c1...
    apfd_c1 apfc_c1 apf_c1 wss_c1...
    meansized_c1 meansizec_c1 medsized_c1 medsizec_c1 ...
    agge_c1 cleanshare_c1 yshared_c1 ysharec_c1 xi_c1;
save('./results/gentab_c1.mat');

% ============= Compute the raw numbers for Case (ii) ======================
load('./Results/regulation_new.mat');
% =========================================================================
%                               Table 4
%                          Aggregate Impacts
% =========================================================================
% --------------- Panel A -------------------
% Aggregate Output
aggyd_c2 = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_c2 = sum(yc(indzhatc:end).*zprob(indzhatc:end));
aggy_c2  = mud*aggyd_c2 + (1-mud)*aggyc_c2;
aggydshare_c2 = mud*aggyd_c2/aggy_c2;
aggycshare_c2 = 1-aggydshare_c2;
% Aggregate Capital
aggkd_c2 = sum(kdd(indzhatd:end).*zprob(indzhatd:end));
aggkd_c2 = aggkd_c2 + ke*sum(zprob(indztilded:end));
aggkc_c2 = sum(kdc(indzhatc:end).*zprob(indzhatc:end));
aggk_c2 = mud*aggkd_c2 + (1-mud)*aggkc_c2 ;
% Aggregate Consumption
aggc_c2 = aggy_c2 - delta*aggk_c2 ;
% Labor Market
aggldd_c2 = sum(ldd(indzhatd:end).*zprob(indzhatd:end));
aggldc_c2 = sum(ldc(indzhatc:end).*zprob(indzhatc:end));
agglsd_c2 = sum(zprob(1:indzhatd-1));
agglsc_c2 = sum(zprob(1:indzhatc-1));
aggld_c2 = mud*aggldd_c2+(1-mud)*aggldc_c2 ;
aggls_c2 = mud*agglsd_c2+(1-mud)*agglsc_c2 ;
% Output per Worker
apld_c2 = aggyd_c2/aggldd_c2;
aplc_c2 = aggyc_c2/aggldc_c2;
apl_c2  = aggy_c2/aggld_c2;
% Output per Firm
totfirmd_c2 = sum(zprob(indzhatd:end));
totfirmc_c2 = sum(zprob(indzhatc:end));
totfirm_c2 = mud*totfirmd_c2 + (1-mud)*totfirmc_c2;
apfd_c2 = aggyd_c2/totfirmd_c2;
apfc_c2 = aggyc_c2/totfirmc_c2;
apf_c2  = aggy_c2/totfirm_c2;

% Average Firm TFP
%%%
meanfd_c2 = zgrid(indzhatd:end)'*zprob(indzhatd:end)/totfirmd_c2 ;
meanfc_c2 = zgrid(indzhatc:end)'*zprob(indzhatc:end)/totfirmc_c2 ;
meanf_c2  = mud*meanfd_c2 + (1-mud)*meanfc_c2 ;

% --------------- Panel B -------------------
% Mean Size
meansized_c2 = sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_c2;
meansizec_c2 = sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_c2;
meansize_c2  = mud*meansized_c2 + (1-mud)*meansizec_c2;
% Median Size
distd_c2 = zeros(zn,1);
distc_c2 = zeros(zn,1);
distd_c2(indzhatd:end) = zprob(indzhatd:end)/totfirmd_c2;
distc_c2(indzhatc:end) = zprob(indzhatc:end)/totfirmc_c2;
dist_c2 = mud*distd_c2+(1-mud)*distc_c2;
cdist_c2 = cumsum(dist_c2) ;
medsize_ind_c2 = find(cdist_c2>0.5,1);
medsize_c2 = ldc(medsize_ind_c2);
cdistd_c2 = cumsum(distd_c2) ;
medsized_ind_c2 = find(cdistd_c2>0.5,1);
medsized_c2 = ldd(medsized_ind_c2);
cdistc_c2 = cumsum(distc_c2) ;
medsizec_ind_c2 = find(cdistc_c2>0.5,1);
medsizec_c2 = ldc(medsizec_ind_c2);

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_c2 = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_c2 = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_c2 = sum(ez_dirty_c2.*zprob(indzhatd:indztilded-1));
agge_c_c2 = sum(ez_clean_c2.*zprob(indztilded:end));
agge_c2 = agge_d_c2 + agge_c_c2;

cleanshare_c2 = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));
xi_c2 = xi;
wss_c2 = wss;

% =========================================================================
%                               Table 5
%                        Distributional Impacts
% =========================================================================
% ================ Compute the Results ======================
cutz = [0.2 0.4 0.6 0.8] ;
indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistd_c2>cutz(iz),1) ;
end
indcz(1,1) = indzhatd ;
% Output accounted for by firms of different productivity
yshared_c2 = zeros(1,5) ;
for iz = 1:1:4
    yshared_c2(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yd(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        yshared_c2(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yd(indcz(iz+1):end)) ;
    end
end
yshared_c2 = yshared_c2/aggyd_c2 ;

indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistc_c2>cutz(iz),1) ;
end
indcz(1,1) = indzhatc ;
% Output accounted for by firms of different productivity
ysharec_c2 = zeros(1,5) ;
for iz = 1:1:4
    ysharec_c2(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yc(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        ysharec_c2(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yc(indcz(iz+1):end)) ;
    end
end
ysharec_c2 = ysharec_c2/aggyc_c2 ;

clearvars -except aggyd_c2 aggyc_c2 aggkd_c2 ...
    aggydshare_c2 aggycshare_c2 ...
    aggkc_c2 aggc_c2 apld_c2 aplc_c2 ...
    totfirmd_c2 totfirmc_c2 meanfd_c2 meanfc_c2 ...
    apfd_c2 apfc_c2 apf_c2 wss_c2...
    meansized_c2 meansizec_c2 medsized_c2 medsizec_c2 ...
    agge_c2 cleanshare_c2 yshared_c2 ysharec_c2 xi_c2;
save('./results/gentab_c2.mat');

% ============= Compute the raw numbers for Case (i') ======================
load('./Results/flattax_new.mat');
% =========================================================================
%                               Table 4
%                          Aggregate Impacts
% =========================================================================
% --------------- Panel A -------------------
% Aggregate Output
aggyd_c1p = sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc_c1p = sum(yc(indzhatc:end).*zprob(indzhatc:end));
aggy_c1p  = mud*aggyd_c1p + (1-mud)*aggyc_c1p;
aggydshare_c1p = mud*aggyd_c1p/aggy_c1p;
aggycshare_c1p = 1-aggydshare_c1p;
% Aggregate Capital
aggkd_c1p = sum(kdd(indzhatd:end).*zprob(indzhatd:end));
aggkd_c1p = aggkd_c1p + ke*sum(zprob(indztilded:end));
aggkc_c1p = sum(kdc(indzhatc:end).*zprob(indzhatc:end));
aggk_c1p = mud*aggkd_c1p + (1-mud)*aggkc_c1p ;
% Aggregate Consumption
aggc_c1p = aggy_c1p - delta*aggk_c1p ;
% Labor Market
aggldd_c1p = sum(ldd(indzhatd:end).*zprob(indzhatd:end));
aggldc_c1p = sum(ldc(indzhatc:end).*zprob(indzhatc:end));
agglsd_c1p = sum(zprob(1:indzhatd-1));
agglsc_c1p = sum(zprob(1:indzhatc-1));
aggld_c1p = mud*aggldd_c1p+(1-mud)*aggldc_c1p ;
aggls_c1p = mud*agglsd_c1p+(1-mud)*agglsc_c1p ;
% Output per Worker
apld_c1p = aggyd_c1p/aggldd_c1p;
aplc_c1p = aggyc_c1p/aggldc_c1p;
apl_c1p  = aggy_c1p/aggld_c1p;
% Output per Firm
totfirmd_c1p = sum(zprob(indzhatd:end));
totfirmc_c1p = sum(zprob(indzhatc:end));
totfirm_c1p = mud*totfirmd_c1p + (1-mud)*totfirmc_c1p;
apfd_c1p = aggyd_c1p/totfirmd_c1p;
apfc_c1p = aggyc_c1p/totfirmc_c1p;
apf_c1p  = aggy_c1p/totfirm_c1p;

% Average Firm TFP
%%%
meanfd_c1p = zgrid(indzhatd:end)'*zprob(indzhatd:end)/totfirmd_c1p ;
meanfc_c1p = zgrid(indzhatc:end)'*zprob(indzhatc:end)/totfirmc_c1p ;
meanf_c1p  = mud*meanfd_c1p + (1-mud)*meanfc_c1p ;

% --------------- Panel B -------------------
% Mean Size
meansized_c1p = sum(ldd(indzhatd:end).*zprob(indzhatd:end))/totfirmd_c1p;
meansizec_c1p = sum(ldc(indzhatc:end).*zprob(indzhatc:end))/totfirmc_c1p;
meansize_c1p  = mud*meansized_c1p + (1-mud)*meansizec_c1p;
% Median Size
distd_c1p = zeros(zn,1);
distc_c1p = zeros(zn,1);
distd_c1p(indzhatd:end) = zprob(indzhatd:end)/totfirmd_c1p;
distc_c1p(indzhatc:end) = zprob(indzhatc:end)/totfirmc_c1p;
dist_c1p = mud*distd_c1p+(1-mud)*distc_c1p;
cdist_c1p = cumsum(dist_c1p) ;
medsize_ind_c1p = find(cdist_c1p>0.5,1);
medsize_c1p = ldc(medsize_ind_c1p);
cdistd_c1p = cumsum(distd_c1p) ;
medsized_ind_c1p = find(cdistd_c1p>0.5,1);
medsized_c1p = ldd(medsized_ind_c1p);
cdistc_c1p = cumsum(distc_c1p) ;
medsizec_ind_c1p = find(cdistc_c1p>0.5,1);
medsizec_c1p = ldc(medsizec_ind_c1p);

% --------------- Panel C -------------------
% Only for the polluting sector
% Aggregate Pollution
ez_dirty_c1p = exp(ksid_0)*yd(indzhatd:indztilded-1).^(1+ksid_1);
ez_clean_c1p = exp(ksic_0)*yd(indztilded:end).^(1+ksic_1);
agge_d_c1p = sum(ez_dirty_c1p.*zprob(indzhatd:indztilded-1));
agge_c_c1p = sum(ez_clean_c1p.*zprob(indztilded:end));
agge_c1p = agge_d_c1p + agge_c_c1p;

cleanshare_c1p = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));
xi_c1p = xi;
wss_c1p = wss;

% =========================================================================
%                               Table 5
%                        Distributional Impacts
% =========================================================================
% ================ Compute the Results ======================
cutz = [0.2 0.4 0.6 0.8] ;
indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistd_c1p>cutz(iz),1) ;
end
indcz(1,1) = indzhatd ;
% Output accounted for by firms of different productivity
yshared_c1p = zeros(1,5) ;
for iz = 1:1:4
    yshared_c1p(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yd(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        yshared_c1p(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yd(indcz(iz+1):end)) ;
    end
end
yshared_c1p = yshared_c1p/aggyd_c1p ;

indcz = zeros(1,5) ;
for iz = 1:1:4
    indcz(1,iz+1) = find(cdistc_c1p>cutz(iz),1) ;
end
indcz(1,1) = indzhatc ;
% Output accounted for by firms of different productivity
ysharec_c1p = zeros(1,5) ;
for iz = 1:1:4
    ysharec_c1p(1,iz) =...
        sum(zprob(indcz(iz):indcz(iz+1)-1).*yc(indcz(iz):indcz(iz+1)-1)) ;
    if iz == 4
        ysharec_c1p(1,iz+1) = ...
            sum(zprob(indcz(iz+1):end).*yc(indcz(iz+1):end)) ;
    end
end
ysharec_c1p = ysharec_c1p/aggyc_c1p ;

clearvars -except aggyd_c1p aggyc_c1p aggkd_c1p ...
    aggydshare_c1p aggycshare_c1p ...
    aggkc_c1p aggc_c1p apld_c1p aplc_c1p ...
    totfirmd_c1p totfirmc_c1p meanfd_c1p meanfc_c1p ...
    apfd_c1p apfc_c1p apf_c1p wss_c1p...
    meansized_c1p meansizec_c1p medsized_c1p medsizec_c1p ...
    agge_c1p cleanshare_c1p yshared_c1p ysharec_c1p xi_c1p;
save('./results/gentab_c1p.mat');

% ================ Print the tables to the terminal =======================
clear all ;
load('./results/gentab_bench.mat')
load('./results/gentab_c1.mat')
load('./results/gentab_c2.mat')
load('./results/gentab_c1p.mat')

disp('===================================================================')
disp('                   Table 4: Aggregate Impacts                      ')
disp('===================================================================')
disp('                     Polluting                  Non-polluting      ')
disp('            -------------------------------------------------------')
disp('Statistics  Benchmark    (i)     (ii)    Benchmark     (i)     (ii)')
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Output        ' '100.00' '    ' ...
        num2str(100*aggyd_c1/aggyd_bench,'%8.2f') ...
     '  ' num2str(100*aggyd_c2/aggyd_bench,'%8.2f')  ...
     '     ' '100.00' '      ' ...
      num2str(100*aggyc_c1/aggyc_bench,'%8.2f') ...
     '  ' num2str(100*aggyc_c2/aggyc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Capital       ' '100.00' '    ' ...
        num2str(100*aggkd_c1/aggkd_bench,'%8.2f') ...
     '  ' num2str(100*aggkd_c2/aggkd_bench,'%8.2f')  ...
     '     ' '100.00' '      ' ...
      num2str(100*aggkc_c1/aggkc_bench,'%8.2f') ...
     '  ' num2str(100*aggkc_c2/aggkc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Consumption   ' '100.00' '    ' ...
        num2str(100*aggc_c1/aggc_bench,'%8.2f') ...
     '  ' num2str(100*aggc_c2/aggc_bench,'%8.2f')  ...
     '    ' '100.00' '      ' ...
      num2str(100*aggc_c1/aggc_bench,'%8.2f') ...
     '  ' num2str(100*aggc_c2/aggc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Wage          ' '100.00' '    ' ...
        num2str(100*wss_c1/wss_bench,'%8.2f') ...
     '  ' num2str(100*wss_c2/wss_bench,'%8.2f')  ...
     '     ' '100.00' '      ' ...
      num2str(100*wss_c1/wss_bench,'%8.2f') ...
     '  ' num2str(100*wss_c2/wss_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Y/Worker      ' '100.00' '    ' ...
        num2str(100*apld_c1/apld_bench,'%8.2f') ...
     '  ' num2str(100*apld_c2/apld_bench,'%8.2f')  ...
     '    ' '100.00' '      ' ...
      num2str(100*aplc_c1/aplc_bench,'%8.2f') ...
     '  ' num2str(100*aplc_c2/aplc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Y/Firm        ' '100.00' '    ' ...
        num2str(100*apfd_c1/apfd_bench,'%8.2f') ...
     '  ' num2str(100*apfd_c2/apfd_bench,'%8.2f')  ...
     '    ' '100.00' '      ' ...
      num2str(100*apfc_c1/apfc_bench,'%8.2f') ...
     '  ' num2str(100*apfc_c2/apfc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Avg TFP       ' '100.00' '    ' ...
        num2str(100*meanfd_c1/meanfd_bench,'%8.2f') ...
     '  ' num2str(100*meanfd_c2/meanfd_bench,'%8.2f')  ...
     '    ' '100.00' '      ' ...
      num2str(100*meanfc_c1/meanfc_bench,'%8.2f') ...
     '  ' num2str(100*meanfc_c2/meanfc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['% in Output   ' num2str(100*aggydshare_bench, '%8.2f') '     ' ...
        num2str(100*aggydshare_c1,'%8.2f') ...
     '    ' num2str(100*aggydshare_c2,'%8.2f')  ...
     '    ' num2str(100*aggycshare_bench, '%8.2f') '       ' ...
      num2str(100*aggycshare_c1,'%8.2f') ...
     '   ' num2str(100*aggycshare_c2,'%8.2f')] ;
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
     '    ' num2str(meansizec_bench,'%8.2f') '       ' ...
      num2str(meansizec_c1,'%8.2f') ...
     '  ' num2str(meansizec_c2,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Median Size   ' num2str(medsized_bench,'%8.2f') '      '...
    num2str(medsized_c1,'%8.2f') ...
     '   ' num2str(medsized_c2,'%8.2f')  ...
     '    ' num2str(medsizec_bench,'%8.2f') '       ' ...
      num2str(medsizec_c1,'%8.2f') ...
     '   ' num2str(medsizec_c2,'%8.2f')] ;
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
tmpstring = ...
    ['Regulation    ' num2str(100*xi_bench,'%8.2f') '      ' ...
    num2str(100*xi_c1,'%8.2f') '   '...
    num2str(100*xi_c2,'%8.2f')] ;
disp(tmpstring)
disp('===================================================================')

disp('===================================================================')
disp('                  Table 5: Distributional Impacts                  ')
disp('===================================================================')
disp('                      QU1     QU2     QU3    QU4     QU5')
disp('-------------------------------------------------------------------')
disp('Polluting Sector')
disp(['   Benchmark         ' num2str(100*yshared_bench,'%8.2f')])
disp(['   Case (i)          ' num2str(100*yshared_c1,'%8.2f')])
disp(['   Case (ii)         ' num2str(100*yshared_c2,'%8.2f')])
disp('-------------------------------------------------------------------')
disp('Non-polluting Sector')
disp(['   Benchmark         ' num2str(100*ysharec_bench,'%8.2f')])
disp(['   Case (i)          ' num2str(100*ysharec_c1,'%8.2f')])
disp(['   Case (ii)         ' num2str(100*ysharec_c2,'%8.2f')])
disp('===================================================================')

disp('===================================================================')
disp('                   Table 6: Aggregate Impacts                      ')
disp('===================================================================')
disp('                     Polluting                  Non-polluting      ')
disp('            -------------------------------------------------------')
disp('Statistics  Benchmark    (ip)     (i)    Benchmark     (ip)     (i)')
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Output        ' '100.00' '    ' ...
        num2str(100*aggyd_c1p/aggyd_bench,'%8.2f') ...
     '  ' num2str(100*aggyd_c1/aggyd_bench,'%8.2f')  ...
     '     ' '100.00' '      ' ...
      num2str(100*aggyc_c1p/aggyc_bench,'%8.2f') ...
     '  ' num2str(100*aggyc_c1/aggyc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Capital       ' '100.00' '    ' ...
        num2str(100*aggkd_c1p/aggkd_bench,'%8.2f') ...
     '  ' num2str(100*aggkd_c1/aggkd_bench,'%8.2f')  ...
     '     ' '100.00' '      ' ...
      num2str(100*aggkc_c1p/aggkc_bench,'%8.2f') ...
     '  ' num2str(100*aggkc_c1/aggkc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Consumption   ' '100.00' '    ' ...
        num2str(100*aggc_c1p/aggc_bench,'%8.2f') ...
     '  ' num2str(100*aggc_c1/aggc_bench,'%8.2f')  ...
     '     ' '100.00' '      ' ...
      num2str(100*aggc_c1p/aggc_bench,'%8.2f') ...
     '  ' num2str(100*aggc_c1/aggc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Wage          ' '100.00' '    ' ...
        num2str(100*wss_c1p/wss_bench,'%8.2f') ...
     '  ' num2str(100*wss_c1/wss_bench,'%8.2f')  ...
     '     ' '100.00' '      ' ...
      num2str(100*wss_c1p/wss_bench,'%8.2f') ...
     '  ' num2str(100*wss_c1/wss_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Y/Worker      ' '100.00' '    ' ...
        num2str(100*apld_c1p/apld_bench,'%8.2f') ...
     '  ' num2str(100*apld_c1/apld_bench,'%8.2f')  ...
     '     ' '100.00' '      ' ...
      num2str(100*aplc_c1p/aplc_bench,'%8.2f') ...
     '  ' num2str(100*aplc_c1/aplc_bench,'%8.2f')] ;
disp(tmpstring)
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['#firms        ' '100.00' '     ' ...
        num2str(100*totfirmd_c1p/totfirmd_bench,'%8.2f') ...
     '   ' num2str(100*totfirmd_c1/totfirmd_bench,'%8.2f')  ...
     '     ' '100.00' '       ' ...
      num2str(100*totfirmc_c1p/totfirmc_bench,'%8.2f') ...
     '  ' num2str(100*totfirmc_c1/totfirmc_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Mean Size     ' num2str(meansized_bench,'%8.2f') '     '...
    num2str(meansized_c1p,'%8.2f') ...
     '   ' num2str(meansized_c1,'%8.2f')  ...
     '    ' num2str(meansizec_bench,'%8.2f') '       ' ...
      num2str(meansizec_c1p,'%8.2f') ...
     '  ' num2str(meansizec_c1,'%8.2f')] ;
disp(tmpstring)
disp('-------------------------------------------------------------------')
tmpstring = ...
    ['Pollution     ' '100.00' '     ' ...
    num2str(100*agge_c1p/agge_bench,'%8.2f') '   '...
    num2str(100*agge_c1/agge_bench,'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Intensity     ' '100.00' '     ' ...
    num2str(100*(agge_c1p/aggyd_c1p)/(agge_bench/aggyd_bench),'%8.2f') ...
    '   '...
    num2str(100*(agge_c1/aggyd_c1)/(agge_bench/aggyd_bench),'%8.2f')] ;
disp(tmpstring)
tmpstring = ...
    ['Clean Share   ' num2str(100*cleanshare_bench,'%8.2f') '      ' ...
    num2str(100*cleanshare_c1p,'%8.2f') '   '...
    num2str(100*cleanshare_c1,'%8.2f')] ;
disp(tmpstring)
disp('===================================================================')