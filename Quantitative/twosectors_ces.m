% -------------------------------------------------------------------------
%                      Program Description
% -------------------------------------------------------------------------
%   
% Purpose:
%     - Main program to solve the stationary equilibrium. 
%     - Two Sectors Model: CES aggregation
%     - Table J in Appendix J
%     - The Size Distribution of Firms and Industrial Water Pollution: A
%       Quantitative Analysis of China.
%  
% Author:
%     - Xin Tang @ International Monetary Fund
% 
% File Dependence:
%     - fcnces.m: evaluates all the allocations at a given wage and returns 
%       the excess demand in the labor market.
%  
% Record of Revisions:
%         Date:                 Description of Changes
%     ============        =================================
%      04/10/2019                 Original Version
% =========================================================================
clc;
clear all;

tic;
% ================== Declare global variables =====================
% Shared with fcn.m
% ------ Model parameters -------
% Technology and Preference
global gamma alpha ke rss sigmaces varces
% Size Distribution
global zgrid zprob mud
% Regulation
global xi tauzc tauzd

% ------ Allocations -------
global zhatc zhatd ztilded indzhatc indzhatd indztilded 
global ldd kdd yd ldc kdc yc pi0d pi1d pid pic
% ---debug ---
global aggld aggls aggyd aggyc outrhs

% =========================================================================
%                      Data Dictionary
% =========================================================================
% Utility variables
tol = 1e-6 ;
% Technology and Preference
beta   = 0.8750 ;           % Discount Factor
gamma  = 0.93 ;             % Span-of-control
alpha  = 0.50/gamma ;       % Capital share
delta  = 0.10 ;             % Depreciation rate
ksid_0 = -3.4114 ;          % Dirty technology: intercept
ksid_1 = -0.3636 ;          % Dirty technology: slope
ksic_0 = -4.3747 ;          % Clean technology: intercept
ksic_1 = -0.3288 ;          % Clean technology: slope
rss    = 1/beta-1+delta ;   % Equilibrium interest rate

% CES = 1.5
% sigmaces = 1.5;
% ke     = 1.50 ;             % Fixed cost of clean technology
% varces = 0.31 ;   % Share of polluting good in final production

% CES = 3.0
ke = 1.30 ;
sigmaces = 3.0 ;
varces = 0.42 ; % ces = 3.0 

% Size Distribution
mud   = 0.20 ;               % Share of entrepreneurs in polluting sector
m     = -2.4567 ;            % Mean of log(z)
sigma = 4.0020 ;             % Std.dev of log(z)
v     = sigma^2 ;            % Variance of log(z)
zmin  = exp(m - 3*sigma) ;   % Lower truncation of log(z)
zmax  = exp(m + 3*sigma) ;   % Upper truncation of log(z)
zend  = exp(m + 3*sigma) + 10820.4 ;      % "Superstar" firm z
zendw = 0.001444/3 ;         % "Superstar" firm mass

% Regulation
xi   = 0.2300 ;      % Loss of profits using dirty technology
phi0 = 1.15 ;       % Tax intercept
phi1 = -0.03 ;      % Tax slope

% =========================================================================
%                  Discretize firm TFP
% =========================================================================
zn       = 5000 ;
zgrid    = linspace(zmin, zmax, zn-1)' ;
zdiff    = (zgrid(2) - zgrid(1))/2.0 ;
zprob    = zeros(zn-1,1) ;
zprob(1) = logncdf(zgrid(1)+zdiff,m,sigma) - logncdf(1e-9,m,sigma) ;
for indz = 2:1:zn-1
    zprob(indz) = logncdf(zgrid(indz)+zdiff,m,sigma) ...
        - logncdf(zgrid(indz)-zdiff,m,sigma) ;
end
zprob(zn-1) = logncdf(zgrid(zn-1)+10000, m, sigma) -...
    logncdf(zgrid(zn-1)-zdiff, m, sigma) ;

% Distribute the residual mass from truncation
XX = sum(zprob) ;
if XX < 1
    zprob = zprob + (1 - XX)/(zn-1) ;
end

% Attach the extreme value part
zgrid = [zgrid; zend] ;
zprob = [zprob*(1-zendw); zendw] ;

% Up to now, zgrid is the z' = (1-tauz)*z^(1-gamma) in the paper
% Now we transform z' to z
zgrid = (zgrid/(phi0^(1/(1-gamma)))).^((1-gamma)/(1-gamma+phi1));

% benchmark
% tauzd = max(0,1-phi0*zgrid.^phi1);
% tauzc = max(0,1-phi0*zgrid.^phi1);

% no tax
tauzd = zeros(zn,1);
tauzc = zeros(zn,1);

% =========================================================================
%               Allocate memory to remaining variables
% =========================================================================
% Polluting Industry
pi0d = zeros(zn,1) ;     % profit dirty
pi1d = zeros(zn,1) ;     % profit clean
pid  = zeros(zn,1) ;     % profit envelope
ldd  = zeros(zn,1) ;     % labor demand
kdd  = zeros(zn,1) ;     % capital demand
yd   = zeros(zn,1) ;     % output
pic  = zeros(zn,1) ;     % profit
ldc  = zeros(zn,1) ;     % labor demand
kdc  = zeros(zn,1) ;     % capital demand
yc   = zeros(zn,1) ;     % output

% =========================================================================
%                  Solve the equilibrium wage
% =========================================================================
% CES = 1.5
% Benchmark
% wage = 0.4847;
% priced = 0.5617;

% No polluting tax
% wage = 0.5429 ;
% priced = 0.4775 ;

% No tax
% wage = 0.7756;
% priced = 0.5616 ;

% CES = 3.0
% Benchmark
% wage = 0.4212;
% priced = 0.5285;

% No polluting tax
% wage = 0.4866 ;
% priced = 0.4618 ;

% No tax
wage = 0.6739;
priced = 0.5286 ;

pguess = [wage priced];

[opprices mdist extflag] = ...
     fminsearch(@fcnces,pguess);
out = fcnces(opprices);
disp('wage price_d price_c')
wss = opprices(1);
priced = opprices(2);
pricec = ( (1-mud^sigmaces*priced^(1-sigmaces))...
    /(1-mud)^sigmaces )^(1/(1-sigmaces));
disp([wss priced pricec])
disp('ld ls outlhs outrhs out')
disp([aggld aggls aggyd/aggyc outrhs out])

% =========================================================================
%             Compute Average Tax and Firm Size Distribution
% =========================================================================
cutleft = [1,20,50,100,400,10000];
totfirm = mud*sum(zprob(indzhatd:end))+(1-mud)*sum(zprob(indzhatc:end));
tote = mud*sum(ldd(indzhatd:end).*zprob(indzhatd:end))+...
    (1-mud)*sum(ldc(indzhatc:end).*zprob(indzhatc:end));
indmc = pic>wss;
indmd = pid>wss;
firmmodelc = zeros(5,1) ;
esmodelc = zeros(5,1) ;
firmmodeld = zeros(5,1) ;
esmodeld = zeros(5,1) ;
% Polluting Sector
for indbin = 1:1:5
    seld = find(ldd>=cutleft(indbin) & ldd<cutleft(indbin+1));
    if isempty(seld) == 1
        continue ;
    end
    firmmodeld(indbin) = sum(zprob(seld).*indmd(seld)) ;
    esmodeld(indbin) =...
        sum(zprob(seld).*ldd(seld).*indmd(seld)) ;    
end

% Non-polluting Sector
for indbin = 1:1:5
    selc = find(ldc>=cutleft(indbin) & ldc<cutleft(indbin+1));
    if isempty(selc) == 1
        continue ;
    end
    firmmodelc(indbin) = sum(zprob(selc).*indmc(selc)) ;
    esmodelc(indbin) =...
        sum(zprob(selc).*ldc(selc).*indmc(selc)) ;    
end

% Whole Distribution
firmmodel = (mud*firmmodeld+(1-mud)*firmmodelc)/totfirm;
esmodel = (mud*esmodeld+(1-mud)*esmodelc)/tote;

fsdata = [0.4698 0.2797 0.1290 0.1020 0.0195];
esdata = [0.0864 0.1611 0.1655 0.3501 0.2369];

string = ['gamma = ', num2str(gamma)];
disp(string)
disp('Whole Distribution')
disp('Firm Size: Model vs Data')
disp([firmmodel'; fsdata]);
disp('Employment Size: Model vs Data')
disp([esmodel'; esdata]);

disp('Polluting Sector')
disp('Firm Size and Employment Share')
disp(firmmodeld'/sum(zprob(indzhatd:end)))
disp(esmodeld'/sum(zprob(indzhatd:end).*ldd(indzhatd:end)))

disp('Non-polluting Sector')
disp('Firm Size and Employment Share')
disp(firmmodelc'/sum(zprob(indzhatc:end)))
disp(esmodelc'/sum(zprob(indzhatc:end).*ldc(indzhatc:end)))

disp('Average Tax =')
aggtaud = sum(tauzd(indzhatd:end).*zprob(indzhatd:end));
aggtauc = sum(tauzc(indzhatc:end).*zprob(indzhatc:end));
disp((mud*aggtaud+(1-mud)*aggtauc)/totfirm);

disp('Yd, Yc, Y, Yd_share')
aggyd = mud*sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc = (1-mud)*sum(yc(indzhatc:end).*zprob(indzhatc:end));
aggyf = (varces*aggyd^((sigmaces-1)/sigmaces)+...
    (1-varces)*aggyc^((sigmaces-1)/sigmaces))^(sigmaces/(sigmaces-1));
disp([aggyd aggyc aggyf priced*aggyd/(priced*aggyd+pricec*aggyc)]);

cleanshare = sum(zprob(indztilded:end))/sum(zprob(indzhatd:end));
disp('Clean Share')
disp(cleanshare)

% save('./Results/benchmark_ces.mat');
% save('./Results/nodirty_ces.mat');
% save('./Results/notax_ces.mat');
% save('./Results/benchmark_ces3.mat');
% save('./Results/nodirty_ces3.mat');
save('./Results/notax_ces3.mat');
