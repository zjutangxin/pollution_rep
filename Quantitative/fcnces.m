% -------------------------------------------------------------------------
%                      Program Description
% -------------------------------------------------------------------------
%   
% Purpose:
%     - Evaluates the allocations and returns the excess demand in the 
%       labor market.
%     - Two sectors model: ces
%     - The Size Distribution of Firms and Industrial Water Pollution: A
%       Quantitative Analysis of China.
%  
% Author:
%     Xin Tang @ International Monetary Fund
%  
% Record of Revisions:
%         Date:                 Description of Changes
%     ============        =================================
%      04/10/2019                  Original Version
% =========================================================================

function out = fcnces(input)

% ================== Declare global variables =====================
% Shared with pollution_main.m
% ------ Model parameters -------
% Technology and Preference
global gamma alpha ke rss sigmaces varces
% Size Distribution
global zgrid zprob mud
% Regulation
global xi tauzd tauzc
% ------ Allocations -------
global zhatc zhatd ztilded indzhatc indzhatd indztilded 
global ldd kdd yd ldc kdc yc pi0d pi1d pid pic
% debug
global aggld aggls aggyd aggyc outrhs

wage = input(1);
pd   = input(2);
pc   = ( (1-varces^sigmaces*pd^(1-sigmaces))...
    /(1-varces)^sigmaces )^(1/(1-sigmaces));

% ================== Factor Demand =====================
kappa = (alpha/(1-alpha))*(wage/rss) ;      % capital-labor ratio
% Polluting Firms
ldd = (wage./((1-alpha)*pd*gamma*(1-tauzd)*kappa^(alpha*gamma)))...
    .^(1/(gamma-1)).*zgrid ;                % labor demand
kdd = kappa*ldd ;                             % capital demand
yd  = zgrid.^(1-gamma)*(kappa^(alpha*gamma)).*(ldd.^gamma) ;   % Output
pi0d = (1-xi)*(pd*(1-tauzd).*yd - wage*ldd - rss*kdd);      % dirty profits
pi1d = (pd*(1-tauzd).*yd - rss*kdd - wage*ldd) - rss*ke;    % clean profits
pid  = max(pi0d,pi1d);
% Non-polluting Firms
ldc = (wage./((1-alpha)*pc*gamma*(1-tauzc)*kappa^(alpha*gamma)))...
    .^(1/(gamma-1)).*zgrid ;                % labor demand
kdc = kappa*ldc ;                             % capital demand
yc  = zgrid.^(1-gamma)*(kappa^(alpha*gamma)).*(ldc.^gamma) ;   % Output
pic  = pc*(1-tauzc).*yc - rss*kdc - wage*ldc ;

% ============== Entry and Technology Adoption ================
indztilded = find(pi1d>pi0d,1);
ztilded = zgrid(indztilded) ;
indzhatd = find(pid>wage,1) ;
indzhatc = find(pic>wage,1) ;
zhatd = zgrid(indzhatd) ;
zhatc = zgrid(indzhatc) ;

% ============== Labor Market Clearing ================
aggldd = ldd(indzhatd:end)'*zprob(indzhatd:end);
aggldc = ldc(indzhatc:end)'*zprob(indzhatc:end);
agglsd = sum(zprob(1:indzhatd-1));
agglsc = sum(zprob(1:indzhatc-1));

aggld = mud*aggldd+(1-mud)*aggldc ;
aggls = mud*agglsd+(1-mud)*agglsc ;

aggyd = mud*sum(yd(indzhatd:end).*zprob(indzhatd:end));
aggyc = (1-mud)*sum(yc(indzhatc:end).*zprob(indzhatc:end));

outrhs = ((varces/(1-varces))*(pc/pd))^sigmaces;

out = (aggld - aggls)^2 + (aggyd/aggyc-outrhs)^2 ;

end     % End of function fcn