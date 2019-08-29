#############################################################
# BUILDING THE ACT_CONS_PAT  /  RES_CONS_PAT 
# The algorithm orientates to Anand et al. 2019
#############################################################



RES_CONS_PAT(:,1)=BASE; % see Balakrihsnan 2011. 
COR = 0.8; % Correlation level. [BASE WITH UNIT-LEVEL]
COR_UNIT=0.2+COR*rand(ProductionEnvironment.UnitSize,1);
% Generating the unit-level consumption                   
for i=2:ProductionEnvironment.UnitSize 
M = [BASE,RES_CONS_PAT_PRE(:,i)];
R = [1 COR_UNIT(i); COR_UNIT(i) 1];
L = chol(R);
M = M*L;
RESOURCE_CONSUMPTION_PRE = M(:,2);
RES_CONS_PAT(:,i)=(RESOURCE_CONSUMPTION_PRE);
end

% Correlation (COR2) level. [UNIT-LEVEL WITH NON-UNIT-LEVEL]
% Building the reference non-unit-level consumption
M = [BASE,RES_CONS_PAT_PRE(:,(ProductionEnvironment.UnitSize+1))];
R = [1 COR2; COR2 1];
L = chol(R);
M = M*L;
RESOURCE_CONSUMPTION_PRE = M(:,2);
BASE_BATCH=(RESOURCE_CONSUMPTION_PRE);
% CORR_BATCH_UNIT = corr(BASE_BATCH,BASE)
% Building all non-unit-level consumption
for i=(ProductionEnvironment.UnitSize+1):ProductionEnvironment.NUMB_RES % ACTIVITIES 
M = [BASE_BATCH,RES_CONS_PAT_PRE(:,i)];
R = [1 COR; COR 1];
L = chol(R);
M = M*L;
RESOURCE_CONSUMPTION_PRE = M(:,2);
RES_CONS_PAT(:,i)=(RESOURCE_CONSUMPTION_PRE)*1.6; % Just for visualization
end 
sum(RES_CONS_PAT,2); % for checking the total rcu


% Multiply each resource consumption with 10 
for j=1:ProductionEnvironment.NUMB_RES % Adapted from Anand et al 2017.
RES_CONS_PAT(:,j) = ceil(abs(RES_CONS_PAT(:,j).*10));
end



%% DENSITY IMPLEMENTATION %
bZero_matrix = rand(ProductionEnvironment.NUMB_PRO,ProductionEnvironment.NUMB_RES)<DENS_RUN; % Zeroing
RES_CONS_PAT =bZero_matrix.*RES_CONS_PAT; % Establish zeros 



%% EXPECTION HANDLER  & CHECKS 
% CHECK: IF EVERY PRODUCT HAS AN ACTIVTY; 
countZero= sum(RES_CONS_PAT==0,2); % Count zeros in a row 
countNonZero= sum(RES_CONS_PAT>0,2);
index = find(countZero==ProductionEnvironment.NUMB_RES); % find the index of the zero line, if too many zeros in activity- it will be randomly fulfilled with the pre design matrix
if index>0
RES_CONS_PAT(:,1)= ceil(BASE*10) ;
end


%% CHECK: IF EVERY ACTIVITY HAS AT LEAST ONE COMPONENT
countZero=sum(RES_CONS_PAT==0);
index = find(countZero==ProductionEnvironment.NUMB_PRO); % find the index of the activity with no product
if index>0
index_pro =randi([1 ProductionEnvironment.NUMB_PRO]);
RES_CONS_PAT(index_pro,index)= randi([1 40]);
end



%% CHECKS  AND DESCRIPTIVE STATISTCS 

%CORR_TEST = corrcoef(RES_CONS_PAT);
%Correlation measurement between Unit and Batch
%CORR = corrcoef(RES_CONS_PAT_t);
CORR_unit_and_batch = corr(RES_CONS_PAT(:,1),RES_CONS_PAT(:,(ProductionEnvironment.UnitSize+1)));
CHECK.CORR_unit_and_batch = mean(CORR_unit_and_batch);

CHECK.AverageNUMBofProductsConsumeResource = mean(countNonZero);      % Average number of products consuming a resource
CHECK.ZeroSHAREoftheConsumptionMatrix = sum(countZero)./numel(RES_CONS_PAT);




end
