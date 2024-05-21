clear;
clc;
cd('C:\data\gesposito\psiEvita\control_exp');

%% load the files
files = dir('*.mat'); % get all mat files in folder
fileNames = {files.name};
nSubjects = length(fileNames);

for i = 1:nSubjects
    output(i).all = load(files(i).name);
end;
clear i;

%% fix length of vectors if missing/extra trials

% for i = 1:nSubjects
%     dX(i,1) = length(output(i).all.PM_haptic.x);
%     dX(i,2) = length(output(i).all.PM_both.x);
%     dX(i,3) = length(output(i).all.PM_audio.x);
% end
% 
% dX = 1:max(dX(:));
% 
% 
% for i = 1:nSubjects
%     dR(i,1) = length(output(i).all.PM_haptic.response);
%     dR(i,2) = length(output(i).all.PM_both.response);
%     dR(i,3) = length(output(i).all.PM_audio.response);
% end
% 
% dR = 1:max(dR(:));

%% fix length of vectors if missing/extra trials
nTrials = 100; % Target number of trials

for i = 1:nSubjects
    conditions = {'PM_haptic', 'PM_audio', 'PM_both'};
    for condition = conditions
        cond = condition{1}; % Extract the string from the cell
        % x data
        xData = output(i).all.(cond).x;
        output(i).all.(cond).x2=xData;
        if length(xData) > nTrials
            output(i).all.(cond).x2 = xData(1:nTrials); % Truncate to nTrials
        elseif length(xData) < nTrials
            output(i).all.(cond).x2(end+1:nTrials) = NaN; % Pad with NaNs to reach nTrials
        end
        
        % response data
        responseData = output(i).all.(cond).response;
        output(i).all.(cond).response2=responseData;
        if length(responseData) > nTrials
            output(i).all.(cond).response2 = responseData(1:nTrials); % Truncate to nTrials
        elseif length(responseData) < nTrials
            output(i).all.(cond).response2(end+1:nTrials) = NaN; % Pad with NaNs to reach nTrials
        end
    end
end

%% stim levels
stimLevels = [-30:0.5:30];
nStimLevels = numel(stimLevels);
nConditions = 3;
%% 2)  the number of positive responses observed at intensities listed
for i = 1:nSubjects
responseH(i,:) = output(i).all.PM_haptic.response2(1:nTrials);
responseA(i,:) = output(i).all.PM_audio.response2(1:nTrials);
responseB(i,:) = output(i).all.PM_both.response2(1:nTrials);
end

%% 3) 'n' the number of trials used at intensities listed in 'x'. this was wrong, it should be right now. 
for i = 1:nSubjects
stimH(i,:) = output(i).all.PM_haptic.x2(1:nTrials);
stimA(i,:) = output(i).all.PM_audio.x2(1:nTrials);
stimB(i,:) = output(i).all.PM_both.x2(1:nTrials);
end

%% number times intensity presented and positive responses for each level
stimH = stimH';
responseH = responseH';
for i = 1:length(stimLevels)
nStimH(i,:)= sum(stimH == stimLevels(i),1);
posRespH(i,:) = sum(responseH==1 & stimH == stimLevels(i),1);
end


stimA = stimA';
responseA = responseA';
for i = 1:length(stimLevels)
nStimA(i,:)= sum(stimA == stimLevels(i),1);
posRespA(i,:) = sum(responseA==1 & stimA == stimLevels(i),1);
end

stimB = stimB';
responseB = responseB';
for i = 1:length(stimLevels)
nStimB(i,:)= sum(stimB == stimLevels(i),1);
posRespB(i,:) = sum(responseB==1 & stimB == stimLevels(i),1);
end


%% create matrix for exporting
s = repmat(1:nSubjects,nStimLevels*nConditions,1); 
S = reshape(s,nStimLevels*nConditions*nSubjects,1);
c = [ones(nStimLevels,nSubjects);ones(nStimLevels,nSubjects)*2;ones(nStimLevels,nSubjects)*3]; 
C = reshape(c,nStimLevels*nConditions*nSubjects,1);
stimLevels = stimLevels';
x = repmat(stimLevels,nSubjects*nConditions,1); % number of ppts x conditions
n = [nStimH; nStimA; nStimB];
N = reshape(n,nStimLevels*nConditions*nSubjects,1);
y = [posRespH; posRespA; posRespB];
Y = reshape(y,(nStimLevels*nConditions)*nSubjects,1);

data.y = Y;
data.n = N;
data.s = S;
data.c = C;
data.x = x;

dataTable = struct2table(data);
writetable(dataTable,'dataForRefit130524_2.csv');




