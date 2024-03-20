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

for i = 1:nSubjects
    dX(i,1) = length(output(i).all.PM_haptic.x);
    dX(i,2) = length(output(i).all.PM_both.x);
    dX(i,3) = length(output(i).all.PM_audio.x);
end

dX = 1:max(dX(:));


for i = 1:nSubjects
    dR(i,1) = length(output(i).all.PM_haptic.response);
    dR(i,2) = length(output(i).all.PM_both.response);
    dR(i,3) = length(output(i).all.PM_audio.response);
end

dR = 1:max(dR(:));

%% x(length(x)+1:length(y)) = NaN;
for i =1:length(output)
    if length(output(i).all.PM_haptic.x)<length(dX)
        output(i).all.PM_haptic.x(length(output(i).all.PM_haptic.x)+1:length(dX)) = NaN;
    end
    if length(output(i).all.PM_haptic.response)<length(dR)
        output(i).all.PM_haptic.response(length(output(i).all.PM_haptic.response)+1:length(dR)) = NaN;
       % output(i).all.PM_haptic.response(length(dR)) = NaN;
    end
    % audio
    if length(output(i).all.PM_audio.x)<length(dX)
        output(i).all.PM_audio.x(length(output(i).all.PM_audio.x)+1:length(dX)) = NaN;
       % output(i).all.PM_audio.x(length(dX)) = NaN;
    end
    if length(output(i).all.PM_audio.response)<length(dR)
        output(i).all.PM_audio.response(length(output(i).all.PM_audio.response)+1:length(dR)) = NaN;
        %output(i).all.PM_audio.response(length(dR)) = NaN;
    end
    %both
    if length(output(i).all.PM_both.x)<length(dX)
        output(i).all.PM_both.x(length(output(i).all.PM_both.x)+1:length(dX)) = NaN;
        %output(i).all.PM_both.x(length(dX)) = NaN;
    end
    if length(output(i).all.PM_both.response)<length(dR)
        output(i).all.PM_both.response(length(output(i).all.PM_both.response)+1:length(dR)) = NaN;
        %output(i).all.PM_both.response(length(dR)) = NaN;
    end
end
%% stim levels
stimLevels = [-30:0.5:30];
nStimLevels = numel(stimLevels);
nConditions = 3;
%% 2)  the number of positive responses observed at intensities listed
for i = 1:nSubjects
responseH(i,:) = output(i).all.PM_haptic.response;
responseA(i,:) = output(i).all.PM_audio.response;
responseB(i,:) = output(i).all.PM_both.response;
end

%% 3) 'n' the number of trials used at intensities listed in 'x'.
for i = 1:nSubjects
stimH(i,:) = output(i).all.PM_haptic.x(1:end-1);
stimA(i,:) = output(i).all.PM_audio.x(1:end-1);
stimB(i,:) = output(i).all.PM_both.x(1:end-1);
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
c = [ones(nStimLevels,nSubjects);ones(nStimLevels,nSubjects)*2;ones(nStimLevels,nSubjects)*3]; % 81 = stim levels, 6 = participants
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
writetable(dataTable,'dataForRefit.csv');




