    %This script is adapted from the PAL_AMPM_Demo script (see behaptic) from the PALAMEDES
%toolbox to assess whether participant's performance improve when both
%tactile and auditory cues are provided
clc;
clear
Priority(1);
% load('randparams.mat') % matrix containing random/counterbalancing across ppts
randparams=[1 1];
%session info:
SN=input('Participant number?');
%start=1;%input('Session?');
filename=[date(),'_PsiEvitaAT_S',num2str(SN)];
%isInterleaved=abs(randparams(SN,1)-1*(start-1));
nBlocks=10;
Vol=input('Volume');
rng(SN)
%avgSpeed= input('avg speed');
%% stimulation params
% Auditory stimulus
t=get(0, 'Screensize');
pixelScreen=t(3);
lengthFrame=213; %mm
InitializePsychSound;
pahandle = PsychPortAudio('Open');
bufferdata=rand(1000,2);
PsychPortAudio('Volume',pahandle,Vol);
bufferhandle = PsychPortAudio('FillBuffer',pahandle,bufferdata');

%% Psi method params:
% addpath('E:\Palamedes')
NumTrials=100 ; %for one block
PF = @PAL_CumulativeNormal; %assumed function=> change to probit
marginalize=[];
stimRange=-30:0.5:30; %mm
priorAlphaRange = -30:0.5:30;% mm
priorBetaRange =  linspace(log10(.05),log10(5),50); %log10(slope)
%if slope=5 the transition from .1 to .9 detection probabili
priorLambdaRange=0:0.02:0.2;

priorAlpha(1,1,:)=ones(size(priorAlphaRange))./length(priorAlphaRange); %uniform prior
priorBeta(1,1,:)=ones(size(priorBetaRange))./length(priorBetaRange); %uniform prior

pg=PAL_pdfNormal(priorLambdaRange,0,.5);
priorLambda(1,1,:)=pg./sum(pg);
s=[size(priorAlpha,3),size(priorBeta,3),size(priorLambda,3)];

pA=repmat(permute(priorAlpha,[3 1 2]),[1 s(2) s(3)]);
pB=repmat(permute(priorBeta,[1 3 2]),[s(1) 1 s(3)]);
pL=repmat(permute(priorLambda,[1 2 3]),[s(1) s(2) 1]);

prior=pA.*pB.*pL;
prior=prior./sum(sum(sum(prior)));
p(:,:,1,:)=prior; %for size compatibility
%% Initialize PM structure gpu to T
%% Initialize PM structure
%(use of single() cuts downnBlocks on memory load)
%delengthFramepending on the computer, we may want to set
%(use of single() cuts down on memory load)
%depending on the computer, we may want to set gpu to T
PM = PAL_AMPM_setupPM('priorAlphaRange',single(priorAlphaRange),...
    'priorBetaRange',single(priorBetaRange),...
    'priorLambdaRange',single(priorLambdaRange), 'numtrials',NumTrials,...
    'PF' , PF,'stimRange',single(stimRange),'marginalize',marginalize,'gammaEQlambda',1,'prior',p);

PM_haptic=PM;
PM_both=PM;
PM_audio=PM;

%% Generate condition sequence
% We don't want  to have too many trials that are consecutively of one
% condition, to avoid inducing wrong expectation in the participant's mind
% Define whether the stimulus is presented before or after the midline
% Modalities: 1=tactile only, 2=both, 3=audio only
%Define order of the modalities
m=repmat([1 2 3],1,2);
mod=zeros(nBlocks,NumTrials*3/nBlocks);
condition=zeros(1,3*NumTrials);
for i=1:3*NumTrials/length(m)
    condition((1:length(m))+length(m)*(i-1))=m(randperm(length(m)));
end
for i=1:nBlocks
    mod(i,:)=condition((1:(NumTrials*3/nBlocks))+(NumTrials*3/nBlocks)*(i-1));
end
disp('saving settings (this may take a while')
save([filename '_setting'])

clearvars -except PM_audio PM_haptic PM_both nBlocks filename ...
    isInterleaved mod blockOrder lengthFrame pixelScreen ...
    pahandle avgSpeed

%% Trial loop
nTB=size(mod,2);
t=0:1/44100:0.2-1/44103;
v1=sin(2*pi*400*t);
for j=1:nBlocks
    disp(['Block ',num2str(j),'/',num2str(nBlocks)])
    disp('Press key when ready to start')
    pause()
    
    for i=1:nTB
        %recover the right PM
        switch mod(j,i)
            case 1
                PM=PM_haptic;
            case 2
                PM=PM_both;
            case 3
                PM=PM_audio;
        end
        WaitSecs(1)
        disp(['Trial ',num2str(i),'/',num2str(nTB)]);
        disp(mod(j,i));
        sound(v1,44100)
        x=PM.x(end);
        xE=round((x+1024/12)*6);
        xF=round((x+lengthFrame/2)*pixelScreen/lengthFrame);
        response=3;
        while response>2
            sound(v1,44100)
            xx=[];
            yy=[];
            if mod(j,i)<3
                EvitaAH_stimulate(xE);
            end
            if mod(j,i) == 3
                EvitaAudioOnly_stimulate();%clear stimulus from evita
            end
            x=0;
            SetMouse(0,0);
            while x<200
                [x,y,buttons]=GetMouse;
            end
            t0=GetSecs;
            while x<xF
                [x,y,buttons]=GetMouse;
                xx=[xx x];
                yy=[yy y];
            end
            if mod(j,i) >1
                PsychPortAudio('Start',pahandle);
                
            end
            t1=GetSecs;
            S(j,i)=lengthFrame/pixelScreen*sum(sqrt(((xx(2:end)-xx(1:end-1)).^2+(yy(2:end)-yy(1:end-1)).^2)))/(t1-t0);
            response=input('Detected? 1=first half, 2=second half, 3= not detected');
        end
        response=response-1;
        %update PM based on response
        PM = PAL_AMPM_updatePM(PM,response);
        %reassign PM to the correct object
        switch mod(j,i)
            case 1
                PM_haptic=PM;
            case 2
                PM_both=PM;
            case 3
                PM_audio=PM;
        end
        close all
    end
    sound(v1,44100)
    pause(0.5)
    sound(v1,44100)
    clc
    clearvars PM response x xE xF xx yy t0 t1
    disp('interim saving (may take sometime)')
    save(filename)
end
disp('finished')
%%
p=input('plot?');
if p
    Colours=[.90 .60 0;.35 .70 0.9;0.2 .9 .5];
    figure
    subplot(1,3,1)
    hold on
    plot(PM_both.threshold,'color',Colours(1,:))
    plot(PM_haptic.threshold,'color',Colours(2,:))
    plot(PM_audio.threshold,'color',Colours(3,:))
    plot(PM_both.threshold+1.97*PM_both.seThreshold,'--','color',Colours(1,:))
    plot(PM_both.threshold-1.97*PM_both.seThreshold,'--','color',Colours(1,:))
    plot(PM_haptic.threshold+1.97*PM_haptic.seThreshold,'--','color',Colours(2,:))
    plot(PM_haptic.threshold-1.97*PM_haptic.seThreshold,'--','color',Colours(2,:))
    plot(PM_audio.threshold+1.97*PM_audio.seThreshold,'--','color',Colours(3,:))
    plot(PM_audio.threshold-1.97*PM_audio.seThreshold,'--','color',Colours(3,:))
    plot(1:100,zeros(1,100),'k')
    xlim([1 100])
    hold off
    legend('Both','Tactile','Audio','location','ne')
    xlabel('trial')
    ylabel('Distance(mm)')
    title('threshold estimate')
    
    subplot(1,3,2)
    hold on
    plot(10.^(PM_both.slope),'color',Colours(1,:))
    plot(10.^(PM_haptic.slope),'color',Colours(2,:))
    plot(10.^(PM_audio.slope),'color',Colours(3,:))
    plot(10.^(PM_both.slope+1.97*PM_both.seSlope),'--','color',Colours(1,:))
    plot(10.^(PM_both.slope-1.97*PM_both.seSlope),'--','color',Colours(1,:))
    plot(10.^(PM_haptic.slope+1.97*PM_haptic.seSlope),'--','color',Colours(2,:))
    plot(10.^(PM_haptic.slope-1.97*PM_haptic.seSlope),'--','color',Colours(2,:))
    plot(10.^(PM_audio.slope+1.97*PM_audio.seSlope),'--','color',Colours(3,:))
    plot(10.^(PM_audio.slope-1.97*PM_audio.seSlope),'--','color',Colours(3,:))
    plot(1:100,zeros(1,100),'k')
    xlim([1 100])
    hold off
    legend('Both','Tactile','Audio','location','ne')
    xlabel('trial')
    set(gca,'yscale','log')
    title('Slope estimate')
    
    subplot(1,3,3)
    hold on
    plot(PM_both.lapse,'color',Colours(1,:))
    plot(PM_haptic.lapse,'color',Colours(2,:))
    plot(PM_audio.lapse,'color',Colours(3,:))
    plot(PM_both.lapse+1.97*PM_both.seLapse,'--','color',Colours(1,:))
    plot(PM_both.lapse-1.97*PM_both.seLapse,'--','color',Colours(1,:))
    plot(PM_haptic.lapse+1.97*PM_haptic.seLapse,'--','color',Colours(2,:))
    plot(PM_haptic.lapse-1.97*PM_haptic.seLapse,'--','color',Colours(2,:))
    plot(PM_audio.lapse+1.97*PM_audio.seLapse,'--','color',Colours(3,:))
    plot(PM_audio.lapse-1.97*PM_audio.seLapse,'--','color',Colours(3,:))
    plot(1:100,zeros(1,100),'k')
    xlim([1 100])
    hold off
    legend('Both','Tactile','Audio','location','ne')
    xlabel('trial')
    ylabel('Distance(mm)')
    title('lapse estimate')
    %%
    figure
    hold on
    plot(PM_both.x,'color',Colours(1,:))
    plot(PM_haptic.x,'color',Colours(2,:))
    plot(PM_audio.x,'color',Colours(3,:))
    plot(1:100,zeros(1,100),'k')
    hold off
    legend('Both','Tactile','Audio','location','ne')
    %% 
 figure
    X= -30:30;
    aB=PM_both.threshold(end);
    bB=10^PM_both.slope(end);
    gB= PM_both.guess(end);
    lB=PM_both.lapse(end);
    YB=gB+(1-gB-lB)./(1+exp(-bB*(X-aB)));
    plot(X,YB,'color',Colours(1,:))
    hold on
    
    aT=PM_haptic.threshold(end);
    bT=10^PM_haptic.slope(end);
    gT= PM_haptic.guess(end);
    lT=PM_haptic.lapse(end);
    YT=gT+(1-gT-lT)./(1+exp(-bT*(X-aT)));
    plot(X,YT,'color',Colours(2,:))
    
    aA=PM_audio.threshold(end);
    bA=10^PM_audio.slope(end);
    gA= PM_audio.guess(end);
    lA=PM_audio.lapse(end);
    YA=gA+(1-gA-lA)./(1+exp(-bA*(X-aA)));
    plot(X,YA,'color',Colours(3,:))
     
    plot(-30:30,0.5,'k')
    plot([0 0],[0 1],':k')
    legend('Both','Tactile','Audio','location','ne')
end
