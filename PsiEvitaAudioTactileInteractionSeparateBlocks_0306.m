%This script is adapted from the PAL_AMPM_Demo script (see below) from the PALAMEDES
%toolbox to assess whether participant's performance improve when both
%tactile and auditory cues are provided
clc;
clear
Priority(1);
randparams=[1 1];
%session info: 
SN=input('Participant number?');
filename=[date(),'_PsiEvitaAT_S',num2str(SN)];

% nBlocks=3;
nBlocks=12; 
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
NumTrials=25 ; %for one block
PF = @PAL_CumulativeNormal; %assumed function=> change to probit
marginalize=[];
stimRange=-20:0.5:20; %mm
priorAlphaRange = -20:0.5:20;% mm
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
%% Initialize PM structure
%(use of single() cuts down on memory load)
%depending on the computer, we may want to set gpu to T
PM = PAL_AMPM_setupPM('priorAlphaRange',single(priorAlphaRange),...
    'priorBetaRange',single(priorBetaRange),...
    'priorLambdaRange',single(priorLambdaRange), 'numtrials',NumTrials,...
    'PF' , PF,'stimRange',single(stimRange),'marginalize',marginalize,'gammaEQlambda',1,'prior',p);
%% Generate condition sequence
%Modalities: 1=tactile only, 2=both, 3=audio only
%Define order of the modalities
blockOrder=[1 2 3];
blockOrder = blockOrder(randperm(3));
blockOrder=repmat(blockOrder,1,nBlocks/3);

disp('saving settings (this may take a while')
save(filename)

clearvars -except PM nBlocks filename ...
    isInterleaved mod blockOrder lengthFrame pixelScreen ...
    pahandle avgSpeed
%%
PM_haptic=PM;
PM_both = PM;
PM_audio = PM;
%% Trial loop
t=0:1/44100:0.2-1/44100;
v1=sin(2*pi*400*t);
%v1 = v1*0.5;
S=zeros(nBlocks,PM.numTrials);
startTime = [];
for j=1:nBlocks
     %blockOrder(j)=3
    switch blockOrder(j)
        case 1
            cond='Tactile only';
    	    PMa = PM_haptic;
        case 2
            cond='Audio-tactile';
    	    PMa = PM_both;
        case 3
            cond='Audio only';
    	    PMa = PM_audio;
    end
    disp(['Block ',num2str(j),'/',num2str(nBlocks)])
    disp(cond)
    % disp('Press key when ready to start')
    WaitSecs(1);
    for i=1:PM.numTrials
        %disp(['Trial ',num2str(i),'/',num2str(nTB)]);
        disp(['Trial ',num2str(i)]);
        sound(v1,44100)
         x=PMa.x(end);
        xE=round((x+1024/12)*6);
        xF=round((x+lengthFrame/2)*pixelScreen/lengthFrame);
        response=3;
        %response=3;
        while response>2
            xx=[];
            yy=[];
            if blockOrder(j)<3
                EvitaAH_stimulate(xE);            
            end
            if blockOrder(j) == 3
                EvitaAudioOnly_stimulate();%clear stimulus from evita
            end
            if blockOrder(j)>1
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
                PsychPortAudio('Start',pahandle);  
                t1=GetSecs;
                S(j,i)=lengthFrame/pixelScreen*sum(sqrt(((xx(2:end)-xx(1:end-1)).^2+(yy(2:end)-yy(1:end-1)).^2)))/(t1-t0);
            end
            response=input('Detected? 1=first half, 2=second half, 3= not detected');
        end
        response=response-1;
        PMa = PAL_AMPM_updatePM(PMa,response);
    end
    sound(v1,44100)
    WaitSecs(0.5)
    sound(v1,44100)
    clc
    disp('interim saving (may take sometime)')
    switch blockOrder(j)
        case 1
            PM_haptic = PMa;
            save(filename,'PM_haptic','-append')
        case 2
            PM_both = PMa;
            save(filename,'PM_both','-append')
        case 3
            PM_audio = PMa;
            save(filename,'PM_audio','-append')
    end
end
disp('finished')
save([filename '_speed'], 'S');
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
    bB=PM_both.slope(end);
    gB= PM_both.guess(end);
    lB=PM_both.lapse(end);
    YB=gB+(1-gB-lB)./(1+exp(bB*(X-aB)));
    plot(X,YB)
    hold on
    
    aT=PM_haptic.threshold(end);
    bT=PM_haptic.slope(end);
    gT= PM_haptic.guess(end);
    lT=PM_haptic.lapse(end);
    YT=gT+(1-gT-lT)./(1+exp(bT*(X-aT)));
    plot(X,YT,'r')
    
    aA=PM_audio.threshold(end);
    bA=PM_audio.slope(end);
    gA= PM_audio.guess(end);
    lA=PM_audio.lapse(end);
    YA=gA+(1-gA-lA)./(1+exp(bA*(X-aA)));
    plot(X,YA,'k')
    
    plot(-30:30,0.5,'k')
    plot([0 0],[0 1],':k')
end