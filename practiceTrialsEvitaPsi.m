 %% stimulation params
% Auditory stimulus
clc;
clear;
rng('shuffle');
Vol = input('volume');
t=get(0, 'Screensize');
pixelScreen=t(3);
lengthFrame=213; %mm
InitializePsychSound;
pahandle = PsychPortAudio('Open');
bufferdata=rand(1000,2);
PsychPortAudio('Volume',pahandle,Vol);
bufferhandle = PsychPortAudio('FillBuffer',pahandle,bufferdata');
%Vol=1; 
Priority(1);
pTrials = repmat([1   2   3 ], 1,6)';
%pTrials = repmat(2,1, 18)';
pDist = [-35  -35 -35  -30 -30  -30 -25 -25 -25 25 25 25 30 30 30 35 35 35]';
p=[pTrials pDist];
%p=p(randperm(size(p,1)),:);
pTrials=p(:,1);
pDist= p(:,2);
   disp('Press key when ready to start')
   pause;
%%
for i = 1:length(pTrials)
    disp(pTrials(i))
    disp(pDist(i))
    pE=(pDist(i)+1024/12)*6;
    pF=(pDist(i)+lengthFrame/2)*pixelScreen/lengthFrame;
    if pTrials(i) < 3
        EvitaAH_stimulate(pE);
    end
    if pTrials(i) == 3
        disp('Only UAdio')
        EvitaAudioOnly_stimulate();
    end
    if pTrials(i)>1
        x=0;
        SetMouse(0,0);
        xx=[];
        yy=[];
        while x<200
            [x,y,buttons]=GetMouse;
        end
        t0=GetSecs;
        while x<pF
            [x,y,buttons]=GetMouse;
            
            xx=[xx x];
            yy=[yy y];
        end
        PsychPortAudio('Start',pahandle);
        t1=GetSecs;
        speed(i)=lengthFrame/pixelScreen*sum(sqrt(((xx(2:end)-xx(1:end-1)).^2+...
            (yy(2:end)-yy(1:end-1)).^2)))/(t1-t0);
    end
    disp('press to continue');
    pause;
end
avgSpeed= mean(speed(8:18))
Vol