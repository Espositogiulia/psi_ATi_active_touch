 %% adapt audio to tactile stimulus staircase
clear;
x = -30;
%% stimulation params
% Auditory stimulus
t=get(0, 'Screensize');
pixelScreen=t(3);
lengthFrame=215; %mm
InitializePsychSound;
pahandle = PsychPortAudio('Open');
bufferdata=rand(1000,2);
bufferhandle = PsychPortAudio('FillBuffer',pahandle,bufferdata');
%%
xE=(x+1024/12)*6;
xF=(x+lengthFrame/2)*pixelScreen/lengthFrame;
%%
Amplitudes = [];
ss= 0.1;
 j = 0;
 Ourm = [];
 Ampl = 1;
 while j < 10
     x=0;
     EvitaAH_stimulate(xE);
     SetMouse(0,0);
                while x<xF
                    [x,y,buttons]=GetMouse;
                end  
                PsychPortAudio('Volume',pahandle,Ampl);
                PsychPortAudio('Start',pahandle);
     Amplold = Ampl;
     val = input('Which is stronger (0-Audio, 1-Tactile)');
     if val == 0
          Ampl = Ampl - ss;
     elseif val == 1
         Ampl = Ampl + ss;
     end

     %outD=vertcat(zeros(44100/2,1),outD); %250 = 4ms
     Amplitudes = [Amplitudes Ampl];
%      disp(Amplitudes(end))
     if length(Amplitudes) > 3
         if (Amplitudes(end -1) < Amplitudes(end)) && (Amplitudes(end -2) > Amplitudes(end-1))         
             
            j = j+1;
%    disp(num2str(j));
            Ourm = [Ourm Amplitudes(end-1)];
         end
         if (Amplitudes(end -1) > Amplitudes(end)) && (Amplitudes(end -2) < Amplitudes(end-1))
            j = j+1;
%             disp(num2str(j));
            Ourm = [Ourm Amplitudes(end-1)];
         end
    end
 end
figure; plot(Amplitudes);
M = mean(Ourm(2:end))