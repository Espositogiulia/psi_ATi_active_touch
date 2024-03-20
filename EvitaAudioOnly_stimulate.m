function EvitaAudioOnly_stimulate()
% evita dimensions = 1024 w x 512 h
%x1 = 512.0;
%y1 = 0.0;
%y2 = 600.0;
%w = 12.0;
u = udp('10.42.0.100', 7771);
fopen(u);
% send texture
oscsend(u,'/texture','fsfff',3.0,'cos',50.0,0.0,5000.0);
%% send taxtelse: 1-taxtel number
%           2-texture number to associate to taxtel
%           2-type of taxtel: 'rect' or others (see TqxTe
% args arlOSCf script for more info
%           3-x1
%           4-y1
%           5-x2
%           6-y2
%           x1,y1: upper-left corner of the rect taxtel
%           x2,y2: bottom-right corner of the rect taxtel
%associate test taxtels to texture 0, i.e. Tex with ampl 0
oscsend(u,'/taxtel','ffsffff',0.0,0.0,'rect',0.0,0.0,1024.0,150.0);
oscsend(u,'/taxtel','ffsffff',1.0,0.0,'rect',0.0,0.0,1024.0,150.0);
oscsend(u,'/taxtel','ffsffff',2.0,0.0,'rect',0.0,0.0,1024.0,150.0);
%send rect line with 2mm(=12 pixels) width
oscsend(u,'/taxtel','ffsffff',3.0,3.0,'rect',0.0,0.0,1024.0,150.0);
fclose(u)
