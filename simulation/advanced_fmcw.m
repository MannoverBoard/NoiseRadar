clc;
clear all;

cflat = @(C) [C{:}];

%%
SPEED_OF_LIGHT = 299792458;
k_boltzmann = 1.38064852e-23;
T0_standard = 290;

getDelayFromRangeTwoWay = @(R) 2*R/SPEED_OF_LIGHT;
getRangeFromDelayTwoWay = @(tau) SPEED_OF_LIGHT*tau/2;

generateReturn = @(ant,tgt_range,tgt_delay) @(t)(ant.Ptx*ant.Gtx*ant.Grx*ant.lambda^2)./((4*pi)^3.*tgt_range(t).^4+eps).*...
  ant.x_bb(t-tgt_delay(t)).*exp(-1j*2*pi*ant.fc*tgt_delay(t));%HACK RCS
generateReturn = @(ant,tgt_range) generateReturn(ant,tgt_range,@(t)getDelayFromRangeTwoWay(tgt_range(t)));

%%
tgt_range = @(t) 30*t+100;

%%
tau_p = 20e-3;
BW = 1e6;
fc = 2.54e9;
lambda = SPEED_OF_LIGHT/fc;
G0 = powerFromDb(8.1);

Ptx = powerFromDb(10);%HACK-ish todo find later


R_test = 1e3;
rcs_test = powerFromDb(0);
SNR_test = powerFromDb(12);
Prx_test = ((Ptx*G0^2*lambda^2*rcs_test)/((4*pi)^3*R_test^4));
Pn = Prx_test / SNR_test;
B_test = BW;
Noise_Figure = Pn/(k_boltzmann*T0_standard*B_test);


%%
alpha = BW/tau_p;
quadRamp = @(t)alpha*(mod(t,tau_p).^2);
chirp = @(t)exp(1j*pi*quadRamp(t));
syncPulse = @(t)1.*(mod(t,tau_p)<tau_p/100);

V0 = sqrt(Ptx);
transmit = @(t)V0*chirp(t);

ant = struct('Ptx',Ptx,'Gtx',G0,'Grx',G0,'fc',fc,'lambda',lambda,'Fn',Noise_Figure,'BW',BW,'tau_p',tau_p,'x_bb',transmit);


%%

fs = 2*BW;
Ts = 1/fs;
t = -eps:1/fs:10*tau_p;
X_tx = ant.x_bb;
X_sync = syncPulse;
X_rx = generateReturn(ant,tgt_range);

Y = @(t)conj(X_tx(t)).*X_rx(t);

%%
lgc = 0<=t & t<=tau_p/40;

H=figure(1);clf;
t_tmp = t(lgc);
x = X_tx(t_tmp);
t_tmp = t_tmp*1e6;
h=plot(t_tmp,real(x),t_tmp,imag(x));
arrayfun(@(h)set(h,'LiNeWiDtH',2),h);
h=xlabel('\bftime [\mus]');h.FontSize=12;
h=ylabel('\bfx_{tx}(t) baseband [V]');h.FontSize=12;
h=legend({'real','imag'});h.FontSize=14;
grid('on');

%%
H=figure(2);clf;
x = X_tx(t);
x_sync = X_sync(t);
h=plot(t*1e6,abs(x),t,x_sync);
arrayfun(@(h)set(h,'LiNeWiDtH',2),h);
h=xlabel('\bftime [\mus]');h.FontSize=12;
%h=ylabel('\bfx_{tx}(t) baseband [V]');h.FontSize=12;
h=legend({'|x|','sync'});h.FontSize=14;
grid('on');


%%
H=figure(3);clf;
lgc = 0<=t & t<=tau_p/20;
t_tmp = t(lgc);
x_tx = X_tx(t_tmp);
x_rx = X_rx(t_tmp);
t_tmp = t_tmp*1e6;

yyaxis('left');
h=plot(t_tmp,real(x_tx),'LineWidth',2);
yyaxis('right');
h=plot(t_tmp,real(x_rx),'LineWidth',2);
% arrayfun(@(h)set(h,'LiNeWiDtH',2),h);
h=xlabel('\bftime [\mus]');h.FontSize=12;
%h=ylabel('\bfx_{tx}(t) baseband [V]');h.FontSize=12;
h=legend({'x_{tx}(t)','x_{rx}(t)'});h.FontSize=14;
grid('on');


%%
H=figure(4);clf;

lgc = true(size(t));
t_tmp = t(lgc);
y_tmp = Y(t_tmp);
x_sync = X_sync(t_tmp);

lgc = (0<=t & t<tau_p/5);
H=figure(4);clf;
yyaxis('right');
h=plot(t_tmp,real(y_tmp),'Linewidth',2);
yyaxis('left');
h=plot(t_tmp,x_sync,'Linewidth',2);
arrayfun(@(h)set(h,'LiNeWiDtH',2),h);
h=xlabel('\bftime [s]');h.FontSize=12;
grid('on');


%%

H=figure(5);clf;
plot(t,tgt_range(t));
h=xlabel('\bftime [s]');h.FontSize=12;
h=ylabel('\bfRange [m]');h.FontSize=12;
grid('on');
h=title('\bfTarget Range vs Time');

%%


y = Y(t);
x_sync = X_sync(t);

noise = 1*randn(size(t));

d = (x_sync >0.5);
d = [0,diff(d)>0];

start_idx = find(d);
num_chunks = numel(start_idx);
y_chunks = {};
for i=1:(num_chunks-1)
  y_chunks{end+1} = y(start_idx(i):start_idx(i+1)-1); %#ok<SAGROW>
end
M = min(cellfun(@numel,y_chunks));

y_grid = cflat(cellfun(@(x)x(1:M).',y_chunks,'Un',0)).';

Y_grid = fft(y_grid,[],2);
Y_grid_mag = 20*log10(abs(Y_grid));

fast_time = Ts*[0:M-1];
slow_time = t(start_idx);
imagesc(fast_time*1e6,slow_time,Y_grid_mag);
h=xlabel('\bffrequences or something');h.FontSize=12;
h=ylabel('\bfslow time [s]');h.FontSize=12;
h=colorbar;

%%

function [Y] = powerFromDb(X)
  Y = 10^(X/10);
end

function [Y] = dBFromPower(X)
  Y = 10*log10(X);
end