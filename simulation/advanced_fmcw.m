clc;
clear all;

cflat = @(C) [C{:}];

%%
SPEED_OF_LIGHT = 299792458;

getDelayFromRangeTwoWay = @(R) 2*R/SPEED_OF_LIGHT;
getRangeFromDelayTwoWay = @(tau) SPEED_OF_LIGHT*tau/2;

generateReturn = @(x,fc,tgt_delay) @(t)x(t-tgt_delay(t)).*exp(-1j*2*pi*fc*tgt_delay(t));
generateReturn = @(x,fc,tgt_range) generateReturn(x,fc,@(t)getDelayFromRangeTwoWay(tgt_range(t)));

%%
tgt_range = @(t) 330000*t+100e3;

%%
tau_p = 20e-3;
BW = 1e6;
fc = 2.54e9;

%%

alpha = BW/tau_p;
quadRamp = @(t)alpha*(mod(t,tau_p).^2);
chirp = @(t)exp(1j*pi*quadRamp(t));
syncPulse = @(t)1.*(mod(t,tau_p)<tau_p/100);

%%

fs = 2*BW;
Ts = 1/fs;
t = -eps:1/fs:10*tau_p;
X_tx = chirp;
X_sync = syncPulse;
X_rx = generateReturn(X_tx,fc,tgt_range);

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
X_rx = X_rx(t_tmp);
t_tmp = t_tmp*1e6;
h=plot(t_tmp,real(x_tx),t_tmp,real(X_rx));
arrayfun(@(h)set(h,'LiNeWiDtH',2),h);
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
h=plot(t_tmp,x_sync,t_tmp,real(y_tmp));
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
