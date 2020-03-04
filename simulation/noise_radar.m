
cflat = @(C) [C{:}];

tau_p = 1;

alpha = 20;
quadRamp = @(t)alpha*(mod(t,tau_p).^2);
chirp = @(t)exp(1j*pi*quadRamp(t));
syncPulse = @(t)1.*(mod(t,tau_p)<tau_p/100);

Atmosphere = @(x,tau,fd) @(t)x(t-tau).*exp(-1j*2*pi*fd);

t = -eps:0.01:10*tau_p;
x_tx = chirp;

delay = 0.5;
x_rx = Atmosphere(x_tx,delay,0);

y_sync = syncPulse(t);
d = [0 diff(y_sync)>0];
y = conj(x_tx(t)).*x_rx(t);

plot(t,d,t,y);
axis('tight');

start_idx = find(d);
num_chunks = numel(start_idx);
y_chunks = {};

for i=1:(num_chunks-1)
  y_chunks{end+1} = y(start_idx(i):start_idx(i+1)-1);
end
M = min(cellfun(@numel,y_chunks));

y_grid = cflat(cellfun(@(x)x(1:M).',y_chunks,'Un',0)).';

Y_grid = fft(y_grid,[],2);
Y_grid_mag = 20*log10(abs(Y_grid));
imagesc(Y_grid_mag)