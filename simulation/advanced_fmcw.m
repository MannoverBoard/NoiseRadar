%EE 724 Fall 2019 Homework 10

%#ok<*UNRCH,*NBRAK,*DEFNU,*CLALL>
clear('global');
clc;
global G; 

save_figs = true;
prefix = 'ee724_hw_12';
BUDGE_IT = true;

STRETCH_PX_UNITTEST = true;
FAST_TIME_TEST = true;
CR_ONLY_TEST  = true;
SAR_IMAGE_UNITTEST = true;
SAR_COLORTEST = true;
SAR_COLORTEST_RGB = true;
ACTUAL_HW_12 = true;

NUM_DRAW_TGT = 11;
SAR_SCENE_SCALE = .9;
SHOW_TRUE_TARGETS = true;
ADD_NOISE = false;
ZOOM_TARGETS = true;
RADAR_HUD = true;

if save_figs
 G.DEFAULT_WINDOW_SIZE = ceil([640,480]);
end
ant_speed = 60;%#ok<NASGU>%FYI, inversely proportional to CR precision,
% but too fast causes circular blurring (but still good color discrim)
CR_cell = 0e3;%#ok<NASGU> %causes a rotation of the imaged region,
% (+CR-> -yaw), (-CR-> +yaw),
% with angle of resulting transformation yaw = -atan2(CR_cell,DR_cell)
% and a downscale of sort value (for (-20e3,20e3)-> ~1/sqrt(2) )
DR_cell = 20e3;%#ok<NASGU> %FYI, proportional to CR precision,              
% but too large causes cross range blurring
FLIGHT_TIME = 12;%#ok<NASGU> %FYI, inversely proportional to CR precision,
% but too long causes circular blurring (but still good color discrim)
LFW_BW = 500e6;%FYI, inversely proportional to DR precision
LFM_tau_p = 100e-6;

%%%%%%%%%%%%%%%%%%
% Actual HW 12 setup
CR_cell = 200;
DR_cell = 20e3;
lambda = 0.03;
SAR_len = 600;
ant_speed = 50;
FLIGHT_TIME = SAR_len/ant_speed;
LFW_BW = 150e6;%FYI, inversely proportional to DR precision
LFM_tau_p = 50e-6;
%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%Anon Functions
tern = @(varargin) varargin{3-logical(varargin{1})}();
%saveas = @(varargin) tern(save_figs,@()saveas(varargin{:}),@()[]);
fixForFilename = @(s)strjoin(regexp(s,['[^-A-Za-z' ...
    '0-9]+'],'split'),'_');
makeFilename = @(varargin) ...     
    fixForFilename(strjoin([{prefix};varargin(:)],'_'));
%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
%SAR Givens
sar_info = SarProto();
sar_info.cell.dim.CR_width = 50;
sar_info.cell.dim.DR_width = 50;

sar_info.cell.prec.CR_width = sar_info.cell.dim.CR_width/100;
sar_info.cell.prec.DR_width = sar_info.cell.dim.DR_width/100;

sar_info.cell.plat.position = @(t)repmat([DR_cell;CR_cell;0],1,length(t));
sar_info.flight_time = FLIGHT_TIME;
sar_info.start_time = -sar_info.flight_time/2;
sar_info.CR_window = @hamming;%FYI this gets suppresses CR sidelobes

%Antenna Parameters
ant = AntennaProto();
ant.Ptx = 1;
ant.fc = 10e9; %X band
ant.fs_BB = 10e6;%baseband sampling rate
%Stretch Processing params
ant.tau_p = LFM_tau_p;
ant.Bt = LFW_BW;
ant.plat.position = @(t) 0 + [0;ant_speed;0]*t;
ant.rx_window = @hamming_time;%FYI this gets suppresses DR sidelobes
%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%


if STRETCH_PX_UNITTEST
   [Hs] = stretchProcessorTest(ant);
end

if FAST_TIME_TEST
  [H,suffix] = oldFastTimeTest(ant,tgts,sar_info,ZOOM_TARGETS,ADD_NOISE,SHOW_TRUE_TARGETS);
end

if CR_ONLY_TEST
  [H,title_strs] = oldCrOnlyTest(ant,sar_info,BUDGE_IT);
end

if SAR_IMAGE_UNITTEST
  try
    [~,~,~,~] = generateSarImage(ant,tgts,sar_info);
    fprintf('SAR UNITTEST passed.\n');
  catch ME
    fprintf('SAR UNITTEST failed threw an exception.\n');
    disp(ME);
    rethrow(ME);
  end
end

if SAR_COLORTEST
  [Hs] = sarColorTest(ant,sar_info,NUM_DRAW_TGT,SAR_SCENE_SCALE,SAR_COLORTEST_RGB,ZOOM_TARGETS);
end

if ACTUAL_HW_12
  [Hs] = actualHomework12(ant,sar_info,SAR_SCENE_SCALE,BUDGE_IT,RADAR_HUD,prefix,save_figs);
end

% /Main workspace
%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%
% Object Prototypes
function [out] = PlatformProto()
  out = struct(...
    'orientation',@(t)repmat(eye(3),[1,1,length(t)]),...
    'position'   ,@(t)zeros(3,length(t)) ...
  );
end

function [out] = AntennaProto()
  out = struct(...
    'plat',PlatformProto(),...
    ... %WAVEFORM PARAMS  
    'Ptx',[],...
    'fc',[],...
    'pattern',@isotropic,...
    'tau_p',[], ...
    'Bt',[], ...
    'fs_BB',[], ...
    'rx_window',@rect ...
  );
  out.getLambda  = @(ant) SPEED_OF_LIGHT()/ant.fc;
  out.getCarrier = @(ant) @(t)exp(2j*pi*ant.fc*t);
end


function [out] = TargetProto()
  out = struct(...
    'plat',PlatformProto(),...
    'sigma',@isotropic     ...
  );
end

function [gain] = isotropic(fc,az,el)
  gain = 1+0*fc.*az.*el;
end

function [f_pattern] = genGaussianBand(f0,B)
  %produces gain pattern centered at f0 with bandwidth B, height 1
  S = -1/(2*B^2);
  f_pattern = @(f)exp(S*(f-f0).^2);
end
%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
% Physical Constants
function [c] = SPEED_OF_LIGHT()
  %Approximate value for speed of light (m/s)
  c = 3e8;%299792458
end

function [T] = STANDARD_TEMP()
  %Standard Temperature (Kelvin)
  T = 290;
end

function [k] = BOLTZMANNS_CONSTANT()
  % Boltzmann's Constant (J/K)
  k = 1.38064852e-23;
end
% /Physical Constants
%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
function [varargout] = genTwoWayReturn(antenna,targets)
  if ~any(nargout==[1,2])
    error('Expected 1 (vrx) or 2 (vrx,R_rel) outputs');
  end
  %Check preconditions & grab vars for copy-value closure capture
  Ptx = antenna.Ptx;assert(~isempty(Ptx),'Ptx should not be empty');
  fc = antenna.fc;assert(~isempty(fc),'fc should not be empty');
  lam = antenna.getLambda(antenna);assert(~isempty(lam),'lambda should not be empty');
  carrier = antenna.getCarrier(antenna);assert(~isempty(carrier),'carrier should not be empty');
  W = antenna.plat.orientation;assert(~isempty(W),'orientation should not be empty');
  G = antenna.pattern;assert(~isempty(G),'Gain should not be empty');
  
  tgts = targets(:);
  ntgt = length(tgts);
  
  r_ant = antenna.plat.position;
  r_tgts = arrayfun(@(tgt)tgt.plat.position,tgts,'Un',0);
  
  r_rel = @(t)cflat(cellfun(@(r_tgt)r_tgt(t)-r_ant(t),r_tgts,'Un',0));
  R_rel = @(t)sqrt(sum(abs(r_rel(t)).^2,1));
  sig = @(f,a,e)cflat(arrayfun(@(n)tgts(n).sigma(f,a(n,:),e(n,:)),1:ntgt,'Un',0));
  sig = @(f,a,e)sig(f,reshape(a,[],ntgt).', reshape(e,[],ntgt).');
    
  vrx = @(vbb,t,ref_delay,tgt_delays)sum(reshape(...
    sqrt(calculateTwoWayPrx(Ptx,G,fc,lam,W(t),r_rel(t),sig)).* ...
    ... % 2 way power attenuation
    vbb(  repmat(t,1,ntgt)-(tgt_delays-ref_delay)).* ...
    ... % transmit baseband shifted forward in time by tau
    ... % but then shifted back by tau0, expects zero centered time t
    carrier(-tgt_delays) ...
    ... %after downmix from RF, the residual phase is the carrier of the delay
    ... %this is where the doppler shift comes in because this term includes fc
    ,[],ntgt),2).';
  
  vrx = @(vbb,t0_tx,t,R_ref) vrx(vbb,t,getDelayFromRangeTwoWay(R_ref(t0_tx)),getDelayFromRangeTwoWay(R_rel(t0_tx+t)));
  %Vrx is the receive waveform back at baseband (downmixed by carrier)
  % t0 is the time offset of the center of the pulse
  % t is expected to be zero centered fast time,
  % R_ref is a range that will provide a reference delay
  %  (typically the tracked target so that the target shows up at 0 range)
  
  if nargout==1
    varargout = {vrx};
  elseif nargout==2
    R_rel = @(t)reshape(R_rel(t),[],3).';
    varargout = {vrx,R_rel};
  end
end

function [Prx] = calculateTwoWayPrx(Ptx,G,fc,lambda,W_ant,r_rel,sigma)
 r_rel = tensorRotate(W_ant,r_rel);
 [R,az,el] = polarFromCartesian(r_rel);
  Prx = (Ptx.*G(fc,az,el).^2.*lambda.^2.*sigma(fc,az,el))./ ...
   ((4*pi)^3*R.^4);
end

function [tau] = getDelayFromRangeTwoWay(R)
  %Computes 2-way delay from a range
  tau = 2*R/SPEED_OF_LIGHT();
end

function [R] = getRangeFromDelayTwoWay(tau)
  %Computes 2-way range from a delay (time)
  R = SPEED_OF_LIGHT()/2*tau;
end

function [Rr] = rangeRateFromDopplerTwoWay(fd,fc)
  Rr = -fd*(SPEED_OF_LIGHT()/(2*fc));
end

function [fd] = getRopplerFromRangeRateTwoWay(Rr,fc)
  fd = -2*Rr*fc/SPEED_OF_LIGHT();
end
%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
% Waveform Generation
function [X]= rect(t,T)
    %Simple window function centered at 0, with width T, 
    % and height 1
    T = T/2;
    t = abs(t/T);
    X = 1.*(-1<=t & t<=+1);
end

function [X] = rect_window(N)
  X = ones(1,N);
end

function [X] = hamming_time(t,T)
  %@(t,T)(0.54+.46*cos(2*pi*t/T)).*rect(t,T);
  X = (0.54+.46*cos(2*pi*t/T)).*rect(t,T);
end

function [f] = genLFM(T,B,w)
   %generates an LFM function that is centered at 0
   % with width T
   % bandwidth B
   % and windowed by w
   if nargin<3
       w = @rect;
   end
   a = B/T;
   f = @(t) exp(1j*pi*a*t.^2).*w(t,T);
end

function [h] = genMatchedFilter(v) 
    %generates the matched filter function given function v
    h = @(t) conj(v(-t));
end

function [t,vbb,ranges,Vo] = genStretchProcessor(ant,range_extent,compensate_hs,sinc_interpolate_factor)
  if nargin<4 || isempty(sinc_interpolate_factor)
    sinc_interpolate_factor = 1;
  end
  if nargin<3 || isempty(compensate_hs)
    compensate_hs = true;
  end
  
  %Generates a Stretch processor
  % (Noting that range precision is proportional to 1/tau_h)
  %LFM Baseband, Carrier, Transmited Signals
  Bt    = ant.Bt;
  tau_p = ant.tau_p;
  alpha = Bt/tau_p;
  vbb = genLFM(tau_p,Bt);
  carrier = ant.getCarrier(ant);
  
  % De-ramp system extra width
  delta_tau_R = getDelayFromRangeTwoWay(range_extent);
    
  tau_h = tau_p + delta_tau_R;  
  Bh = Bt*tau_h/tau_p;%larger bandwidth because it runs for longer
  if ~isfield(ant,'rx_window') || isempty(ant.rx_window)
    window = @rect;
  else
    window = ant.rx_window;
  end
  h_s = genLFM(tau_h,Bh,window);
  
  fs_BB = ant.fs_BB;
  Ts_BB = 1/fs_BB;
  t = -tau_h/2:Ts_BB:tau_h/2;%"Fast time" samples
  Nif = length(t)*sinc_interpolate_factor; 
  %Adds extra samples in FFT stage, does not actually increase resolution
  f = fs_BB/2*linspace(-1,1,Nif);    
  range_delays = f/alpha;
  ranges = getRangeFromDelayTwoWay(range_delays);
  
  %check that max delta f can be captured by this fs_BB
  delta_f_R = alpha*delta_tau_R;
  assert(delta_f_R<=fs_BB/2,...
    sprintf(['IF frequency must be higher than %g Hz to capture a range'... 
    ' extent of %f'],2*delta_f_R,range_extent));
  
  if compensate_hs
    h_s = @(t,tau0,tau_hat) 1/(tau_p)*h_s(t-(tau_hat-tau0));
  else
    %uncompensated de-ramp, for testing only
    h_s = @(t,tau0,tau_hat) 1/(tau_p)*h_s(t);
  end
  
  v_if = @(vrx,t0_tx,tau0,tau_hat) vrx(t0_tx,t).*conj(carrier(-tau_hat)); 
    
  mixer = @(vrx,t0_tx,tau0,tau_hat) ...
    conj(v_if(vrx,t0_tx,tau0,tau_hat)).*h_s(t,tau0,tau_hat);
  
  mixer = @(vrx,t0_tx,R_ref) ...
    mixer(vrx,t0_tx,getDelayFromRangeTwoWay(R_ref(t0_tx)),getDelayFromRangeTwoWay(R_ref(t0_tx+t)));
    
  Vo0 = Ts_BB;
  Vo = @(vrx,t0_tx,R_rel)fftshift(fft(mixer(vrx,t0_tx,R_rel),Nif))*Vo0;
  %fft output of stretch processor,
  % expects a function vrx of signature vrx = @(vbb,t0,t,R_ref) 
  %  typically output by genTwoWayReturn
  % t0_tx is the zero centered transmit time (middle of pulse)
  % R_rel is the reference (tracked) range as a function of time
end
% /Waveform Generation
%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
% Spatial Helper functions
function [u_parallel,u_perpendicular] = projectUontoV(U,V)
  U_mag = sqrt(sum(abs(U).^2,1));
  V_mag = sqrt(sum(abs(V).^2,1));
  u_parallel = U.*dot(U,V)./U_mag./V_mag;
  u_perpendicular = U-u_parallel;
end

function [u] = unitize(pos)
  u = pos./getRange(pos);
end

function [range] = getRange(ned)
  range = sqrt(sum(abs(ned).^2,1)); 
end

function [range_rate] = getRangeRate(pos,vel)
   range_rate = dot(pos,vel)./getRange(pos);
end

function [rr_para,rr_perp] = getRangeRateVectors(pos,vel)
   [rr_para,rr_perp] = projectUontoV(vel,pos);
end

function [R,az,el] = polarFromCartesian(ned)
  R = sqrt(sum(abs(ned).^2,1)); 
  el = -asin(ned(3,:)./R);
  el(isnan(el))=0;
  az = atan2(ned(2,:),ned(1,:));
end

function [R] = Yaw(y)
    %Yaw(y) = Rotate North->East
    %Yaw(y) = [+c,-s, 0]
    %         [+s,+c, 0]
    %         [ 0, 0, 1]
    z = zeros(size(y));
    c = cos(y);
    s = sin(y);
    R = reshape([c;s;z;-s;c;z;z;z;1+z],3,3,[]);
end

function [R] = Pitch(p)
    %Pitch(p) = Rotate Down->North
    %Pitch(p) = [+c, 0,+s]
    %           [ 0, 1, 0]
    %           [-s, 0,+c]
    z = zeros(size(p));
    c = cos(p);
    s = sin(p);
    R = reshape([c;z;-s;z;1+z;z;s;z;c],3,3,[]);
end

function [R] = Roll(r)
    %Yaw(y) = Rotate East->Down
    %Yaw(y) = [ 1, 0, 0]
    %         [ 0,+c,-s]
    %         [ 0,+s,+c]
    z = zeros(size(r));
    c = cos(r);
    s = sin(r);
    R = reshape([1+z;z;z;z;c;s;z;-s;c],3,3,[]);
end

function [r2] = tensorRotate(W,r)
  %Expects Rotations of W (potentially a tensor)
  % and series of target positions
  %W(0),W(1),...*[r1(0),r1(1),...r1(n),r2(0),...];
  szw = size(W);
  if length(szw)<=2
    r2 = W*r;
    return;
  end
  szr = size(r);
  N = szw(3);%num time steps
  idx = 0:N:szr(2)-1;
  r2 = zeros(size(r));
  for n=1:N
    r2(:,n+idx) = W(:,:,n)*r(:,n+idx);
  end
end

function [vel] = estimateVelocity(pos,time,dim)
  if nargin<3
    dim = 2;
  end
  sz = size(pos);
  S = struct('type','()','subs',[]);
  S.subs = repmat({':'},1,length(sz));
  S.subs{dim}=sz(dim)+1;
  pos = subsasgn(pos,S,nan);
  
  shp = ones(size(sz));
  shp(dim)=length(time)+1;
  time = reshape([time nan],shp);
  
  vel = diff(pos,[],dim)./diff(time,[],dim);
end

function [speed] = estimateSpeed(varargin)
  vel = estimateVelocity(varargin{:});
  speed = getRange(vel);
end
% /Spatial Helper functions
%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
% Matlab Helper functions
function [out] = cflat(X)
    out = [X{:}];
end

function [Y] = unroll(X)
  Y = X(:);
end

function [cm] = radar_hud(N)
    if nargin<1 || isempty(N)
        N = 64;
    end
    if N<=0
        error('N must be positive');
    end
    bottom = [0 0 0];
    botmiddle = [0 125 0]/255;
    middle = [60 255 60]/255;
    topmiddle = [150, 255, 120]/255;
    top = [1 1 1];
    C = [bottom;botmiddle;middle;topmiddle;top];
    
    n = linspace(0,1,N);
    cm = interp1(linspace(0,1,size(C,1)),C,n);
end

function [cm] = america(N)
    if nargin<1 || isempty(N)
        N = 64;
    end
    if N<=0
        error('N must be positive');
    end
    bottom = [0 0 0.5];
    botmiddle = [0 0.5 .9];
    middle = [1 1 1];
    topmiddle = [.8 0 0];
    top = [0.5 0 0];
    C = [bottom;botmiddle;middle;topmiddle;top];
    
    n = linspace(0,1,N);
    cm = interp1(linspace(0,1,size(C,1)),C,n);
end

function [out]=nextFigure()
    global G;
    if isemptyfield(G,'FIGNUM')
        G.FIGNUM=1;
    end
    out = figure(G.FIGNUM);clf;
    if ~isemptyfield(G,'DEFAULT_WINDOW_SIZE')
      out.Position = [out.Position(1:2) G.DEFAULT_WINDOW_SIZE];
    end
    G.FIGNUM=G.FIGNUM+1;
end

function [out] = isvec(v)
    out = ~all(size(v)>1);
end

function [out] = isrow(v)
    sz = size(v);
    out = isvec(v) && (sz(1)==1) && (sz(2)>1);
end

function [out] = isemptyfield(S,field)
  out = isempty(S) || ~isfield(S,field) || isempty(S.(field));
end

function [s]=vec2str(v)
    if isscalar(v)
        s=num2str(v);
        return;
    end
    if isvec(v)
        if isrow(v)
            sep = ',';
        elseif iscolumn(v)
            sep = ';';
        end
        s = ['[',strjoin(arrayfun(@num2str,v,'Un',0),sep),']'];
        return;
    end
    assert(false,'vec2str not implemented for matrices');
end

function [varargout] = minmax(varargin)
  if nargout<1
    error('Expected at least one output');
  end
  out1 = cell(1,2);[out1{:}] = min(varargin{:});
  out2 = cell(1,2);[out2{:}] =max(varargin{:});
  if nargin==1
    %[mn,mx],idx mn,idx mx
    varargout = {[out1{1},out2{1}]};
  elseif nargin==2
    varargout = [out1(1),out2(1)];
  elseif nargin==3
    varargout = [out1(:).',out2(1)];
  else
    varargout = [out1(:).',out2(:)];
  end
end
% /Matlab Helper functions
%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
% SAR Functions
function [Ts,slow_t,cross_ranges,edge_ranges,center_ranges] = calculateSarCrossrange(ant,sar_info)
  %Helper function to calculate SAR cross range times and imaged cross
  %range from antenna and sar_info structs
  lambda = ant.getLambda(ant);
  
  %approximate max speed to approximate how fast things needs to be sampled
  %in time (within so many meters)
  CR_axis = diff(ant.plat.position(sar_info.timeLinspace(sar_info,2)),[],2);
  avg_speed = getRange(CR_axis)/sar_info.flight_time;
  CR_axis = unitize(CR_axis);
  [~,DR_min] = projectUontoV(sar_info.cell.plat.position(sar_info.start_time),CR_axis);
  %DR_axis = unitize(DR_min);
  DR_min = getRange(DR_min);
  %Down_axis = cross(DR_axis,CR_axis);
  %worldFromBody = [DR_axis,CR_axis,Down_axis];
  
  N = 1e3;%hack-ish
  flight_t = sar_info.timeLinspace(sar_info,N);
  speeds = estimateSpeed(ant.plat.position(flight_t)-sar_info.cell.plat.position(flight_t),...
    flight_t);
  max_speed = max(abs(speeds));
  dt = 5/max_speed;
  
  flight_t = sar_info.timeSampled(sar_info,dt);
  
  ant_pos = ant.plat.position(flight_t);
  ant_center_pos =  sar_info.cell.plat.position(flight_t)-ant_pos;
  center_ranges = minmax(unroll(getRange(ant_center_pos)));
  
  ant_pos = reshape(ant_pos,3,1,[]);
  ant_edge_pos = sar_info.cell.getCorners(sar_info.cell,flight_t)-ant_pos;
  edge_ranges = minmax(unroll(getRange(ant_edge_pos)));

  %calculates maximum doppler using tangent speed of furthest point
  %relative to center divided by down range to center
  max_rr = (sar_info.cell.dim.CR_width/2*avg_speed/DR_min);
  max_fd = abs(getRopplerFromRangeRateTwoWay(max_rr,ant.fc));
  min_fs = 2*max_fd;
  max_Ts = 1/min_fs;
  
  if isfield(sar_info,'Ts') && ~isempty(sar_info.Ts)
    Ts = sar_info.Ts;
  else
    Ts = max_Ts;
  end
  
  if Ts>sar_info.flight_time/2
    error(['Current SAR parameters have resulted in a maximum slow time rate of (%g [s]).'...
     'This value greater than half of the total flight time (%g [s]). Results will be invalid.'],...
     Ts,sar_info.flight_time/2);
  elseif Ts>max_Ts
    warning(['Current SAR parameters have resulted in a maximum slow time rate of (%g [s]).'...
     'This value less than what was specified (%g [s]). This will affect results.'],...
     max_Ts,Ts);
  end
  
  slow_t = sar_info.timeSampled(sar_info,Ts);
  
  fs = 1/Ts;
  f = fs/2*linspace(-1,1,length(slow_t));
  %2/lambda* CR*V/(DR) = f
  %2/lambda* RR = f
  min_DR = center_ranges(1);
  cross_ranges = lambda*min_DR*f/(2*avg_speed);
  
  delta_CR = abs(cross_ranges(1)-cross_ranges(2));
  if delta_CR>=sar_info.cell.dim.CR_width
    error(['Current SAR parameters have resulted in a cross range precision (%f [m]).' ...
     'This value greater than the cell dimension (%f [m]). Results will be invalid.'], ...
     delta_CR,sar_info.cell.dim.CR_width);
  elseif delta_CR>=sar_info.cell.prec.CR_width
    warning(['Current SAR parameters have resulted in a cross range precision (%f [m]).' ...
     'This value greater than what was specified (%f [m]). This may affect results.'],...
     delta_CR,sar_info.cell.prec.CR_width);
  end
  
  %%%%
  %Check postconditions
  assert(isscalar(Ts) && ~isempty(Ts),'Ts is expected to be a scalar');
  assert(isvector(slow_t),'slow_t output is expected to be a vector');
  assert(isvector(cross_ranges),'cross_ranges output is expected to be a vector');
  assert(isvector(edge_ranges  ) && length(edge_ranges  )==2,'edge_ranges output is expected to be a vector');
  assert(isvector(center_ranges) && length(center_ranges)==2,'center_ranges output is expected to be a vector');
end

function [ranges,cross_ranges,data,image] = generateSarImage(ant,tgts,sar_info,correct_cell_center)
  if nargin<4 || isempty(correct_cell_center)
    correct_cell_center = true;
  end
  %%%%%%
  [slow_Ts,slow_t,cross_ranges,edge_ranges,cell_ranges] = calculateSarCrossrange(ant,sar_info);
  if correct_cell_center
    range_extent = sqrt(sar_info.cell.dim.DR_width^2+sar_info.cell.dim.DR_width^2);
    ant_pos = ant.plat.position; cell_pos = sar_info.cell.plat.position;
    R_rel = @(t) getRange(ant_pos(t)-cell_pos(t));
  else
    range_extent = abs(diff(edge_ranges));
    cell_center = mean(cell_ranges);
    R_rel = @(t) cell_center+zeros(size(t));
  end
  [tau,vtx,ranges,Vo] = genStretchProcessor(ant,range_extent);
  delta_DR = abs(ranges(2)-ranges(1));
  if delta_DR>sar_info.cell.prec.DR_width
    warning(['Current SAR parameters have resulted in a down range precision (%f [m]).' ...
     'This value less than what was specified (%f [m]). This may affect results.'],...
     delta_DR,sar_info.cell.prec.DR_width);
  end
    
  tau_width = max(tau(:))-min(tau(:));
  approx_delay_correction_jitter = 2*getDelayFromRangeTwoWay(getRange(...
    ant.plat.position(sar_info.start_time)-sar_info.cell.plat.position(sar_info.start_time)));
  if(tau_width+approx_delay_correction_jitter>=slow_Ts)
    error(['Fast time window plus delay correction jitter is larger than slow time sampling rate. '...
      'The transmitter scheduler is overconstrained, and cannot function as expected.']);
  end
  %check that fast time samples can fit within a slow time window
  
  [vbb,~] = genTwoWayReturn(ant,tgts);
  %reference (center) position when doing SAR
  vbb = @(t0,t)vbb(vtx,t0,t,R_rel);
  
  data = zeros(length(slow_t),length(ranges));
  for slow_idx = 1:length(slow_t)
    slow_t0 = slow_t(slow_idx);
    data(slow_idx,:) = Vo(vbb,slow_t0,R_rel);
  end
  if isfield(sar_info,'CR_window') && ~isempty(sar_info.CR_window)
    CR_window = sar_info.CR_window(size(data,1));
    data = data.*CR_window(:);
  end
  image = fliplr(abs(fftshift(fft(data,[],1),1)).');
  
  %%%%
  %Check postconditions
  nDR = length(ranges);
  nCR = length(cross_ranges);
  assert(isvec(ranges      ),'ranges output is expected to be a vector');
  assert(isvec(cross_ranges),'cross_ranges output is expected to be a vector');
  assert(all(size(data )==[nCR,nDR]),'data is expected to be a matrix');
  assert(all(size(image)==[nDR,nCR]),'image is expected to be a matrix');
end
% /SAR Functions
%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
% Scene generation
function [tgts] = createTestTargetScene_hw12(sar_info,config_num,scale,colored)
  if nargin<4 || isempty(colored)
    colored = false;
  end
  if nargin<3 || isempty(scale)
    scale = 1;
  end
  if nargin<2 || isempty(config_num)
    config_num = 1;
  end
  cell_r0 = sar_info.cell.plat.position(sar_info.start_time+sar_info.flight_time/2);
  
  MY = sar_info.cell.dim.CR_width/2*23/25*scale;
  MX = sar_info.cell.dim.DR_width/2*23/25*scale;
  if config_num==1
    targets_x = [0];
    targets_y = [0];
  elseif config_num==2
    targets_x = [0,-MX,+MX];
    targets_y = zeros(size(targets_x));
  elseif config_num==3
    targets_x = [0,-MX,+MX];
    targets_y = [0,+MY,-MY];
  end
  
  target_r0 = [targets_x;targets_y;zeros(size(targets_x))];
  
  tgts = TargetProto();tgts=tgts(ones(size(targets_x)));
  tgt_colors = hsv(length(tgts));
  for i=1:length(tgts)
      tgt = TargetProto();
      target_ri = cell_r0+target_r0(:,i);
      tgt.plat.position = @(t)repmat(target_ri,1,length(t));
      if colored
        f_color =  genColoredBand(tgt_colors(i,1),tgt_colors(i,2),tgt_colors(i,3));
        tgt.sigma = @(f,a,e) 0*a.*e+10*f_color(f);
      end
      tgts(i)=tgt;
  end
end
  
function [tgts] = createTestTargetScene(sar_info,NUM_DRAW_TGT,scale,colored)
  if nargin<4 || isempty(colored)
    colored = false;
  end
  if nargin<3 || isempty(scale)
    scale = 1;
  end
  cell_r0 = sar_info.cell.plat.position(sar_info.start_time+sar_info.flight_time/2);
  MY = sar_info.cell.dim.CR_width/2*.9*scale;
  MX = sar_info.cell.dim.DR_width/2*.9*scale;
  targets_x = [linspace(-MX,MX,NUM_DRAW_TGT),linspace(MX,-MX,NUM_DRAW_TGT),linspace(-MX,MX,NUM_DRAW_TGT)];
  targets_y = [-MY+zeros(1,NUM_DRAW_TGT),linspace(-MY,+MY,NUM_DRAW_TGT),+MY+zeros(1,NUM_DRAW_TGT)];
  
  targets_x = [targets_x,MX*3/4+MY/2*.8*sin(linspace(0,pi,NUM_DRAW_TGT*2))];
  targets_y = [targets_y,     0+MY/2*.8*cos(linspace(0,pi,NUM_DRAW_TGT*2))];

  targets_x = [targets_x,-MX*3/4+MX/8*sin(linspace(0,2*pi,NUM_DRAW_TGT*2))];
  targets_y = [targets_y,-MY/2+MY/8*cos(linspace(0,2*pi,NUM_DRAW_TGT*2))];

  target_r0 = [targets_x;targets_y;zeros(size(targets_x))];
  
  tgts = TargetProto();tgts=tgts(ones(size(targets_x)));
  tgt_colors = hsv(length(tgts));
  for i=1:length(tgts)
      tgt = TargetProto();
      target_ri = cell_r0+target_r0(:,i);
      tgt.plat.position = @(t)repmat(target_ri,1,length(t));
      if colored
        f_color =  genColoredBand(tgt_colors(i,1),tgt_colors(i,2),tgt_colors(i,3));
        tgt.sigma = @(f,a,e) 0*a.*e+1*f_color(f);
      end
      tgts(i)=tgt;
  end
end
% /Scene generation
%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
% Actual Homework
function [Hs] = actualHomework12(ant,sar_info,SAR_SCENE_SCALE,BUDGE_IT,RADAR_HUD,prefix,save_figs)
  Hs = [];
  
  %ant.rx_window = [];
  sar_info.CR_window = [];
  RCMCs = [false,true];
  configs = [1,2,3];
  figure_title_num = [26,28,30];
  for c_idx = 1:length(configs)
    config = configs(c_idx);
    tgts = createTestTargetScene_hw12(sar_info,config,SAR_SCENE_SCALE,false);
    H=nextFigure();clf;Hs(end+1)=H;
    for r_idx=1:length(RCMCs)
      RCMC = RCMCs(r_idx);
      [range,cross_range,data,~] = generateSarImage(ant,tgts,sar_info,RCMC);
      subplot(length(RCMCs),1,r_idx);
      if BUDGE_IT
        range_m = 540*linspace(-1,1,length(range));
        cross_k = 240/2*linspace(-1,1,length(cross_range));
        imagesc(cross_k,range_m,abs(data.'));
      else
        imagesc(cross_range,range,abs(data.'));
      end
      ax=gca();ax.YDir='normal';
      if RADAR_HUD && ~BUDGE_IT
        colormap(brighten(radar_hud(),.2));
      else
        colormap(brighten(flipud(gray()),-.0));
      end
      if BUDGE_IT
        h=xlabel('\bf Cross-range sample, \itk\rm');h.FontSize=11;
        h=ylabel('\bf Range cell, \itm\rm');h.FontSize=11;
        axis([-120,120,-51,64]);
      else
        h=xlabel('\bfCrossrange\rm (rel. to scene center) [m]');h.FontSize=11;
        h=ylabel('\bfDownrange\rm (rel. to scene center) [m]');h.FontSize=11;
        axis([sar_info.cell.dim.CR_width/2*[-1,1],sar_info.cell.dim.DR_width/2*[-1,1]]);
      end
      if RCMC
        title_str ='With RCMC'; 
      else
        title_str = ['Without RCMC'];
      end
      if BUDGE_IT
        title_str = sprintf('Figure 15.%d (%s)',figure_title_num(c_idx)+r_idx-1,title_str);
      end
      h=title(title_str);
    end
    filename = sprintf('%s_config_%d',prefix,config);
    if save_figs
      saveas(H,filename,'png');
    end
  end
end
% /Actual Homework
%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%
% Unit Testing Functions
function [Hs] = stretchProcessorTest(antenna)
  ant = antenna;%make a copy
  ant.position = @(t)zeros(3,length(t));
  ant.rx_window = [];
  R0_ref = 30;%so targets aren't at 0 range and cause power to be inf
  tgt_ranges = R0_ref+[0,3];
  tgt_speeds = [0:4e3:20e3];
  
  tgts = TargetProto();tgts = tgts(ones(length(tgt_speeds),length(tgt_ranges)));
  for speed_idx = 1:length(tgt_speeds)
    for range_idx = 1:length(tgt_ranges)
      pos = [tgt_ranges(range_idx);0;0];
      vel = [tgt_speeds(speed_idx);0;0];
      tgt = TargetProto();
      tgt.plat.position = @(t)pos+vel*t;
      tgts(speed_idx,range_idx)=tgt;
    end
  end
  
  range_extent = 100;
  t0 = 0;
  Hs = [];
  tests = [struct('tracker',0,'compensate',0,'expected','Staggered & Spread','window',[-10,50]),...
           struct('tracker',1,'compensate',0,'expected','Spread'            ,'window',[-6,6]),...
           struct('tracker',1,'compensate',1,'expected','Perfect Alignment' ,'window',[-6,6])];
  for test_idx = 1:length(tests)
    %compensate uses the speed to adjust hs
    [~,vtx,range,Vo] = genStretchProcessor(ant,range_extent,tests(test_idx).compensate,8);
    
    lgc = (tests(test_idx).window(1)<=range & range<=tests(test_idx).window(2));
    %lgc = logical(ones(size(lgc)));%HACK
    
    H=nextFigure();hold('on');
    for speed_idx = 1:size(tgts,1)

      %Either track the target's speed or not
      if ~tests(test_idx).tracker
        R_ref = @(t) getRange(tgts(speed_idx,1).plat.position(t0))+zeros(1,length(t));
      else
        R_ref = @(t)getRange(ant.plat.position(t)-tgts(speed_idx,1).plat.position(t));
      end
      [vrx,~] = genTwoWayReturn(ant,tgts(speed_idx,:));
      vrx0 = @(t0,t)vrx(vtx,t0,t,R_ref);
      
      data = Vo(vrx0,t0,R_ref);%Run the stretch processor
      
      if speed_idx==1
        X0 = max(abs(data(:)));
      end
      plot(range(lgc),abs(data(lgc))/X0);
    end
    axis('tight');
    grid('on');
    h=xlabel('\bfRange [m]');h.FontSize=11;
    h=ylabel('\bfPower [Normalized]');h.FontSize=11;
    h=title(['\bf' sprintf('Stretch Processor Unittest Expected: %s',tests(test_idx).expected)]);
    h.FontSize=12;
    Hs(end+1)=H; %#ok<AGROW>
  end
end

function [H,title_strs] = oldCrOnlyTest(ant,sar_info,BUDGE_IT)
  Hs = [];
  title_strs = {};
  
  tgts = createTestTargetScene_hw12(sar_info,2);
  [~,slow_t,sar_CR,~,~] = calculateSarCrossrange(ant,sar_info);
  vbb = @(t)1+zeros(size(t));
  %vbb = @(t)exp(1j*2*pi*1e4*t.^2);
  carrier = @(t) exp(1j*2*pi*ant.fc*t);
  vtx = @(t)carrier(t).*vbb(t);

  %both heterodyne's and doppler corrects
  mixer = @(vrx,t,R) vrx(t).*conj(vtx(t-2*R(t)/SPEED_OF_LIGHT));

  R_cell = @(t) getRange(ant.plat.position(t)-sar_info.cell.plat.position(t));
  [vbb,R_rel] = genTwoWayReturn(vtx,ant,tgts);
  
  H=nextFigure();clf;Hs(end+1) = H;
  subplot(2,1,1);
  plot(slow_t,R_cell(slow_t)/1e3,slow_t,R_rel(slow_t)/1e3);
  h=xlabel('\bftime (s)');h.FontSize=11;
  h=ylabel('\bfRange (km)');h.FontSize=11;
  legend([{'Range to Cell'},...
      arrayfun(@(i)sprintf('Clutter Target #%d',i),1:length(tgts),...
      'Un',0)]);
  grid('on');
  title_strs{1} = 'Range from Radar to Targets';
  h=title(['\bf' title_strs{1}]);h.FontSize=12;
  
  vm = mixer(vbb,slow_t,R_cell);

  Vm = FFT(slow_t,vm);
  subplot(2,1,2);
  plot(sar_CR,abs(Vm).^2);
  grid('on');
  h=xlabel('\bfCrossrange - y (m)');h.FontSize=11;
  h=ylabel('\bf|V_o(y)|^2 (W)');h.FontSize=11;
  h=title('\bfOutput of SAR Processor');h.FontSize=12;
  
  H=nextFigure();clf;Hs(end+1) = H;
  if BUDGE_IT
      colormap(brighten(flipud(gray()),-.7))
  else
      colormap(radar_hud());
  end

  NX = 25;
  x = -NX:NX;
  Vimage = [repmat(zeros(size(Vm)),NX,1); ...
            abs(Vm); ...
            repmat(zeros(size(Vm)),NX,1)].';
  ax=gca();
  if BUDGE_IT
      ylgc = -25<=sar_CR & sar_CR<=25;
      hold('on');
      imagesc(sar_CR(ylgc),x,Vimage(ylgc,:).');
      h=ylabel('\bfDownrange - x (from cell center) [m]');h.FontSize=11;
      h=xlabel('\bfCrossrange - y (from cell center) [m]');h.FontSize=11;
      suffix = '_budge_tranpose';
      if SHOW_TRUE_TARGETS
          plot(targets_y,targets_x,'rx','MarkerSiZE',20);
          legend({'True Target Positions'});
          suffix = [suffix '_tgts'];
      end
      ax.GridColor=[0,0,0];
  else
      imagesc(x,sar_CR,Vimage);
      h=xlabel('\bfDownrange - x (from cell center) [m]');h.FontSize=11;
      h=ylabel('\bfCrossrange - y (from cell center) [m]');h.FontSize=11;
      ax.GridColor=[1,1,1];
      ax.YDir='normal';
      suffix = '';
  end
  axis('tight');
  grid('on');
  h=title('\bfSAR Image of Cell');h.FontSize=12;
  title_strs{2} = ['sar',suffix];
end

function [H,suffix] = oldFastTimeTest(ant,tgts,sar_info,ZOOM_TARGETS,ADD_NOISE,SHOW_TRUE_TARGETS,RADAR_HUD)
  suffix = '';
  sar = sar_info;%COPY
  sar.Ts = 5e-3;
  range_extent = hypot(sar.cell.dim.DR_width,sar.cell.dim.CR_width); 
  [tau,vbb,range,Vo] = genStretchProcessor(ant,range_extent,[],3);
  [slow_Ts,slow_t,cross_ranges,~,~] = calculateSarCrossrange(ant,sar_info);
  tau_width = max(tau(:))-min(tau(:));
  assert(tau_width<slow_Ts);
  %check that fast time samples can fit within a slow time window

  vrx = genTwoWayReturn(ant,tgts);
  R_cell = @(t) getRange(ant.plat.position(t)-sar.cell.plat.position(t));
  %reference (center) position when doing SAR
  vrx = @(t0,t)vrx(vbb,t0,t,R_cell);
  
  targets_x = zeros(size(tgts));
  targets_y = zeros(size(tgts));
  cell_r0 = sar.cell.plat.position(slow_t(1));
  for ti=1:length(tgts)
    pos = tgts(ti).plat.position(slow_t(1))-cell_r0;
    targets_y(ti) = pos(2);
    targets_x(ti) = pos(1);
  end

  H=nextFigure();clf;hold('on');
  data = zeros(length(slow_t),length(range));

  for slow_idx = 1:length(slow_t)
    slow_t0 = slow_t(slow_idx);
    data(slow_idx,:) = Vo(vrx,slow_t0,R_cell);
  end
  if ADD_NOISE
    V0 = abs(fft(data,[],1));
    V0 = max(abs(V0(:)));
    data = data+V0/1e3/sqrt(2)*(randn(size(data))+1j*randn(size(data)));
  end
  Vimage = fliplr(abs(fftshift(fft(data,[],1),1)).');
  imagesc(cross_ranges,range,Vimage);
  axis('tight');
  axis('square');
  axis([sar.cell.dim.CR_width/2*[-1,1],sar.cell.dim.DR_width/2*[-1,1]]);
  colormap(brighten(flipud(gray()),-.9));
  h=xlabel('\bfcross range [m]');h.FontSize=11;
  h=ylabel('\bfrange referenced from cell range (m)');h.FontSize=11;
  if SHOW_TRUE_TARGETS
      plot(targets_y,targets_x,'ro','MarkerSiZE',20);
      legend({'True Target Positions'});
      suffix = [suffix '_tgts'];
  end
  ax=gca();
  ax.GridColor=[0,0,0];
  if ZOOM_TARGETS
    extra = 1.10;
    xx = minmax(targets_x(:));
    yy = minmax(targets_y(:));
    xx = extra*(xx-mean(xx))+mean(xx);
    yy = extra*(yy-mean(yy))+mean(yy);
    axis([yy xx]);
    axis('square');
  end
  if RADAR_HUD
    colormap(brighten(radar_hud(),.5));
    ax.GridColor=[1,1,1];
  end
end

function [Hs] = sarColorTest(ant,sar_info,NUM_DRAW_TGT,SAR_SCENE_SCALE,SAR_COLORTEST_RGB,ZOOM_TARGETS)
  Hs = [];
  tgts = createTestTargetScene(sar_info,NUM_DRAW_TGT,SAR_SCENE_SCALE,SAR_COLORTEST_RGB);
  color_ant = ant;%make a copy
  
  targets_x = zeros(size(tgts));
  targets_y = zeros(size(tgts));
  cell_r0 = sar_info.cell.plat.position(sar_info.start_time);
  for ti=1:length(tgts)
    pos = tgts(ti).plat.position(sar_info.start_time)-cell_r0;
    targets_y(ti) = pos(2);
    targets_x(ti) = pos(1);
  end
  if ZOOM_TARGETS
    extra = 1.10;
    xx = minmax(targets_x(:));
    yy = minmax(targets_y(:));
    xx = extra*(xx-mean(xx))+mean(xx);
    yy = extra*(yy-mean(yy))+mean(yy);
  end
  
  f0 = GREEN_FREQ();
  f0s = [RED_FREQ(),GREEN_FREQ(),BLUE_FREQ()];
  ranges = cell(1,length(f0s));
  cross_ranges = cell(1,length(f0s));
  rgb_imgs = cell(1,length(f0s));
  for i=1:length(f0s)
    color_ant.fc = f0s(i);
    [range,cross_range,~,img] = generateSarImage(color_ant,tgts,sar_info);
    ranges{i} = range;
    cross_ranges{i} = cross_range;
    rgb_imgs{i} = img;
  end
  [~,idx] = min(abs(f0s-f0));
  cross_range = cross_ranges{idx};
  range = ranges{idx};
  
  [grid_x,grid_y] = meshgrid(cross_range,range);
  rgb_img = zeros([size(rgb_imgs{idx}),length(f0s)]);
  for i=1:length(f0s)
    [f_x,f_y] = meshgrid(cross_ranges{i},ranges{i});
    rgb_img(:,:,i) = interp2(f_x,f_y,rgb_imgs{i},grid_x,grid_y,'bilinear',0);
  end
  X0 = max(rgb_img(:));
  rgb_img = rgb_img/X0;
  try
    rgb_img_adjust = imadjust(rgb_img,[0,.3],[0,.9],.8);
  catch
    rgb_img_adjust = rgb_img;
  end
  H = nextFigure();Hs(end+1)=H;
  imagesc(cross_range,range,rgb_img_adjust);
  ax=gca();
  ax.YDir='normal';
  axis('tight');
  if ZOOM_TARGETS
    axis([yy xx]);
    axis('square');
  end
  h=xlabel('\bfCrossrange [m]');h.FontSize=11;
  h=ylabel('\bfDownRange  [m]');h.FontSize=11;
  h=title('\bf3 Channel SAR Image (pseudo RGB)');h.FontSize=12;
  
  if SAR_COLORTEST_RGB
    H = nextFigure();Hs(end+1)=H;
    chans = {'Red','Green','Blue'};
    for i=1:length(chans)
      subplot(1,3,i);
      img = rgb_img_adjust;
      img(:,:,[1:3]~=i)=0;
      imagesc(cross_range,range,img);
      ax=gca();
      ax.YDir='normal';
      axis('tight');
      if ZOOM_TARGETS
        axis([yy xx]);
        axis('square');
      end
      h=xlabel('\bfCrossrange [m]');h.FontSize=11;
      h=ylabel('\bfDownRange  [m]');h.FontSize=11;
      h=title(['\bf' chans{i} ' Channel SAR Image']);h.FontSize=11;
    end
    h=sgtitle('\bf3 Channel SAR Image Split (pseudo RGB)');h.FontSize=12;
  end
end
% /Unit Testing Functions
%%%%%%%%%%%%%%%%%%

function [w] = hamming(N)
    n = 0:(N-1);
    w = 0.54-0.46*cos(2*pi*n/(N-1));
end

