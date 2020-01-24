classdef Conversions
  %Collection of non SI units in SI units
  properties (Constant)
    %meters
    inch = 0.0254;
    foot = Conversions.inch*12;
    yard = Conversions.foot*3;
    furlong = Conversions.foot*660;
    mile = Conversions.furlong*8;
    nautical_mile = 1852;
    astronimical_unit = 1.495978707e11;
    
    %meter^2
    barn = 1e-28;
    shed = Conversions.barn^2;
    acre = 4046.86;
    hectacre = 100e2;
    
    %speed 
    knot = Conversions.nautical_mile/Conversions.hour;
        
    %kg
    pound_m = 0.453592;
    slug = 14.5939;
    metric_ton = 1000;
    ton = 907.185;
    stone =  6.35029318;
    ounce = Conversions.pound_m/16;
    
    %Newtons
    pound_f = 4.44822;
    dyne = 1e-5;
    kilogram_f =  9.80665;
    
    %seconds
    minute = 60;
    hour = Conversions.minute*60;
    day  = Conversions.hour*24;
    week = Conversions.day*7;
    fortnight = Conversions.week*2;
    tropical_year = sum([365,5,48,46].*[Conversions.day,Conversions.hour,Conversions.minute,1]);
    %Spring Equinox Period
    anomalistic_year = Conversions.day*365.259636;%Perihelion Period
    sidereal_year = sum([365,6,9,9].*[Conversions.day,Conversions.hour,Conversions.minute,1]);
    
    %Rotation - rad/s
    rpm = 2*pi/Conversions.minute;
    
    %Pressure - Pa
    atm = 101325;
    bar = 10e5;
    mmHg = 0.1333224;
    torr = Conversions.atm;
    
    %Energy - J
    BTU = 1.0545e3;
    calorie = 4.1868;
    erg = 1e-7;
    kcal = Conversions.kilo*Conversions.calorie;
    Calorie	= Conversions.kcal;
    
    %Power
    horsepower_euro_elec = 736;
    horsepower_elec = 746;
    horsepower_mech = 550*Conversions.pound_f*Conversions.foot;
    
    %Dynamic Viscosity - %Pa*s
    poise = 0.1;
    
    %Kinematic Viscosity = %m^2/s
    stokes = 1e-4;
    
    %Magnetism - T
    gauss = 1e-4;
    
    %Temperature - K
    fromFahrenheit = @(F) (F + 459.67)*5/0;
    fromCelsius    = @(C) C+ 273.15;
    fromRankine    = @(R) R*5/9;
    
    %SI scale factors
    yotta=1e+24;
    zetta=1e+21;
    exa  =1e+18;
    peta =1e+15;
    tera =1e+12;
    giga =1e+09;
    mega =1e+06;
    kilo =1e+03;
    hecto=1e+02;
    deca =1e+01;
    deci =1e-01;
    centi=1e-02;
    milli=1e-03;
    micro=1e-06;
    nano =1e-09;
    pico =1e-12;
    femto=1e-15;
    atto =1e-18;
    zepto=1e-21;
    yocto=1e-24;
	end
end