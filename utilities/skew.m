function [out] = skew(x)
  %calculates the skew matrix of a vector - useful for representing cross
  %products as matrices
  out = [ 0   ,-x(3), x(2);...
          x(3),    0,-x(1);...
         -x(2), x(1),  0];
end