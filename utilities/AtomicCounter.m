classdef AtomicCounter < handle
  %Monotonic int64 counter, until it reaches int64 max then it wraps back
  %to 0
  properties (Constant,Hidden)
    value_type = 'int64';
  end
  properties(Access=private)
    value_ int64 = 0;
  end
  
  methods
    function self = AtomicCounter(value)
      if nargin<1
        self.value_ = 0;
      else
        self.value_ = floor(value);
      end
    end

    function [value] = next(self)
      value = self.value_;
      next_value = self.value_+1;
      if self.value_==next_value
        %Integer wrap
        next_value = 0;
      end
      self.value_ = next_value;
    end

    function reset(self,value)
      if nargin<2
        value = 0;
      end
      self.value_ = value;
    end
  end
end
