classdef IdManager < handle
  %Monotonic int64 counter, with removal
  properties (Constant,Hidden)
    value_type = 'int64';
  end
  properties(Access=private)
    value_ int64 = 0;
    unregistered_ids = Set();
  end
  
  methods
    function self = IdManager(value)
      if nargin<1
        self.value_ = 0;
      else
        self.value_ = floor(value);
      end
    end
    
    function [value] = next(self)
      if ~isempty(self.unregistered_ids)
        value = min(self.unregistered_ids.values);
        self.unregistered_ids.remove(value);
        return;
      end
      value = self.value_;
      next_value = self.value_+1;
      if self.value_==next_value
        %Integer wrap
        next_value = 0;
      end
      self.value_ = next_value;
    end
    
    function [self] = unregister(self,value)
      if value>=self.value_ || self.unregistered_ids.isMember(value)
        %value hasn't been given out, so do nthing
        return;
      end
      self.unregistered_ids.add(value);
    end
    
    function reset(self,value)
      if nargin<2
        value = 0;
      end
      self.unregistered_ids.clear();
      self.value_ = value;
    end
  end
end
