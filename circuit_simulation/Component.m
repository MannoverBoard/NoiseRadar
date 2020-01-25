classdef Component < handle & UniqueId
  properties (Hidden,SetAccess=private)
    h_env; %handle to simulation environment
    name_;
  end
  properties (Access=protected)
    symbols;
    values;
  end
  properties (Abstract,Constant)
    type_name;
    type_short_name;
  end
  properties (Dependent)
    name;
  end
  
  methods
    function self = Component(varargin)
      if nargin<2
        name = '';
      else
        name = varargin{2};
      end
      if nargin<1
        h_env = [];
      else
        h_env = varargin{1};
      end
      self.h_env = h_env;
      self.name_ = name;
      self.setEnvironment(h_env);
    end
    
    function [name] = get.name(self)
      name = self.name_;
    end
    
    function set.name(self,name)
      if isempty(self.h_env)    
        self.name_ = name;
      else
        [out_name,~] = self.h_env.attemptAssignComponentName(self.uid,name);
        self.name_ = out_name;
      end
    end

    function setEnvironment(self,h_env)
      self.h_env = h_env;
      assert(~isempty(h_env),'New environment must be non-empty');
      self.uid = h_env.registerComponent(self);
    end
    
    function [out] = substitute(self,expr)
      assert(numel(self.symbols)==numel(self.values),'Total number of symbols and values must be the same');
      out = expr;
      for i=numel(self.symbols)
        out = subs(out,self.symbols(i),self.values(i));
      end
    end
  end
  
  methods (Abstract,Access=?CircuitEnvironment)
    function [out] = addToCircuitAC(self,admittance_matrix) %#ok<INUSD>
    end
  end
end

