classdef Inductor < Component
  properties (Constant)
    type_name = 'Inductor';
    type_short_name = 'L';
    Connectors = @()struct('pos',[],'neg',[]);
    default_value = 1e-6;
  end
  properties(Dependent)
    value;
  end
  methods
    function self = Inductor(varargin)
      self@Component(varargin{:});
      if nargin>=3
        self.values = varargin{3};
      else
        self.values = [self.default_value];
      end
    end

    function [value] = get.value(self)
      value = self.values(1);
    end
    function set.value(self,value)
      self.values(1) = value;
    end
  end
  
  methods (Abstract,Access=?CircuitEnvironment)
    function [admittance_matrix] = addToCircuitAC(self,indexFromNode,admittance_matrix)
      is_fully_connected = self.isFullyConnected();
      if ~is_fully_connected
        warning('Component:Unconnected','Component %s is not fully connected. Cannot add to circuit',self.name);
        return;
      elseif isempty(self.connectors)
        %no connections
        return;
      end
      connector_index = self.connectors;%copy
      fns = fieldname(self.connectors);
      for fn_idx = numel(fns)
        fn = fns{fn_idx};
        connector_index.(fn) = indexFromNode(self.connectors.(fn));
      end
      s = self.h_env.LaplaceVariable;
      b = (s*self.symbol(1));
      pos = connector_index.pos;
      neg = connector_index.neg;
      admittance_matrix(pos,pos) =  b;
      admittance_matrix(neg,neg) =  b;
      admittance_matrix(pos,neg) = -b;
      admittance_matrix(neg,pos) = -b;
    end
  end
  methods(Abstract,Access=?Component)
    function [self] = updateSymbols(self)
      self.symbols = sym(sprinf('%s%d',self.type_short_name,self.uid));
    end
  end
end