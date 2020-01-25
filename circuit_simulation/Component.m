classdef Component < handle & UniqueName
  properties (Hidden,SetAccess=private)
    h_env = []; %handle to simulation environment
  end
  
  properties (Access=protected)
    symbols = [];
    values = [];
    connections = [];
  end
  
  properties (Abstract,Constant)
    type_name;
    Connections;%prototype class to get named connections
  end
  properties(Constant)
    type_short_name = 'Component';
  end
  
  methods
    function self = Component(h_env,name,varargin)
      if nargin<2
        name = '';
      end
      if nargin<1 || isempty(h_env)
        h_env = [];
      end
      self@UniqueName([],name);
      self.setEnvironment(h_env);
      self.connectors = self.Connectors();
    end
    
    function [self] = setEnvironment(self,h_env)
      if self.h_env==h_env
        %ignore self sets
        return;
      end
      if ~isempty(self.h_env)
        self.h_env.unregister(self);
      end
      self.h_env = h_env;
      if ~isempty(self.h_env)
        self.h_env.register(self);
        self.updateSymbols();
      end
    end
    
    function [out] = substitute(self,expr)
      out = subs(expr,self.symbols,self.values);
    end
    
    function [out,varargout] = isFullyConnected(self)
      % Are all connectors connected to valid nodes?
      out = true;
      if isempty(self.connections)
        out = true;
        return;
      end
      if isempty(self.h_env)
        out = false;
        return;
      end
      fns = fieldname(self.connections);
      unconnected = {};
      for fn_idx = numel(fns)
        fn = fns{fn_idx};
        node = self.connections.(fn);
        if isempty(node) || ~self.h_env.node_manager.registry.isKey(node)
          out = false;
          unconnected{end+1} = fn; %#ok<AGROW>
        end
      end
      varargout = {};
      if nargout>=2
        varargout{end+1} = unconnected;
      end
    end
  end
  
  methods (Abstract,Access=?CircuitEnvironment)
    %Adds self to AC circuit addmittance matrix
    [admittance_matrix,rhs_currents] = addToCircuitAC(self,node2index,admittance_matrix,rhs_currents)  %#ok<INUSL>
  end
  methods(Abstract,Access=?Component)
    %Called after environment is set
    [self] = updateSymbols(self)
  end
end

