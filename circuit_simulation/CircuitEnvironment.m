classdef CircuitEnvironment < handle
  
  properties (Constant)
    uid_type = AtomicCounter.value_type;
    type_keytype = 'char';
  end
  properties (GetAccess=public,SetAccess=private)
    component_uid_manager;
    type_uid_manager;
    type_registry;
    component_registry;
    node_registry;
  end
  methods
    function self = CircuitEnvironment(varargin)
      expected_args = {};
      optional_args = {};
      for i=1:length(expected_args)
        arg = expected_args{i};
        matches = strcmpi(varargin,arg);
        if ~any(matches)
          error('Expected argument %s',arg);
        elseif sum(matches)>1
          error('Got multiple instances of argument %s',arg);
        else
          idx = find(matches,1,'first');
          if idx>=length(varargin)
            error('Given argument %s, but not supplied value.',arg);
          end
          value = varargin{idx+1};
          self.(arg) = value;
        end
      end
      for i=1:length(optional_args)
        arg = optional_args{i};
        matches = strcmpi(varargin,arg);
        num_matches = sum(matches);
        if num_matches>1
          error('Got multiple instances of argument %s',arg);
        elseif num_matches==1
          idx = find(matches,1,'first');
          if idx>=length(varargin)
            error('Given argument %s, but not supplied value.',arg);
          end
          value = varargin{idx+1};
          self.(arg) = value;
        end
      end
      self.component_uid_manager = AtomicCounter(1);%start indices at 1
      self.type_uid_manager      = AtomicCounter(1);%start indices at 1
      self.component_registry = containers.Map('KeyType',self.uid_type    ,'ValueType','any');
      self.node_registry      = containers.Map('KeyType',self.uid_type    ,'ValueType','any');
      self.type_registry      = containers.Map('KeyType',self.type_keytype,'ValueType','any');
    end
    
    function [component_uid] = registerComponent(self,component)
      component_uid = self.component_uid_manager.next();
      self.component_registry(component_uid) = component;
      type_name = class(component);
      if ~self.type_registry(type_name)
        type_uid = self.type_uid_manager.next();
        entry = struct('UID',type_uid,'short_name',component.type_short_name,'components',Set());
      else
        entry = self.type_registry(type_name);
      end
      self.type_registry(type_name) = entry;
    end
    
  end
  
end