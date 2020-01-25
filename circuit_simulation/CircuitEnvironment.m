classdef CircuitEnvironment < handle
  
  properties (Constant)
    uid_type = AtomicCounter.value_type;
    name_keytype = 'char';
    Laplace = syms('s');
    DEFAULT_SHORT_NAME = 'Component';
  end
  properties (GetAccess=public,SetAccess=private)
    component_uid_manager;
    component_registry;
    component_name_registry;
    
    type_uid_manager;
    type_registry;
    
    node_uid_manager;
    node_registry;
    node_name_registry;
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
      
      self.type_registry      = containers.Map('KeyType',self.name_keytype,'ValueType','any');
      
      self.component_name_registry = containers.Map('KeyType',name_keytype,'ValueType','any');
      self.node_name_registry      = containers.Map('KeyType',name_keytype,'ValueType','any');
    end
    
    function [component_uid] = registerComponent(self,component)
      component_uid = self.component_uid_manager.next();
      self.component_registry(component_uid) = component;
      
      %Check if this type has been encountered yet if not add it to the registry
      entry = getTypeEntryComponentUID(component_uid);
      entry.components.add(component_uid);
      
      %Add name to name registry, creating one if necessary
      self.attemptAssignComponentName(component_uid,component.name);
    end
    
  end
  
  methods(Access=private)
    function [entry] = getTypeEntryComponentUID(self,component_uid)
      entry = [];
      if ~self.component_registry.isKey(component_uid)
        return;
      end
      component = self.component_registry.isKey(component_uid);
      if isempty(component)
        return;
      end
      entry = self.getTypeEntryByName(component.type_name,component.name);
    end
    
    function [entry] = getTypeEntryByName(self,type_name,type_short_name)
      if nargin<3 || isempty(type_short_name)
        type_short_name = self.DEFAULT_SHORT_NAME;
      end
      if ~self.type_registry(type_name)
        type_uid = self.type_uid_manager.next();
        entry = ComponentTypeEntry();
        entry.short_name = type_short_name;
        entry.uid = type_uid;
        self.type_registry(type_name) = entry;
      else
        entry = self.type_registry(type_name);
      end
    end
  end
  
  methods (Access=?Component)
    function [out_name,success] = attemptAssignComponentName(self,component_uid,name)
      %Get name, if empty then make one, and add it to global unique name
      %registry
      if nargin<3 || isempty(name)
        name = '';
      end
      success = true;
      entry = self.getTypeEntryComponentUID(component_uid);
      if isempty(entry)
        warning('Attempting to assign component name in environment for unregistered component');
        out_name = '';
        success=false;
        return;
      end
      if isempty(name)
        component_num = entry.counter.next;
        name = sprintf('%s%d',entry.short_name,component_num);
      end
      if self.component_name_registry.isKey(name)
        existing_uid = self.component_name_registry(name);
        if existing_uid==component_uid
          %name is same and UID is same, do nothing
          out_name = name;
          return;
        end
        name = sprintf('%d:%s',component_uid,name);
        success = false;
      else
        self.component_name_registry(name) = component_uid;
      end
      component = self.component_registry(component_uid);
      out_name = name;
      component.name = out_name;
    end
  end
  
  methods (Static,Access=private)
    function [out] = isValidName(name)
      out = isempty(regexp(name,'[^-a-zA-Z0-9_]','once'));
    end
    
    function [out] = getObjectRegistry()
      
  end
end






