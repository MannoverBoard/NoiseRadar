classdef CircuitEnvironment < handle
  properties (Constant)
    LaplaceVariable = sym('s');
  end
  properties (GetAccess=public,SetAccess=private)
    component_manager;
    node_manager;
    type_manager;
  end
  properties(Constant,Hidden)
    ComponentName     = class(Component);
    NodeName          = class(CircuitNode);
    ComponentNameType = class(ComponentTypeEntry);
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
      self.component_manager = UniqueNamedObjectManager();
      self.node_manager      = UniqueNamedObjectManager();
      self.type_manager      = UniqueNamedObjectManager();
      
      self.getTypeEntryByName(self.NodeName,NodeName.type_short_name);%register node type
    end
    
    function [uid] = register(self,object)
      uid = [];
      if isa(object,self.ComponentName)
        uid = self.registerComponent(self,object);
      elseif isa(object,self.ComponentNameType)
        uid = self.registerType(object);
      else
        warning('Attempting to register unknown type %s',class(object));
      end
    end
  end
  
  methods(Access=private)
    function [component_uid] = registerComponent(self,component)
      component.setObjectManager(self.component_manager);
      component_uid = component.uid;
      
      %Check if this type has been encountered yet if not add it to the registry
      entry = getTypeEntryComponentUID(component_uid);
      entry.register(component_uid);
      if isempty(component.raw_name)
        component.name = entry.getDefaultName(component_uid);
      end
    end
    
    function [node_uid] = registerNode(self,node)
      node.setObjectManager(self.node_manager);
      node_uid = node.uid;
      
      entry = getTypeEntryByName(class(node));
      entry.register(component_uid);
      if isempty(node.raw_name)
        node.name = entry.getDefaultName(node_uid);
      end
    end
    
    function [entry] = getTypeEntryComponentUID(self,component_uid)
      entry = [];
      if ~self.component_manager.registry.isKey(component_uid)
        return;
      end
      component = self.component_manager.registry(component_uid);
      if isempty(component)
        return;
      end
      entry = self.getTypeEntryByComponent(component.type_name,component.type_short_name);
    end
    
    function [entry] = getTypeEntryByName(self,type_name,type_short_name)
      if nargin<3 || isempty(type_short_name)
        type_short_name = Component.type_short_name;
      end
      if ~self.type_manager.name_registry(type_name)
        entry = ComponentTypeEntry(self.type_manager,type_name);
        entry.short_name = type_short_name;
      else
        uid = self.type_manager.name_registry(type_name);
        entry = self.type_manager.registry(uid);
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
    function [out] = sanitizeName(name)
      out = regexprep(name,'[^-a-zA-Z0-9_]');
    end
  end
end






