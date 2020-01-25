classdef UniqueNamedObjectManager < UniqueObjectManager
  properties(Constant,Hidden)
    name_type = 'char';
    delimiter = ':';
    relacement_char = '_';
  end
  properties(GetAccess=public,SetAccess=private)
    name_registry;
  end
  methods
    function self = UniqueNamedObjectManager(varargin)
      self@UniqueObjectManager(varargin{:});  
      self.name_registry = containers.Map('KeyType',self.name_type,'ValueType',self.uid_type);
    end
    function [object_uid,final_name] = register(self,object,name)
      %assumes object is UniqueId child
      [object_uid] = register@UniqueObjectManager(self,object);
      if nargin<3 || isempty(name)
        name = '';
      end
      object.raw_name = '';
      object.name_ = UniqueNamedObjectManager.defaultObjectName(object_uid);
      self.name_registry(object.name_) = object_uid;
      if ~isempty(name)
        final_name = self.attemptRenameObject(object,name);
      end
    end
    function unregister(self,object)
      object_uid = object.uid;
      if isempty(object_uid)
        %don't re-unregister things that have already been unregistered
        return;
      end
      unregister@UniqueObjectManager(self,object);
      if self.name_registry.isKey(object.name)
        self.name_registry.remove(object.name);
      end
      object.name_ = object.raw_name;
    end
    function [final_name] = attemptRenameObject(self,object,name)
      if ~self.registry.isKey(object.uid)
        [object_uid,~] = self.register(object);
      else
        object_uid = object.uid;
      end
      if strcmp(object.name,name)
        %ignore self sets
        final_name=name;
        return;
      end
      old_name = object.name;
      default_name = UniqueNamedObjectManager.defaultObjectName(object_uid);
      if nargin<3 || isempty(name)
        %Rename to empty -> rename to default unique name
        object.raw_name = '';
        object.name_ = default_name;
      else
        object.raw_name = name;
        if self.name_registry.isKey(name)
          %already a name, append default
          object.name_ = strcat(object.raw_name,default_name);
        else
          object.name_ = object.raw_name;
        end
      end
      self.name_registry(object.name_) = object_uid;
      self.name_registry.remove(old_name);
      final_name = object.name_;
    end
  end
  
  methods(Static,Hidden)
    function [name] = defaultObjectName(object_uid)
      name = sprintf('%s%d',UniqueNamedObjectManager.delimiter,object_uid);
    end
    function [name] = fixName(name)
      name = regexprep(name,UniqueNamedObjectManager.delimiter,UniqueNamedObjectManager.relacement_char);
    end
  end
end