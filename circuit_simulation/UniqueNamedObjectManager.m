classdef UniqueNamedObjectManager < UniqueObjectManager
  properties(Constant,Hidden)
    name_type = 'char';
    delimiter = ':';
  end
  properties(GetAccess=public,SetAccess=private)
    name_registry;
  end
  methods
    function self = UniqueNamedObjectManager(varargin)
      self@UniqueObjectManager(varargin{:});  
      self.name_registry = containers.Map('KeyType',self.name_type,'ValueType',self.uid_type);
    end
    function [object_uid,final_name] = register(self,object,names_to_try)
      %assumes object is UniqueId child
      [object_uid] = register@UniqueObjectManager(self,object);
      if nargin<3 || isempty(names_to_try)
        names_to_try = {};
      end
      if ~iscell(names_to_try)
        names_to_try = {names_to_try};
      end
      suffix = UniqueNamedObjectManager.defaultObjectName(object_uid);
      object.name_ = suffix;
      self.name_registry(suffix) = object_uid;
      final_name = suffix;
      if ~isempty(names_to_try)
        names_to_try = [names_to_try,cellfun(@(s)strcat(s,suffix),names_to_try,'Un',0)];
        final_name = self.attemptRenameObject(object,names_to_try);
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
      tmp = regexp(object.name,self.delimiter,'split');
      object.name_ = strjoin(tmp(1:end-1),self.delimiter);
    end
    function [final_name] = attemptRenameObject(self,object,names_to_try)
      if ~self.registry.isKey(object.uid)
        [object_uid,~] = self.register(object);
      else
        object_uid = object.uid;
      end
      if nargin<3 || isempty(names_to_try)
        %Rename to empty -> rename to default unique name
        default_name = UniqueNamedObjectManager.defaultObjectName(object_uid);
        object.name_ = default_name;
        self.name_registry(default_name) = object_uid;
        return;
      end
      if ~iscell(names_to_try)
        names_to_try = {names_to_try};
      end
      old_name = object.name;
      final_name = [];
      for name_c = names_to_try
        name = name_c{1};
        if self.name_registry.isKey(name)
          continue;
        end
        final_name = name;
        self.name_registry(final_name) = object_uid;
        object.name_ = final_name;
        break;
      end
      if isempty(final_name)
        warning('Unable to rename unique name to Object #%d',object_uid);
        final_name = object.name;
      else
        self.name_registry.remove(old_name);
      end
    end
  end
  
  methods(Static,Hidden)
    function [name] = defaultObjectName(object_uid)
      name = sprintf('%s%d',UniqueNamedObjectManager.delimiter,object_uid);
    end
  end
end