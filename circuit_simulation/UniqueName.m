classdef UniqueName < UniqueId
  properties(Access={?UniqueName,?UniqueNamedObjectManager},Hidden)
    name_ = '';
  end
  properties(Dependent)
    name;
  end
  properties(GetAccess=public,SetAccess=?UniqueObjectManager,Hidden)
    h_unique_named_object_manager = [];
  end
  methods
    function [self] = UniqueName(h_unique_named_object_manager,name)
      if nargin<1
        h_unique_named_object_manager = [];
      end
      if nargin<2
        name = [];
      end
      self@UniqueId(h_unique_named_object_manager);
      self.h_unique_named_object_manager = h_unique_named_object_manager;
      if ~isempty(self.h_unique_named_object_manager)
        self.h_unique_named_object_manager.register(self,name);
      else
        self.name = name;
      end
    end
    function [name] = get.name(self)
      name = self.name_;
    end
    function set.name(self,name)
      if isempty(self.h_unique_named_object_manager)
        self.name_ = name;
      else
        self.h_unique_named_object_manager.attemptRenameObject(self,name);
      end
    end
    function [self] = setObjectManager(self,h_unique_named_object_manager)
      if self.h_unique_named_object_manager==h_unique_named_object_manager
        %ignore self sets
        return;
      end
      if ~isempty(self.h_unique_named_object_manager)
        self.h_unique_named_object_manager.unregister(self);
      end
      self.h_unique_named_object_manager = h_unique_named_object_manager;
      setObjectManager@UniqueId(self,h_unique_named_object_manager);
      if isempty(self.h_unique_named_object_manager)
        return;
      end
      self.h_unique_named_object_manager.register(self);
    end
    function unregister(self)
      self.setObjectManager([]);
    end
  end
end