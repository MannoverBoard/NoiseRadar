classdef ComponentTypeEntry < UniqueName
    properties
        short_name = [];
        components = Set();
        counter = AtomicCounter(1);
    end
end