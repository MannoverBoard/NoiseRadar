classdef ComponentTypeEntry < handle
    properties
        UID = [];
        short_name = [];
        components = Set();
        counter = AtomicCounter(1);
    end
end