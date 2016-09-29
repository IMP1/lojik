local Module = {}
Module.__index = Module

function Module.new(x, y, moduleBlueprint, ...)
    local this = {}
    this.x = x
    this.y = y
    this.inputs = moduleBlueprint.inputs
    this.connections = moduleBlueprint.connections
    this.gates = {}
    for i, gateParams in ipairs(moduleBlueprint.gates) do
        this.gates[i] = Gate.new(unpack(gateParams))

        -- for inputIndex, gateIndex in ipairs(moduleBlueprint.inputs) do
        --     if gateIndex[1] == i then
        --         table.insert(gateInputs, { gateIndex[2], inputs[inputIndex] })
        --     end
        -- end
        
        for _, connection in ipairs(moduleBlueprint.connections) do
            if connection[1] == i then
                this.gates[i]:setInput(connection[2], this.gates[connection[3]])
            end
        end
    end
    setmetatable(this, Module)
    this:assignInputs(...)
    return this
end

function Module:assignInputs(...)
    local assignedInputs = {...}
    for i, input in pairs(self.inputs) do
        self.gates[input[1]]:setInput(input[2], assignedInputs[i])
    end
end

return Module