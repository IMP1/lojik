local Module = {}
Module.__index = Module

function Module.new(x, y)
    local this = {}
    this.x = x
    this.y = y
    this.gates = {}
    this.inputAssignments = {}
    setmetatable(this, Module)
    return this
end

function Module:addGate(gate)
    table.insert(self.gates, gate)
    gate.x = gate.x + self.x
    gate.y = gate.y + self.y
end

function Module:assignInputs(...)
    local assignedInputs = {...}
    for _, gate in pairs(self.gates) do
        for i, inputRef in pairs(gate.inputs) do
            if type(inputRef) == "number" then
                gate.inputs[i] = assignedInputs[inputRef]
            end
        end
    end
end

return Module