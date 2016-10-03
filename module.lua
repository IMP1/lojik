local Module = {}
Module.__index = Module
Module.__tostring = function(mod) return "module: " end

--[[
    Module Blueprint

    {
        -- list of gates
        gates = {
            -- x pos, y pos, gate blueprint
            {  64,  64, Gate.AND      },
            { 192,  64, Gate.NOT      },
            { 256,  64, Gate.SPLITTER },
        },
        -- list of module inputs and what gates they go to.
        inputs = {
            -- first input goes to first gate, first input thereof.
            { 1, 1 },
        },
        -- list of module outputs and what gates they come from.
        outputs = {
            -- first output is third gate, second output thereof.
            { 3, 2 },
        },
        -- list of internal module connections between gates.
        connections = {
            -- the first gate's second input will come from the second gate's first input.
            { 1, 2, 2, 1 },
            -- the second gate's first input will come from the first gate's first input.
            { 2, 1, 1, 1 },
        }
    }

--]]

function Module.new(x, y, moduleBlueprint)
    local this = {}
    this.x = x
    this.y = y
    this.inputs = moduleBlueprint.inputs
    this.connections = moduleBlueprint.connections
    this.gates = {}
    for i, gateParams in ipairs(moduleBlueprint.gates) do
        this.gates[i] = Gate.new(gateParams[1] + this.x, gateParams[2] + this.y, gateParams[3])

        -- for inputIndex, gateIndex in ipairs(moduleBlueprint.inputs) do
        --     if gateIndex[1] == i then
        --         table.insert(gateInputs, { gateIndex[2], inputs[inputIndex] })
        --     end
        -- end
        
        
    end

    for i, gateParams in ipairs(moduleBlueprint.gates) do
        for _, connection in ipairs(moduleBlueprint.connections) do
            if connection[1] == i then
                this.gates[i]:setInput(connection[2], this.gates[connection[3]], connection[4])
            end
        end
    end
    setmetatable(this, Module)
    return this
end

function Module:assignInputs(...)
    local assignedInputs = {...}
    for i, input in pairs(self.inputs) do
        self.gates[input[1]]:setInput(input[2], unpack(assignedInputs[i]))
    end
end

function Module:getOutputAt(t)
    local outputValues = {}
    for _, outputIndex in pairs(self.outputs) do
        local result = { self.gates[outputIndex[1]]:getOutputAt(t) }
        table.insert(outputValues, result[outputIndex[2]])
    end
    return unpack(outputValues)
end

return Module