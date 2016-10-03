local Gate = {}
Gate.__index = Gate
Gate.__tostring = function(gate) 
    local result = "Gate: "
    result = result .. gate.name .. " "
    result = result .. tostring(#gate.inputNames)
    result = result .. " -> "
    result = result .. tostring(#gate.outputNames)
    return result
end

local function nand(a, b)
    return not (a and b)
end

function Gate.new(x, y, component, ...)
    local this = {}
    this.x = x
    this.y = y
    this.name = component.name
    this.currentState = nil
    this.lastUpdate = -1
    this.inputNames = component.inputNames
    this.outputNames = component.outputNames
    this.inputs = { ... }
    this.func = component.func
    this.delay = component.delay
    setmetatable(this, Gate)
    return this
end

function Gate:setInput(index, input, inputOutputIndex)
    assert(inputOutputIndex ~= nil)

    self.inputs[index] = {input, inputOutputIndex}
end

function Gate:getOutputAt(t)
    if self.lastUpdate == t then 
        return self.currentState 
    end
    self.lastUpdate = t
    local inputs = {}
    for i, input in ipairs(self.inputs) do
        if input[1] == self then
            inputs[i] = input[1].currentState
        else
            local outputs = { input[1]:getOutputAt(t - self.delay) }
            inputs[i] = outputs[input[2]]
        end
    end
    self.currentState = { self.func(unpack(inputs)) }
    return unpack(self.currentState)
end

Gate.NAND = {
    name        = "NAND",
    inputNames  = {"A", "B"}, 
    outputNames = {"O"}, 
    func        = function(a, b) return nand(a, b) end,
    delay       = 0
}

Gate.NOT = {
    name        = "NOT",
    inputNames  = {"A"}, 
    outputNames = {"O"}, 
    func        = function(a) return nand(a, a) end,
    delay       = 0
}

Gate.AND = {
    name        = "AND",
    inputNames  = {"A", "B"},
    outputNames = {"O"}, 
    func        = function(a, b) return nand(nand(a, b), nand(a, b)) end,
    delay       = 0
}

Gate.OUTPUT = {
    name        = "OUTPUT",
    inputNames  = { "A" },
    outputNames = { "O" },
    func        = function(a) return a end,
    delay       = 0
}

Gate.SPLITTER = {
    name        = "FORK",
    inputNames  = { "A" },
    outputNames = { "O", "P" },
    func        = function(a) return a, a end,
    delay       = 0
}

return Gate