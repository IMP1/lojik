local Gate = {}
Gate.__index = Gate

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

function Gate:setInput(index, input)
    table.insert(self.inputs, input)
end

function Gate:getOutputAt(t)
    if self.lastUpdate == t then 
        return self.currentState 
    end
    local inputs = {}
    for i, input in ipairs(self.inputs) do
        if type(input) == "number" then
            self.currentState = false
            return self.currentState
        elseif input == self then
            inputs[i] = input.currentState
        else
            inputs[i] = input:getOutputAt(t - self.delay)
        end
    end
    self.currentState = self.func(unpack(inputs))
    return self.currentState
end

Gate.NAND = {
    name        = "NAND",
    inputNames  = {"A", "B"}, 
    outputNames = {"O"}, 
    func        =   function(a, b) 
                        return nand(a, b)
                    end,
    delay = 0
}

Gate.NOT = {
    name        = "NOT",
    inputNames  = {"A"}, 
    outputNames = {"O"}, 
    func        =   function(a) 
                        return nand(a, a)
                    end,
    delay = 0
}

Gate.AND = {
    name        = "AND",
    inputNames  = {"A", "B"},
    outputNames = {"O"}, 
    func        =   function(a, b) 
                        return nand(nand(a, b), nand(a, b))
                    end,
    delay = 0
}


return Gate