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
    this.inputs = { ... }
    this.inputNames = component.inputNames
    this.outputNames = component.outputNames
    this.func = component.func
    this.delay = component.delay
    setmetatable(this, Gate)
    return this
end

function Gate:getOutputAt(t)
    local inputs = {}
    for i, input in pairs(self.inputs) do
        if type(input) == "number" then
            return false
        end
        inputs[i] = input:getOutputAt(t - self.delay)
    end
    return self.func(unpack(inputs))
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
                        return Gate.NOT.func(nand(a, b))
                    end,
    delay = 0
}


return Gate