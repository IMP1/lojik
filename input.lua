local Input = {}
Input.__index = Input
Input.__tostring = function(input)
    return "Input: " .. input.name
end

function Input.new(x, y, component)
    local this = {}
    this.name = component.name
    this.x = x
    this.y = y
    this.func = component.func
    setmetatable(this, Input)
    return this
end

function Input:getOutputAt(t)
    return self.func(t)
end

Input.CLOCK = {
    name = "clock",
    func = function(t)
        return t % 2 == 1
    end 
}

Input.CONSTANT = {
    name = "up",
    func = function(t)
        return true
    end 
}

return Input