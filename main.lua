T = true
F = false

Gate   = require("gate")
Input  = require("input")
Module = require("module")

availableGates = {
    ["AND"] = Gate.AND,
    ["NOT"] = Gate.NOT,
}

TICK_DELAY = 0.5
timer = 0
current_step = 0

function love.load()
    inputs = {
        Input.new(32, 64,  Input.CLOCK),
        Input.new(32, 128, Input.CLOCK),
    }
    gates = {}
    modules = {}

    Gate.OR = {
        name        = "OR",
        inputNames  = {"A", "B"},
        outputNames = {"O"}, 
        func        =   function(a, b) 
                            return Gate.NOT.func(Gate.AND.func(Gate.NOT.func(a), Gate.NOT.func(b)))
                        end,
        delay = 0
    }

    Gate.DELAY = {
        name        = "delay",
        inputNames  = {"A"},
        outputNames = {"O"},
        func        =   function(a) 
                            return a 
                        end,
        delay       = 1
    }

    Input.GROUND = {
        name = "ground",
        func =  function (t)
                    return false
                end
    }

    modules[1] = Module.new(64, 16)
    modules[1]:addGate(Gate.new(96, 128, Gate.NOT, 1))
    modules[1]:addGate(Gate.new(192, 96, Gate.OR, 2, modules[1].gates[1]))
    modules[1]:addGate(Gate.new(256, 32, Gate.DELAY, 3))
    modules[1]:addGate(Gate.new(344, 96, Gate.AND, modules[1].gates[2], modules[1].gates[3]))

    -- modules[1]:assignInputs(inputs[2], inputs[1], inputs[1])

end

function love.update(dt)
    timer = timer + dt
    while timer > TICK_DELAY do
        timer = timer - TICK_DELAY
        current_step = current_step + 1
    end
end

function love.draw()
    for _, input in pairs(inputs) do
        drawInput(input)
    end
    for _, gate in pairs(gates) do
        drawGate(gate)
    end
    for _, mod in pairs(modules) do
        drawModule(mod)
    end
end 

function drawInput(input, x, y)
    local mode = input:getOutputAt(current_step) and "fill" or "line"
    love.graphics.rectangle(mode, input.x, input.y, 32, 32)
    love.graphics.printf(input.name, input.x, input.y + 36, 32, "center")
end

function drawGate(gate)
    local mode = gate:getOutputAt(current_step) and "fill" or "line"
    love.graphics.circle(mode, gate.x + 16, gate.y + 16, 16)
    love.graphics.printf(gate.name, gate.x, gate.y + 36, 32, "center")
    for _, input in pairs(gate.inputs) do
        if type(input) ~= "number" then
            love.graphics.line(gate.x, gate.y + 16, input.x + 32, input.y + 16)
        end
    end
end

function drawModule(mod)
    for _, gate in pairs(mod.gates) do
        drawGate(gate)
    end
end