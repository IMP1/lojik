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
        Input.new(32, 460, Input.CONSTANT)
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

    local blinkToConstant = {
        gates = {
            {  96, 128, Gate.NOT   },
            { 192,  96, Gate.OR    },
        },
        inputs = {
            { 1, 1 }, 
            { 2, 1 }
        },
        connections = {
            -- { gateX, gateX input index, gateY that gateX will use for input },
            { 2, 2, 1 }
        },
        outputs = {
            2
        }
    }

    modules[1] = Module.new(64, 16, blinkToConstant)
    modules[1]:assignInputs(inputs[2], inputs[1])



    local constantToBlink = {
        gates = {
            {  64,  64, Gate.AND   },
            { 192,  64, Gate.NOT   },
            { 256,  64, Gate.OR    },
        },
        inputs = {
            { 1, 1 },
        },
        connections = {
            { 1, 2, 2 },
            { 2, 1, 1 },
            { 3, 1, 2 },
            { 3, 2, 2 },
        },
        outputs = { 2 }
    }

    modules[2] = Module.new(64, 344, constantToBlink)
    modules[2]:assignInputs(inputs[3])

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
        drawConnection(input, gate)
    end
end

function drawModule(mod)
    for _, gate in pairs(mod.gates) do
        drawGate(gate)
    end
end

function drawConnection(from, to)
    if (from.x + 32 > to.x) then
        local midY
        if (math.abs(from.y - to.y) < 32) then
            midY = from.y - 16
        else
            midY = (from.y + 16) * 0.7 + (to.y + 16) * 0.3
        end
        love.graphics.line(from.x + 32, from.y + 16, from.x + 48, from.y + 16)
        love.graphics.line(from.x + 48, from.y + 16, from.x + 48, midY)
        love.graphics.line(from.x + 48, midY, to.x - 16, midY)
        love.graphics.line(to.x - 16, midY, to.x - 16, to.y + 16)
        love.graphics.line(to.x, to.y + 16, to.x - 16, to.y + 16)
    else
        love.graphics.line(from.x + 32, from.y + 16, to.x, to.y + 16)
    end
end