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
        outputs = {
            2
        },
        connections = {
            -- { gateX, gateX input index, gateY that gateX will use for input },
            { 2, 2, 1, 1 }
        },
    }

    modules[1] = Module.new(64, 16, blinkToConstant)
    modules[1]:assignInputs({inputs[2], 1}, {inputs[1], 1})



    local constantToBlink = {
        gates = {
            {  64,  64, Gate.AND      },
            { 160,  64, Gate.NOT      },
            { 256,  64, Gate.DELAY    },
            { 344,  64, Gate.SPLITTER },
        },
        inputs = {
            { 1, 2 },
        },
        outputs = {
            { 3, 2 },
        },
        connections = {
            { 1, 1, 4, 1 },
            { 2, 1, 1, 1 },
            { 3, 1, 2, 1 },
            { 4, 1, 3, 1 },
        }
    }

    modules[2] = Module.new(64, 344, constantToBlink)
    modules[2]:assignInputs({inputs[3], 1})

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

function drawModule(mod)
    -- Draw Gates
    for _, gate in pairs(mod.gates) do
        drawGate(gate)
    end
    -- Draw Internal Connections
    for _, con in pairs(mod.connections) do
        local gateTo   = mod.gates[con[1]]
        local gateFrom = mod.gates[con[3]]
        local from = { getGateOutputPosition (gateFrom, con[4]) }
        local to   = { getGateInputPosition  (gateTo,   con[2]) }
        drawConnection(from, to)
    end
    -- Draw Input Connections
    for i, con in ipairs(mod.inputs) do

        local conGateIndex = con[1]
        local conInputIndex = con[2]

        local gateTo   = mod.gates[conGateIndex]
        local gateFrom = mod.gates[conGateIndex].inputs[conInputIndex][1]

        local gateFromOutputIndex = mod.gates[i].inputs[con[1]][2]

        local from = { getGateOutputPosition (gateFrom, gateFromOutputIndex) }
        local to   = { getGateInputPosition  (gateTo,   conInputIndex) }
        drawConnection(from, to)
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

end

function getGateInputPosition(gate, inputIndex)
    local x = gate.x
    local y = gate.y
    if gate.inputs then
        y = y + 32 * (inputIndex / (#gate.inputs + 1))
    else
        y = y + 16
    end
    return x, y
end

function getGateOutputPosition(gate, outputIndex)
    local x = gate.x + 32
    local y = gate.y
    if gate.outputs then
        y = y + 32 * (outputIndex / (#gate.outputs + 1))
    else
        y = y + 16
    end
    return x, y
end

function drawConnection(from, to)
    if (from[1] + 32 > to[1]) then
        local midY
        if (math.abs(from[2] - to[2]) < 32) then
            midY = from[2] - 16
        else
            midY = (from[2] + 16) * 0.7 + (to[2] + 16) * 0.3
        end
        love.graphics.line(from[1],      from[2], from[1] + 16, from[2])
        love.graphics.line(from[1] + 16, from[2], from[1] + 16, midY)
        love.graphics.line(from[1] + 16, midY,    to[1] - 16,   midY)
        love.graphics.line(to[1] - 16,   midY,    to[1] - 16,   to[2])
        love.graphics.line(to[1],        to[2],   to[1] - 16,   to[2])
    else
        love.graphics.line(from[1], from[2], to[1], to[2])
    end
end