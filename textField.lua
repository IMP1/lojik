local TextField = {}
TextField.__index = TextField

function TextField.new()
    local this = {}
    setmetatable(this, TextField)
    this.text = ""
    this.selected = false
    this.cursorPosition = 0
    return this
end

return TextField