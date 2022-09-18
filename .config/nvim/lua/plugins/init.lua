local present, _ = pcall(require, "packerInit")
local packer

if present then
    packer = require "packer"
else
    return false
end

local use = packer.use

return packer.startup({
    function()
        use "sindrets/diffview.nvim"

    end,
    config = {
        display = {
            open_fn = function()
                return require("packer.util").float({ border = "rounded" })
            end
        },
        git = {
            clone_timeout = 600
        }
    },
})
