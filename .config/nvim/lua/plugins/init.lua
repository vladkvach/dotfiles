local present, _ = pcall(require, "packerInit")
local packer

if present then
    packer = require "packer"
else
    return false
end

-- Have packer use a popup window
packer.init({
    display = {
        open_fn = function()
            return require("packer.util").float({ border = "rounded" })
        end,
    },
    git = {
        clone_timeout = 600
    }
})

-- Install your plugins here
return packer.startup(function(use)
    use('wbthomason/packer.nvim') -- Have packer manage itself
    use('nvim-lua/plenary.nvim') -- Useful lua functions used by lots of plugins
    use('lewis6991/gitsigns.nvim') -- Git

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if PACKER_BOOTSTRAP then
        require("packer").sync()
    end
end)
