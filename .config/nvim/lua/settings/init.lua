local function apply_options(scope, options)
    for opt, val in pairs(options) do
        scope[opt] = val
    end
end

vim.g.mapleader = ' ' -- TODO: move this into mappings
local options = {
    termguicolors = true, -- Enable 24 bits color in terminal
    guicursor = "a:block,i-ci-ve:ver25", -- Set the gui cursor to nothing for each mode
    inccommand = "nosplit", -- Show effect of command incrementally while writing it (substitution)
    mouse = "a", -- Enable mouse in all mode
    tabstop = 4, -- A tab is 4 spaces (display)
    shiftwidth = 4, -- Indent to 4 spaces
    softtabstop = 4, -- A tab is 4 spaces (insert mode)
    expandtab = true, -- Use spaces instead of tab character
    smartindent = true, -- Smart auto indent on new line
    clipboard = "unnamedplus", -- Use + register for yank, delete, change and put operation
    hidden = true, -- Make sure hidden buffer are not unloaded
    backup = false, -- Disable file backups
    writebackup = false, -- Disable file backup on save
    swapfile = false, -- Disable swap files
    laststatus = 3, -- Single status line for the whole frame
}

local window_options = {
    signcolumn = "yes", -- Show the sign colum (for gitsigns)
    number = true, -- Show line numbers
    relativenumber = true, -- Use relative line numbers
    scrolloff = 8, -- Keep at least 8 lines above the cursor.
    wrap = false, -- Disable line wrap
    colorcolumn = '120' -- Color the 120 character as a limit for line length
}

local global_options = {
    tex_flavor = 'latex',
    do_filetype_lua = 1,
    did_load_filetpyes = 0
}

apply_options(vim.o, options)
apply_options(vim.wo, window_options)
apply_options(vim.g, global_options)
