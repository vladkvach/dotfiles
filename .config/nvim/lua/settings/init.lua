local function apply_options(scope, options)
    for opt, val in pairs(options) do
        scope[opt] = val
    end
end

local options = {
    termguicolors = true, -- Enable 24 bits color in terminal
    guicursor = "a:block,i-ci-ve:ver25", -- Set the gui cursor to nothing for each mode
    inccommand = "nosplit", -- Show effect of command incrementally while writing it (substitution)
    mouse = "a", -- Enable mouse in all mode
    tabstop = 4, -- A tab is 4 spaces (display). maximum width of tab character (measured in spaces)
    shiftwidth = 4, -- Indent to 4 spaces (size of indent (measured in spaces), should equal tabstop)
    softtabstop = 4, -- A tab is 4 spaces (insert mode). should be the same as the other two above
    expandtab = true, -- Use spaces instead of tab character. expand tabs to spaces
    smartindent = true, -- smart indenting on new line for C-like programs
    clipboard = "unnamedplus", -- Use + register for yank, delete, change and put operation
    hidden = true, -- Make sure hidden buffer are not unloaded
    backup = false, -- disable file backups
    writebackup = false, -- Disable file backup on save
    swapfile = false, -- disable swap files (can open already open files)
    laststatus = 3, -- Single status line for the whole frame
    autoread = true, -- auto file change detection
    autoindent = true, -- copy the indentation from previous line
    smarttab = true, -- tab infront of a line inserts blanks based on shiftwidth
    ignorecase = true, -- Ignorecase when searching
    incsearch = true, -- start searching on each keystroke
    smartcase = true, -- ignore case when lowercase, match case when capital case is used
    hlsearch = true, -- highlight the search results
    lazyredraw = true, -- useful for when executing macros.
    ttimeoutlen = 10, -- ms to wait for a key code seq to complete
    history = 10000, -- numbers of entries in history for ':' commands and search patterns (10000 = max)
    updatetime = 300, -- used for CursorHold event (for document highlighting detection)
    completeopt = 'menuone,preview,noinsert',
}

local window_options = {
    signcolumn = "yes", -- Show the sign colum (for gitsigns)
    number = true, -- Show line numbers
    relativenumber = true, -- Use relative line numbers
    scrolloff = 8, -- Keep at least 8 lines above the cursor.
    wrap = false, -- disable wrap long text into multiple lines
    colorcolumn = '120' -- Color the 120 character as a limit for line length
}

local global_options = {
    tex_flavor = 'latex',
    mapleader = ',',
    do_filetype_lua = 1,
    did_load_filetpyes = 0,
    loaded = 1,
    loaded_netrwPlugin = 1,
    coc_global_extensions = { 'coc-pyright', 'coc-tsserver', 'coc-json', 'coc-git', 'coc-html', 'coc-css', 'coc-sh',
        'coc-markdownlint' }
}

apply_options(vim.o, options)
apply_options(vim.wo, window_options)
apply_options(vim.g, global_options)
