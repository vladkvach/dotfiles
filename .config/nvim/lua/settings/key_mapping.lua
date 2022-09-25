-- defaults
local opts = { noremap = true, silent = true }
local api = vim.api
-- TODO: disabled since trying out "clipboard = unnamedplus" option
-- copy
--api.nvim_set_keymap('', '<C-c>', '"+y', opts)
-- paste
--api.nvim_set_keymap('', '<C-v>', '"+p', opts)
-- cut
--api.nvim_set_keymap('', '<C-x>', '"+d', opts)
-- paste in insert mode
--api.nvim_set_keymap('i', '<C-v>', '<Esc>"+pa', opts)

-- shift the movement keys by 0 to the right
api.nvim_set_keymap('', 'j', 'h', opts)
api.nvim_set_keymap('', 'k', 'j', opts)
api.nvim_set_keymap('', 'l', 'k', opts)
api.nvim_set_keymap('', 'č', 'l', opts)

-- mapping ESC to ć
api.nvim_set_keymap('n', 'ć', '<Esc>', opts)
api.nvim_set_keymap('n', 'Ć', '<Esc>', opts)
api.nvim_set_keymap('v', 'ć', '<Esc>', opts)
api.nvim_set_keymap('v', 'Ć', '<Esc>', opts)
api.nvim_set_keymap('c', 'ć', '<Esc>', opts)
api.nvim_set_keymap('c', 'Ć', '<Esc>', opts)
-- make the cursor stay on the same character when leaving insert mode
api.nvim_set_keymap('i', 'ć', '<Esc>l', opts)
api.nvim_set_keymap('i', 'Ć', '<Esc>l', opts)

-- fast scrolling
api.nvim_set_keymap('n', 'K', '9j', opts)
api.nvim_set_keymap('n', 'L', '9k', opts)
api.nvim_set_keymap('v', 'K', '9j', opts)
api.nvim_set_keymap('v', 'L', '9k', opts)

-- stay in normal mode after inserting a new line
api.nvim_set_keymap('', 'o', 'o <Bs><Esc>', opts)
api.nvim_set_keymap('', 'O', 'O <Bs><Esc>', opts)

-- mapping that opens .vimrc in a new tab for quick editing
api.nvim_set_keymap('n', '<Leader>ev', '<Cmd>tabe $MYVIMRC<CR>', opts)
-- mapping that sources the vimrc in the current filea doesn't work, should change all require calls to dofile
-- or clear all require cache and reimport
-- api.nvim_set_keymap('n', '<Leader>sv', '<Cmd>lua dofile(vim.fn.stdpath(\'config\')..\'/init.lua\')<CR>', { noremap = true, silent = false })

-- Mapping U to Redo.
api.nvim_set_keymap('', 'U', '<C-r>', opts)
api.nvim_set_keymap('', '<C-r>', '<NOP>', opts)

-- indent via Tab
api.nvim_set_keymap('n', '<Tab>', '>>_', opts)
api.nvim_set_keymap('n', '<S-Tab>', '<<_', opts)
api.nvim_set_keymap('v', '<Tab>', '>>_', opts)
api.nvim_set_keymap('v', '<S-Tab>', '<<_', opts)
api.nvim_set_keymap('i', '<Tab>', '\t', opts)
api.nvim_set_keymap('i', '<S-Tab>', '\b', opts)

-- window movement
api.nvim_set_keymap('', '<C-w>j', '<C-w>h', opts)
api.nvim_set_keymap('', '<C-w>k', '<C-w>j', opts)
api.nvim_set_keymap('', '<C-w>l', '<C-w>k', opts)
api.nvim_set_keymap('', '<C-w>č', '<C-w>l', opts)

-- opening terminal with shortcut
api.nvim_set_keymap('', '<Leader><CR>', '<Cmd>silent !$TERM &<CR>', opts)

-- jumping back and forth
api.nvim_set_keymap('', '<C-K>', '<C-O>', opts)
api.nvim_set_keymap('', '<C-L>', '<C-I>', opts)

-- autocomplete
-- if autocomplete popup menu opens pressing enter will complete the first match
-- api.nvim_set_keymap('i', '<Tab>', 'v:lua.smart_tab()', {expr = true, noremap = true})
api.nvim_set_keymap('i', '<CR>', 'pumvisible() ? "<C-n><Esc>a" : "<CR>"',
    { expr = true, noremap = true, silent = true })
