local opts = { silent = true, noremap = true }
local nvim_tree = require('nvim-tree')

vim.api.nvim_set_keymap('n', '<C-n>', '<Cmd>NvimTreeToggle<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>r', '<Cmd>NvimTreeRefresh<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>n', '<Cmd>NvimTreeFindFile<CR>', opts)

local tree_cb = require('nvim-tree.config').nvim_tree_callback
local list = {
    { key = '<C-t>', cb = tree_cb('tabnew') },
    { key = '<CR>', cb = tree_cb('edit') },
    { key = 'o', cb = tree_cb('edit') },
    { key = '<2-LeftMouse>', cb = tree_cb('edit') },
    { key = '<2-RightMouse>', cb = tree_cb('cd') },
    { key = '<Tab>', cb = tree_cb('preview') },
    { key = 'R', cb = tree_cb('refresh') },
    { key = 'a', cb = tree_cb('create') },
    { key = 'd', cb = tree_cb('remove') },
    { key = 'r', cb = tree_cb('rename') },
    { key = 'x', cb = tree_cb('cut') },
    { key = 'y', cb = tree_cb('copy') },
    { key = 'p', cb = tree_cb('paste') },
    { key = '<', cb = tree_cb('dir_up') },
    { key = 'q', cb = tree_cb('close') }
}

nvim_tree.setup {
    auto_reload_on_write = true,
    disable_netrw = false,
    hijack_cursor = false,
    hijack_netrw = true,
    hijack_unnamed_buffer_when_opening = false,
    ignore_buffer_on_setup = false,
    open_on_setup = false,
    open_on_setup_file = false,
    open_on_tab = false,
    sort_by = 'name',
    update_cwd = false,
    view = {
        width = 30,
        hide_root_folder = false,
        side = 'left',
        preserve_window_proportions = false,
        number = false,
        relativenumber = false,
        signcolumn = 'yes',
        mappings = {
            custom_only = false,
            list = list,
        },
    },
    renderer = {
        indent_markers = {
            enable = false,
            icons = {
                corner = '└ ',
                edge = '│ ',
                none = '  ',
            },
        },
        icons = {
            webdev_colors = true,
        },
    },
    hijack_directories = {
        enable = true,
        auto_open = true,
    },
    update_focused_file = {
        enable = false,
        update_cwd = false,
        ignore_list = {},
    },
    ignore_ft_on_setup = {},
    system_open = {
        cmd = '',
        args = {},
    },
    diagnostics = {
        enable = false,
        show_on_dirs = false,
        icons = {
            hint = '',
            info = '',
            warning = '',
            error = '',
        },
    },
    filters = {
        dotfiles = false,
        custom = {},
        exclude = {},
    },
    git = {
        enable = true,
        ignore = true,
        timeout = 400,
    },
    actions = {
        use_system_clipboard = true,
        change_dir = {
            enable = true,
            global = false,
            restrict_above_cwd = false,
        },
        open_file = {
            quit_on_open = false,
            resize_window = false,
            window_picker = {
                enable = true,
                chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890',
                exclude = {
                    filetype = { 'notify', 'packer', 'qf', 'diff', 'fugitive', 'fugitiveblame' },
                    buftype = { 'nofile', 'terminal', 'help' },
                },
            },
        },
    },
    trash = {
        cmd = 'trash',
        require_confirm = true,
    },
    log = {
        enable = false,
        truncate = false,
        types = {
            all = false,
            config = false,
            copy_paste = false,
            diagnostics = false,
            git = false,
            profile = false,
        },
    },
}
