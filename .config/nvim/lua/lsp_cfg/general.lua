local lsp_installer_servers = require('nvim-lsp-installer.servers')
local lsp_installer = require('nvim-lsp-installer')

-- install LSP servers
local function installServer(name)
    local ok, server = lsp_installer_servers.get_server(name)
    if ok then
        if not server:is_installed() then
            server:install()
        end
    end
end

local function installServers(names)
    for _, name in pairs(names) do
        installServer(name)
    end
end

-- find a list of available ones here: https://github.com/williamboman/nvim-lsp-installer
installServers({ 'ccls', 'pyright', 'angularls', 'bashls', 'dockerls', 'sumneko_lua', 'jsonls', 'cssls' })

-- setup installed servers
lsp_installer.on_server_ready(function(server)
    local opts = {
        automatic_installation = true,
        ui = {
            icons = {
                server_installed = "✓",
                server_pending = "➜",
                server_uninstalled = "✗"
            }
        }
    }

    if server.name == 'sumneko_lua' then
        local sumneko_lua_opts = require('lsp_cfg/servers/sumneko_lua')
        opts = vim.tbl_deep_extend('force', sumneko_lua_opts, opts)
    end

    if server.name == 'ccls' then
        local ccls_opts = require('lsp_cfg/servers/ccls')
        opts = vim.tbl_deep_extend('force', ccls_opts, opts)
    end

    if server.name == 'pyright' then
        local pyright_opts = require('lsp_cfg/servers/pyright')
        opts = vim.tbl_deep_extend('force', pyright_opts, opts)
    end

    -- This setup() function is exactly the same as lspconfig's setup function.
    -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/ADVANCED_README.md
    server:setup(opts)
end)

-- diagnostic symbols
local signs = { Error = "", Warn = "", Hint = "", Info = "" }
for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- completion symbols
vim.lsp.protocol.CompletionItemKind = {
    "   (Text) ",
    "   (Method)",
    "   (Function)",
    "   (Constructor)",
    " ﴲ  (Field)",
    "[] (Variable)",
    "   (Class)",
    " ﰮ  (Interface)",
    "   (Module)",
    " 襁 (Property)",
    "   (Unit)",
    "   (Value)",
    " 練 (Enum)",
    "   (Keyword)",
    "   (Snippet)",
    "   (Color)",
    "   (File)",
    "   (Reference)",
    "   (Folder)",
    "   (EnumMember)",
    " ﲀ  (Constant)",
    " ﳤ  (Struct)",
    "   (Event)",
    "   (Operator)",
    "   (TypeParameter)"
}
