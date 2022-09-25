local opts = {
    cmd = { "pyright-langserver", "--stdio" },
    filetypes = { "python" },
    single_file_support = true,
    settings = {
        python = {
            analysis = {
                autoSearchPaths = true,
                diagnosticMode = "workspace",
                useLibraryCodeForTypes = true
            }
        }
    },
}
return opts
