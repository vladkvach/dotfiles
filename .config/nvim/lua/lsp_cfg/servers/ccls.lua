local opts = {
    cmd = { "ccls" },
    filetypes = { "c", "cpp", "objc", "objcpp" },
    offset_encoding = "utf-32",
    init_options = {
        compilationDatabaseDirectory = "build";
        index = {
            threads = 0;
        };
        clang = {
            excludeArgs = { "-frounding-math" };
        };
    }
}
return opts
