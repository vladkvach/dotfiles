local fn = vim.fn
local packer_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
local missing_packer = fn.empty(fn.glob(packer_path))

if missing_packer > 0 then
    print("Cloning packer")
    fn.delete(packer_path, "rf")
    PACKER_BOOTSTRAP = fn.system({
        'git',
        'clone',
        'https://github.com/wbthomason/packer.nvim',
        '--depth',
        '20',
        packer_path
    })
    print("Installing packer close and reopen Neovim...")
    vim.cmd([[packadd packer.nvim]])
    missing_packer = fn.empty(fn.glob(packer_path))

    if missing_packer > 0 then
        error('Could not clone packer !\n Packer path: ' .. packer_path)
        return false
    else
        print('Packer cloned successfully')
        vim.api.nvim_command([[packadd packer.nvim]])

        -- Autocommand that reloads neovim whenever you save the plugins.lua file
        vim.cmd([[
    augroup packer_user_config
      autocmd!
      autocmd BufWritePost plugins.lua source <afile> | PackerSync
    augroup end
  ]]     )
    end
else
    vim.api.nvim_command([[packadd packer.nvim]])

    -- Autocommand that reloads neovim whenever you save the plugins.lua file
    vim.cmd([[
    augroup packer_user_config
      autocmd!
      autocmd BufWritePost plugins.lua source <afile> | PackerSync
    augroup end
  ]] )
end
