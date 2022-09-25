local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

local ensure_packer = function()
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path })
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

local packer = require('packer').startup(function(use)
  -- Packer should manage itself
  use 'wbthomason/packer.nvim'

  use { 'kyazdani42/nvim-tree.lua', requires = { 'kyazdani42/nvim-web-devicons', }, tag = 'nightly' }

  -- git integration
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }

  -- surround vim
  use 'tpope/vim-surround'

  -- nerd commenter
  use 'scrooloose/nerdcommenter'

  -- theme
  use 'ellisonleao/gruvbox.nvim'

  -- status line
  use { 'nvim-lualine/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons', opt = true } }

  -- show recent files on empty nvim command
  use 'mhinz/vim-startify'

  -- TODO: prettify telescope vim, make it use regex & shorten the window
  -- telescope - searching / navigation
  use { 'nvim-telescope/telescope.nvim', requires = { { 'nvim-lua/plenary.nvim' } } }

  -- better hotfix window (for showing and searching through results in telescope's find usages)
  -- TODO: learn how to use?
  use { 'kevinhwang91/nvim-bqf' }

  -- better highlighting
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  -- prettier tabs
  use { 'akinsho/bufferline.nvim', tag = 'v2.*', requires = 'kyazdani42/nvim-web-devicons' }

  -- key information
  use { 'folke/which-key.nvim' }

  -- show indentation levels
  use 'lukas-reineke/indent-blankline.nvim'

  -- nodejs extension host
  use { 'neoclide/coc.nvim', branch = 'release' }

  -- highlight variables under cursor
  use 'RRethy/vim-illuminate'

  -- displaying the colours in the file
  use 'norcalli/nvim-colorizer.lua'

  -- The missing auto-completion for cmdline!
  use 'gelguy/wilder.nvim'

  -- this will automatically install listed dependencies
  -- only the first time NeoVim is opened, because that's when Packer gets installed
  if packer_bootstrap then
    require('packer').sync()
  end
end)

-- plugin specific configs go here
local cfg_path = fn.stdpath('config')
local fname_arr = vim.split(fn.glob(cfg_path .. '/lua/plugins/packages/*.lua'), '\n')

for _, file in pairs(fname_arr) do
  require('plugins/packages/' .. file:gsub(cfg_path .. '/lua/plugins/packages/', ''):sub(1, -5))
end


return packer
