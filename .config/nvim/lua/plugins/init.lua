local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
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

  -- status line
  use 'glepnir/galaxyline.nvim'

  -- show recent files on empty nvim command
  use 'mhinz/vim-startify'

  -- lsp config
  use { 'neovim/nvim-lspconfig', 'williamboman/nvim-lsp-installer', }

  -- for LSP autocompletion
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-cmdline'
  use 'hrsh7th/nvim-cmp'

  -- For vsnip users.
  use 'hrsh7th/cmp-vsnip'
  use 'hrsh7th/vim-vsnip'

  -- TODO: prettify telescope vim, make it use regex & shorten the window
  -- telescope - searching / navigation
  use { 'nvim-telescope/telescope.nvim', requires = { { 'nvim-lua/plenary.nvim' } } }

  -- better hotfix window (for showing and searching through results in telescope's find usages)
  -- TODO: learn how to use?
  use { 'kevinhwang91/nvim-bqf' }

  -- better highlighting
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  -- prettier tabs
  use 'romgrk/barbar.nvim'

  -- nice diagnostic pane on the bottom
  use 'folke/lsp-trouble.nvim'

  -- support the missing lsp diagnostic colors
  use 'folke/lsp-colors.nvim'

  -- better LSP UI (for code actions, rename etc.)
  use 'tami5/lspsaga.nvim'

  -- show indentation levels
  use 'lukas-reineke/indent-blankline.nvim'

  -- highlight variables under cursor
  use 'RRethy/vim-illuminate'

  -- this will automatically install listed dependencies
  -- only the first time NeoVim is opened, because that's when Packer gets installed
  if packer_bootstrap then
    require('packer').sync()
  end
end)

-- plugin specific configs go here
require('packages/barbar')
require('packages/gitsigns')
require('packages/indent-guide-lines')
require('packages/lsp-colors')
require('packages/lsp-trouble')
require('packages/lsp-saga')
require('packages/nvim-cmp')
require('packages/nvim-tree')
require('packages/nvim-treesitter')
require('packages/telescope')


return packer
