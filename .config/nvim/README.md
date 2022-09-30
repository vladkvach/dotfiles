## _Installation_

```sh
git clone https://github.com/vladkvach/dotfiles.git
mv dotfiles/.config/nvim ~/.config
```

## _Icons_

Icons and other special characters are used all around the config to give NeoVim a prettier look. However, your terminal will not display these icons correctly unless it uses the correct font.

Install one of the icon fonts listed [here](https://www.nerdfonts.com/). Just follow their instructions for your specific OS. After installation is complete, don't forget to configure your terminal to start using the new font. Each terminal does this differently, so be sure to checkout official documentations if you run into any troubles.

## _Language Server_

| **Language** |                                **LSP impl**                                |               **LSP client**                |
| :----------: | :------------------------------------------------------------------------: | :-----------------------------------------: |
|     C++      |             [coc-clangd](https://github.com/clangd/coc-clangd)             | [coc](https://github.com/neoclide/coc.nvim) |
|    CMake     | [cmake-language-server](https://github.com/regen100/cmake-language-server) | [coc](https://github.com/neoclide/coc.nvim) |
|    Python    |         [coc-pyright](https://github.com/fannheyward/coc-pyright)          | [coc](https://github.com/neoclide/coc.nvim) |


