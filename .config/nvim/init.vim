" Plugins
call plug#begin('~/.vim/plugged')

Plug 'https://github.com/junegunn/fzf.vim'
Plug 'https://github.com/itchyny/lightline.vim'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/tpope/vim-fugitive'

call plug#end()

" Switch Tabs
nnoremap th :tabnext<CR>
nnoremap tl :tabprev<CR>
nnoremap tn :tabnew<CR>

" Set Statements
set number
set expandtab
set hidden
set ignorecase
set smartcase
set mouse=a
set shiftwidth=4

