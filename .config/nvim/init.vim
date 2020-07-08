" Plugins
call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf.vim'
Plug 'itchyny/lightline.vim'
Plug 'valloric/youcompleteme'
Plug 'tpope/vim-fugitive'
Plug 'rbgrouleff/bclose.vim'
Plug 'francoiscabrol/ranger.vim' 


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

