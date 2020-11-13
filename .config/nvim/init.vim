" Plugins
call plug#begin('~/.vim/plugged')

if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'junegunn/fzf.vim'
Plug 'starcraftman/vim-eclim'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-fugitive'
Plug 'rbgrouleff/bclose.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'tmhedberg/simpylfold'
Plug 'konfekt/fastfold'
Plug 'yggdroot/indentline'
Plug 'scrooloose/nerdtree' 
Plug 'gabrielelana/vim-markdown' 

call plug#end()

" Switch Tabs
nnoremap tl :tabnext<CR>
nnoremap th :tabprev<CR>
nnoremap tn :tabnew<CR>

" Set Statements
set number
set expandtab
set hidden
set ignorecase
set smartcase
set noshowmode
set mouse=a
set shiftwidth=4
set tabstop=2
set softtabstop=2
set completeopt-=preview


" Deoplete
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option('sources.java', ['jc', 'javacomplete2', 'file', 'buffer'])
call deoplete#custom#option('smart_case', v:true)
inoremap <silent><expr> <TAB>
    \ pumvisible() ?  "\<C-n>" :
    \ <SID>check_back_space() ? "\<TAB>" :
    \ deoplete#mappings#manual_complete()
function! s:check_back_space() abort "" {{{
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction "" }}}

" Java completion
autocmd FileType java setlocal omnifunc=javacomplete#Complete
autocmd FileType java JCEnable

" NERDTREE
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map tt :NERDTreeToggle<CR>

" Markdown
let g:markdown_enable_spell_checking = 0
