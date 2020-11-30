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
Plug 'konfekt/fastfold'
Plug 'yggdroot/indentline'
Plug 'scrooloose/nerdtree' 
Plug 'gabrielelana/vim-markdown' 
Plug 'aperezdc/vim-template'

call plug#end()

" Switch Tabs
nnoremap tl :tabnext<CR>
nnoremap th :tabprev<CR>
nnoremap tn :tabnew<CR>
nnoremap tc :tabclose<CR>

" Switch Splits
tnoremap <A-h> <C-\><C-N><C-w>h
tnoremap <A-j> <C-\><C-N><C-w>j
tnoremap <A-k> <C-\><C-N><C-w>k
tnoremap <A-l> <C-\><C-N><C-w>l
inoremap <A-h> <C-\><C-N><C-w>h
inoremap <A-j> <C-\><C-N><C-w>j
inoremap <A-k> <C-\><C-N><C-w>k
inoremap <A-l> <C-\><C-N><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

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

" NERDTree and Terminal
map tt :NERDTreeToggle<CR>
let NERDTreeShowHidden=1
tnoremap <Esc> <C-\><C-n>
autocmd TabNew,VimEnter *
    \ NERDTree |
    \ execute "wincmd l" |
    \ split |
    \ execute "wincmd j" |
    \ execute "term" |
    \ execute "resize 10" |
    \ execute "set nonumber" |
    \ execute "wincmd k"

" Autoclose if only NERDTree and terminal remain
autocmd bufenter * if (winnr("$") == 2 && (exists("b:NERDTree") || &buftype ==# 'terminal')) | q | endif
autocmd bufenter * if (winnr("$") == 1 && (exists("b:NERDTree") || &buftype ==# 'terminal')) | q | endif

" Markdown
let g:markdown_enable_spell_checking = 0
