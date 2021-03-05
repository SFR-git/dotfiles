" Plugins
call plug#begin('~/.vim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'yggdroot/indentline'
Plug 'preservim/nerdtree'
Plug 'vim-syntastic/syntastic'
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }
Plug 'jbgutierrez/vim-better-comments'
Plug 'tpope/vim-commentary'
Plug 'turbio/bracey.vim', {'do': 'npm install --prefix server'}
Plug 'ryanoasis/vim-devicons'
Plug 'kkoomen/vim-doge', { 'do': { -> doge#install() } }

"" Git
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
Plug 'tveskag/nvim-blame-line'
Plug 'itchyny/vim-gitbranch'

"" Languages
Plug 'gabrielelana/vim-markdown' 
Plug 'artur-shaik/vim-javacomplete2'
Plug 'jelera/vim-javascript-syntax'
Plug 'deoplete-plugins/deoplete-jedi'
Plug 'hdima/python-syntax'
Plug 'tell-k/vim-autopep8'

"" Colorschemes
Plug 'tyrannicaltoucan/vim-quantum'

call plug#end()

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
set updatetime=100
set background=dark
set termguicolors

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

" DoGe comment
nnoremap dg :DogeGenerate<CR>

" Colorscheme
let g:quantum_black=1
let g:quantum_italics=1
colorscheme quantum

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

" Syntastic
let g:syntastic_mode_map = { "mode": "active" }
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_enable_signs = 1
let g:syntastic_javascript_checkers = ['eslint']
autocmd BufWritePre,BufEnter * SyntasticCheck

" Lightline
let g:lightline = {
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'filename', 'gitbranch', 'readonly', 'modified' ] ]
    \ },
    \ 'component_function': {
    \   'gitbranch': 'Branch'
    \ },
    \ 'colorscheme': 'quantum',
    \ }
function! Branch()
    return gitbranch#name() != '' ? ' ' . gitbranch#name() : ''
endfunction

" Better Comments
highlight ErrorBetterComments guifg=#dd7186
highlight QuestionBetterComments guifg=#70ace5
highlight TodoBetterComments guifg=#d7956e
highlight HighlightBetterComments guifg=#a48add
highlight StrikeoutBetterComments guifg=#6a6c6c

" Markdown
let g:markdown_enable_spell_checking = 0

" Prettier
let g:prettier#config#tab_width = '4'
let g:prettier#autoformat = 0
let g:prettier#config#trailing_comma = 'all'
let g:prettier#config#single_quote = 'yes'
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync

" Java completion
autocmd FileType java setlocal omnifunc=javacomplete#Complete
autocmd FileType java JCEnable

" Blamer
autocmd BufEnter * EnableBlameLine
let g:blameLineGitFormat = '%an, %ar - %s'

" NERDTree Auto-refresh
autocmd BufWritePost * NERDTreeRefreshRoot

" NERDTree Git
let g:NERDTreeGitStatusConcealBrackets = 1
let g:NERDTreeGitStatusIndicatorMapCustom = {
    \ 'Modified'  :'',
    \ 'Staged'    :'',
    \ 'Untracked' :'',
    \ 'Renamed'   :'',
    \ 'Unmerged'  :'',
    \ 'Deleted'   :'',
    \ 'Dirty'     :'',
    \ 'Ignored'   :'',
    \ 'Clean'     :'*',
    \ 'Unknown'   :'?',
    \ }
highlight GitGutterAdd    guifg=#009900 ctermfg=2
highlight GitGutterChange guifg=#bbbb00 ctermfg=3
highlight GitGutterDelete guifg=#ff2222 ctermfg=1

" Open NERDTree and terminal on start

if $nosplit != 1
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
        \ execute "wincmd k" |
        \ execute "set signcolumn=yes"
endif

" Autoclose if only NERDTree and terminal remain
for i in [1, 2]
    autocmd bufenter * if (winnr("$") <= 2 && (exists("b:NERDTree") || &buftype ==# 'terminal')) | q | endif
endfor

" .cutter -> .yml
autocmd BufRead,BufNewFile *.cutter set filetype=yaml

" Autopep8
let g:autopep8_on_save = 1
let g:autopep8_disable_show_diff = 1
