execute pathogen#infect()

" term fix
set ttimeoutlen=0
" midclick paste through ssh works when in input
set mouse=nv

nnoremap    <space>         <nop>
let mapleader = " "
nnoremap    <c-n>           :bn<cr>
nnoremap    <c-p>           :bp<cr>
nnoremap    <leader>q       :bd<cr>
nnoremap    <leader>l       :lopen<cr>
nnoremap    <leader><space> gg=G''zz
nnoremap    Q               <nop>
nnoremap    <leader>ht      :GhcModType<cr>
nnoremap    <leader>hc      :GhcModTypeClear<cr>
nnoremap    <leader>hk      :GhcModCheck<cr>
nnoremap    <leader>hl      :GhcModLint<cr>

set encoding=utf8
set spelllang=sv
command S set spell!
command SV set spell spelllang=sv | syntax spell toplevel
command EN set spell spelllang=en | syntax spell toplevel

set undofile
set backspace=indent,eol,start
set history=100
set hidden
set clipboard=unnamed

set dir=/tmp
set backupdir=/tmp
set undodir=/tmp

syntax on
filetype plugin indent on
let g:tex_flavor="latex"

set guioptions=c
set showcmd
set ruler
set wildmenu
set wrap linebreak breakindent showbreak=>\ "
set foldmethod=marker
set fillchars=

set incsearch
set ignorecase
set smartcase

set autoindent
set ts=2 sw=2 et
set smarttab

set complete+=d
set completeopt+=menuone
inoremap <c-f> <space><bs><c-x><c-o>
inoremap <c-@> <c-n>

command ERC e ~/.vimrc
command -nargs=1 PAD %s/$/\=repeat(' ',<args>-virtcol('$'))
command RTW %s/\s\+$//

" c++ completes, Syntastic & Marching
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_enable_balloons = 0
command -nargs=1 FLAG let g:syntastic_cpp_compiler_options .= " " . <args> | let g:marching#clang_command#options["cpp"] .= " " . <args>

let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_compiler_options = "-Wall -Wextra -pedantic -std=c++14"
let g:marching#clang_command#options = {"cpp" : "-std=c++14"}

" Ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"

" Unite
nnoremap    <leader>p       :Unite register<cr>
nnoremap    <leader>g       :Unite grep<cr>
nnoremap    <leader>f       :Unite file<cr>
nnoremap    <leader>b       :Unite buffer<cr>
nnoremap    <leader><tab>   :Unite line:buffers<cr>


" Windows
if has('win32') || has('win64')
    command ERC e ~/_vimrc
    set guifont=Dina:h6
    set dir=%TMP%
    set backupdir=%TMP%
    set undodir=%TMP%
endif
