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
set nohlsearch
set ignorecase
set smartcase

set backspace=
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

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_enable_balloons = 0
let g:syntastic_enable_signs = 0
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_compiler_options = "-Wall -Wextra -pedantic -std=c++14"

let g:marching#clang_command#options = {"cpp" : "-std=c++14"}
command -nargs=1 FLAG let g:syntastic_cpp_compiler_options .= " " . <args> | let g:marching#clang_command#options["cpp"] .= " " . <args>

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
