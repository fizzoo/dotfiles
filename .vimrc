" För kompileringen, i src. Ta bort gobj mappen om man vill ändra något, objfiler däri som inte vill.
" ./configure --enable-pythoninterp --with-python-config-dir=/mingw64/lib/python2.7/config
" make -f Make_ming.mak ARCH=x86-64 PYTHON=/c/programs/python2 DYNAMIC_PYTHON=yes PYTHON_VER=27
execute pathogen#infect()

" term fix
set ttimeoutlen=0

" div maps, mapleader behöver vara tidig
nnoremap    <space>         <nop>
let mapleader = " "
nnoremap    <c-n>           :bn<cr>
nnoremap    <c-p>           :bp<cr>
nnoremap    <leader>q       :bd<cr>
nnoremap    <leader>l       :lopen<cr>
nnoremap    <leader>t       / $<cr>
nnoremap    <leader><space> gg=G''zz
nnoremap    Q               <nop>
nnoremap    <leader>ht      :GhcModType<cr>
nnoremap    <leader>hc      :GhcModTypeClear<cr>
nnoremap    <leader>hk      :GhcModCheck<cr>
nnoremap    <leader>hl      :GhcModLint<cr>

" språk
set encoding=utf8
set spelllang=sv
command S set spell!
command SV set spell spelllang=sv | syntax spell toplevel
command EN set spell spelllang=en | syntax spell toplevel

" minne
set undofile
set backspace=indent,eol,start
set history=100
set hidden
set clipboard=unnamed

" tempsaker
set dir=/tmp
set backupdir=/tmp
set undodir=/tmp

" syntax
set list listchars=tab:>-,trail:·,nbsp:%
syntax on
filetype plugin indent on
let g:tex_flavor="latex"

" användbara/fina prylar
set guioptions=c
set showcmd
set ruler
set wildmenu
set wrap linebreak breakindent showbreak=>\ "
set foldmethod=marker
set fillchars=

" sökning
set incsearch
set ignorecase
set smartcase

" indentering
set autoindent
set shiftwidth=4
set tabstop=4
set softtabstop=4
set smarttab
set expandtab

" completion
set complete+=d
set completeopt+=menuone
inoremap <c-f> <space><bs><c-x><c-o>
inoremap <c-@> <c-n>

" c++ completes, Syntastic & Marching
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_enable_balloons = 0
command -nargs=1 FLAG let g:syntastic_cpp_compiler_options .= " " . <args> | let g:marching#clang_command#options["cpp"] .= " " . <args>

let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_compiler_options = "-Wall -Wextra -pedantic -std=c++14"
let g:marching#clang_command#options = {"cpp" : "-std=c++14"}

" diverse commands
command ERC e ~/.vimrc
command -nargs=1 PAD %s/$/\=repeat(' ',<args>-virtcol('$'))
command RTW %s/\s\+$//

" Bufmessage för att få ut i text det vim skriver
function! BUFFERMESSAGE(msgcmd)
    redir => message
    silent execute a:msgcmd
    redir END
    silent put=message
endfunction
command! -nargs=+ -complete=command BUFFERMESSAGE call BUFFERMESSAGE(<q-args>)

" binär (vim -b) (körs som default nu på .bin)
augroup Binary
    au!
    au BufReadPre  *.bin let &bin=1
    au BufReadPost *.bin if &bin | %!xxd
    au BufReadPost *.bin set ft=xxd | endif
    au BufWritePre *.bin if &bin | %!xxd -r
    au BufWritePre *.bin endif
    au BufWritePost *.bin if &bin | %!xxd
    au BufWritePost *.bin set nomod | endif
augroup END

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
