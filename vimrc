if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
let &cpo=s:cpo_save
unlet s:cpo_save
set backspace=indent,eol,start
set fileencodings=ucs-bom,utf-8,latin1
set guicursor=n-v-c:block,o:hor50,i-ci:hor15,r-cr:hor30,sm:block,a:blinkon0
set helplang=en
set history=50
set nohlsearch
set ruler
set viminfo='20,\"50
" vim: set ft=vim :
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent
" Display an incomplete command in the lower right
set showcmd
" filename in terminal window title
set title
" syntax highlighting on
syntax on
if $COLORTERM == 'gnome-terminal'
    set t_Co=256
    colorscheme molokai
    endif

" new ideas from
" http://vimdoc.sourceforge.net/htmldoc/usr_05.html#vimrc_example.vim 
"
" Display the match for a search pattern when halfway typing it
set incsearch
set smartcase 
nnoremap Q <Nop>
