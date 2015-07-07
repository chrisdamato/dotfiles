if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
let &cpo=s:cpo_save
unlet s:cpo_save
set backspace=indent,eol,start
set fileencodings=utf-8
set ffs=unix,dos,mac
set helplang=en
set history=500
set ruler
set viminfo='20,\"50
" vim: set ft=vim :

" tabs and matters tab and space
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent
set smarttab

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

if has("gui_running")
  set guioptions=acegim
  set t_Co=256
  set background=dark
  colorscheme molokai
  set nonu
endif


" new ideas from
" http://vimdoc.sourceforge.net/htmldoc/usr_05.html#vimrc_example.vim 
"
" Display the match for a search pattern when halfway typing it
set incsearch
set ignorecase
set smartcase 
set nohlsearch
set magic

nnoremap Q <Nop>

" new ideas from http://amix.dk/vim/vimrc.html 
" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread
" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk
" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
" Remember info about open buffers on close
set viminfo^=%

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap VIM 0 to first non-blank character
map 0 ^

" Move a line of text using ALT+[jk] or Comamnd+[jk] on mac
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

if has("mac") || has("macunix")
  nmap <D-j> <M-j>
  nmap <D-k> <M-k>
  vmap <D-j> <M-j>
  vmap <D-k> <M-k>
endif

