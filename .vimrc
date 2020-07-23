syntax on
set autoindent
set expandtab
set tabstop=4
set shiftwidth=4
" set cursorline
set number
set noswapfile
set backspace=2
set undofile
set undodir=~/.vim/undo
set hlsearch
set hidden
set incsearch
set ignorecase
set smartcase
" set t_Co=254
set browsedir=buffer
set clipboard=unnamed
set history=1000
set cot-=preview

" statusline
set statusline=%F
set statusline+=%m
set statusline+=%r
set statusline+=%h
set statusline+=%w
set statusline+=%=
set statusline+=%{fugitive#statusline()}
set statusline+=[ENC=%{&fileencoding}]
set statusline+=[LOW=%l/%L]
