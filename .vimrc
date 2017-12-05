syntax on
set autoindent
set expandtab
set tabstop=2
set shiftwidth=2
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

""" powerline
python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup
set laststatus=2
set showtabline=2
set noshowmode

" SpaceにLeaderを割り当てる
let mapleader = "\<Space>"

" C-gをEscに設定
nmap <C-g> <Esc>
imap <C-g> <Esc>

imap <C-f> <RIGHT>
imap <C-b> <LEFT>

"""""""""""""""""""""""""
" Plug
"""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

" fzf
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" vim-go
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }

" typescript
Plug 'leafgarland/typescript-vim'

Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-surround'
" Plug 'Shougo/neosnippet'
" Plug 'Shougo/neosnippet-snippets'
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-fugitive'
Plug 'Lokaltog/vim-easymotion'

" colorscheme
Plug 'nanotech/jellybeans.vim'

call plug#end()

" fzf
set rtp+=/usr/local/opt/fzf
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>x :Commands<CR>
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>g :GFiles<CR>
nnoremap <Leader>a :Ag<CR>
nnoremap <Leader>k :bd<CR>
command! FZFMru call fzf#run({
      \  'source': v:oldfiles,
      \  'sink': 'e',
      \  'options': '-m -x +s',
      \  'down': '40%'})
nnoremap <Leader>r :FZFMru<CR>

colorscheme jellybeans

" utilsnipets
let g:UltiSnipsSnippetDirectories=["UltiSnips", "mycoolsnippets"]

" " neosnippet
" imap <C-k> <Plug>(neosnippet_expand_or_jump)
" smap <C-k> <Plug>(neosnippet_expand_or_jump)
" xmap <C-k> <Plug>(neosnippet_expand_target)
"
" smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
" if has('conceal')
"   set conceallevel=2 concealcursor=niv
" endif
"
" let g:neosnippet#enable_snipmate_compatibility=1
" let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'

" easymotion
map <Leader>o <Plug>(easymotion-bd-f)
nmap <Leader>o <Plug>(easymotion-overwin-f)
nmap s <Plug>(easymotion-overwin-f2)
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)
map <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

" vim-go
let g:go_fold_enable = ['block', 'import', 'varconst', 'package_comment']
let g:go_highlight_array_whitespace_error=1
let g:go_highlight_extra_types=1
let g:go_highlight_space_tab_error=1
let g:go_highlight_trailing_whitespace_error = 1
let g:go_highlight_operators=1
let g:go_highlight_functions=1
let g:go_highlight_methods=1
let g:go_highlight_types=1
let g:go_highlight_fields=1
let g:go_highlight_structs=1
let g:go_highlight_generate_tags=1
let g:go_highlight_build_constraints=1
let g:go_highlight_variable_declarations=1
let g:go_highlight_variable_assignments=1

let g:go_decls_includes = 'func,type'
let g:go_decls_mode = 'func,type'

" go highlight
autocmd FileType go :highlight goErr cterm=bold ctermfg=214
autocmd FileType go :match goErr /\<err\>/
