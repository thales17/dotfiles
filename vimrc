syntax on

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
"------------------
Plugin 'VundleVim/Vundle.vim'
Plugin 'python/black'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'vim-syntastic/syntastic'
Plugin 'scrooloose/nerdtree'
Plugin 'embear/vim-uncrustify'
Plugin 'fatih/vim-go'
Plugin 'rust-lang/rust.vim'
"------------------
call vundle#end()
filetype plugin indent on

set noswapfile
set tabstop=2
set shiftwidth=2
set backspace=indent,eol,start

let NERDTreeShowHidden=1
let g:syntastic_python_python_exec = '/usr/bin/python3'
let g:syntastic_python_checkers=['python3']
let g:uncrustify_config_file = '~/.uncrustify.cfg'
let g:vim_markdown_folding_disabled = 1
let g:go_fmt_command = 'goimports'
let g:rustfmt_autosave = 1

autocmd Filetype c setlocal noexpandtab tabstop=8 softtabstop=8 shiftwidth=8
autocmd Filetype go setlocal noexpandtab tabstop=4 softtabstop=4 shiftwidth=4
autocmd BufWritePre *.py execute ':Black'
autocmd Filetype c autocmd BufWritePre <buffer> call Uncrustify()

map <silent> <C-n> :NERDTreeToggle<CR>
map <silent> <C-x> :vert term<CR>

set background=dark