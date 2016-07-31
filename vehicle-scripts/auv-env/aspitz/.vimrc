set tabstop=2
set shiftwidth=2
set expandtab
set ru

set smartindent
syn on

imap <tab> <esc>

" Switch between header and source files
map <F4> :e %:p:s,.h$,X,:s,.cpp$,.h,:s,X$,.cpp,<CR>

" Persistent undos
set undodir=/home/software/.vim/undo
set undofile

set relativenumber

" Detect glsl files.
autocmd! BufNewFile,BufRead *.glsl,*.geom,*.vert,*.frag,*.gsh,*.vsh,*.fsh,*.vs,*.fs set filetype=glsl

au BufNewFile,BufRead *.cpp,*.h set syntax=cpp11
au BufNewFile,BufRead *.ino,*.pde set filetype=arduino
au BufNewFile,BufRead *.scala set filetype=scala
au BufNewFile,BufRead *.xi set filetype=xi

color desert

abbr teh the
abbr funciton function

set backspace=indent,eol,start

" Collaborative vim!
"set noswapfile
"set updatetime=500
"autocmd CursorHold * call Timer()
"function! Timer() 
"  call feedkeys("f\e")
"  :e
"endfunction
"
"au InsertLeave <buffer> update
"au CursorHold <buffer> update
"
"function! WriteFile()
"  if &buftype == ""
"    write
"  endif
"  return '%f %h%w%m%r%=%-14(%l,%c%V%) %(%P%)'
"endfunction
"setlocal statusline=%!WriteFile()
"set laststatus=2
