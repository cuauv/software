" CUAUV Software Vim Configuration
" A minimal but convenient Vim configuration for viewing and quick editing.
"
" Tips:
" - Use gcc to comment the current line, or gc to comment the visual selection

filetype plugin indent on

"
" Options
"

" Spaces
set tabstop=4       " Number of visual spaces per TAB
set softtabstop=4   " Number of spaces in tab when editing
set shiftwidth=4    " Number of spaces to use for autoindent
set expandtab       " Tabs are space
set autoindent
set copyindent      " Copy indent from the previous line

" UI
set termguicolors   " Enable true-color colorscheme support
set wildmenu        " Visual autocomplete for command menu
set cursorline	    " Highlight current line
set mouse=a         " Enable selecting with mouse
set splitbelow      " Open horizontal splits below current split
set splitright      " Open vertical splits to the right of current split
set hidden          " Okay to background modified buffers
set laststatus=2    " Window will always have a status line
set scrolloff=4	    " Leave lines visible at top and bottom of buffer
set noshowmode      " Annoying mode display, the cursor shows which mode we're in
set ttimeoutlen=0   " Respond to escape immediately

" Searching
set ignorecase      " Case-insensitive
set smartcase       " Override ignorecase if search includes capital letters
set nohlsearch      " Don't highlight search after search is completed
set gdefault        " When using :s command, replace all instances on line by default

set clipboard=unnamedplus

" Swap/backup/undo
set noswapfile      " Disable concurrent editing warning, Vim warns when saving a modified file anyway
set undofile        " Enable persistent undo
let g:netrw_home='~/.local/share/nvim'  " Don't store history in vim config dir
 
" Load colorscheme
set background=dark
colorscheme space-vim-dark
syntax enable			" Enable syntax processing

"
" Autocmd
"

" Jump to last cursor position in file
function! SetCursorPosition()
  if &filetype !~ 'svn\|commit\c'
    if line("'\"") > 0 && line("'\"") <= line("$") |
      execute 'normal! g`"zvzz' |
    endif
  end
endfunction

augroup restore_cursor
	autocmd!
	autocmd BufReadPost * call SetCursorPosition()
augroup END

" vim:shiftwidth=2
