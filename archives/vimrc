"运行在非兼容(VI)模式下(命令模式下 TAB 补全){{{
set nocompatible
"}}}

"设定文件编码{{{
"内部编码
set encoding=utf-8
"默认保存编码
set fileencoding=utf-8
"识别编码
set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
"正确处理中文折行
set formatoptions+=mM
"将不明宽度的字符作为双字节
set ambiwidth=double
"以下字符将被视为单词的一部分 (ASCII)：
set iskeyword+=33-47,58-64,91-96,123-128
"插入模式下，“←”如何删除光标前的字符：行首空白、换行符、插入点之前的字符
set backspace=indent,eol,start
"}}}

"字体{{{
if has("win32")
    set guifont=Yahei_Consolas_Hybrid:h12
"    set guifontwide=Yahei_Consolas_Hybrid:h12
endif
"}}}

"状态栏样式 {{{
set laststatus=2
  function! CurDir()
     let curdir = substitute(getcwd(), '/Users/amir/', "~/", "g")
     return curdir
  endfunction
set statusline=\ %F%m%r%h\ [%{(&fenc==\"\")?&enc:&fenc}%{(&bomb?\",BOM\":\"\")}]\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:\ %l/%L:%c
"在状态栏位置显示补全菜单
set wildmenu
"}}}

"UI选项{{{ 
"简洁启动模式
set shortmess=atI
"设定 GUI 选项
"set guioptions=gmrLtT  m:菜单 T:工具栏 r:滚动条
set guioptions=gLt
"命令行高度
set cmdheight=1
"显示标尺
set ruler
"}}}

"缓冲区选项{{{ 
"开启语法加亮
syntax on
"配色风格
colorscheme desert
"显示不完整的段落
set display=lastline
"自动折行
"set nowrap
set wrap
"按完整单词折行
set nolinebreak
"set linebreak
"设定行距 ( GUI 界面中生效 )
set linespace=4
"行宽（输入时自动插入换行符）
"set textwidth=80
set textwidth=0
"命令行历史纪录
set history=500
"粘贴模式
"set paste "set nopaste
"}}}

"缩进选项{{{
"设定制表符宽度
set tabstop=4
"设定 Tab 缩进的空格数
set shiftwidth=4
"将缩进转换为空格
set expandtab
"设定自动缩进(新行与前一行缩进相同)
"set autoindent
set noautoindent
"}}}

"搜索选项{{{ 
"禁用增量搜索
set incsearch
"set noincsearch
"搜索时忽略大小写
set ignorecase
"set noignorecase
"高亮显示搜索结果
set hlsearch
"}}}

"虚空间操作{{{
"允许所有模式下的虚空间编辑 （虚空间:不包含任何文本的空间。如换行符之后）
"set virtualedit=all
"禁止在虚空间编辑
"set virtualedit=
"换行符后一格作为虚空间
set virtualedit=onemore,block
"}}}

"设定折叠方式{{{
"set foldmethod=manual
set foldmethod=marker
"}}}

"按键选项{{{
"使用 Space 翻页
noremap <Space> <PageDown>
"j k 在屏幕行间上下移动；gj gk 在物理行间上下移动 
noremap j gj
noremap k gk
noremap gj j
noremap gk k
"}}}

"记忆最后编辑状态{{{
au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif
set viminfo='1000,f1,<500
"}}}

"Windows全屏{{{
if has('gui_running') && has("win32")
    map <F11> :call libcallnr("gvimfullscreen.dll", "ToggleFullScreen", 0)<CR>
endif

chdir $VIM/..
"}}}

"vimim设置{{{
":let g:vimim_tab_as_onekey=1 	                    "Tab键为点石成金
:let g:vimim_one_row_menu=1 	                    "缺省：菜单横排
":let g:vimim_custom_color=1 	                    "缺省：VimIM颜色
":let g:vimim_custom_color=0 	                    "关闭菜单颜色选项
":let g:vimim_chinese_input_mode='static' 	        "中文静态输入模式
":let g:vimim_toggle_list='english,wubi,pinyin' 	"设定循环先后次序
":let g:vimim_plugin_folder=0 	                    "缺省是Vim plugin
":let g:vimim_chinese_punctuation=1 	            "缺省：全角标点
""""mapping 选项
":let g:vimim_ctrl_space_to_toggle=1 	            "使用 ctrl+space 开关输入法
":let g:vimim_ctrl_space_to_toggle=2 	            "使用 ctrl+space 切换输入法
":let g:vimim_ctrl_space_to_toggle=3 	            "使用 ctrl+space 点石成金
:let g:vimim_ctrl_h_to_toggle=1 	                "使用 ctrl+h 开关输入法
":let g:vimim_ctrl_h_to_toggle=2 	                "使用 ctrl+h 切换输入法
":let g:vimim_ctrl_h_to_toggle=3 	                "使用 ctrl+h 点石成金
""""双拼选项
":let g:vimim_shuangpin=0 	                        " 缺省无双拼
":let g:vimim_shuangpin='abc' 	                    " 双拼：智能ABC
":let g:vimim_shuangpin='ms' 	                    " 双拼：微软
:let g:vimim_shuangpin='nature' 	                " 双拼：自然码
":let g:vimim_shuangpin='plusplus' 	                " 双拼：拼音加加
":let g:vimim_shuangpin='purple' 	                " 双拼：紫光
":let g:vimim_shuangpin='flypy' 	                " 双拼：小鹤
""""云输入选项
":let g:vimim_cloud='baidu' 	                    " 缺省：百度云输入
":let g:vimim_cloud='google' 	                    " 谷歌云输入
":let g:vimim_cloud='sogou' 	                    " 搜狗云输入
":let g:vimim_cloud='qq' 	                        " QQ云输入
":let g:vimim_cloud='sogou.dynamic' 	            " 搜狗纯云输入，动态
":let g:vimim_cloud='sogou.static' 	                " 搜狗纯云输入，静态
":let g:vimim_cloud='qq.fanti' 	                    " QQ云输入，开启繁体
":let g:vimim_cloud='qq.mixture' 	                " QQ云输入，开启混合模式
":let g:vimim_cloud='qq.fuzzy' 	                    " QQ云输入，开启模糊音
":let g:vimim_cloud='qq.wubi' 	                    " QQ云输入，五笔
":let g:vimim_cloud='qq.wubi.dynamic' 	            " QQ纯云输入，五笔，动态
":let g:vimim_cloud='qq.shuangpin.abc' 	            " QQ云输入，双拼智能ABC
":let g:vimim_cloud='qq.shuangpin.ms' 	            " QQ云输入，双拼微软拼音2003
":let g:vimim_cloud='qq.shuangpin.plusplus' 	    " QQ云输入，双拼拼音加加
":let g:vimim_cloud='qq.shuangpin.purple' 	        " QQ云输入，双拼紫光拼音
":let g:vimim_cloud='qq.shuangpin.flypy' 	        " QQ云输入，双拼小鹤双拼
":let g:vimim_cloud='qq.shuangpin.nature' 	        " QQ云输入，双拼自然码
:let g:vimim_cloud=-1 	                            " 彻底关闭云输入
""""自己的云样本URL
:let g:vimim_mycloud = 0                            " 缺省关闭
":let g:vimim_mycloud = "dll:/data/libvimim.so:192.168.0.1"
":let g:vimim_mycloud = "dll:/home/im/plugin/libmyplugin.so:arg:func"
":let g:vimim_mycloud = "dll:".$HOME."/plugin/libvimim.so"
":let g:vimim_mycloud = "dll:".$HOME."/plugin/cygvimim.dll"
":let g:vimim_mycloud = "app:".$VIM."/src/mycloud/mycloud"
":let g:vimim_mycloud = "app:python d:/mycloud/mycloud.py"
":let g:vimim_mycloud = "http://pim-cloud.appspot.com/ms/"
":let g:vimim_mycloud = "http://pim-cloud.appspot.com/abc/"
":let g:vimim_mycloud = "http://pim-cloud.appspot.com/qp/"
"}}}

""python block{{{
"python << EOF
"import vim
"def SetBreakpoint():
"    import re
"
"    nLine = int( vim.eval( 'line(".")'))
"
"    strLine = vim.current.line
"    strWhite = re.search( '^(\s*)', strLine).group(1)
"
"    vim.current.buffer.append(
"       "%(space)spdb.set_trace() %(mark)s Breakpoint %(mark)s" %
"         {'space':strWhite, 'mark': '#' * 30}, nLine - 1)
"
"    for strLine in vim.current.buffer:
"        if strLine == "import pdb":
"            break
"    else:
"        vim.current.buffer.append( 'import pdb', 0)
"        vim.command( 'normal j1')
"
"vim.command( 'map <f7> :py SetBreakpoint()<cr>')
"
"def RemoveBreakpoints():
"    import re
"    nCurrentLine = int( vim.eval( 'line(".")'))
"    nLines = []
"    nLine = 1
"    for strLine in vim.current.buffer:
"        if strLine == 'import pdb' or strLine.lstrip()[:15] == 'pdb.set_trace()':
"            nLines.append( nLine)
"        nLine += 1
"    nLines.reverse()
"    for nLine in nLines:
"        vim.command( 'normal %dG' % nLine)
"        vim.command( 'normal dd')
"        if nLine < nCurrentLine:
"            nCurrentLine -= 1
"    vim.command( 'normal %dG' % nCurrentLine)
"vim.command( 'map <s-f7> :py RemoveBreakpoints()<cr>')
"
"def RunDebugger():
"    vim.command( 'wall')
"    strFile = vim.eval( "g:mainfile")
"    vim.command( "!start python -m pdb %s" % strFile)
"vim.command( 'map <s-f12> :py RunDebugger()<cr>')
"EOF
"}}}

"" END OF FILE """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim:filetype=vim foldmethod=marker autoindent expandtab shiftwidth=4
