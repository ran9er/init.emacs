%% -*- encoding: utf-8-unix; -*-
%% File-name:    <myclass.cls>
%% Create:       <2011-11-09 17:11:35 ran9er>
%% Time-stamp:   <2011-11-09 17:11:40 ran9er>

\ProvidesClass{myclass}
\LoadClass[11pt,twoside,b5paper,titlepage]{article}
% \RequirePackage{mystyle}  \usepackage

% ** 中文断行
\XeTeXlinebreaklocale "zh"
\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
\RequirePackage{indentfirst}

% ** 行距
\linespread{1.2}

% ** 页边距
\RequirePackage[top=1in,bottom=1in,left=1in,right=0.8in]{geometry}

% ** 章节标题
\RequirePackage[center,pagestyles]{titlesec}
%\titleformat{command}[shape]{format}{label}{sep}{before}[after]
\titleformat{\section}{\centering\Large\bfseries}{\thesection}{1em}{}
\titleformat{\subsection}{\large\bfseries}{\thesubsection}{1em}{}

% ** 页眉 页脚
\newpagestyle{main}{
\sethead{\small\thesection\quad\sectiontitle}{}{$\cdot$~\thepage~$\cdot$}
\setfoot{}{}{}\headrule}
\pagestyle{main}

% ** 链接
\RequirePackage[%
    pdfstartview=FitH,%
    CJKbookmarks=true,%
    bookmarks=true,%
    bookmarksnumbered=true,%
    bookmarksopen=true,%
    colorlinks=true,%
    citecolor=blue,%
    linkcolor=blue,%
    anchorcolor=green,%
    urlcolor=blue%
]{hyperref}

% ** 目录
\RequirePackage{titletoc}
\titlecontents{chapter}[0pt]{\vspace{.5\baselineskip}\bfseries}
    {第\CJKnumber{\thecontentslabel}章\quad}{}
    {\hspace{.5em}\titlerule*[10pt]{$\cdot$}\contentspage}
\titlecontents{section}[0em]{\vspace{.25\baselineskip}}
    {\thecontentslabel\quad}{}
    {\hspace{.5em}\titlerule*[10pt]{$\cdot$}\contentspage}

% ** 脚注
% 每页重新计数
\makeatletter
\@addtoreset{footnote}{page}
\makeatother
% 符号脚注
%\renewcommand{\thefootnote}{\fnsymbol{footnote}}

% ** 字体
\RequirePackage{fontspec}
% \setmainfont[BoldFont=SimHei,ItalicFont=SimHei]{SimSun}
% \setsansfont[BoldFont=SimHei]{SimHei}
% \setmonofont{NSimSun}
\setmainfont[BoldFont=Adobe Song Std]{Adobe Song Std}
%\setsansfont[BoldFont=Adobe Heiti Std]{AR PL KaitiM GB}
%\setmonofont{BitstreamVera Sans Mono}

%\defaultfontfeatures{Mapping=tex-text}
\RequirePackage{xeCJK}
\punctstyle{kaiming} %标点压缩风格

% **xltxtra
\RequirePackage{xltxtra}

\RequirePackage{color,xcolor}
%%%%%%%%%%%%%%%%%%%%%%%%%listings宏包粘贴源码%%%%%%%%%%%%%%%%%%%%
\RequirePackage{listings}            % 方便粘贴源代码，部分代码高亮功能
\lstloadlanguages{Lisp}              % 所要粘贴代码的编程语言
\lstset{
%  language=Lisp,
  basicstyle=\small,
  numbers=left,
  keywordstyle=\color{blue},
  numberstyle={\tiny\color{gray}},
  stepnumber=1, %行号会逐行往上递增
  numbersep=5pt,
  commentstyle=\small\color{red},
  backgroundcolor=\color[rgb]{0.95,1.0,1.0},
  extendedchars=true,
  escapechar=\%,
  texcl=true,
  showstringspaces=true,
  showspaces=false,
  showtabs=false,
%  frame=shadowbox, framexleftmargin=5mm, rulesepcolor=\color{red!20!green!20!blue!20!},
  frame=leftline,
% frame=single,
%  TABframe=single,
  tabsize=4,
  breaklines=tr,
  extendedchars=false %这一条命令可以解决代码跨页时，章节标题，页眉等汉字不显示的问题
}

%%%%数学公式定理的重定义%%%%
\newtheorem{example}{例}                                   % 整体编号
\newtheorem{algorithm}{算法}
\newtheorem{theorem}{定理}[section]                         % 按 section 编号
\newtheorem{definition}{定义}
\newtheorem{axiom}{公理}
\newtheorem{property}{性质}
\newtheorem{proposition}{命题}
\newtheorem{lemma}{引理}
\newtheorem{corollary}{推论}
\newtheorem{remark}{注解}
\newtheorem{condition}{条件}
\newtheorem{conclusion}{结论}
\newtheorem{assumption}{假设}

%%%%章节等名称重定义%%%%
\renewcommand{\contentsname}{目录}     
\renewcommand{\indexname}{索引}
\renewcommand{\listfigurename}{插图目录}
\renewcommand{\listtablename}{表格目录}
\renewcommand{\figurename}{图}
\renewcommand{\tablename}{表}
\renewcommand{\appendixname}{附录}

\RequirePackage{mycmd}
