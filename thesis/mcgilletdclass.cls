%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE: mcgilletdclass.cls 
%
% Version 0.6 2005.02.21
% 	Modified by Qing
% 	1. fixed set title problem (/maketitle)
%	2. fixed the ETDAppendix
%	3. TexShade has compatible problems with Ragged package
%      \RaggedRight is not compatible with Texshade
%      Added a new option: TexShade for texshade
%      By default: no texshade
% 
% Version 0.5 2005.02.15
%   Modified by Qing 
%   Added two commands \TableCaptionOpt,
%          and \FigureCaptionOpt
%
% Version 0.4 2005.01.25
%	Modified by Qing Zou
%   Index 
%
% Version 0.3 2005.01.25
%
% Version 0.2 - 2005.01.13
%   Modified by Qing Zou
%   \RequirePackage{}[Date...]
%
%--------------------------------------
% Version 0.1 - 2004.12.01
%	Created by Qing (Jason) Zou 
%	Based on the ufthesis dcoumentclass template developed by
%       Ron Smith of the University of Florida and distributed under 
%       the LPPL license (see http://www.latex-project.org/lppl.txt)
%--------------------------------------
% Digital Collection Program
% McGill University
% =====================================
% 
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\NeedsTeXFormat{LaTeX2e}[2000/06/01]
\ProvidesClass{mcgilletdclass}[2004/12/01 v0.1 McGill University Thesis Class]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Defining Class options and loading required packages        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage{ifthen}[1996/08/02 v1.0m Standard LaTeX ifthen package (DPC)]

%% Declaring Options \& Initializing Flags
\newcommand{\QZ@ptsize}{}%%define a null command for font-size
\newcounter{QZ@ptcnt}%%define a new null counter

\newboolean{QZ@texshade}%
\setboolean{QZ@texshade}{false}
\DeclareOption{TexShade}{\setboolean{QZ@texshade}{true}}

\DeclareOption{10pt}{\renewcommand{\QZ@ptsize}{10pt}%
\setcounter{QZ@ptcnt}{10}}%

\DeclareOption{11pt}{\renewcommand{\QZ@ptsize}{11pt}%
\setcounter{QZ@ptcnt}{11}}%

\DeclareOption{12pt}{\renewcommand{\QZ@ptsize}{12pt}%
\setcounter{QZ@ptcnt}{12}}%

\newboolean{QZ@TOCHang}%
\setboolean{QZ@TOCHang}{true}%
\DeclareOption{NoTocHang}{\setboolean{QZ@TOCHang}{false}}

\newboolean{QZ@BfSet}%
\setboolean{QZ@BfSet}{false}%
\DeclareOption{Bold}{\setboolean{QZ@BfSet}{true}}

\newboolean{QZ@CPage}%
\setboolean{QZ@CPage}{false}%
\DeclareOption{CPage}{\setboolean{QZ@CPage}{true}}

\newboolean{QZ@Draft}%
\setboolean{QZ@Draft}{false}%
\DeclareOption{Draft}{\setboolean{QZ@Draft}{true}}%

\newboolean{QZ@Justify}%
\setboolean{QZ@Justify}{false}%
\DeclareOption{Justify}{\setboolean{QZ@Justify}{true}}%

\newboolean{QZ@nopenalties}%
\setboolean{QZ@nopenalties}{true}%
\DeclareOption{nopenalties}{\setboolean{QZ@nopenalties}{false}}%

%%==============================================================%%
%% Executing Options \& Loading Required Packages               %% 
%%--------------------------------------------------------------%%
\DeclareOption*{\OptionNotUsed}

\ExecuteOptions{12pt,letterpaper}
\ProcessOptions\relax
\LoadClass[\QZ@ptsize]{report}[2000/05/19 v1.4b Standard LaTeX document class]

\typeout{ }
\typeout{*****************************************}
\typeout{* Updating commands for Thesis Template *}
\typeout{*****************************************}

%%============================================================================
%% Including a package for line spacing
%%----------------------------------------------------------------------------
\RequirePackage[nodisplayskipstretch]{setspace}[2000/12/01 6.7 Contributed and Supported LaTeX2e package]%

%%============================================================================
%% Including a package for underline 
%%----------------------------------------------------------------------------
\RequirePackage[normalem]{ulem}%

%%============================================================================
%% Including a package for controling sectional headers. (Jusitfy?)
%%----------------------------------------------------------------------------
\RequirePackage{sectsty}[1999/04/12 v2.0.1 Commands to change all sectional heading styles]%
\ifthenelse{\boolean{QZ@Justify}}{}{%
\RequirePackage{ragged2e}[1999/06/08 v1.02 Ragged2e Package (MS)]%
\setlength{\RaggedRightRightskip}{0pt plus 4em}%
\ifthenelse{\boolean{QZ@texshade}}%
{}%
{\RaggedRight}%
}%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Page layout for McGill ETD                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\normalfont\small\normalsize% Make sure font size is set to normalsize
\newcounter{QZ@TmpCnt}% Used to count the number of lines per page
\setcounter{QZ@TmpCnt}{1}% and as a counter for handling TOC section spacing.
\newlength{\QZ@TOChdent}% Used to define hanging indents.
\setlength{\QZ@TOChdent}{1.0em}%
\newlength{\QZ@MyHeader}% Used to set space between top of page and
\setlength{\QZ@MyHeader}{0.6in}% page numbers
\newlength{\QZ@NumPoints}%
\newlength{\QZ@StartofHeader}%
\newlength{\QZ@EndofFooter}%
\newlength{\QZ@TempLength}%

%%    Setting the margin reference point
\setlength{\hoffset}{0pt}%
\setlength{\voffset}{0pt}%

%%    Setting the height of the text page
\setlength{\textheight}{\topskip}%
\addtolength{\textheight}{44\baselineskip}%
\ifthenelse{\value{QZ@ptcnt}=11}{%
\addtolength{\textheight}{3\baselineskip}}{}%
\ifthenelse{\value{QZ@ptcnt}=10}{%
\addtolength{\textheight}{9\baselineskip}}{}%

%%    Setting the width of the text page
\setlength{\oddsidemargin}{0.25in}%
\@settopoint{\oddsidemargin}%
\setlength{\evensidemargin}{\oddsidemargin}%
\setlength{\textwidth}{\paperwidth}%
\addtolength{\textwidth}{-2in}%
\addtolength{\textwidth}{-\oddsidemargin}%
\addtolength{\textwidth}{-\evensidemargin}%
\@settopoint{\textwidth}%
\setlength{\marginparpush}{8pt}%
\setlength{\marginparwidth}{\paperwidth}%
\addtolength{\marginparwidth}{-1in}%
\addtolength{\marginparwidth}{-\hoffset}%
\addtolength{\marginparwidth}{-\evensidemargin}%
\addtolength{\marginparwidth}{-\textwidth}%
\addtolength{\marginparwidth}{-2\marginparpush}%
\@settopoint{\marginparwidth}%
\settowidth{\parindent}{\ }%
\setlength{\parindent}{6\parindent}%
\ifthenelse{\boolean{QZ@Justify}}{}{%
\setlength{\RaggedRightParindent}{\parindent}}%

%%    Positioning the Header
\ifthenelse{\value{QZ@ptcnt} = 10}{\setlength{\headheight}{10.0pt}}{}%
\ifthenelse{\value{QZ@ptcnt} = 11}{\setlength{\headheight}{11.0pt}}{}%
\ifthenelse{\value{QZ@ptcnt} = 12}{\setlength{\headheight}{12.0pt}}{}%
\@settopoint{\QZ@MyHeader}%
\setlength{\topmargin}{\QZ@MyHeader}%
\addtolength{\topmargin}{-1in}%
\@settopoint{\topmargin}%
\setlength{\headsep}{-\topmargin}%
\addtolength{\headsep}{-\headheight}%

%%    Positioning the Footer
\setlength{\QZ@StartofHeader}{\voffset}%
\addtolength{\QZ@StartofHeader}{1in}%
\addtolength{\QZ@StartofHeader}{\topmargin}%
\setlength{\footskip}{0pt}%
\setlength{\QZ@EndofFooter}{100pt}%
\whiledo{\lengthtest{\QZ@EndofFooter > \QZ@StartofHeader}}%
    {\addtolength{\footskip}{0.5pt}%
    \setlength{\QZ@EndofFooter}{-\voffset}%
    \addtolength{\QZ@EndofFooter}{-1in}%
    \addtolength{\QZ@EndofFooter}{-\topmargin}%
    \addtolength{\QZ@EndofFooter}{-\headheight}%
    \addtolength{\QZ@EndofFooter}{-\headsep}%
    \addtolength{\QZ@EndofFooter}{-\textheight}%
    \addtolength{\QZ@EndofFooter}{-\footskip}%
    \addtolength{\QZ@EndofFooter}{\paperheight}}%
    
%%    Creating a 2 inch margin
\newlength{\BigLength}%
\setlength{\BigLength}{0pt}%
\newcommand{\BigMargin}{\hspace*{1in}\normalfont\normalsize%
\settoheight{\QZ@TempLength}{()}%
\vspace*{-\baselineskip}\vspace*{-\topskip}\vspace*{0.25in}
% ^ TOC, LOF, LOT space between title and topmargin 
\vspace*{\QZ@TempLength}\vspace*{\BigLength} \\}%

%% creating a one inch margin
\newcommand*{\SmallMargin}{%
\vspace*{0.5in}\\}%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Page-breaking penalties and page number  %
%%          formatting commands                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\ifthenelse{\boolean{QZ@nopenalties}}{%
\clubpenalty=10000    % Do not allow orphans
\@clubpenalty \clubpenalty
\widowpenalty=10000   % Do not allow widows
}{}%
\brokenpenalty=10000  % Do not allow hyphenation across
                      % page boundaries.
\providecommand{\frontmatter}{}%
\providecommand{\mainmatter}{}%
\providecommand{\backmatter}{}%
\ifthenelse{\boolean{QZ@Draft}}{%
  \renewcommand{\frontmatter}{\clearpage\pagenumbering{arabic}%
    \pagestyle{plain}}%
  \renewcommand{\mainmatter}{\clearpage%
    \pagestyle{myheadings}\markboth{}{}}}%
{ \renewcommand{\frontmatter}{\clearpage\pagenumbering{roman}%
    \pagestyle{plain}}%
  \renewcommand{\mainmatter}{\clearpage\pagenumbering{arabic}%
    \pagestyle{myheadings}\markboth{}{}}}%
\renewcommand{\backmatter}{\let\clearpage\relax}%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Line-spacing commands                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newboolean{SetDSpace}%
\setboolean{SetDSpace}{true}%
\newcommand{\BaseDiff}{0}
\newcommand{\GoSingle}{\renewcommand{\baselinestretch}{1}%
\normalfont\tiny\normalsize}%
\ifthenelse{\value{QZ@ptcnt} = 10}{%
\newcommand{\GoDouble}{\renewcommand{\baselinestretch}{1.667}%
\renewcommand{\BaseDiff}{0.667}\normalfont\tiny\normalsize}}{}%
\ifthenelse{\value{QZ@ptcnt} = 11}{%
\newcommand{\GoDouble}{\renewcommand{\baselinestretch}{1.618}%
\renewcommand{\BaseDiff}{0.618}\normalfont\tiny\normalsize}}{}%
\ifthenelse{\value{QZ@ptcnt} = 12}{%
\newcommand{\GoDouble}{\renewcommand{\baselinestretch}{1.655}%
\renewcommand{\BaseDiff}{0.655}\normalfont\tiny\normalsize}}{}%
\GoDouble\GoSingle%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Formatting Chapter Headings                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\setcounter{secnumdepth}{2}%
\renewcommand{\chaptername}{CHAPTER}
\renewcommand{\appendixname}{APPENDIX}
\renewcommand{\@chapapp}{\chaptername}
\let\@afterindentfalse\@afterindenttrue%
\@afterindenttrue%
\newenvironment{QZ@Cent}{\centering}{\par}%
\renewcommand{\chapter}{%
    \clearpage%
    \thispagestyle{plain}%
    \GoSingle%
    \global\@topnum\z@%  Keep floats from top of page
    \@afterindentfalse%
    \secdef\@chapter\@schapter}%
\renewcommand{\@chapter}[2][?]{%
    \ifthenelse{\value{secnumdepth} > -1}{%
      \refstepcounter{chapter}%
      \typeout{***************************}%
      \typeout{\@chapapp\space\thechapter.}%
      \typeout{***************************}%
      \addcontentsline{toc}{chapter}{\protect\numberline{\thechapter}#1}%
      %\addcontentsline{toc}{chapter}{#1}%
    }{%
      \addcontentsline{toc}{chapter}{#1}}%
    \setcounter{footnote}{0}%
    \@makechapterhead{#2}}%
\renewcommand{\@makechapterhead}[1]{%
    \begin{QZ@Cent}%
    \BigMargin%
    \ifthenelse{\boolean{QZ@BfSet}}{%
		\ifthenelse{\value{secnumdepth} > -1}{%
			\bf{\@chapapp\space\thechapter}\\}{}%
			\bf{#1}\\}%
	{\ifthenelse{\value{secnumdepth} > -1}{%
			\@chapapp\space\thechapter\\}{}%
			#1\\}%
	\end{QZ@Cent}%
    \@afterheading%
    \ifthenelse{\boolean{SetDSpace}}{\GoDouble}{}%
    }%
\renewcommand{\@schapter}[2][?]{%
    \@makeschapterhead{#2}}%
\renewcommand{\@makeschapterhead}[1]{%
    \begin{QZ@Cent}%
    \BigMargin%
    #1\end{QZ@Cent}%
    \@afterheading%
    \ifthenelse{\boolean{SetDSpace}}{\GoDouble}%
    {\addvspace{\BaseDiff\baselineskip}}%
    }%
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Formatting Section Headings                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\ifthenelse{\boolean{QZ@BfSet}}%
{\sectionfont{\normalsize\normalfont\singlespace\bfseries}%
 \subsectionfont{\normalsize\normalfont\singlespace\bfseries}%
 \subsubsectionfont{\normalsize\normalfont\singlespace\bfseries}%
 \paragraphfont{\normalsize\normalfont\bfseries}%
 \renewcommand{\paragraph}[1]{\@paragraph{#1.}}%
 \newcommand{\@paragraph}{\@startsection{paragraph}{4}{\parindent}%
    {0.1ex}%
    {-1.0em}%
    {\SS@parafont}}%
 \subparagraphfont{\normalsize\normalfont\bfseries}}%
{\sectionfont{\normalsize\normalfont\singlespace}%
 \subsectionfont{\normalsize\normalfont\singlespace}%
 \subsubsectionfont{\normalsize\normalfont\singlespace}%
 \paragraphfont{\normalsize\normalfont}%
 \renewcommand{\paragraph}[1]{\@paragraph{#1}.}%
 \newcommand{\@paragraph}{\@startsection{paragraph}{4}{\parindent}%
    {0.1ex}%
    {0pt}%
    {\SS@parafont}}%
 \subparagraphfont{\normalsize\normalfont\underline}}%
\renewcommand{\section}{%
    \@startsection{section}{1}{\z@}%
    {-0\baselineskip}%
    {0.1ex}%
    {\SS@sectfont}}
\renewcommand{\subsection}{%
    \@startsection{subsection}{2}{\z@}%
    {-0\baselineskip}%
    {0.1ex}%
    {\SS@subsectfont}}
\renewcommand{\subsubsection}{%
    \@startsection{subsubsection}{3}{2.0em}%
    {-0.1ex}%
    {0.1ex}%
    {\SS@subsubsectfont}}
\renewcommand{\subparagraph}{%
    \@startsection{subparagraph}{5}{\z@}%
    {0.1ex}%
    {-1.0em}%
    {\SS@subparafont}}
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Formatting Generated Lists -- TOC, LOF and LOT %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\renewcommand{\@dotsep}{4.5}%
\renewcommand{\@pnumwidth}{1.55em}%
\ifthenelse{\boolean{QZ@Justify}}{%
\renewcommand{\@tocrmarg}{2.55em}}{%
\renewcommand{\@tocrmarg}{2.55em plus 1.45em}}%
\renewcommand*{\l@chapter}[2]{%
  \setcounter{QZ@TmpCnt}{0}%
  \ifthenelse{\value{tocdepth} > -1}{%
    \addpenalty{-\@highpenalty}%
    \vspace{\BaseDiff\baselineskip}%
    \@dottedtocline{0}{\UFdent}{\QZ@chapnumwidth}{#1}{#2}%
  }{}}%
\newcommand*{\l@extrachapter}[2]{%
  \setcounter{QZ@TmpCnt}{0}%
  \ifthenelse{\value{tocdepth} > -1}{%
    \addpenalty{-\@highpenalty}%
    \vspace{\BaseDiff\baselineskip}%
    \@dottedtocline{0}{0pt}{0pt}{#1}{#2}%
  }{}}%
\newcommand*{\l@extraentry}[2]{%
  \setcounter{QZ@TmpCnt}{0}%
  \ifthenelse{\value{tocdepth} > -1}{%
    \addpenalty{-\@highpenalty}%
    \vspace{\BaseDiff\baselineskip}%
    \noindent #1\par%
  }{}}%
\renewcommand*{\l@section}{%
  \ifthenelse{\value{QZ@TmpCnt} = 0}{%
     \setcounter{QZ@TmpCnt}{1}%
     \vspace{\BaseDiff\baselineskip}}{}%
  \@dottedtocline{1}{\QZ@secindent}{\QZ@secnumwidth}}%
\renewcommand*{\l@subsection}{\@dottedtocline{2}{\QZ@subsecindent}%
              {\QZ@subsecnumwidth}}%
\renewcommand*{\l@subsubsection}{\@dottedtocline{3}{7.0em}{4.1em}}%
\renewcommand*{\l@paragraph}{\@dottedtocline{4}{10em}{5em}}%
\renewcommand*{\l@subparagraph}{\@dottedtocline{5}{12em}{6em}}%
\renewcommand*{\l@figure}{\vspace{\BaseDiff\baselineskip}%
              \@dottedtocline{-5}{\QZ@figindent}{\QZ@fignumwidth}}%
\renewcommand*{\l@table}{\vspace{\BaseDiff\baselineskip}%
              \@dottedtocline{-5}{\QZ@tabindent}{\QZ@tabnumwidth}}%
\newcommand{\leaddots}{%
     \leaders\hbox{$\m@th
        \mkern \@dotsep mu\hbox{.}\mkern \@dotsep
        mu$}\hfill}%
\renewcommand{\@dottedtocline}[5]{%
  \ifnum #1>\c@tocdepth \else
    \vskip \z@ \@plus.2\p@
    {\leftskip #2\relax \rightskip \@tocrmarg \parfillskip -\rightskip
     \parindent #2\relax\@afterindenttrue
     \interlinepenalty\@M
     \leavevmode
     \@tempdima #3\relax
%%   Handling those sections that are included in TOC, but have
%%   no section numbering. Also including hanging indent when the
%%   NoTocHang option was not used. Only the following 4 lines added.
     \ifthenelse{#1 > \value{secnumdepth}}{%
     \setlength{\@tempdima}{0pt}}{}%
     \ifthenelse{#1 > \value{secnumdepth} \or \boolean{QZ@TOCHang}}{%
     \addtolength{\leftskip}{\QZ@TOChdent}}{}%
     \advance\leftskip \@tempdima \null\nobreak\hskip -\leftskip
     {#4}\nobreak
     \leaddots
     \nobreak
     \hb@xt@\@pnumwidth{\hfil\normalfont \normalcolor #5}%
     \par}%
  \fi}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Indenting Generated Lists -- TOC, LOF and LOT  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newlength{\UFdent}%
\setlength{\UFdent}{0pt}%
\newlength{\QZ@chapnumwidth}%
\setlength{\QZ@chapnumwidth}{2.3em}%
\newlength{\QZ@secindent}%
\setlength{\QZ@secindent}{0pt}%
\AtBeginDocument{%
\addtolength{\QZ@secindent}{\UFdent}%
%% Verifying that the chapters are numbered.
\ifthenelse{\value{secnumdepth} > -1}{%
\addtolength{\QZ@secindent}{\QZ@chapnumwidth}}{}%
}
\newlength{\QZ@secnumwidth}%
\setlength{\QZ@secnumwidth}{3em}%
\newlength{\QZ@subsecindent}%
\setlength{\QZ@subsecindent}{0pt}%
\AtBeginDocument{%
\addtolength{\QZ@subsecindent}{\QZ@secindent}%
%% Verifying that the sections are numbered.
\ifthenelse{\value{secnumdepth} > 0}{%
\addtolength{\QZ@subsecindent}{\QZ@secnumwidth}}{}%
}
\newlength{\QZ@subsecnumwidth}%
\setlength{\QZ@subsecnumwidth}{3em}%
\newlength{\QZ@figindent}%
\settowidth{\QZ@figindent}{Fig}%
\newlength{\QZ@fignumwidth}%
\setlength{\QZ@fignumwidth}{2.3em}%
\newlength{\QZ@tabindent}%
\settowidth{\QZ@tabindent}{Ta}%
\newlength{\QZ@tabnumwidth}%
\setlength{\QZ@tabnumwidth}{2.3em}%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Page formating                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newenvironment{RomanPagenumber}[1]
{\setcounter{page}{#1}\renewcommand{\thepage}{\Roman{page}}}
{\pagenumbering{arabic}}

\newenvironment{romanPagenumber}[1]
{\setcounter{page}{#1}\renewcommand{\thepage}{\roman{page}}}
{\pagenumbering{arabic}}

\newenvironment{arabicPagenumber}[1]
{\setcounter{page}{#1}\renewcommand{\thepage}{\arabic{page}}}
{\pagenumbering{arabic}}

\newenvironment{AlphPagenumber}[1]
{\setcounter{page}{#1}\renewcommand{\thepage}{\Alph{page}}}
{\pagenumbering{arabic}}

\newenvironment{alphPagenumber}[1]
{\setcounter{page}{#1}\renewcommand{\thepage}{\alph{page}}}
{\pagenumbering{arabic}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Table of Contents                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\TOCHeading}[1]{%
	\renewcommand{\contentsname}{#1}%
}%
\setcounter{tocdepth}{2}%
\renewcommand{\contentsname}{TABLE OF CONTENTS}%
\newenvironment{simpleenv}[4]{\clearpage}{\clearpage}%
\renewcommand{\tableofcontents}{%
    \setboolean{SetDSpace}{false}%
    \begin{simpleenv}{}{}{}{}%
      \pagestyle{plain}%
      \chapter*{\contentsname}%
      \vspace*{-\QZ@ptsize}%
      \noindent\phantom{Table}\hfill\par%
      %%\uline{page}
      \@starttoc{toc}%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        List of Tables                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\LOTHeading}[1]{%
	\renewcommand{\listtablename}{#1}%
}%
\renewcommand{\listtablename}{LIST OF TABLES}%
\renewcommand{\listoftables}{%
    \setboolean{SetDSpace}{false}%
    \begin{simpleenv}{}{}{}{}%
      \pagestyle{plain}%
      \chapter*{\listtablename}%
      \vspace*{-\QZ@ptsize}%
      \addcontentsline{toc}{extrachapter}{\listtablename}%
      \noindent\uline{Table}\hfill\uline{page}\par%
      \@starttoc{lot}%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        List of Figures                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\LOFHeading}[1]{%
	\renewcommand{\listfigurename}{#1}%
}%
\renewcommand{\listfigurename}{LIST OF FIGURES}%
\renewcommand{\listoffigures}{%
    \setboolean{SetDSpace}{false}%
    \begin{simpleenv}{}{}{}{}%
      \pagestyle{plain}%
      \chapter*{\listfigurename}%
      \vspace*{-\QZ@ptsize}%
      \addcontentsline{toc}{extrachapter}{\listfigurename}%
      \noindent\uline{Figure}\hfill\uline{page}\par%
      \@starttoc{lof}%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%
\renewcommand{\appendix}{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \renewcommand{\@chapapp}{\appendixname}%
  \renewcommand{\thechapter}{\@Alph\c@chapter}}%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        List of References                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\bibHeading}[1]{%
	\renewcommand{\bibname}{#1}%
}%
\renewcommand{\bibname}{REFERENCES}%
\renewenvironment{thebibliography}[1]%
     {\let\clearpage\relax
     %\pagestyle{plain}%
      \markboth{}{}%
      \setboolean{SetDSpace}{true}%
      \chapter*{\bibname}%
      \addcontentsline{toc}{extrachapter}{\bibname}%
      \setboolean{SetDSpace}{true}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \setlength{\parsep}{\BaseDiff\baselineskip}%
            \setlength{\itemsep}{0ex}%
            \setlength{\leftmargin}{\labelwidth}%
            \addtolength{\leftmargin}{\labelsep}%
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty10000
      \@clubpenalty \clubpenalty
      \widowpenalty10000%
      \sfcode`\.=1000\relax
      \let\clearpage\relax}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist
      \let\clearpage\relax
      }


%% Special Pages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Title Page                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\renewcommand{\maketitle}{%
    \clearpage%
    \setboolean{SetDSpace}{false}%
    \thispagestyle{empty}%
    %\GoDouble%
    \begin{QZ@Cent}%
    \BigMargin%
    \Title%
    \vfill%
	\GoSingle%
    \normalsize\normalfont%
	\FullName\normalsize\normalfont \\*[\BaseDiff\baselineskip]%
    \vfill%
    \DegreeType\normalsize\normalfont \\*[\BaseDiff\baselineskip]%
	\vfill%
    \ETDDepartment\normalsize\normalfont \\*[\BaseDiff\baselineskip]%
    \vfill%
    \ETDUniversity\normalsize\normalfont  \\*[\BaseDiff\baselineskip]%
    \ETDUniversityAddr\normalsize\normalfont \\*[\BaseDiff\baselineskip]%
    \ETDThesisDate\normalsize\normalfont \\*[\BaseDiff\baselineskip]%
    \vfill%
    \ETDRequirements\normalsize\normalfont \\*[\BaseDiff\baselineskip]%
 	\ETDCopyright\normalsize\normalfont \\*[\BaseDiff\baselineskip]%

    \end{QZ@Cent}%
    \vspace*{0.5in}%
    \setboolean{SetDSpace}{true}%
    \clearpage}%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Formula Environment                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%==================================================
\newenvironment{MathFormula}%
{}{}
\newenvironment{ChemFormula}%
{}{}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Emphasis                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%=================================================
%%             ForeignWord
%%-------------------------------------------------
\newcommand*{\BForeignWord}[1]{%
	\@Bold{#1}
}
\newcommand*{\BIForeignWord}[1]{%
	\@BoldItalic{#1}
}
\newcommand*{\BIUForeignWord}[1]{%
	\@BoldItalicUnderline{#1}
}
\newcommand*{\BUForeignWord}[1]{%
	\@BoldUnderline{#1}
}
\newcommand*{\IUForeignWord}[1]{%
	\@ItalicUnderline{#1}
}
\newcommand*{\IForeignWord}[1]{%
	\@Italic{#1}
}
\newcommand*{\UForeignWord}[1]{%
	\@Underline{#1}
}
%%=================================================
%%          ProperName
%%-------------------------------------------------
\newcommand*{\BProperName}[1]{%
	\@Bold{#1}
}
\newcommand*{\BIProperName}[1]{%
	\@BoldItalic{#1}
}
\newcommand*{\BIUProperName}[1]{%
	\@BoldItalicUnderline{#1}
}
\newcommand*{\BUProperName}[1]{%
	\@BoldUnderline{#1}
}
\newcommand*{\IUProperName}[1]{%
	\@ItalicUnderline{#1}
}
\newcommand*{\IProperName}[1]{%
	\@Italic{#1}
}
\newcommand*{\UProperName}[1]{%
	\@Underline{#1}
}
%%=================================================
%%           Technical Term
%%-------------------------------------------------
\newcommand*{\BTechnicalTerm}[1]{%
	\@Bold{#1}
}
\newcommand*{\BITechnicalTerm}[1]{%
	\@BoldItalic{#1}
}
\newcommand*{\BIUTechnicalTerm}[1]{%
	\@BoldItalicUnderline{#1}
}
\newcommand*{\BUTechnicalTerm}[1]{%
	\@BoldUnderline{#1}
}
\newcommand*{\IUTechnicalTerm}[1]{%
	\@ItalicUnderline{#1}
}
\newcommand*{\ITechnicalTerm}[1]{%
	\@Italic{#1}
}
\newcommand*{\UTechnicalTerm}[1]{%
	\@Underline{#1}
}
%%=================================================
%%           Title
%%-------------------------------------------------
\newcommand*{\BTitle}[1]{%
	\@Bold{#1}
}
\newcommand*{\BITitle}[1]{%
	\@BoldItalic{#1}
}
\newcommand*{\BIUTitle}[1]{%
	\@BoldItalicUnderline{#1}
}
\newcommand*{\BUTitle}[1]{%
	\@BoldUnderline{#1}
}
\newcommand*{\IUTitle}[1]{%
	\@ItalicUnderline{#1}
}
\newcommand*{\ITitle}[1]{%
	\@Italic{#1}
}
\newcommand*{\UTitle}[1]{%
	\@Underline{#1}
}
%%=================================================
%%           VisualEmphasis
%%-------------------------------------------------
\newcommand*{\BVisualEmphasis}[1]{%
	\@Bold{#1}
}
\newcommand*{\BIVisualEmphasis}[1]{%
	\@BoldItalic{#1}
}
\newcommand*{\BIUVisualEmphasis}[1]{%
	\@BoldItalicUnderline{#1}
}
\newcommand*{\BUVisualEmphasis}[1]{%
	\@BoldUnderline{#1}
}
\newcommand*{\IUVisualEmphasis}[1]{%
	\@ItalicUnderline{#1}
}
\newcommand*{\IVisualEmphasis}[1]{%
	\@Italic{#1}
}
\newcommand*{\UVisualEmphasis}[1]{%
	\@Underline{#1}
}

\newcommand*{\@Bold}[1]{\bfseries{#1}\normalfont\normalsize}
\newcommand*{\@BoldItalic}[1]{\emph{\bfseries{#1}}\normalfont\normalsize}
\newcommand*{\@BoldItalicUnderline}[1]{\underline{\emph{\bfseries{#1}}}\normalfont\normalsize}
\newcommand*{\@BoldUnderline}[1]{\underline{\bfseries{#1}}\normalfont\normalsize}
\newcommand*{\@ItalicUnderline}[1]{\emph{\underline{#1}}\normalfont\normalsize}
\newcommand*{\@Italic}[1]{\emph{#1}\normalfont\normalsize}
\newcommand*{\@Underline}[1]{\underline{#1}\normalfont\normalsize}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Special cases                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\SingleInlineQuote}[1]{
	`{#1}'%
}
\newcommand*{\DoubleInlineQuote}[1]{
	``{#1}''%
}
%%\newenvironment{\quote}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Caption                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\TableCaption}[1]{
	\caption{#1}%
}
\newcommand*{\FigureCaption}[1]{
	\caption{#1}%
}

\newcommand*{\TableCaptionOpt}[2]{
	\caption[#1]{#2}%
}
\newcommand*{\FigureCaptionOpt}[2]{
	\caption[#1]{#2}%
}
%%\newenvironment{\quote}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Caption                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%===============================================================
%%               BulletList
%%---------------------------------------------------------------
\newenvironment{BulletList}%
{
\begin{itemize}
}
{
\end{itemize}
}
%%===============================================================
%%               RomanList
%%---------------------------------------------------------------
\newenvironment{RomanList}%
{
\renewcommand{\labelenumi}{\Roman{enumi}}
\renewcommand{\labelenumii}{\Roman{enumii}}
\renewcommand{\labelenumiii}{\Roman{enumiii}}
\renewcommand{\labelenumiv}{\Roman{enumiv}}
\begin{enumerate}
}
{
\end{enumerate}
\renewcommand{\labelenumi}{\arabic{enumi}}
\renewcommand{\labelenumii}{\arabic{enumii}}
\renewcommand{\labelenumiii}{\arabic{enumiii}}
\renewcommand{\labelenumiv}{\arabic{enumiv}}
}
%%===============================================================
%%               romanList
%%---------------------------------------------------------------
\newenvironment{romanList}%
{
\renewcommand{\labelenumi}{\roman{enumi}}
\renewcommand{\labelenumii}{\roman{enumii}}
\renewcommand{\labelenumiii}{\roman{enumiii}}
\renewcommand{\labelenumiv}{\roman{enumiv}}
\begin{enumerate}
}
{
\end{enumerate}
\renewcommand{\labelenumi}{\arabic{enumi}}
\renewcommand{\labelenumii}{\arabic{enumii}}
\renewcommand{\labelenumiii}{\arabic{enumiii}}
\renewcommand{\labelenumiv}{\arabic{enumiv}}
}
%%===============================================================
%%               alphList
%%---------------------------------------------------------------
\newenvironment{alphList}%
{
\renewcommand{\labelenumi}{\alph{enumi}}
\renewcommand{\labelenumii}{\alph{enumii}}
\renewcommand{\labelenumiii}{\alph{enumiii}}
\renewcommand{\labelenumiv}{\alph{enumiv}}
\begin{enumerate}
}
{
\end{enumerate}
\renewcommand{\labelenumi}{\arabic{enumi}}
\renewcommand{\labelenumii}{\arabic{enumii}}
\renewcommand{\labelenumiii}{\arabic{enumiii}}
\renewcommand{\labelenumiv}{\arabic{enumiv}}
}
%%===============================================================
%%               AlphList
%%---------------------------------------------------------------
\newenvironment{AlphList}%
{
\renewcommand{\labelenumi}{\Alph{enumi}}
\renewcommand{\labelenumii}{\Alph{enumii}}
\renewcommand{\labelenumiii}{\Alph{enumiii}}
\renewcommand{\labelenumiv}{\Alph{enumiv}}
\begin{enumerate}
}
{
\end{enumerate}
\renewcommand{\labelenumi}{\arabic{enumi}}
\renewcommand{\labelenumii}{\arabic{enumii}}
\renewcommand{\labelenumiii}{\arabic{enumiii}}
\renewcommand{\labelenumiv}{\arabic{enumiv}}
}
%%===============================================================
%%               arabicList
%%---------------------------------------------------------------
\newenvironment{arabicList}%
{
\renewcommand{\labelenumi}{\arabic{enumi}}
\renewcommand{\labelenumii}{\arabic{enumii}}
\renewcommand{\labelenumiii}{\arabic{enumiii}}
\renewcommand{\labelenumiv}{\arabic{enumiv}}
\begin{enumerate}
}
{
\end{enumerate}
\renewcommand{\labelenumi}{\arabic{enumi}}
\renewcommand{\labelenumii}{\arabic{enumii}}
\renewcommand{\labelenumiii}{\arabic{enumiii}}
\renewcommand{\labelenumiv}{\arabic{enumiv}}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Preface                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\SetPrefaceName}[1]{\renewcommand*{\ETDPrefaceName}{#1}}%
\newcommand*{\ETDPrefaceName}{Preface}%

\newcommand*{\SetPrefaceText}[1]{\renewcommand*{\ETDPrefaceText}{#1}}%
\newcommand*{\ETDPrefaceText}{Preface text goes here!}%
\newcommand{\Preface}{%
    \begin{simpleenv}{}{}{}{}%
    \setboolean{SetDSpace}{false}%
    \pagestyle{plain}%
    \GoSingle%
    \begin{QZ@Cent}%
          \bfseries{\ETDPrefaceName} 
      \end{QZ@Cent}%
    \vspace*{0.5in}
      \par%
      \GoDouble%
      \addcontentsline{toc}{extrachapter}{\ETDPrefaceName}%
      \ETDPrefaceText%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%

\newcommand{\ETDPreface}[1]{%
    \begin{simpleenv}{#1}{}{}{}%
    \clearpage%
    \setboolean{SetDSpace}{false}%
    \GoSingle%
    \begin{QZ@Cent}%
          \bfseries{\ETDPrefaceName} 
      \end{QZ@Cent}%
    \vspace*{0.5in}
      \par%
      \GoDouble%
      \addcontentsline{toc}{extrachapter}{\ETDPrefaceName}%
      #1%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Dedication                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\SetDedicationName}[1]{\renewcommand*{\ETDDedicationName}{#1}}%
\newcommand*{\ETDDedicationName}{DEDICATION}%

\newcommand*{\SetDedicationText}[1]{\renewcommand*{\ETDDedicationText}{#1}}%
\newcommand*{\ETDDedicationText}{Dedication text goes here!}%
\newcommand{\Dedication}{%
    \begin{simpleenv}{}{}{}{}%
		\setboolean{SetDSpace}{false}%
		\pagestyle{plain}%
		\GoSingle%
		\begin{QZ@Cent}%
	        \bfseries{\ETDDedicationName} 
	    \end{QZ@Cent}%
	  \vspace*{0.25in}% space between title and text start
      \par%
      \GoDouble%
      \addcontentsline{toc}{extrachapter}{\ETDDedicationName}%
      \ETDDedicationText%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%

\newcommand{\ETDdedication}[1]{%
    \begin{simpleenv}{#1}{}{}{}%
		\clearpage%
		\setboolean{SetDSpace}{false}%
		\GoSingle%
		\begin{QZ@Cent}%
	        \bfseries{\ETDDedicationName} 
	    \end{QZ@Cent}%
	  \vspace*{0.5in}
      \par%
      \GoDouble%
      \addcontentsline{toc}{extrachapter}{\ETDDedicationName}%
      #1%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Acknowledgments                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\SetAcknowledgeName}[1]{\renewcommand*{\ETDAcknowledgeName}{#1}}%
\newcommand*{\ETDAcknowledgeName}{ACKNOWLEDGEMENTS}%

\newcommand*{\SetAcknowledgeText}[1]{\renewcommand*{\ETDAcknowledgeText}{#1}}%
\newcommand*{\ETDAcknowledgeText}{Acknowledgements text goes here!}%
\newcommand{\Acknowledge}{%
    \begin{simpleenv}{}{}{}{}%
		\pagestyle{plain}%
		\setboolean{SetDSpace}{false}%
		\GoSingle%
		\begin{QZ@Cent}%
	        \bfseries{\ETDAcknowledgeName} 
	    \end{QZ@Cent}%
	  \vspace*{0.25in}
      \par%
      \GoDouble%
      %\chapter*{\ETDAcknowledgeName}%
      \addcontentsline{toc}{extrachapter}{\ETDAcknowledgeName}%
      \ETDAcknowledgeText%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Linking statement                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\SetLinkName}[1]{\renewcommand*{\ETDLinkName}{#1}}%
\newcommand*{\ETDLinkName}{LinkMENTS}%

\newcommand*{\SetLinkText}[1]{\renewcommand*{\ETDLinkText}{#1}}%
\newcommand*{\ETDLinkText}{Linking statement text goes here!}%
\newcommand{\Link}{%
    \begin{simpleenv}{}{}{}{}%
    \pagestyle{plain}%
    \setboolean{SetDSpace}{false}%
    \GoSingle%
    \begin{QZ@Cent}%
          \bfseries{\ETDLinkName} 
      \end{QZ@Cent}%
    \vspace*{0.25in}
      \par%
      \GoDouble%
      %\chapter*{\ETDLinkName}%
      \addcontentsline{toc}{extrachapter}{\ETDLinkName}%
      \ETDLinkText%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Abstract                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\SetAbstractEnName}[1]{\renewcommand*{\ETDAbstractEnName}{#1}}%
\newcommand*{\ETDAbstractEnName}{Abstract}%

\newcommand*{\SetAbstractEnText}[1]{\renewcommand*{\ETDAbstractEnText}{#1}}%
\newcommand*{\ETDAbstractEnText}{Abstract text goes here!}%

\newcommand*{\SetAbstractFrName}[1]{\renewcommand*{\ETDAbstractFrName}{#1}}%
\newcommand*{\ETDAbstractFrName}{Abstract}%

\newcommand*{\SetAbstractFrText}[1]{\renewcommand*{\ETDAbstractFrText}{#1}}%
\newcommand*{\ETDAbstractFrText}{Abstract text goes here!}%

\newcommand*{\SetAbstractOtName}[1]{\renewcommand*{\ETDAbstractOtName}{#1}}%
\newcommand*{\ETDAbstractOtName}{Abstract}%

\newcommand*{\SetAbstractOtText}[1]{\renewcommand*{\ETDAbstractOtText}{#1}}%
\newcommand*{\ETDAbstractOtText}{Abstract text goes here!}%

\newcommand*{\AbstractEn}{%
    \clearpage%
    \setboolean{SetDSpace}{false}%
	\pagestyle{plain}%
    \GoSingle%
    \addcontentsline{toc}{extrachapter}{\ETDAbstractEnName}%
    \begin{QZ@Cent}%
	    \bfseries{\ETDAbstractEnName} 
    \vspace*{0.25in}
    %\SmallMargin%
    \end{QZ@Cent}%
    \setboolean{SetDSpace}{true}%
    \par%
    \GoDouble%
	\ETDAbstractEnText%
}%    

\newcommand*{\AbstractFr}{%
    \clearpage%
    \setboolean{SetDSpace}{false}%
	\pagestyle{plain}%
    \GoSingle%
    \addcontentsline{toc}{extrachapter}{\ETDAbstractFrName}%
    \begin{QZ@Cent}%
	    \bfseries{\ETDAbstractFrName} 
    \vspace*{0.25in}
    %\SmallMargin%
    \end{QZ@Cent}%
    \setboolean{SetDSpace}{true}%
    \par%
    \GoDouble%
	\ETDAbstractFrText%
}%    

\newcommand*{\AbstractOt}{%
    \clearpage%
    \setboolean{SetDSpace}{false}%
	\pagestyle{plain}%
    \GoSingle%
    \addcontentsline{toc}{extrachapter}{\ETDAbstractOtName}%
    \begin{QZ@Cent}%
	    \bfseries{\ETDAbstractOtName} 
		\SmallMargin%
    \end{QZ@Cent}%
    \setboolean{SetDSpace}{true}%
    \par%
    \GoDouble%
	\ETDAbstractOtText%
}%    

\newcommand*{\SetAppendixName}[1]{\renewcommand*{\ETDAppendixName}{#1}}%
\newcommand*{\ETDAppendixName}{APPENDIX}%

\newcommand*{\SetAppendixText}[1]{\renewcommand*{\ETDAppendixText}{#1}}%
\newcommand*{\ETDAppendixText}{Appendix text goes here!}%

\newcommand*{\ETDAppendix}[2]{%
    \begin{simpleenv}{}{}{}{}%
		\setboolean{SetDSpace}{true}%
		%\pagestyle{plain}%
		\begin{QZ@Cent}%
	        \bfseries{#1} 
	    \end{QZ@Cent}%
	  \vspace*{0.25in}%
	  %\SmallMargin%
      %\par%
      \GoDouble%
      \addcontentsline{toc}{extrachapter}{#1}%
      #2%
    \end{simpleenv}%
	}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Contribution to original knowledge      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\SetContribKnowledgeName}[1]{\renewcommand*{\ETDContribKnowledgeName}{#1}}%
\newcommand*{\ETDContribKnowledgeName}{ContribKnowledgeMENTS}%

\newcommand*{\SetContribKnowledgeText}[1]{\renewcommand*{\ETDContribKnowledgeText}{#1}}%
\newcommand*{\ETDContribKnowledgeText}{ContribKnowledge text goes here!}%
\newcommand{\ContribKnowledge}{%
    \begin{simpleenv}{}{}{}{}%
    \pagestyle{plain}%
    \setboolean{SetDSpace}{false}%
    \GoSingle%
    \begin{QZ@Cent}%
          \bfseries{\ETDContribKnowledgeName} 
      \end{QZ@Cent}%
    \vspace*{0.25in}
      \par%
      \GoDouble%
      %\chapter*{\ETDContribKnowledgeName}%
      \addcontentsline{toc}{extrachapter}{\ETDContribKnowledgeName}%
      \ETDContribKnowledgeText%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Contribution of Authors                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\SetContribAuthorsName}[1]{\renewcommand*{\ETDContribAuthorsName}{#1}}%
\newcommand*{\ETDContribAuthorsName}{ContribAuthorsMENTS}%

\newcommand*{\SetContribAuthorsText}[1]{\renewcommand*{\ETDContribAuthorsText}{#1}}%
\newcommand*{\ETDContribAuthorsText}{ContribAuthors text goes here!}%
\newcommand{\ContribAuthors}{%
    \begin{simpleenv}{}{}{}{}%
    \pagestyle{plain}%
    \setboolean{SetDSpace}{false}%
    \GoSingle%
    \begin{QZ@Cent}%
          \bfseries{\ETDContribAuthorsName} 
      \end{QZ@Cent}%
    \vspace*{0.25in}
      \par%
      \GoDouble%
      %\chapter*{\ETDContribAuthorsName}%
      \addcontentsline{toc}{extrachapter}{\ETDContribAuthorsName}%
      \ETDContribAuthorsText%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        Getting personal information and         %
%%          defining other miscellaneous commands  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*{\SetTitle}[1]{\renewcommand*{\Title}{#1}}%
\newcommand*{\Title}{No Title Given}%
\@onlypreamble{\SetTitle}%

\newcommand*{\SetAuthor}[1]{\renewcommand*{\FullName}{#1}}%
\newcommand*{\FullName}{Please Define Your Name}%
\@onlypreamble{\SetAuthor}%

\newcommand*{\SetThesisType}[1]{\renewcommand*{\ThesisType}{#1}}%
\newcommand*{\ThesisType}{THESIS OR DISSERTATION}%
\@onlypreamble{\SetThesisType}%

\newcommand*{\SetDegreeType}[1]{\renewcommand*{\DegreeType}{#1}}%
\newcommand*{\DegreeType}{UNDEFINED DEGREE}%
\@onlypreamble{\SetDegreeType}%

\newcommand*{\SetGradMonth}[1]{\renewcommand*{\GradMonth}{#1}}%
\newcommand*{\GradMonth}{UNDEFINED MONTH}%
\@onlypreamble{\SetGradMonth}%

\newcommand*{\SetGradYear}[1]{\renewcommand*{\GradYear}{#1}}%
\newcommand*{\GradYear}{UNDEFINED YEAR}%
\@onlypreamble{\SetGradYear}%

\newcommand*{\SetDepartment}[1]{\renewcommand*{\ETDDepartment}{#1}}%
\newcommand*{\ETDDepartment}{UNDEFINED DEPARTMENT}%
\@onlypreamble{\SetDepartment}%

\newcommand*{\SetChair}[1]{\renewcommand*{\Chair}{#1}}%
\newcommand*{\Chair}{UNDEFINED Chair}%
\@onlypreamble{\SetChair}%

\newcommand*{\SetUniversity}[1]{\renewcommand*{\ETDUniversity}{#1}}%
\newcommand*{\ETDUniversity}{McGill University}%
\@onlypreamble{\SetUniversity}%

\newcommand*{\SetUniversityAddr}[1]{\renewcommand*{\ETDUniversityAddr}{#1}}%
\newcommand*{\ETDUniversityAddr}{Montreal, Quebec}%
\@onlypreamble{\SetUniversityAddr}%

\newcommand*{\SetThesisDate}[1]{\renewcommand*{\ETDThesisDate}{#1}}%
\newcommand*{\ETDThesisDate}{Date111}%
\@onlypreamble{\SetThesisDate}%

\newcommand*{\SetRequirements}[1]{\renewcommand*{\ETDRequirements}{#1}}%
\newcommand*{\ETDRequirements}{Date222}%
\@onlypreamble{\SetRequirements}%

\newcommand*{\SetCopyright}[1]{\renewcommand*{\ETDCopyright}{#1}}%
\newcommand*{\ETDCopyright}{Date223333}%
\@onlypreamble{\SetCopyright}%


%%    Adding personal notes to your text
\newboolean{ShowNotes}% Used to display private notes.
\setboolean{ShowNotes}{false}%
\newcommand{\NOTE}[1]{%
    \ifthenelse{\boolean{ShowNotes}}{%
    \begin{spacing}{1}%
    \par\noindent\marginpar{NOTE}%
    \parbox[t]{\textwidth}{\hrulefill\\%
    #1\\*[-0.5\baselineskip]%
    \hspace*{0.1in}\hspace{-0.1in}\hrulefill\\}%
    \vspace{0.25in}\end{spacing}}{}%
    }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Footnotes                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\setlength{\footnotesep}{0.7\baselineskip}%
\addtolength{\footnotesep}{\BaseDiff\baselineskip}%
\setlength{\skip\footins}{2.5\baselineskip}%
\renewcommand{\footnoterule}{%
   \kern -3\p@
   \kern +8pt
   \hrule \@width 2in
   \kern -8pt
   \kern +2.6\p@}
\renewcommand{\@makefnmark}{%
  \hbox{\@textsuperscript{\normalfont\@thefnmark}}~}%
\renewcommand\@makefntext[1]{%
    \normalsize%
    \parindent 1em%
    \noindent
    \hb@xt@1.8em{\hss\@makefnmark}#1}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Figures and Tables                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\renewcommand{\thefigure}{%
  {\ifnum \c@chapter>\z@ \thechapter--\fi \@arabic\c@figure}}%
\renewcommand{\thetable}{%
  {\ifnum \c@chapter>\z@ \thechapter--\fi \@arabic\c@table}}%
\newlength{\abovefigcaptskip}%
\newlength{\belowfigcaptskip}%
\newlength{\abovetabcaptskip}%
\newlength{\belowtabcaptskip}%
\setlength{\abovefigcaptskip}{\abovecaptionskip}%
\setlength{\belowfigcaptskip}{\belowcaptionskip}%
\setlength{\abovetabcaptskip}{\belowcaptionskip}%
\setlength{\belowtabcaptskip}{\abovecaptionskip}%
\renewenvironment{table}
     {%\setlength{\abovecaptionskip}{\abovetabcaptskip}%
      \setlength{\belowcaptionskip}{\belowtabcaptskip}%
      \@float{table}}
     {\end@float}
\renewenvironment{figure}
     {%\setlength{\abovecaptionskip}{\abovefigcaptskip}%
      \setlength{\belowcaptionskip}{\belowfigcaptskip}%
      \@float{figure}}
     {\end@float}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  McGill ETD Multiple Index Generation           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\printindex}[4][\jobname]{%
    \IfFileExists{#1.ind}{%
    \setboolean{SetDSpace}{true}%
    \begin{simpleenv}{#1}{#2}{#3}{#4}%
      \pagestyle{plain} %
      \addcontentsline{toc}{extrachapter}{#2}%
      \chapter*{\bf{#3}}{\GoDouble{#4}\par\GoSingle}%
      \input{#1.ind}%
    \end{simpleenv}%
    \setboolean{SetDSpace}{true}}
    {\typeout{No file #1.ind found.}}}%
\newcommand{\see}[2]{\emph{see} #1}%
\renewcommand{\makeindex}[1][\jobname]{\begingroup
  \makeatletter
  \if@filesw \expandafter\newwrite\csname #1@idxfile\endcsname
  \expandafter\immediate\openout \csname #1@idxfile\endcsname #1.idx\relax
  \typeout{Writing index file #1.idx }\fi \endgroup}
\@onlypreamble{\makeindex}%
\renewcommand{\index}[2][\jobname]{\@bsphack\begingroup
               \def\protect##1{\string##1\space}\@sanitize
               \@wrindex{#1}{#2}}
\renewcommand{\@wrindex}[2]{\let\thepage\relax
   \xdef\@gtempa{\@ifundefined{#1@idxfile}{}{\expandafter
      \write\csname #1@idxfile\endcsname{\string
      \indexentry{#2|RSpset}{\thepage}}}}\endgroup\@gtempa
   \if@nobreak \ifvmode\nobreak\fi\fi\@esphack}
\newcommand{\indxspace}{\par\vspace{\BaseDiff\baselineskip}}
\newcommand{\IndexSet}{%
\renewcommand{\@idxitem}{\par\setlength{\leftskip}{0pt}%
                         \setlength{\hangindent}{\QZ@TOChdent}}%
\renewcommand{\subitem}{\par\setlength{\leftskip}{0.25in}%
                         \setlength{\hangindent}{\QZ@TOChdent}}%
\renewcommand{\subsubitem}{\par\setlength{\leftskip}{0.5in}%
                         \setlength{\hangindent}{\QZ@TOChdent}}%
\renewcommand{\indexspace}{}
\renewcommand{\indxspace}{\par\vspace{\BaseDiff\baselineskip}}
\renewenvironment{theindex}{%
                \setlength{\parindent}{\z@}%
                \parskip\z@ \@plus .3\p@\relax
                \setlength{\rightskip}{\@tocrmarg}%
                \setlength{\parfillskip}{-\rightskip}%
                \let\item\@idxitem}
} %% End of the IndexSet definition
\newcommand{\RSpnum}[1]{\makebox[\@pnumwidth][r]{#1}}
\newcommand{\RSnopnum}[1]{\makebox[\@pnumwidth][r]{}}
\newcommand{\RSpset}[1]{\RSpnum{#1}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Fixing the enumerate and itemize environments  %
%%  by updating the general list parameters.       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\renewcommand*{\@listi}{\leftmargin\leftmargini
      \setlength{\topsep}{0in}%
      \setlength{\partopsep}{0in}%
      \setlength{\parsep}{0in}%
      \setlength{\itemsep}{0in}}
\let\@listI\@listi
\@listi
\renewcommand*{\@listii}{\leftmargin\leftmarginii
      \labelwidth\leftmarginii
      \advance\labelwidth-\labelsep
      \setlength{\topsep}{0in}%
      \setlength{\parsep}{0in}%
      \setlength{\itemsep}{\parsep}}
\renewcommand*{\@listiii}{\leftmargin\leftmarginiii
      \labelwidth\leftmarginiii
      \advance\labelwidth-\labelsep
      \setlength{\topsep}{0in}%
      \setlength{\parsep}{0in}%
      \setlength{\partopsep}{0in}%
      \setlength{\itemsep}{\parsep}}
\ifthenelse{\boolean{QZ@CPage}}{%
  \renewcommand{\ps@myheadings}{%
      \let\@oddfoot\@empty\let\@evenfoot\@empty
      \def\@evenhead{\hfil\thepage\hfil}%
      \def\@oddhead{\hfil\thepage\hfil}%
      \let\@mkboth\@gobbletwo
      \let\chaptermark\@gobble
      \let\sectionmark\@gobble
      }
  }{}

%% Disabling some unused commands
\renewcommand{\author}{}
\renewcommand{\date}{}
%\renewcommand{\emph}{}
%\renewcommand{\bfseries}{}
%\renewcommand{\underline}{}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  ETD Thesis Local Configuration                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\InputIfFileExists{mcgilletdclass.cfg}{%
\typeout{***************************************}
\typeout{* Local config file mcgilletdclass.cfg used *}
\typeout{***************************************}}{}
\typeout{ }
\typeout{*****************************************}
\typeout{*\space \space Finished with Thesis Template}
\typeout{*****************************************}
\typeout{ }
\AtBeginDocument{%
\typeout{**************************************}
\typeout{* Executing Begin Document Statement *}
\typeout{**************************************}
}
\endinput
%%
%% End of file `mcgilletdclass.cls'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
%
%% ===========================================================================
%% Removing 3 minor commands left-over by the setspace 2000/12/01 6.7 package
%% ---------------------------------------------------------------------------
%\providecommand{\filename}{filenametext}%
%\providecommand{\filedate}{filedatetext}%
%\providecommand{\fileversion}{filevertext}%
%\renewcommand{\filename}{}%
%\renewcommand{\filedate}{}%
%\renewcommand{\fileversion}{}%