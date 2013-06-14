%%                                           %
 %    bmc_article.cls             ver: 1.03  %
 %                                           %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %                                          %%
 %    A Style Class based on article for    %%  
 %     formating towards BioMed Central     %%
 %               guidelines                 %%
 %                                          %%
 %           <1 September 2003>             %%
 %                                          %%
 %                                          %%
 %                                          %%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % 
 % See Readme.txt for ToDo list.
%%
%

\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{bmc_article}[2003/09/01 v1.01 BioMed Central article class]
\LoadClassWithOptions{article} 

\renewcommand\@biblabel[1]{#1.}
\def\hi{}
\def\pb{\\*[0.42cm]}


\AtBeginDocument{}

\def\@abstractline{\textwidth}

\newcounter{bmc@correspondingauthor} 
\setcounter{bmc@correspondingauthor}{0} %never changes
\newcounter{bmc@emailcounter} \setcounter{bmc@emailcounter}{0}


% Corresponding Authors.
% adds an asterisk * as a marker.
\def\correspondingauthor{$^*$}
\def\@corresponding{\footnotesize\correspondingauthor Corresponding author} 


% Email.
% A variable to store names and emails.
\gdef\bmc@email{Email: }
\newcommand\bmc@curname{}
\newcommand\email[1]{\relax
    \g@addto@macro\bmc@email{#1; }} % has trailing ;

\def\@bmc@email{\relax\footnotesize{\bmc@email}}  

% Address
\def\address#1{ \def\@address{\begin{hi}\footnotesize#1\end{hi}}}
\def\iid(#1){\hi$^#1$}

% Thanks
\def\thempfootnote{\normalfont\@arabic\c@mpfootnote}
\def\thanks#1{%
  \stepcounter{footnote}%
  \hbox{\@textsuperscript{\normalfont\thefootnote}}%
    \protected@xdef\@thanks{\@thanks%
        \protect\footnotetext[\the\c@footnote]{#1}}%
  }
 
% Maketitle
\let\old@maketitle\maketitle
\renewcommand\maketitle{  
   \begin{flushleft}\mbox{
    \global\let\@date\@empty % @date = null. 
    {\sffamily\begin{minipage}{\textwidth}%
             \@maketitle                                           
             {\raggedright%
                {\noindent\@address}\\ \hbox{} 
                {\noindent\@bmc@email}\\ \hbox{}
                {\noindent\@corresponding}%
             }%end \raggedright  
      \end{minipage}%
      }
    \renewcommand\thefootnote{\old@thefootnote}  %
    } % \fbox
    \end{flushleft}
}

\def\and{, }

\def\@maketitle{%
  \newpage
  \null
  \vskip 2em%
    {\noindent\LARGE \bfseries\@title \par}%
    \vskip 1.5em%
    {\large
      %\lineskip .5em%
        \noindent\@author
      \par}%
    \vskip 1em%
  \par
  \vskip 1.5em}


% reformats abstract to BMC looks 
\let\old@abstract\abstract 
\renewenvironment{abstract}                  
   {  
      \ifhmode\begingroup\parskip0pt\par\noindent\endgroup\fi%
      \begin{sffamily}                        
         \vspace{1cm} % use \topsep in list
            {\noindent\bfseries\large\abstractname\vspace{-0.5em}}
				{\par\vbox{}} 
     }{                                      
            \ifhmode\begingroup\parskip0pt\par\noindent\endgroup\fi % 
      \end{sffamily}                         
   }



%% \@startsection{sec-name}{level}{indent}{pre-skip} 
%%               {post-skip}{style}*[srt title]{title}
\renewcommand\section{\@startsection {section}{1}{\z@}%
                     {-0.84cm \@plus -1ex \@minus -.2ex} {.2ex \@plus.2ex} %%
                     {\normalfont\large\bfseries\sffamily}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                     {-0.84cm \@plus -1ex \@minus -.2ex} {.2ex \@plus.2ex} %%
                     {\normalfont\normalsize\bfseries\sffamily}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                     {-0.84cm \@plus -1ex \@minus -.2ex} {.2ex \@plus.2ex} %%
                     {\normalfont\normalsize\bfseries\itshape}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                     {-.2em \@plus -1ex \@minus -.2ex} {-0.5em} %%
                     {\normalfont\small\bfseries\sffamily}}
\renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                     {-.2em \@plus -1ex \@minus -.2ex} {-0.5em} %%
                     {\normalfont\normalsize\sffamily}}

\endinput
%% end of bmc_article.cls
