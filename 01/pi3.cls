% -----------------------------------------------------------------------
% pi3.cls
% Based on pi1-muster.tex and defs.tex used before 2003 for
% PI 1 in Bremen.
% Adapted by Diedrich Wolter, Thomas Röfer, Christoph Lüth and Klaus Hartke.
% Adapted again by Jasper van de Ven (only minor changes, most of the work was done by the persons that were mentioned earlier).

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{pi3}[2011/11/01 $Revision: 1.8 $  PI3 document class]
\DeclareOption{Uebungsblatt}{\def\uebungsblatt}
\DeclareOption{Loesungsblatt}{\def\loesungsblatt}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{scrartcl}}
\ProcessOptions\relax
\LoadClass[a4paper,twoside,BCOR=0mm]{scrartcl}

\usepackage{ngerman}
\usepackage[utf8]{inputenc}
\usepackage{amssymb}
\usepackage{amstext}
\usepackage{graphicx}
\usepackage{moreverb}
\usepackage{fancyhdr}
\usepackage{xcolor}
\usepackage{listings}

%%% -------------------------------------------------
%%% 
%%%                Allgemeine Definitionen
%%%
%%% -------------------------------------------------

\pagestyle{fancy}
\thispagestyle{empty}
\setlength{\parindent}{0ex}
\setlength{\parskip}{1ex}
\def\thismodule{\jobname}

%%% -------------------------------------------------
%%% 
%%%   Allgemeine DefinitionenHaskell-definition für listings
%%%
%%% -------------------------------------------------

\lstdefinelanguage{HaskellPI3Auf}%
   {otherkeywords={=>},%
   morekeywords={abstype,then,else,case,class,data,default,deriving,%
      hiding,if,infix,infixl,infixr,import,instance,let,module,%
      newtype,of,qualified,type,where,do,in,as},%
   sensitive,%
   literate={+}{{$+$}}1 {++}{{$+\!+$}}2
           {=>}{{$\Rightarrow$}}2 
           {\\}{{$\lambda$}}1
           {tau}{{$\tau$}}1 {sigma}{{$\sigma$}}1
           {alpha}{{$\alpha$}}1 {beta}{{$\beta$}}1
           {gamma}{{$\gamma$}}1 {delta}{{$\delta$}}1
           {-}{{$-$}}1 {/}{{$/$}}1 {*}{{$*$}}1
           {>}{{$>$}}1 {<}{{$<$}}1 {/=}{{$\not=$}}1
           {>>}{{$\bindo$}}2 {>>=}{{$\bind$}}2
           {<|}{{$\triangleleft$}}1 {<|>}{{$\langle|\rangle$}}2
           {<.}{{$\langle\circ$}}2 {.>}{{$\uparrow\rangle$}}2
           {<.>}{{$\langle\circ\rangle$}}2 {<^>}{{$\langle\uparrow\rangle$}}2
           {->}{{$\rightarrow$}}2 {>=}{{$\ge$}}2 {<-}{{$\leftarrow$}}2
           {<=}{{$\le$}}2 {<==>}{{$\Longleftrightarrow$}}3
           {\ .\ }{{ $\circ$ }}3
           {(.)}{{($\circ$)}}3,
   morecomment=[l]--,%
   morecomment=[n]{\{-}{-\}},%
   morestring=[b]"%
  }[keywords,comments,strings]%
% General language-independent style for listings
\lstset{%
  keywordstyle=\bfseries\color{blue!70!black},
  identifierstyle=\slshape,
  commentstyle=\color{green!30!black},
  texcl=true,
  % language=HaskellPI3,
  flexiblecolumns=false,
  basewidth={0.5em,0.45em},
  extendedchars=false,
  numbers=none %,
}
\lstnewenvironment{code}{\lstset{language=HaskellPI3Auf,basicstyle=\small}}{}
\lstnewenvironment{xcode}{\lstset{language=HaskellPI3Auf,basicstyle=\small}}{}
\def\hs{\lstinline[language=HaskellPI3Auf]}

\def\haskellcode{\@ifnextchar [{\@haskelllines}{\@haskellcode}}
\def\@haskelllines[#1]#2{\def\thismodule{#1}\lstinputlisting[linerange={#1},language=HaskellPI3Auf]{#2}}
\def\@haskellcode#1{\def\thismodule{#1}\lstinputlisting[numbers=left, numberstyle=\tiny, numbersep=5pt,language=HaskellPI3Auf]{#1.hs}}

\renewcommand\include[1]{\def\thismodule{#1}\input{#1.lhs}}

%%%    einige mathematische Definitionen

\newcommand{\tup}[1]{\langle #1 \rangle}
\newcommand{\Nat}{\mathbb{N}}
\newcommand{\qed}{\hfill$\Box$}
\newtheorem{theorem}{Theorem}
\newenvironment{proof}{\medskip\noindent\textit{Proof.}}{\par}
\newcommand{\grenzop}[3]{\mathop{#1}\limits^{#2}_{#3}}
\newcommand{\langrechtspfeil}[2]{\grenzop{\longrightarrow}{#1}{#2}}
\newcommand{\bob}[0]{\hspace*{\fill}$\Box$}

%% Daten für den Titel als Befehle deklariern
\newcommand{\tuts}{}
\newcommand{\Tutoren}[1]{\renewcommand{\tuts}{#1}}

\newcommand{\dasblatt}{}
\newcommand{\ausgabe}{}
\newcommand{\abgabe}{}
\newcommand{\Blatt}[3]{\renewcommand{\dasblatt}{#1}\renewcommand{\@ausgabe}{#2}\renewcommand{\@abgabe}{#3}}
\newcommand{\blatt}[1]{\renewcommand{\dasblatt}{#1}}

\newcommand{\derkurs}{\textbf{Praktische Informatik 3}}
\newcommand{\Kurs}[1]{\renewcommand{\derkurs}{#1}}

%%%
%%% -------------------------------------------------
%%%


%%% -------------------------------------------------
%%% 
%%%                Abgaben
%%%
%%% -------------------------------------------------


%% Fuer die Studis bereits Kursinfos vordefinieren:
%%
\newcommand\thissemester{WS 2013/14}
\newcommand\thistitle{\textbf{Praktische Informatik 3}}
\newcommand\thisshorttitle{PI3}
%%
%%

%% Tutor, Gruppe, Teilnehmer, Semester:
\gdef\@tutor{\ClassError{pi3}{Kein Tutor angegeben}
                        {Bitte mit \@backslashchar tutor{} den Namen des Tutors angeben.}}
\gdef\@uebungsgruppe{\ClassError{pi3}{Kein Uebungsgruppe angegeben}
                        {Bitte mit \@backslashchar uebungsgruppe{} die Nummer der Uebungsgruppe angeben.}}
\gdef\@teilnehmer{\ClassError{pi3}{Keine Teilnehmer angegeben}
                        {Bitte mit \@backslashchar teilnehmer{} Namen der Teilnehmer der Uebungsgruppe angeben.}}
\newcommand{\tutor}[1]{\gdef\@tutor{#1}}
\newcommand{\uebungsgruppe}[1]{\gdef\@uebungsgruppe{#1}}
\newcommand{\teilnehmer}[1]{\gdef\@teilnehmer{#1}}

\ifx\loesungsblatt\undefined
\ifx\uebungsblatt\undefined
\renewcommand\thesection{Aufgabe \dasblatt.\arabic{section}}
% \renewcommand\labelenumi{\alph{enumi})}
\renewcommand{\@maketitle}{%
    \begin{tabular*}{5cm}{@{}lr@{}}
      \begin{tabular}[t]{@{}l@{}}
        \begin{tabular}[t]{@{}l@{\hskip 20mm}l}
            \thistitle & \thissemester \hspace{4ex} \\
            Tutor: {\emph{\@tutor}}  &  Gruppe: {\@uebungsgruppe} \\
        \end{tabular} \vspace{5mm} \\
        {\Huge\sf Bearbeitung des \dasblatt.\"Ubungsblatts} \\[4pt]
        % {\Large\sf L\"osungsvorschlag} \\[1ex]
        %{\Large Abgabe: {#2}} \\[1ex]
        \hfill
      \end{tabular}
      & \hfill
      \begin{tabular}[t]{l}
      \@teilnehmer    
      \end{tabular} \\[2ex]
    \end{tabular*}\\
    \rule{\linewidth}{1pt}

% -----------------------------------------------------------------------
% Create headings (Thanks to Elmar Loos)
}
    \fancyhead{} %clear all fields
    \fancyhead[LO,RE]{PI 3, \@semester{}}
    \fancyhead[RO,LE]{Tutor: {\@tutor}, Gruppe: \@uebungsgruppe}
    \fancyfoot{} %clear all fields
    \fancyfoot[RO,LE]{\thepage}
\fi
\fi

%%% -------------------------------------------------
%%% 
%%%                Loesungsblaetter
%%%
%%% -------------------------------------------------
%%% 

\ifx\loesungsblatt\undefined
\else

\renewcommand\thesection{Aufgabe \dasblatt.\arabic{section}}
\renewcommand{\maketitle}{%
$\!\!\!$% eh?!
\begin{tabular}[t]{l}
\derkurs \\[.5ex]
{\LARGE\sf {\dasblatt}. "Ubungsblatt} \\[0.5ex]
{\LARGE \sf {L"osung}}\\
\end{tabular} %%
\hfill %%
\tuts \\%
\rule{\textwidth}{1pt}}

\fi

%%% -------------------------------------------------
%%%

%%% -------------------------------------------------
%%% 
%%%                Aufgabenblaetter
%%%
%%% -------------------------------------------------

\ifx\uebungsblatt\undefined
\else

% new exercise. args are the name, and the credits.
\newcounter{excnt}
\newcommand{\teilaufgabe}[2]{%
 \medskip\stepcounter{excnt} %
 {\fbox{\Large\dasblatt.\arabic{excnt}} %
 {\large \sf{#1} \hfill %
         {({#2} Punkte)}}}\\}


% bonus exercise 
% \newcommand{\optex}[2]{%
%  \stepcounter{excnt} %
%  {\bfseries\fbox{\Large\@dasblatt.\arabic{excnt}} %
%  {\large \sf\bfseries{#1}\hfill}\textit{{#2} Bonuspunkte}\\}}

% solution to an exercise. arg is the name.
\newcommand{\solution}[1]
{\stepcounter{excnt} %
 \fbox{\Large\dasblatt.\arabic{excnt}} %
 {\large \textit{#1}}\\}

% points for parts
\newcommand{\Teilpunkte}[1]{\hfill(\textit{#1 Punkte})}
      
\gdef\@ausgabe{\ClassError{pi3}{Kein Ausgabedatum abgegeben}
                        {Bitte mit \@backslashchar ausgabe{} das Ausgabedatum angeben.}}
\gdef\@abgabe{\ClassError{pi3}{Kein Abgabedatum angegeben}
                        {Bitte mit \@backslashchar abgabe{} das Abgabedatum angeben.}}
\renewcommand{\ausgabe}[1]{\gdef\@ausgabe{#1}}
\renewcommand{\abgabe}[1]{\gdef\@abgabe{#1}}

% Header. Args are the current number, and the date
\newcommand{\Uebblatt}[1]{% Wird vom Uebungsblatt (s.u.) auch verwendet
$\!\!\!$% eh?!
\begin{tabular}[t]{l}
\derkurs \\[.5ex]
{\LARGE\sf {\dasblatt}. {#1}} \\[0.5ex]
{\large Ausgabe: \@ausgabe} \\[.5ex]
{\large Abgabe: \@abgabe} \\
\end{tabular} %%
\hfill %%
\tuts \\%
\rule{\textwidth}{1pt}}

\renewcommand\labelenumii{-}
\renewcommand{\maketitle}{\Uebblatt{"Ubungsblatt}}

\setcounter{excnt}{0}

\fi

%%% 
%%% -------------------------------------------------
%%%


\endinput
