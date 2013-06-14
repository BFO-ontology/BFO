%%                                        %
 % bmc_article.bst              ver: 1.01 %
 %                                        %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%                                      %%
 %%  BibTeX BST file for BioMed Central  %%
 %%    a style syntax for latex .bib     %%
 %%           bibliographies             %%
 %%                                      %%
 %%         <1 September 2003>           %%
 %%                                      %%
 %%                                      %%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % 
 % 
 % The order in the reference list is that by which the 
 % works were originally cited in the text, or that in 
 % the database.
 %
 %
 % This file is based on the style 'unsrt.bst'
 % ------------------------------------------ 
 % BibTeX standard bibliography style `unsrt'
 % version 0.99a for BibTeX versions 0.99a
 % or later, LaTeX version 2.09.
 % Copyright (C) 1985, all rights reserved.
 % Copying of this file is authorized only if either
 % (1) you make absolutely no changes to your copy, including name, or
 % (2) if you do make changes, you name it something other than
 % btxbst.doc, plain.bst, unsrt.bst, alpha.bst, and abbrv.bst.
 % This restriction helps ensure that all standard styles are identical.
 % The file btxbst.doc has the documentation for this style.
 % --------------------------------------------------------- 
%%

 


ENTRY
  { address
    archive
    author
    booktitle
    chapter
    edition
    editor
    eid
    howpublished
    institution
    journal
    key
    month
    note
    number
    organization
    pages
    publisher
    school
    series
    title
    type
    url
    volume
    year
  }
  {}
  { label }


% Global variables - most initialised and used locally.
STRINGS { longest.label }
STRINGS { cur tag source bibinfo}
STRINGS { p q r s t z name names }
STRINGS { lr lp lt ls}   % local copys fxns with these call no other fxns.
INTEGERS { number.label longest.label.width }
INTEGERS { multiresult nameptr namesleft numnames global.counter}
INTEGERS { i j k l ll li}
INTEGERS { output.state before.all mid.sentence after.sentence after.block }

FUNCTION {init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
  #0 'global.counter :=
}


% Logical operators on integers
FUNCTION {not}
{   { #0 } { #1 } if$ 
}
FUNCTION {and}
{   'skip$ { pop$ #0 } if$
}
FUNCTION {or}
{   { pop$ #1 } 'skip$ if$
}
FUNCTION {field.or.null}
{ duplicate$ empty$ { pop$ "" } 'skip$ if$
}

FUNCTION {remove.dots}
{ 'z :=
  ""
  { z empty$ not }
  { z #1 #1 substring$
    z #2 global.max$ substring$ 'z :=
    duplicate$ "." = 'pop$
      { * }
    if$
  }
  while$
}

%%
 % myreverse
 % 
 % Takes 1 string
 % Returns the myreverse string
 % 
 % not to be confused with REVERSE (opposite of ITERATE)
%%
FUNCTION {myreverse}
{
  'lt :=
  "" 'ls :=
  { lt empty$ not }
  { 
    lt #1 #1 substring$ ls * 'ls :=  
    lt #2 global.max$ substring$ 'lt :=
  }
  while$ 
  ls
}

%%
 % search 
 %
 % Takes 2 strings (txt, pattern)
 % Retruns 1 if found 0 if not 
%% 
FUNCTION {search}
{ 
  'lp :=     % pattern 
  'lt :=     % text to search
  #0 'i :=  % result  
  lp text.length$ 'll := % length of the search pattern
  { lt empty$ not }
  {  lt #1 ll substring$ lp = 
      { #1 'li :=  
        "" 'lt := } % force exit
      { lt #2 global.max$ substring$ 'lt := } % pop 1 char
    if$
  }
  while$ 
  li
}


%%
 % general replace
 %
 % Takes 3 strings (txt, pattern, replace-str)
 % replaces all instances of pattern
 % Retruns a new string
%% 
FUNCTION {replace}
{ 
  'lr :=     % replace string
  'lp :=     % pattern 
  'lt :=     % text to search
  "" 'ls :=  % result string 
  lp text.length$ 'll := % length of the search pattern
  { lt empty$ not }
  {  lt #1 ll substring$ lp = 
      { ls lr * 'ls :=  
        lt ll #1 + global.max$ substring$ 'lt := } 
      { ls lt #1 #1 substring$ * 'ls :=  
        lt #2 global.max$ substring$ 'lt := } 
    if$
  }
  while$ 
  ls
}


%%
 % strip.letters
 %
 % Takes 1 arg (string)
 % if string has letters get rid of them
 %  - useful for 2nd -> 2
 % Returns string
%% 
FUNCTION {strip.letters}
{ 
  "" 's :=
  duplicate$ missing$
    'pop$
    { 
      't := 
       { t "" = not }
       { % ascii '0' = 48,  '9' = 57  
         t #1 #1 substring$ chr.to.int$ 'i :=  
         i #47 > i #58 < and 
           { s t #1 #1 substring$ * 's := } 
           'skip$ 
         if$
         t #2 global.max$ substring$ 't :=
       }
       while$
    }
  if$
  s
}

FUNCTION {output.nonnull}
{ 's :=
  output.state mid.sentence =
    { ", " * write$ }
    { output.state after.block =
        { add.period$ write$
          newline$
          "\newblock " write$
        }
        { output.state before.all =
            %{ "OS=(" * output.state int.to.str$ * ") " * write$ }
            'write$
            { add.period$ " " * write$ } % after.sentence
          if$
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}

FUNCTION {output}
{ duplicate$ empty$
    'pop$
    'output.nonnull
  if$
}

% raises an error (warning message) if type not present.
FUNCTION {output.check}
{ 't :=
  duplicate$ empty$
    { pop$ "empty " t * " in " * cite$ * warning$ }
    'output.nonnull
  if$
}

FUNCTION {fin.entry}
{ add.period$
  write$
  newline$
}

FUNCTION {new.block}
{ output.state before.all =
    'skip$
    { after.block 'output.state := }
  if$
}
FUNCTION {new.sentence}
{ output.state after.block =
    'skip$
    { output.state before.all =
        'skip$
        { after.sentence 'output.state := }
      if$
    }
  if$
}
FUNCTION {add.blank}
{  " " * before.all 'output.state :=
}

FUNCTION {add.bold.colon}
{ duplicate$ empty$
    'skip$
    { "\textbf{:}" * add.blank }
  if$
}

FUNCTION {add.colon}
{ duplicate$ empty$
    'skip$
    { ":" * add.blank }
  if$
}
FUNCTION {bold}
{ duplicate$ empty$
    { pop$ "" }
    { "\textbf{" swap$ * "}" * }
  if$
}
FUNCTION {emphasize}
{ duplicate$ empty$
    { pop$ "" }
    { "\emph{" swap$ * "}" * }
  if$
}
FUNCTION {tie.or.space.prefix}
{ duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$
}

FUNCTION {capitalize}
{ "u" change.case$ "t" change.case$ }

FUNCTION {space.word}
{ " " swap$ * " " * }

% if field not entered - push empty string ""
FUNCTION {bibinfo.check}
{ swap$
  duplicate$ missing$
    { pop$ pop$
      "" }
    { duplicate$ empty$
        { swap$ pop$ }
        { swap$ pop$ }
      if$
    }
  if$
}
FUNCTION {bibinfo.warn}
{ swap$
  duplicate$ missing$
    {
      swap$ "missing " swap$ * " in " * cite$ * warning$ pop$
      ""
    }
    { duplicate$ empty$
        {
          swap$ "empty " swap$ * " in " * cite$ * warning$
        }
        { swap$
          pop$
        }
      if$
    }
  if$
}

FUNCTION {format.thesis.type}
{ type duplicate$ empty$
    'pop$
    { swap$ pop$
      "t" change.case$ "type" bibinfo.check
    }
  if$
}


 % Here are the language-specific definitions for explicit words.
 % Each function has a name bbl.xxx where xxx is the English word.
 % The language selected here is ENGLISH
FUNCTION {bbl.and}
{ "and"}

FUNCTION {bbl.etal}
{ "et~al." }

FUNCTION {bbl.editors}
{ "(Eds)" }
%{ "editors" }

FUNCTION {bbl.editor}
{ "(Ed)" }
%{ "editor" }

FUNCTION {bbl.edby}
{ "Edited by" }

FUNCTION {bbl.edition}
{ "edition" }
%{ "edn." }

FUNCTION {bbl.volume}
{ "Volume" }
%{ "vol." }

FUNCTION {bbl.of}
{ "of" }

FUNCTION {bbl.number}
{ "no." }

FUNCTION {bbl.nr}
{ "no." }

FUNCTION {bbl.in}
{ "in" }

FUNCTION {bbl.pages}
{ "" }

FUNCTION {bbl.page}
{ "" }

FUNCTION {bbl.chapter}
{ "chap." }

FUNCTION {bbl.techrep}
{ "Tech. Rep." }

FUNCTION {bbl.mthesis}
{ "Master's thesis" }

FUNCTION {bbl.phdthesis}
{ "PhD thesis" }

MACRO {jan} {"Jan."}

MACRO {feb} {"Feb."}

MACRO {mar} {"Mar."}

MACRO {apr} {"Apr."}

MACRO {may} {"May"}

MACRO {jun} {"Jun."}

MACRO {jul} {"Jul."}

MACRO {aug} {"Aug."}

MACRO {sep} {"Sep."}

MACRO {oct} {"Oct."}

MACRO {nov} {"Nov."}

MACRO {dec} {"Dec."}

MACRO {acmcs} {"ACM Computing Surveys"}

MACRO {acta} {"Acta Informatica"}

MACRO {cacm} {"Communications of the ACM"}

MACRO {ibmjrd} {"IBM Journal of Research and Development"}

MACRO {ibmsj} {"IBM Systems Journal"}

MACRO {ieeese} {"IEEE Transactions on Software Engineering"}

MACRO {ieeetc} {"IEEE Transactions on Computers"}

MACRO {ieeetcad}
 {"IEEE Transactions on Computer-Aided Design of Integrated Circuits"}

MACRO {ipl} {"Information Processing Letters"}

MACRO {jacm} {"Journal of the ACM"}

MACRO {jcss} {"Journal of Computer and System Sciences"}

MACRO {scp} {"Science of Computer Programming"}

MACRO {sicomp} {"SIAM Journal on Computing"}

MACRO {tocs} {"ACM Transactions on Computer Systems"}

MACRO {tods} {"ACM Transactions on Database Systems"}

MACRO {tog} {"ACM Transactions on Graphics"}

MACRO {toms} {"ACM Transactions on Mathematical Software"}

MACRO {toois} {"ACM Transactions on Office Information Systems"}

MACRO {toplas} {"ACM Transactions on Programming Languages and Systems"}

MACRO {tcs} {"Theoretical Computer Science"}


% Takes 2 args (author field) and string "author"
FUNCTION {format.names}
{ 'bibinfo :=
  duplicate$ empty$ 'skip$ {
  's :=
  "" 't :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
    "{vv~}{ll}{ f{}}{ jj}"
      format.name$
      remove.dots
      bibinfo bibinfo.check
      't :=
      nameptr #1 >
        {
          namesleft #1 >
            { ", " * t * }
            {
              "," *
              s nameptr "{ll}" format.name$ duplicate$ "others" =
                { 't := }
                { pop$ }
              if$
              t "others" =
                {
                  " " * bbl.etal *
                }
                { " " * t * }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
  } if$
}


%"{f{~}~}{vv~}{ll}{ jj}"
FUNCTION {format.names.ed}
{
  'bibinfo :=
  duplicate$ empty$ 'skip$ {
  's :=
  "" 't :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      "{vv~}{ll}{ f{}}{ jj}"
      format.name$
      remove.dots
      bibinfo bibinfo.check
      't :=
      nameptr #1 >
        {
          namesleft #1 >
            { ", " * t * }
            {
              "," *
              s nameptr "{ll}" format.name$ duplicate$ "others" =
                { 't := }
                { pop$ }
              if$
              t "others" =
                {

                  " " * bbl.etal *
                }
                { " " * t * }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
  } if$
}
FUNCTION {format.authors}
{ author "author" format.names
}
FUNCTION {get.bbl.editor}
{ editor num.names$ #1 > 'bbl.editors 'bbl.editor if$ }

FUNCTION {format.editors}
{ editor "editor" format.names duplicate$ empty$ 'skip$
    {
      " " * get.bbl.editor *
%%       "," * " " * get.bbl.editor *
    }
  if$
}
FUNCTION {format.note}
{ 
  url empty$
    'skip$
    { 
        "\urlprefix\url{[" url * "]}" * output }
  if$
   note empty$
     { "" }
     {  
      note "l" change.case$ "in press" = 
        { after.sentence 'output.state :=
          " in press" }
        {
          after.sentence 'output.state :=
          note #1 #1 substring$
          duplicate$ "{" =
            'skip$
            { output.state mid.sentence =
                { "l" }
                { "u" }
              if$
              change.case$ % leave note * As Is *
            }
          if$ 
          note #2 global.max$ substring$ * "note" bibinfo.check
          "[" swap$ * "]" *
        }
      if$
    }
  if$
}

FUNCTION {bold.format.title}
{ title
  "title" bibinfo.check
  duplicate$ empty$ 'skip$
    {
      bold
    }
  if$
}

FUNCTION {emph.format.title}
{ title
  "title" bibinfo.check
  duplicate$ empty$ 'skip$
    {
      emphasize
    }
  if$
}

% Returns ""
FUNCTION {output.bibitem}
{ newline$
  "\bibitem{" write$
  cite$ write$
  "}" write$
  newline$
  ""
  before.all 'output.state :=
}

FUNCTION {output.allinfo.comment}
{ } 

FUNCTION {n.dashify}
{
  't :=
  ""
    { t empty$ not }
    { t #1 #1 substring$ "-" =
        { t #1 #2 substring$ "--" = not
            { "--" *
              t #2 global.max$ substring$ 't :=
            }
            {   { t #1 #1 substring$ "-" = }
                { "-" *
                  t #2 global.max$ substring$ 't :=
                }
              while$
            }
          if$
        }
        { t #1 #1 substring$ *
          t #2 global.max$ substring$ 't :=
        }
      if$
    }
  while$
}

FUNCTION {word.in}
{ bbl.in capitalize
  " " * }

FUNCTION {format.date}
{
  before.all 'output.state :=
  ""
  duplicate$ empty$
  year  "year"  bibinfo.check duplicate$ empty$
    { swap$ 'skip$
        { "there's a month but no year in " cite$ * warning$ }
      if$
      *
    }
    { swap$ 'skip$
        {
          swap$
          " " * swap$
        }
      if$
      *
      remove.dots
    }
  if$

  duplicate$ "" =
    'skip$
    { " " swap$ * }
  if$
}

% Source = book, conference, journal, manual 
% have the source avaliable on the stack
FUNCTION {format.source}
{ 'source := 
  source "source" bibinfo.check
  duplicate$ empty$ 'skip$
    {
      emphasize
    }
  if$
}

FUNCTION {format.title}
{ title "title" bibinfo.check
  duplicate$ empty$ 'skip$
    {
      bold
    }
  if$
}



FUNCTION {either.or.check}
{ empty$
    'pop$
    { "can't use both " swap$ * " fields in " * cite$ * warning$ }
  if$
}
FUNCTION {format.bvolume}
{ volume empty$
    { "" }
    { bbl.volume volume tie.or.space.prefix
      "volume" bibinfo.check * *
      series "series" bibinfo.check
      duplicate$ empty$ 'pop$
        { swap$ bbl.of space.word * swap$
          emphasize * }
      if$
      emphasize
	  "volume and number" number either.or.check
    }
  if$
}
FUNCTION {format.number.series}
{ volume empty$
    { number empty$
        { series field.or.null }
        { series empty$
            { number "number" bibinfo.check }
        { output.state mid.sentence =
            { bbl.number }
            { bbl.number capitalize }
          if$
          number tie.or.space.prefix "number" bibinfo.check * *
          bbl.in space.word *
          series "series" bibinfo.check *
        }
      if$
    }
      if$
    }
    { "" }
  if$
}

FUNCTION {format.edition}
{ edition duplicate$ empty$ 'skip$
    {
      output.state mid.sentence =
        { "l" }
        { "t" }
      if$ change.case$
      "edition" bibinfo.check
      " " * bbl.edition *
    }
  if$
}
FUNCTION {multi.page.check}
{ 't :=
  #0 'multiresult :=
    { multiresult not
      t empty$ not
      and
    }
    { t #1 #1 substring$
      duplicate$ "-" =
      swap$ duplicate$ "," =
      swap$ "+" =
      or or
        { #1 'multiresult := }
        { t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult
}

FUNCTION {format.pages}
{ pages duplicate$ empty$ 'skip$
    { duplicate$ multi.page.check
        {
          n.dashify
        }
        {
        }
      if$
      "pages" bibinfo.check
      ":" swap$ *
    }
  if$
}

FUNCTION {format.journal.pages}
{ pages duplicate$ empty$ 'pop$
    { swap$ duplicate$ empty$
        { pop$ pop$ format.pages }
        {
          ":" *
          swap$
          n.dashify
          "pages" bibinfo.check
          *
        }
      if$
    }
  if$
}
FUNCTION {format.journal.eid}
{ eid "eid" bibinfo.check
  duplicate$ empty$ 'pop$
    { swap$ duplicate$ empty$ 'skip$
      {
          ":" *
      }
      if$
      swap$ *
    }
  if$
}

% ignore issue (num) if == 1
FUNCTION {format.vol.num.pages}
{ volume field.or.null
  duplicate$ empty$ 'skip$
    {
      "volume" bibinfo.check bold
    }
  if$
  number "number" bibinfo.check duplicate$ empty$ 'skip$
    {
      duplicate$ "1" = 
         {pop$ ""}
         { swap$ duplicate$ empty$
               { "there's a number but no volume in " cite$ * warning$ }
               'skip$
            if$
            swap$
            "(" swap$ * ")" * 
         }
      if$
    }
  if$ *
  eid empty$
    { format.journal.pages }
    { format.journal.eid }
  if$
}

FUNCTION {format.chapter.pages}
{ chapter empty$
    'format.pages
    { type empty$
        { bbl.chapter }
        { type "l" change.case$
          "type" bibinfo.check
        }
      if$
      chapter tie.or.space.prefix
      "chapter" bibinfo.check
      * *
      pages empty$
        'skip$
        { ", " * format.pages * }
      if$
    }
  if$
}



%% FUNCTION {format.in.ed.booktitle}
%% { booktitle format.source duplicate$ empty$ 'skip$
%%     {
%%       editor "editor" format.names.ed duplicate$ empty$ 'pop$
%%         {
%%           bbl.edby
%%           " " * swap$ *
%%           swap$
%%           "," *
%%           " " * swap$
%%           * }
%%       if$
%%       word.in swap$ *
%%     }
%%   if$
%% }

FUNCTION{format.edited.by}
{ editor "editor" format.names.ed duplicate$ empty$ 
  'pop$
  {  bbl.edby
     " " * swap$ *
     swap$
     ". " *
     swap$
     * }
  if$
}

FUNCTION {format.in.booktitle}
{ booktitle format.source duplicate$ empty$ 
    'skip$
    { word.in swap$ * }
  if$
}
FUNCTION {format.in.journal}
{ journal format.source duplicate$ empty$ 
    'skip$
    { word.in swap$ * }
  if$
}
FUNCTION {empty.misc.check}
{ author empty$ title empty$ howpublished empty$
  month empty$ year empty$ note empty$
  and and and and and
    { "all relevant fields are empty in " cite$ * warning$ }
    'skip$
  if$
}
FUNCTION {format.tr.number}
{ number "number" bibinfo.check
  type duplicate$ empty$
    { pop$ bbl.techrep }
    'skip$
  if$
  "type" bibinfo.check
  swap$ duplicate$ empty$
    { pop$ "t" change.case$ }
    { tie.or.space.prefix * * }
  if$
}
FUNCTION {format.article.crossref}
{
  key duplicate$ empty$
    { pop$
      journal duplicate$ empty$
        { "need key or journal for " cite$ * " to crossref " * crossref * warning$ }
        { "journal" bibinfo.check emphasize word.in swap$ * }
      if$
    }
    { word.in swap$ * " " *}
  if$
  " \cite{" * crossref * "}" *
}
FUNCTION {format.crossref.editor}
{ editor #1 "{vv~}{ll}" format.name$
  "editor" bibinfo.check
  editor num.names$ duplicate$
  #2 >
    { pop$
      "editor" bibinfo.check
      " " * bbl.etal
      *
    }
    { #2 <
        'skip$
        { editor #2 "{ff }{vv }{ll}{ jj}" format.name$ "others" =
            {
              "editor" bibinfo.check
              " " * bbl.etal
              *
            }
            {
             bbl.and space.word
              * editor #2 "{vv~}{ll}" format.name$
              "editor" bibinfo.check
              *
            }
          if$
        }
      if$
    }
  if$
}
FUNCTION {format.book.crossref}
{ volume duplicate$ empty$
    { "empty volume in " cite$ * "'s crossref of " * crossref * warning$
      pop$ word.in
    }
    { bbl.volume
      capitalize
      swap$ tie.or.space.prefix "volume" bibinfo.check * * bbl.of space.word *
    }
  if$
  editor empty$
  editor field.or.null author field.or.null =
  or
    { key empty$
        { series empty$
            { "need editor, key, or series for " cite$ * " to crossref " *
              crossref * warning$
              "" *
            }
            { series emphasize * }
          if$
        }
        { key * }
      if$
    }
    { format.crossref.editor * }
  if$
  " \cite{" * crossref * "}" *
}
FUNCTION {format.incoll.inproc.crossref}
{
  editor empty$
  editor field.or.null author field.or.null =
  or
    { key empty$
        { booktitle format.source duplicate$ empty$
            { "need editor, key, or booktitle for " cite$ * " to crossref " *
              crossref * warning$
            }
            { word.in swap$ * }
          if$
        }
        { word.in key * " " *}
      if$
    }
    { word.in format.crossref.editor * " " *}
  if$
  " \cite{" * crossref * "}" *
}

% Takes 1 arg (publisher or organisation}
%   if present appends address to the front. ie New York: Sachai Ltd
% returns the formatted string.
FUNCTION {format.org.or.pub}
{ 't :=
  t empty$
    {   address empty$ 
            'skip$
            { address "address" bibinfo.check 't := }
        if$ }
    {   address empty$
            'skip$
            { address "address" bibinfo.check ": " * t * 't :=}
        if$
    }
  if$
  t
}

FUNCTION {format.publisher.address}
{ publisher "publisher" bibinfo.warn format.org.or.pub
}

FUNCTION {format.organization.address}
{ organization "organization" bibinfo.check format.org.or.pub
}

FUNCTION {article}
{ output.bibitem
  author empty$
    { format.editors "author or editor" output.check
      add.colon
    }
    { format.authors output.nonnull
      add.colon
      crossref missing$
        { "author and editor" editor either.or.check }
        'skip$
      if$
    }
  if$
  bold.format.title "title" output.check
  new.sentence
  crossref missing$
    {
      journal "journal" bibinfo.check
      format.source "journal" output.check
      format.date "year" output.check
      format.vol.num.pages output
    }
    { format.article.crossref output.nonnull
      format.pages output
    }
  if$
  format.note output
  fin.entry
}
FUNCTION {wholejournal}
{ output.bibitem
  author empty$
    { format.editors "author or editor" output.check
      add.colon
    }
    { format.authors output.nonnull
      add.colon
      crossref missing$
        { "author and editor" editor either.or.check }
        'skip$
      if$
    }
  if$
  bold.format.title "title" output.check
  new.sentence
  crossref missing$
    {
      journal emphasize word.in swap$ * "journal" output.check
      format.date "year" output.check
      format.vol.num.pages output
    }
    { format.article.crossref output.nonnull
      format.pages output
    }
  if$
  format.note output
	  fin.entry
}
FUNCTION {book}
{ output.bibitem
  author empty$
    { format.editors "author and editor" output.check
      add.colon
    }
    { format.authors output.nonnull
      add.colon
      crossref missing$
        { "author and editor" editor either.or.check }
        'skip$
      if$
    }
  if$
  title format.source "source" output.check
  crossref missing$
    { format.bvolume output
      new.sentence
      format.number.series output
      format.publisher.address output
    }
    {
      new.sentence
      format.book.crossref output.nonnull
    }
  if$
  format.edition output
  format.date "year" output.check
  format.note output
  fin.entry
}

FUNCTION {inbook}
{ output.bibitem
  author empty$
    { format.editors "author and editor" output.check
      add.colon
    }
    { format.authors output.nonnull
      add.colon
      crossref missing$
        { "author and editor" editor either.or.check }
        'skip$
      if$
    }
  if$
  title format.source "title" output.check
  crossref missing$
    {
      format.publisher.address output
      format.bvolume output
      new.sentence
      format.number.series output
    }
    {
      new.sentence
      format.book.crossref output.nonnull
    }
  if$
  format.edition output
  format.date "year" output.check
  add.blank
  format.chapter.pages "chapter and/or pages" output.check
  format.note output
  fin.entry
}

% In <source> . Volume X. y edition. Edited by <ed>. 
% Add: Pub; date:pages.
FUNCTION {incollection}
{ output.bibitem
  format.authors "author" output.check
  add.colon
  bold.format.title "title" output.check
  new.sentence
  crossref missing$
  {   format.in.booktitle "booktitle" output.check
      %format.in.ed.booktitle "booktitle" output.check
      format.bvolume output
      format.number.series output
      format.edition output
      booktitle empty$ 
        'skip$
        {format.edited.by}
      if$
      format.publisher.address output
    }
    { format.incoll.inproc.crossref output.nonnull
    }
  if$
  format.date "year" output.check
  before.all 'output.state := %no space,comma,etc.. -> 1991:23
  format.pages output
  format.note output
  fin.entry
}


FUNCTION {inproceedings}
{ output.bibitem
  format.authors "author" output.check
  add.colon
  bold.format.title "title" output.check
  new.sentence
  crossref missing$
  % In <source> . Volume X. x nd edition. Edited by <ed>. 
    { format.in.booktitle "booktitle" output.check
      format.bvolume output
      format.edition output
      format.number.series output
      booktitle missing$ 
        'skip$ 
        { booktitle missing$ 
            'skip$
            {format.edited.by}
          if$
        }
      if$
      publisher missing$
        { format.organization.address output }
        { organization "organization" bibinfo.check output
          format.publisher.address output
        }
      if$
    }
    { format.incoll.inproc.crossref output.nonnull
    }
  if$
  format.date "year" output.check
  before.all 'output.state := %no space,comma,etc..
  format.pages output
  format.note output
  fin.entry
}

FUNCTION {conference} {inproceedings}


FUNCTION {mastersthesis}
{ output.bibitem
  format.authors "author" output.check
  add.colon
  bold.format.title
  "title" output.check
  new.sentence
  bbl.mthesis format.thesis.type format.source output.nonnull
  school "school" bibinfo.warn output
  address "address" bibinfo.check output
  format.date "year" output.check
  format.note output
  fin.entry
}

% old manual style syntax
% all optional:  (author, title, howpublished, month, year, note)
% maybe missing: (organization, address, edition)
FUNCTION {misc}
{ output.bibitem
  author empty$
    { organization missing$ 'skip$
      { organization output }
    if$
    }
    { format.authors output }
  if$  
  add.colon
  bold.format.title output
  new.sentence
  howpublished "howpublished" bibinfo.check format.source output
  new.sentence
  edition missing$ 'skip$
    {edition output}
  if$
  author empty$
    'skip$
    { organization missing$ 
        'skip$
        {organization output}
      if$
    }
  if$
  address missing$ 'skip$
    {address output}
  if$
  format.date output
  format.note output
  fin.entry
  empty.misc.check
}

FUNCTION {webpage}
{ output.bibitem
  author empty$
    { organization missing$ 'skip$
      { organization output }
    if$
    }
    { format.authors output }
  if$  
  add.colon
  bold.format.title " " * output
  new.sentence
  howpublished "howpublished" bibinfo.check format.source output
  new.sentence
  edition missing$ 'skip$
    {edition output}
  if$
  author empty$
    'skip$
    { organization missing$ 
        'skip$
        {organization output}
      if$
    }
  if$
  address missing$ 'skip$
    {address output}
  if$
  format.date output
  format.note output
  fin.entry
  empty.misc.check
}


FUNCTION {inpress}
{ output.bibitem
  author empty$
    { organization missing$ 'skip$
      { organization output }
    if$
    }
    { format.authors output }
  if$  
  add.colon
  bold.format.title output
  new.sentence
  journal emphasize output
  "in press" output
  fin.entry
  empty.misc.check
}



FUNCTION {manual}
{ output.bibitem
  author empty$
    { organization missing$ 'skip$
      { organization output }
    if$
    }
    { format.authors output }
  if$  
  add.colon
  title output
  emphasize
  new.sentence
  howpublished "howpublished" bibinfo.check format.source output
  new.sentence
  edition missing$ 'skip$
    {edition output}
  if$
  author empty$
    'skip$
    { organization missing$ 
        'skip$
        {organization output}
      if$
    }
  if$
  address missing$ 'skip$
    {address output}
  if$
  format.date output
  format.note output
  fin.entry
  empty.misc.check
}

FUNCTION {booklet}{misc}


FUNCTION {phdthesis}
{ output.bibitem
  format.authors "author" output.check
  add.colon
  bold.format.title
  "title" output.check
  new.sentence
  bbl.phdthesis format.thesis.type format.source output.nonnull
  school "school" bibinfo.warn output
  address "address" bibinfo.check output
  format.date "year" output.check
  format.note output
  fin.entry
}

FUNCTION {proceedings}
{ output.bibitem
  editor empty$
    { organization "organization" bibinfo.check output
    }
    { format.editors output.nonnull }
  if$
  add.colon
  title format.source "source" output.check
  format.bvolume output
  format.number.series output
  editor empty$
    { publisher empty$
        'skip$
        {
          format.publisher.address output
        }
      if$
    }
    { publisher empty$
        {
          format.organization.address output }
        {
          organization "organization" bibinfo.check output
          format.publisher.address output
        }
      if$
     }
  if$
  format.date "year" output.check
  format.note output
  fin.entry
}

FUNCTION {techreport}
{ output.bibitem
  format.authors "author" output.check
  add.colon
  bold.format.title
  "title" output.check
  new.sentence
  format.tr.number output.nonnull
  institution "institution" bibinfo.warn output
  address "address" bibinfo.check output
  format.date "year" output.check
  format.note output
  fin.entry
}

FUNCTION {unpublished}
{ output.bibitem
  format.authors "author" output.check
  add.colon
  bold.format.title "title" output.check
  format.date output
  format.note "note" output.check
  fin.entry
}

FUNCTION {default.type} { misc }



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%  %%%%  %%%   %%%%%%%    %% %%%%%%%%%
%% %%%% %%  %%%% %  %%%%%  %  %% %%%%%%%%%
%% %%%%%   %%%%% %%  %%%  %%  %% %%%%%%%%%
%% %%%%%% %%%%%% %%%  %  %%%  %% %%%%%%%%%
%% %%%%%   %%%%% %%%%   %%%%  %% %%%%%%%%%
%% %%%% %%  %%%% %%%%%%%%%%%  %% %%%%%%%%%
%% %%  %%%%  %%% %%%%%%%%%%%  %%        %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %  
 % Function for printing out all data in xml format. 
 % { address archive author booktitle chapter edition editor 
 %   howpublished institution journal key month note number
 %   organization pages publisher school series title
 %   type url volume year 
 % }
 %
%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%
 % output.xml
 %
%%
FUNCTION {output.xml}
{ duplicate$ "" = 
   'pop$
   { "  " swap$ * write$ newline$ }
 if$
}

%%
 % markup.xml
 %
 % Takes 2 args text & tag {"the text " "tag"}
 % Returns 1 string        {"<tag>the text <\tag>"} 
%%
FUNCTION {markup.xml}
{ 
  "" 'r :=  % result
  't :=  % tag
  duplicate$ missing$
    'pop$
    { 'r := }
  if$
  r "" = 
    'skip$
    { "<" t * ">" * r * "</" * t * ">" * 'r := }
 if$
 r
}

%% Takes 2 args - tag, string
FUNCTION{markup.xml.title}
{   pop$ % tag
    duplicate$ missing$
      'pop$
      {
       's := % string
        s "\&" "&amp" replace 's := 
        s "p" markup.xml 's :=
        s "title" markup.xml output.xml
      } 
    if$
}


%%
 % markup.xml.pages
 %
 % Takes 1 arg (pages string)
 % seperates into <fpage> and <lpage> if 
 %    there is a - seperator. else no lpage.
 %
 % need to remove others
%%
FUNCTION{markup.xml.pages}
{
  "" 'r := % fpage
  "" 's := % lpage
  duplicate$ missing$
    'pop$
    {   
        't :=
        t " " "" replace 't :=  % remove all spaces. pgs must be - seperated
        { t empty$ not }
        {  
              t #1 #1 substring$ "-" = 
              { 
                t #2 global.max$ substring$ 's := 
                "" 't :=  % break while loop
              }{ 
                r t #1 #1 substring$ * 'r := 
                t #2 global.max$ substring$ 't := 
              }
              if$
        }
        while$
    }
    if$
  r "fpage" markup.xml output.xml
  s "lpage" markup.xml output.xml
}


%%
 % markup.xml.names
 %
 % Takes 2 args
 % tag, namefield (eg. author "au")
 % splits fields into <fnm> and <snm>
 % writes info 
 % returns nothing
%% 
FUNCTION {markup.xml.names}
{
  't :=  % tag
  "" 'r :=
  "" 's :=
  duplicate$ empty$
    { pop$
      organization missing$  
        { institution "cnm" markup.xml 
          "au" markup.xml "aug" markup.xml output.xml  }
        { organization "cnm" markup.xml 
          "au" markup.xml "aug" markup.xml output.xml  }
      if$ }
    {
      "  <aug>" write$ newline$
      'names :=               % names string
      names num.names$ 'j :=  % num of names
      #1 'i :=            % init counter
      { i #1 j + < }     % while (i <= j)
      {
        "    <au>" write$

        % Initial first names
        % if first name all in capitals 
        %  - assume is initial list of first names.
        names i "{ff{ }}" format.name$
          duplicate$ "u" change.case$ = 
          { names i "{ff{ }}" format.name$ }
          { names i "{f{}}" format.name$ }
          if$
        
        % if last name but no first name use cnm; else snm
        duplicate$ "" = 
          { names i "{ll{ }}" format.name$ % last names space seperated. 
            "cnm" markup.xml write$ }
          { names i "{ll{ }}" format.name$ % last names space seperated. 
            "snm" markup.xml write$ }
        if$

        "fnm" markup.xml write$ 
      
        "</au>" write$ newline$
        i #1 + 'i :=
      }
      while$      
      "  </aug>" write$ newline$
    }
  if$
}


%%
 % markup.xml.pub
 %
 % Takes 4 args (school address publisher tag)
 % prints concatenation 
 % returns nothing
%% 
FUNCTION {markup.xml.pub}
{
  't :=    % Tag
  "" 'p := % Publisher 
  "" 'q := % Address
  "" 'r := % School
  "" 's := % Answer
  duplicate$ missing$ 'pop$ { 'p := } if$
  duplicate$ missing$ 'pop$ { 'q := } if$
  duplicate$ missing$ 'pop$ { 'r := } if$
  "" r = not     % school not empty
    { r 's := }   % return school as publisher 
    {             % else 
      "" p = "" q = and % address and pub empty 
        'skip$
        {
          "" q = 
            { p 's := }         % return pub
            { "" p =
                { q 's := }         % return add
                { q ": " * p * 's := }  % return add : pub
              if$
            }
          if$
        }
      if$
    }
  if$
  s t markup.xml output.xml
}

%%
 % xml.phd
 %
 % Takes nothing
 % Returns type (phd/msc) of empty string
%%
FUNCTION {xml.phd}
{   
    "phdthesis" type$ =
    "mastersthesis" type$ = OR
    { "phdthesis" type$ =
        { bbl.phdthesis format.thesis.type }
        { bbl.mthesis format.thesis.type }
      if$
    }
    { type } % usually empty
  if$ 
}

% markup.xml.edition
%
% edition is a numeric value. ie "2"
% if format 2nd 1st etc.. strip letters.
%
FUNCTION {markup.xml.edition}
{ pop$ pop$  %clear stack
  edition strip.letters duplicate$ "" = 
      { pop$ edition } 
      'skip$ 
  if$
  "edition"   markup.xml  output.xml
}


%%  [bmc@xmlcomment]
 %   this is an environment that returns nothing 
 %   so although it will be embedded in the bbl file it
 %   will not display anything
%%
FUNCTION{begin.bmcxmlcomment}{
    newline$ 
    "\newcommand{\BMCxmlcomment}[1]{}" write$ newline$
    newline$ 
    "\BMCxmlcomment{" write$ newline$ 
    newline$ "<refgrp>" write$ newline$
}

FUNCTION{end.bmcxmlcomment}{ 
    newline$ 
    "</refgrp>" write$ newline$
    "} % end of \BMCxmlcomment" write$ newline$
}

%%
 % export.xml
 % 
 % done for each entry referenced in the BibTeX database 
%%
FUNCTION {export.xml}{ 
  newline$
  global.counter #1 + 'global.counter :=
  "<bibl id=" quote$ * "B" * global.counter int.to.str$ * quote$ * ">" * 
  write$ newline$
%  title "\&" "&amp" replace "title" markup.xml.title     
  title         "title"     markup.xml.title     
  author        "aug"       markup.xml.names % org. and inst. here
  howpublished missing$ 
    { booktitle missing$ 
      { journal missing$
          { xml.phd } % <type> Phd/Msc 
          {journal}
        if$
      } {booktitle}
    if$
    } { howpublished }
  if$           "source"    markup.xml output.xml
  school 
  address  
  publisher     "publisher" markup.xml.pub
  editor        "editor"    markup.xml output.xml
  edition       "edition"   markup.xml.edition     
  series        "p"         markup.xml 
                "title"     markup.xml 
                "series"    markup.xml output.xml    
  chapter       "p"         markup.xml 
                "title"     markup.xml 
                "section"   markup.xml output.xml    
  % month       % ignore
  year          "pubdate"   markup.xml output.xml    
  
  note missing$ 
      { volume  "volume"    markup.xml output.xml    
        number  "issue"     markup.xml output.xml    
        pages               markup.xml.pages    
        url     "url"       markup.xml output.xml }
      { note "l" change.case$ "in press" = 
          { "  <inpress />" write$ newline$ }
          { volume  "volume"    markup.xml output.xml    
            number  "issue"     markup.xml output.xml    
            pages               markup.xml.pages    
            url     "url"       markup.xml output.xml 
            note    "note"      markup.xml output.xml 
          }   
        if$ 
      }
  if$
  "</bibl>" write$ newline$
} 

% xml fields not used
% suppliment conference xrefbib issn issb archive key
% BioMed_Central_Tex_Template_v1.01


%%%%%%%%%%%%%%%%
%              %
%     Main     %
%              %
%%%%%%%%%%%%%%%%
 
READ

FUNCTION {initialize.longest.label}
{ "" 'longest.label :=
  #1 'number.label :=
  #0 'longest.label.width :=
}
FUNCTION {longest.label.pass}
{ number.label int.to.str$ 'label :=
  number.label #1 + 'number.label :=
  label width$ longest.label.width >
    { label 'longest.label :=
      label width$ 'longest.label.width :=
    }
    'skip$
  if$
}
EXECUTE {initialize.longest.label}
ITERATE {longest.label.pass}
FUNCTION {begin.bib}
{ "%% BioMed_Central_Bib_Style_v1.01" write$ newline$ newline$
  preamble$ empty$
    'skip$
    { preamble$ write$ newline$ }
  if$
  "\begin{thebibliography}{"  longest.label  * "}" *
  write$ newline$
 "\providecommand{\url}[1]{[#1]}"
  write$ newline$
  "\providecommand{\urlprefix}{}"
  write$ newline$
}

EXECUTE {begin.bib}
EXECUTE {init.state.consts}
ITERATE {call.type$}
FUNCTION {end.bib}
{ newline$
  "\end{thebibliography}" write$ newline$
}
EXECUTE {end.bib}
EXECUTE {begin.bmcxmlcomment}
ITERATE {export.xml}
EXECUTE {end.bmcxmlcomment}
%% End of customized bst file
%%
%% End of file `basic.bst'.
