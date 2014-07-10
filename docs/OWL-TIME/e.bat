set SORTTIME=%time:~0,2%%time:~3,2%%time:~6,2%
if "%SORTTIME:~0,1%"==" " set SORTTIME=0%SORTTIME:~1,6%
del bfo-owl-time.pdf /Q
pdflatex bfo-owl-time.tex
move bfo-owl-time.pdf Paper-%date%-%SORTTIME%.pdf
start Paper-%date%-%SORTTIME%.pdf

