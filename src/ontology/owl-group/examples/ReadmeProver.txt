(1) Installation of prover9

Prover9 is available from http://www.cs.unm.edu/~mccune/mace4/. I tried using the graphical interface, but for some reason it was endlessly running on my side and never finished (even with a time limit). I therefore installed from the sources at http://www.cs.unm.edu/~mccune/mace4/download/
Installation instructions (from the prover9 site): Download the .tar.gz file, unpack it, go to the LADR directory, and "make all".

Note: on my mac os, make was not installed, and  I needed to install the developers tools (Xcode) from https://developer.apple.com/xcode/ Once installed, open Xcode and go to  Xcode > preferences> downloads to install command line tools. (There may be a way to install command line tools without installing Xcode but I didn't look into it)

(2) Running prover9

I will use the script written by Chris Mungall at http://code.google.com/p/bfo/source/browse/trunk/src/ontology/owl-group/examples/nuclei-cell-division.prover9 as an example.

Prover takes a file as input, and produces an output file (you can tweak options to use stdin and stdout, but I used files to keep things clean)
In the input file, the first part (formulas(sos) are the fact or assumptions made). The second part is the formulas(goals), i.e., what you are trying to prove.

Go to LADR-2009-11A/bin, and run prover. I downloaded the file on my desktop, so my command line looks like:

[bioinfomac2:~/Desktop/LADR-2009-11A/bin] mcourtot% ./prover9 -f ~/Desktop/nuclei-cell-division.prover9 > ~/Desktop/nuclei-cell-division.out
-------- Proof 1 -------- 

THEOREM PROVED

------ process 7665 exit (max_proofs) ------
Hhere "theorem proved" means that prover9 found a proof supporting the formula stated as goal in the input file, which was 
-(all p all t
   (type(p,Nucleus) ->
     ( exists w (type(w,Cell) & part_of_at_all_times(p,w))))).


Note the negation in front of the goal: this means prover found a case in which there exists a Nucleus which is *not* part of a Cell at all times.

The output file is available at http://code.google.com/p/bfo/source/browse/trunk/src/ontology/owl-group/examples/nuclei-cell-division.out.




