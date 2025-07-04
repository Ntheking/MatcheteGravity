(* ::Package:: *)

Package["Matchete`"]


(* ::Title:: *)
(*Matchete`NCM`*)


(* ::Subtitle:: *)
(*Contains the definitions of the NonCommutativeMultiply*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


PackageImport["GroupMagic`"]


(* ::Subsubsection::Closed:: *)
(*Exported*)


Unprotect@ NonCommutativeMultiply; (* overwrite NonCommutativeMultiply *)


(* ::Subsubsection::Closed:: *)
(*Internal*)


PackageScope["SetNonCommutative"]
PackageScope["CommutativeQ"]
PackageScope["Commutative"]


PackageScope["Commutator"]
PackageScope["AntiCommutator"]


PackageScope["FermionTrace"]
PackageScope["CanonizeSpinorLines"]


PackageScope["OpenSpinChainQ"]


(* ::Section::Closed:: *)
(*Usage definitions*)


(* ::Subsubsection::Closed:: *)
(*Exported*)


NonCommutativeMultiply::usage = "NonCommutativeMultiply[a, b, ...] (alternatively a**b**...) denotes the Dirac product of spinors, Dirac matrices, etc.";


(* ::Subsubsection::Closed:: *)
(*Internal*)


SetNonCommutative::usage = "SetNonCommutative[obj] defines the object obj to be non-commutative, where obj can be a Sequence, List or Symbol.";
CommutativeQ::usage      = "CommutativeQ[obj] returns True if obj is a comutative object and false otherwise.";
Commutative::usage       = "Commutative[obj] is an auxiliary head that can be used to treat the object obj temporarily as commutative.";


Commutator::usage     = "Commutator[X,Y] gives the commutator of X and Y, i.e. [X,Y]=X**Y-Y**X.";
AntiCommutator::usage = "AntiCommutator[X,Y] gives the anti-commutator of X and Y, i.e. {X,Y}=X**Y+Y**X.";


FermionTrace::usage        = "FermionTrace[expr] closes fermion lines in expr and performs Dirac gamma traces.";
CanonizeSpinorLines::usage = "CanonizeSpinorLines[expr] separates out nested spinor lines, breaking up the NCM accordingly.";


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*NonCommutativeMultiply functionality*)


(* ::Subsection::Closed:: *)
(*Commutative check*)


(* ::Text:: *)
(*Default assumption is that everything is commutative*)


CommutativeQ[x_NonCommutativeMultiply]:= (CommutativeQ[x]= ClosedSpinChainQ[x]);
CommutativeQ[f_?CommutativeQ[x___]]:= (CommutativeQ[f[x]]= And@@ CommutativeQ/@ {x});

(* The above speeds up DeriveSubstitutions by a factor of ~2 or so *)
(*CommutativeQ@ x_NonCommutativeMultiply:= ClosedSpinChainQ@ x;
CommutativeQ[f_?CommutativeQ[x___]] := And@@ CommutativeQ/@ {x};*)

CommutativeQ[f_[x___]] := False;
CommutativeQ[_] := True; 


(* ::Text:: *)
(*Define the non-commutative objects*)


CommutativeQ@ Alternatives[Pattern, Blank, BlankSequence, BlankNullSequence, Except, Hold] = False;


SetNonCommutative[x_, y__]  := (SetNonCommutative@ x; SetNonCommutative@ y;);
SetNonCommutative[x_Symbol] := (CommutativeQ@ x ^= False;);


(* ::Text:: *)
(*Commutative is a head that can be used to temporarily treat an object as commutative*)


CommutativeQ@ Commutative@ _ ^= True; 


(* ::Subsection::Closed:: *)
(*Properties of NonCommutativeMultiply*)


(* ::Text:: *)
(*Remove attributes problematic for pattern matching*)


ClearAttributes[NonCommutativeMultiply, {Flat, OneIdentity}];


(* ::Text:: *)
(*Set definitions for the non-commutative product*)


(* ::Subsubsection::Closed:: *)
(*General properties*)


(* ::Text:: *)
(*NCM contractions*)


NonCommutativeMultiply[a___, NonCommutativeMultiply[b__], c___]:= NonCommutativeMultiply[a, b, c] /;FreeQ[List@b,Fermion]
NonCommutativeMultiply[] = 1;
NonCommutativeMultiply@NonCommutativeMultiply@x___:=NonCommutativeMultiply@x;


(* Extracting commuting objects *)
NonCommutativeMultiply[a___, b_?CommutativeQ, c___]:= b * NonCommutativeMultiply[a, c];
NonCommutativeMultiply[a___, b_?CommutativeQ * x_, c___]:= b * NonCommutativeMultiply[a, x, c];
(* Distributivity *)
NonCommutativeMultiply[a___, b_Plus, c___]:= NonCommutativeMultiply[a, #, c] & /@ b 


(* ::Subsubsection::Closed:: *)
(*Contraction of spin chains*)


NonCommutativeMultiply[a___, x:NonCommutativeMultiply@ b___, c___]:=
	If[ClosedSpinChainQ@ x,
		x NonCommutativeMultiply[a, c],
		NonCommutativeMultiply[a, b, c]
	];


(* ::Subsubsection::Closed:: *)
(*Canonical ordering of operators with transpose*)


(*No terms with two fields transposed*)
NonCommutativeMultiply[A: Transp@Field[_,Fermion,___] , B___/;FreeQ[List@B,Fermion] ,C: Transp@Bar@Field[_,Fermion,___]]:= - Transp[C]** Transp[B] ** Transp[A] 
(*No complex fermion transpose on the left*)
NonCommutativeMultiply[A: Transp@Field[labelA_,Fermion,___]/;!$FieldAssociation[labelA][SelfConjugate], B___/;FreeQ[List@B,Fermion], C: Field[labelC_,Fermion,___]/;$FieldAssociation[labelC][SelfConjugate]]:= 
	- Transp[C]** Transp[B] ** Transp[A] 
(*If there must be a transpose  choose canonical ordering *)
NonCommutativeMultiply[A:Transp@Field[labelA_,Fermion,___]/; $FieldAssociation[labelA][SelfConjugate], B___/;FreeQ[List@B,Fermion], C: Field[labelC_,Fermion,___]/;$FieldAssociation[labelC][SelfConjugate]]:= 
	- Transp[C]** Transp[B] ** Transp[A]  /;!OrderedQ[{labelA,labelC}] 
NonCommutativeMultiply[A:Transp@Field[labelA_,Fermion,___]/;!$FieldAssociation[labelA][SelfConjugate], B___/;FreeQ[List@B,Fermion], C: Field[labelC_,Fermion,___]/;!$FieldAssociation[labelC][SelfConjugate]]:= 
	- Transp[C]** Transp[B] ** Transp[A]  /;!OrderedQ[{labelA,labelC}] 
NonCommutativeMultiply[A:Bar@Field[labelA_,Fermion,___], B___/;FreeQ[List@B,Fermion], C: Transp@Bar@Field[labelC_,Fermion,___] ]:=
	- Transp[C]** Transp[B] ** Transp[A]  /;!OrderedQ[{labelA,labelC}]


(*If same field, place field with most derivative on the right*)
NonCommutativeMultiply[A:Transp@Field[label_,Fermion,_,Lind1_], B___/;FreeQ[List@B,Fermion], C: Field[label_,Fermion,_,Lind2_]]:=
	- Transp[C]** Transp[B] ** Transp[A]  /;(Length[Lind1]>Length[Lind2]);
NonCommutativeMultiply[A:Bar@Field[label_,Fermion,__,Lind1_], B___/;FreeQ[List@B,Fermion], C: Transp@Bar@Field[label_,Fermion,__,Lind2_]]:=
	- Transp[C]** Transp[B] ** Transp[A]  /;(Length[Lind1]>Length[Lind2]);


NonCommutativeMultiply[EoM@ Transp[f1:Field[_, Fermion, _, {}]], d_DiracProduct, f2:Field[_, Fermion, _, {}]]:=
	- Transp@ f2 ** Transp@ d ** EoM@ f1;
NonCommutativeMultiply[EoM@ Bar[f1:Field[_, Fermion, _, {}]], d_DiracProduct, Transp[f2:Bar@ Field[_, Fermion, _, {}]]]:=
	- f2 ** Transp@ d ** EoM@ Transp@ Bar@ f1;


(* ::Subsection::Closed:: *)
(*Utility functions*)


Commutator[x_, y_]     := x ** y - y ** x;
AntiCommutator[x_, y_] := x ** y + y ** x;


(* ::Subsection::Closed:: *)
(*Non-commutative symbols used in Matchete should be declared here*)


(* ::Text:: *)
(*From Definitions.m*)


SetNonCommutative[Fermion, Ghost];


(* ::Text:: *)
(*From DiracAlgebra.m*)


SetNonCommutative[DiracProduct];
SetNonCommutative[GammaM, Gamma5, GammaCC];


(* ::Text:: *)
(*From FunctionalTools.m*)


(* SetNonCommutative[TransposeThisSpinChain] *)
(* For some reason the lie above does not work, but the one below does... *)
CommutativeQ[TransposeThisSpinChain]=False


(* ::Section:: *)
(*Active manipulation*)


(* ::Subsection:: *)
(*Organize fermion lines*)


(* ::Subsubsection::Closed:: *)
(*Fermion trace*)


(* ::Text:: *)
(*Function for closing fermion lines in traces as appear in some supertraces. *)


FermionTrace::cantclose = "The fermion line cannot be closed";
FermionTrace::multopens = "Multiple open fermion lines encountered in one term.";


OpenSpinChainQ= Not@* ClosedSpinChainQ;


FermionTrace@ expr_:= Module[{out= BetterExpand@ expr},	
	(*Trace terms in the sums separately*)
	If[Head@ out === Plus, Return[FermionTrace/@ out];]; 
	
	Switch[Count[{out}, _NonCommutativeMultiply? OpenSpinChainQ, All]
		,0 ,
			4 out
		,1 ,
			out/. x_NonCommutativeMultiply? OpenSpinChainQ:> TraceSpin@ x// ContractMetric
		,_ ,
			Message[FermionTrace::multopens];
			Abort[];
	]
];


TraceSpin@ spinChain_NonCommutativeMultiply:= Module[{spinCount= CumulativeSpin@ spinChain},
	If[Last@ spinCount =!= 0,
		Message[FermionTrace::cantclose];
		Abort[];	
	];
	(*Check if the fermion line can be closed through the trace *)
	If[MemberQ[spinCount, -1],
		(*Sign is from Grassmanian variables*)
		Return[-RotateLeft[spinChain, FirstPosition[spinCount, -1]]];
	];
	(*Otherwise do a Dirac trace*)
	DiracTrace@ spinChain
];


(* ::Subsubsection::Closed:: *)
(*Spinor line canonization*)


(* ::Text:: *)
(*Function for separating out nested spinor lines, breaking up the NCM accordingly*)


CanonizeSpinorLines::lopens = "The Spinor line has >1 L-open spinors.";
CanonizeSpinorLines::ropens = "The Spinor line has >1 R-open spinors.";


CanonizeSpinorLines@ fline_NonCommutativeMultiply:= Module[
		{cumSpinCount, max, min, l, r, expr},
	cumSpinCount= CumulativeSpin@ fline;
	min= Min@ cumSpinCount;
	If[min < -1,
		Message[CanonizeSpinorLines::lopens];
		Abort[];
	];
	If[Last@ cumSpinCount> 1,
		Message[CanonizeSpinorLines::ropens];
		Abort[];
	];
	
	If[min ===-1,
		cumSpinCount[[;; First@ FirstPosition[cumSpinCount, -1] -1]]--;
	]; 
	max= Max@ cumSpinCount;
		
	(*If a closed spinor line appear it is separated out*)
	{l, r}= First@ SequencePosition[cumSpinCount, {max..}, Overlaps-> False]+ {0, 1};
	If[r <= Length@ fline,
		Return[fline[[l;;r]] CanonizeSpinorLines[fline[[;; l-1]]** fline[[r+ 1;;]] ] ];
	];

	fline
];


CanonizeSpinorLines@ expr_:= expr//. x_NonCommutativeMultiply:> CanonizeSpinorLines@ x;
