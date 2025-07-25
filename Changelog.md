# Matchete changelog

## v0.2.0 (2024-04-22)
- Traces are now computed covariantly with Wilson lines rather than CDE. Improved 'Match' performance at one-loop order.
- Match automatically identifies heavy and light fields and updates the masses according to the Lagrangian.
- Introduced Option DummyCoefficients -> True/False in EOMSimplify.
- Implemented GetOperatorCoefficient to extract coefficients of specific EFT operators.
- Improved GreensSimplify's ability to determine when operators trivially vanish under symmetry.
- Collection of coefficients in CollectOperators and GreensSimplify now accounts for flavor symmetries of operators.
- Improved performance of GreensSimplify.
- New, robust implementation of HcSimplify.
- Various bug fixes:
    - Fixed ContractCGs for higher dimensional groups.
	- Fixed GreensSimplify bug with flavor indices.
	- Fixed GreensSimplify bug with multiple gamma matrices on the same fermion line.
	- Allows HcTerms as input in various functions.
	- Allows constants in GreensSimplify and CollectOperators.

### v0.1.8 (2024-02-19)
- Fixed a bug in the CDE of CovariantLoop/Match that caused non-cyclicity of the traces.
- Improved HcSimplify.
- Improved EvaluateLoopFunctions.
- Implemented simplification routines to LF (Loop functions).
- Introduced ShiftRankCouplings, which absorbs EFT corrections into the renormalized couplings.
- EFT power counting is now directly determined from the Lagrangian rather than from the field definitions.

### v0.1.7 (2023-11-02)
- Fixed a bug in GreensSimplify in models with Majorana fermions.
- Tweaks to HcSimplify.
- Fixed an issue with EOMSimplify when field redefinitions took the form of the kinetic term.
- Fixed an issue where CheckLagrangian would not recognize some pre-defined objects.

### v0.1.6 (2023-10-09)
- Fixed a bug in field redefinitions involving non-hermitian superleading terms.
- Field redefinitions now count powers of hbar prior to inserting redefinitions, improving performance.
- Introduced the option "DummyCoefficients" for EOMSimplify, improving performance when simplifying large Lagrangians.

### v0.1.5 (2023-06-20)
- Various bugfixes applied to the simplification routines.

### v0.1.4 (2023-04-13)
- Improved performance of EOMSimplify
- Improved performance of Match
- Fixed bug with option "EFTOrder" in "CovariantLoop"
- Fixed bug with DiracTrace of 8 or more gammas
- Extended DiracTrace with NDR value for traces of gamma5 and 6 or more gammas

### v0.1.3 (2023-03-14)
- Added check for invalid spinor contractions with the C matrix in Check Lagrangian
- Fixed use of syntax e.g. "EFTOrder-> {6}" in Match
- Fixed action of gauge groups on fields in conjugate (Bar'ed) representations

### v0.1.2 (2023-02-10)
- Fixed issues when dealing with global gauge groups
- Improved MatchToCGs efficiency

### v0.1.1 (2023-01-14)
- Fixed an issue with field redefinitions on Mathematica 12
- Minor bug fixes

## v0.1.0 (2022-12-08)
- Initial release
