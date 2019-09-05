L[0] := 0;
B[0] := 0;
T[0] := 0;
conj[0] := 0;
Adj[0] := 0;
trace[___, 0, ___] := 0;
conj[e_] := Conjugate[e];

sarahOutputDir  = FileNameJoin @ { "SARAH" };
susynoOutputDir = FileNameJoin @ { "Susyno" };

sarahBetaXiS  = First[Drop[#,1]& /@ Get[FileNameJoin[{sarahOutputDir, "BetaLSi.m"}]]];
susynoBetaXiS = First /@ Get[FileNameJoin[{susynoOutputDir, "betaNMSSM.m"}]][[8]];

$Assumptions = {
    Element[mHu2, Reals],
    Element[mHd2, Reals],
    Element[ms2, Reals],
    Element[\[Lambda], Reals],
    Element[\[Kappa], Reals],
    Element[MassB, Reals],
    Element[MassWB, Reals],
    Element[MS, Reals],
    Element[\[Mu], Reals],
    Element[B[\[Mu]], Reals],
    Element[B[MS], Reals],
    Element[T[\[Lambda]], Reals],
    Element[T[\[Kappa]], Reals]
};

convertToSARAH = {
    g[1] -> g1,
    g[2] -> g2,
    M[1] -> MassB,
    M[2] -> MassWB,
    y[{"u", "Q", "Hu"}, {f[_], f[_]}] :> Yu,
    y[{"e", "L", "Hd"}, {f[_], f[_]}] :> Ye,
    y[{"d", "Q", "Hd"}, {f[_], f[_]}] :> Yd,
    \[Mu][{"Hu", "Hd"}] -> \[Mu], (* CHECK sign! *)
    \[Mu][{"S", "S"}] -> MS,
    b[{"Hu", "Hd"}] -> B[\[Mu]], (* CHECK sign! *)
    b[{"S", "S"}] -> B[MS],
    m2[{"Hu", "Hu"}] -> mHu2,
    m2[{"Hd", "Hd"}] -> mHd2,
    m2[{"Q", "Q"}, {f[7], f[8]}] -> mq2,
    m2[{"u", "u"}, {f[7], f[8]}] -> mu2,
    m2[{"d", "d"}, {f[7], f[8]}] -> md2,
    m2[{"L", "L"}, {f[7], f[8]}] -> ml2,
    m2[{"e", "e"}, {f[7], f[8]}] -> me2,
    m2[{"S", "S"}] -> ms2,
    y[{"S", "S", "S"}] -> 2 \[Kappa] (* CHECK prefactor! *),
    h[{"S", "S", "S"}] -> 2 T[\[Kappa]] (* CHECK prefactor! *),
    y[{"Hu", "Hd", "S"}] -> \[Lambda],
    h[{"Hu", "Hd", "S"}] -> T[\[Lambda]],
    h[{"u", "Q", "Hu"}, {f[_], f[_]}] :> T[Yu],
    h[{"d", "Q", "Hd"}, {f[_], f[_]}] :> T[Yd],
    h[{"e", "L", "Hd"}, {f[_], f[_]}] -> T[Ye],
    l[{"S"}] -> L1,
    s[{"S"}] -> L[L1]
};

sarahBetaXiS  = Refine[sarahBetaXiS];
susynoBetaXiS = Refine[susynoBetaXiS //. convertToSARAH];

extractWrongTerms = {
    T[Yu|Yd|Ye] :> 0,
    Yu|Yd|Ye :> 0
};

sarahBetaXiS  = Expand[sarahBetaXiS]  //. extractWrongTerms;
susynoBetaXiS = Expand[susynoBetaXiS] //. extractWrongTerms;

PickCoeff[expr_, coeff_] := Coefficient[expr, coeff] coeff;

sarahBetaXiS  = PickCoeff[#, \[Lambda] g2^2]& /@ sarahBetaXiS;
susynoBetaXiS = PickCoeff[#, \[Lambda] g2^2]& /@ susynoBetaXiS;

Print["SARAH: ", InputForm[sarahBetaXiS]];

Print["Susyno: ", InputForm[susynoBetaXiS]];

Print["diff = ", InputForm[Expand[sarahBetaXiS - susynoBetaXiS]]];
