{{mq2[i1, i2], (-2*g1^2*MassB*conj[MassB]*Kronecker[i1, i2])/15 - 
   (32*g3^2*MassG*conj[MassG]*Kronecker[i1, i2])/3 - 
   6*g2^2*MassWB*conj[MassWB]*Kronecker[i1, i2] + 
   (g1*Kronecker[i1, i2]*Tr1[1])/Sqrt[15] + 
   2*mHd2*MatMul[Adj[Yd], Yd][i1, i2] + 2*mHu2*MatMul[Adj[Yu], Yu][i1, i2] + 
   2*MatMul[Adj[T[Yd]], T[Yd]][i1, i2] + 
   2*MatMul[Adj[T[Yu]], T[Yu]][i1, i2] + MatMul[mq2, Adj[Yd], Yd][i1, i2] + 
   MatMul[mq2, Adj[Yu], Yu][i1, i2] + 2*MatMul[Adj[Yd], md2, Yd][i1, i2] + 
   MatMul[Adj[Yd], Yd, mq2][i1, i2] + 2*MatMul[Adj[Yu], mu2, Yu][i1, i2] + 
   MatMul[Adj[Yu], Yu, mq2][i1, i2], 
  (16*g3^2*(g1^2*(MassB + 2*MassG) + 15*(-8*g3^2*MassG + 
        3*g2^2*(2*MassG + MassWB)))*conj[MassG]*Kronecker[i1, i2])/45 + 
   (g1^2*g2^2*MassB*conj[MassWB]*Kronecker[i1, i2])/5 + 
   16*g2^2*g3^2*MassG*conj[MassWB]*Kronecker[i1, i2] + 
   (2*g1^2*g2^2*MassWB*conj[MassWB]*Kronecker[i1, i2])/5 + 
   33*g2^4*MassWB*conj[MassWB]*Kronecker[i1, i2] + 
   32*g2^2*g3^2*MassWB*conj[MassWB]*Kronecker[i1, i2] + 
   6*g2^4*Kronecker[i1, i2]*Tr2[2] + (32*g3^4*Kronecker[i1, i2]*Tr2[3])/3 + 
   (2*g1^2*Kronecker[i1, i2]*Tr2U1[1, 1])/15 + 
   (4*g1*Kronecker[i1, i2]*Tr3[1])/Sqrt[15] + 
   (4*g1^2*mHd2*MatMul[Adj[Yd], Yd][i1, i2])/5 - 
   4*mHd2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   2*mHu2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   2*ms2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   2*conj[T[\[Lambda]]]*T[\[Lambda]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   12*mHd2*trace[Yd, Adj[Yd]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   4*mHd2*trace[Ye, Adj[Ye]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   6*trace[conj[T[Yd]], Tp[T[Yd]]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   2*trace[conj[T[Ye]], Tp[T[Ye]]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   6*trace[md2, Yd, Adj[Yd]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   2*trace[me2, Ye, Adj[Ye]]*MatMul[Adj[Yd], Yd][i1, i2] - 
   2*trace[ml2, Adj[Ye], Ye]*MatMul[Adj[Yd], Yd][i1, i2] - 
   6*trace[mq2, Adj[Yd], Yd]*MatMul[Adj[Yd], Yd][i1, i2] - 
   2*\[Lambda]*conj[T[\[Lambda]]]*MatMul[Adj[Yd], T[Yd]][i1, i2] - 
   6*trace[conj[T[Yd]], Tp[Yd]]*MatMul[Adj[Yd], T[Yd]][i1, i2] - 
   2*trace[conj[T[Ye]], Tp[Ye]]*MatMul[Adj[Yd], T[Yd]][i1, i2] + 
   (8*g1^2*mHu2*MatMul[Adj[Yu], Yu][i1, i2])/5 - 
   2*mHd2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Yu], Yu][i1, i2] - 
   4*mHu2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Yu], Yu][i1, i2] - 
   2*ms2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Yu], Yu][i1, i2] - 
   2*conj[T[\[Lambda]]]*T[\[Lambda]]*MatMul[Adj[Yu], Yu][i1, i2] - 
   12*mHu2*trace[Yu, Adj[Yu]]*MatMul[Adj[Yu], Yu][i1, i2] - 
   6*trace[conj[T[Yu]], Tp[T[Yu]]]*MatMul[Adj[Yu], Yu][i1, i2] - 
   6*trace[mq2, Adj[Yu], Yu]*MatMul[Adj[Yu], Yu][i1, i2] - 
   6*trace[mu2, Yu, Adj[Yu]]*MatMul[Adj[Yu], Yu][i1, i2] + 
   (g1^2*conj[MassB]*((597*g1^2*MassB + 5*(16*g3^2*(2*MassB + MassG) + 
          9*g2^2*(2*MassB + MassWB)))*Kronecker[i1, i2] + 
      180*(2*MassB*MatMul[Adj[Yd], Yd][i1, i2] - MatMul[Adj[Yd], T[Yd]][i1, 
         i2] + 4*MassB*MatMul[Adj[Yu], Yu][i1, i2] - 
        2*MatMul[Adj[Yu], T[Yu]][i1, i2])))/225 - 
   2*\[Lambda]*conj[T[\[Lambda]]]*MatMul[Adj[Yu], T[Yu]][i1, i2] - 
   6*trace[conj[T[Yu]], Tp[Yu]]*MatMul[Adj[Yu], T[Yu]][i1, i2] - 
   (4*g1^2*MassB*MatMul[Adj[T[Yd]], Yd][i1, i2])/5 - 
   2*conj[\[Lambda]]*T[\[Lambda]]*MatMul[Adj[T[Yd]], Yd][i1, i2] - 
   6*trace[Adj[Yd], T[Yd]]*MatMul[Adj[T[Yd]], Yd][i1, i2] - 
   2*trace[Adj[Ye], T[Ye]]*MatMul[Adj[T[Yd]], Yd][i1, i2] + 
   (4*g1^2*MatMul[Adj[T[Yd]], T[Yd]][i1, i2])/5 - 
   2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[T[Yd]], T[Yd]][i1, i2] - 
   6*trace[Yd, Adj[Yd]]*MatMul[Adj[T[Yd]], T[Yd]][i1, i2] - 
   2*trace[Ye, Adj[Ye]]*MatMul[Adj[T[Yd]], T[Yd]][i1, i2] - 
   (8*g1^2*MassB*MatMul[Adj[T[Yu]], Yu][i1, i2])/5 - 
   2*conj[\[Lambda]]*T[\[Lambda]]*MatMul[Adj[T[Yu]], Yu][i1, i2] - 
   6*trace[Adj[Yu], T[Yu]]*MatMul[Adj[T[Yu]], Yu][i1, i2] + 
   (8*g1^2*MatMul[Adj[T[Yu]], T[Yu]][i1, i2])/5 - 
   2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[T[Yu]], T[Yu]][i1, i2] - 
   6*trace[Yu, Adj[Yu]]*MatMul[Adj[T[Yu]], T[Yu]][i1, i2] + 
   (2*g1^2*MatMul[mq2, Adj[Yd], Yd][i1, i2])/5 - \[Lambda]*conj[\[Lambda]]*
    MatMul[mq2, Adj[Yd], Yd][i1, i2] - 3*trace[Yd, Adj[Yd]]*
    MatMul[mq2, Adj[Yd], Yd][i1, i2] - trace[Ye, Adj[Ye]]*
    MatMul[mq2, Adj[Yd], Yd][i1, i2] + 
   (4*g1^2*MatMul[mq2, Adj[Yu], Yu][i1, i2])/5 - \[Lambda]*conj[\[Lambda]]*
    MatMul[mq2, Adj[Yu], Yu][i1, i2] - 3*trace[Yu, Adj[Yu]]*
    MatMul[mq2, Adj[Yu], Yu][i1, i2] + 
   (4*g1^2*MatMul[Adj[Yd], md2, Yd][i1, i2])/5 - 2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Adj[Yd], md2, Yd][i1, i2] - 6*trace[Yd, Adj[Yd]]*
    MatMul[Adj[Yd], md2, Yd][i1, i2] - 2*trace[Ye, Adj[Ye]]*
    MatMul[Adj[Yd], md2, Yd][i1, i2] + 
   (2*g1^2*MatMul[Adj[Yd], Yd, mq2][i1, i2])/5 - \[Lambda]*conj[\[Lambda]]*
    MatMul[Adj[Yd], Yd, mq2][i1, i2] - 3*trace[Yd, Adj[Yd]]*
    MatMul[Adj[Yd], Yd, mq2][i1, i2] - trace[Ye, Adj[Ye]]*
    MatMul[Adj[Yd], Yd, mq2][i1, i2] + 
   (8*g1^2*MatMul[Adj[Yu], mu2, Yu][i1, i2])/5 - 2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Adj[Yu], mu2, Yu][i1, i2] - 6*trace[Yu, Adj[Yu]]*
    MatMul[Adj[Yu], mu2, Yu][i1, i2] + 
   (4*g1^2*MatMul[Adj[Yu], Yu, mq2][i1, i2])/5 - \[Lambda]*conj[\[Lambda]]*
    MatMul[Adj[Yu], Yu, mq2][i1, i2] - 3*trace[Yu, Adj[Yu]]*
    MatMul[Adj[Yu], Yu, mq2][i1, i2] - 
   8*mHd2*MatMul[Adj[Yd], Yd, Adj[Yd], Yd][i1, i2] - 
   4*MatMul[Adj[Yd], Yd, Adj[T[Yd]], T[Yd]][i1, i2] - 
   4*MatMul[Adj[Yd], T[Yd], Adj[T[Yd]], Yd][i1, i2] - 
   8*mHu2*MatMul[Adj[Yu], Yu, Adj[Yu], Yu][i1, i2] - 
   4*MatMul[Adj[Yu], Yu, Adj[T[Yu]], T[Yu]][i1, i2] - 
   4*MatMul[Adj[Yu], T[Yu], Adj[T[Yu]], Yu][i1, i2] - 
   4*MatMul[Adj[T[Yd]], Yd, Adj[Yd], T[Yd]][i1, i2] - 
   4*MatMul[Adj[T[Yd]], T[Yd], Adj[Yd], Yd][i1, i2] - 
   4*MatMul[Adj[T[Yu]], Yu, Adj[Yu], T[Yu]][i1, i2] - 
   4*MatMul[Adj[T[Yu]], T[Yu], Adj[Yu], Yu][i1, i2] - 
   2*MatMul[mq2, Adj[Yd], Yd, Adj[Yd], Yd][i1, i2] - 
   2*MatMul[mq2, Adj[Yu], Yu, Adj[Yu], Yu][i1, i2] - 
   4*MatMul[Adj[Yd], md2, Yd, Adj[Yd], Yd][i1, i2] - 
   4*MatMul[Adj[Yd], Yd, mq2, Adj[Yd], Yd][i1, i2] - 
   4*MatMul[Adj[Yd], Yd, Adj[Yd], md2, Yd][i1, i2] - 
   2*MatMul[Adj[Yd], Yd, Adj[Yd], Yd, mq2][i1, i2] - 
   4*MatMul[Adj[Yu], mu2, Yu, Adj[Yu], Yu][i1, i2] - 
   4*MatMul[Adj[Yu], Yu, mq2, Adj[Yu], Yu][i1, i2] - 
   4*MatMul[Adj[Yu], Yu, Adj[Yu], mu2, Yu][i1, i2] - 
   2*MatMul[Adj[Yu], Yu, Adj[Yu], Yu, mq2][i1, i2]}, 
 {ml2[i1, i2], (-6*g1^2*MassB*conj[MassB]*Kronecker[i1, i2])/5 - 
   6*g2^2*MassWB*conj[MassWB]*Kronecker[i1, i2] - 
   Sqrt[3/5]*g1*Kronecker[i1, i2]*Tr1[1] + 
   2*mHd2*MatMul[Adj[Ye], Ye][i1, i2] + 2*MatMul[Adj[T[Ye]], T[Ye]][i1, i2] + 
   MatMul[ml2, Adj[Ye], Ye][i1, i2] + 2*MatMul[Adj[Ye], me2, Ye][i1, i2] + 
   MatMul[Adj[Ye], Ye, ml2][i1, i2], 
  (3*g2^2*(55*g2^2*MassWB + 3*g1^2*(MassB + 2*MassWB))*conj[MassWB]*
     Kronecker[i1, i2])/5 + 6*g2^4*Kronecker[i1, i2]*Tr2[2] + 
   (6*g1^2*Kronecker[i1, i2]*Tr2U1[1, 1])/5 - 
   4*Sqrt[3/5]*g1*Kronecker[i1, i2]*Tr3[1] + 
   (12*g1^2*mHd2*MatMul[Adj[Ye], Ye][i1, i2])/5 - 
   4*mHd2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   2*mHu2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   2*ms2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   2*conj[T[\[Lambda]]]*T[\[Lambda]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   12*mHd2*trace[Yd, Adj[Yd]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   4*mHd2*trace[Ye, Adj[Ye]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   6*trace[conj[T[Yd]], Tp[T[Yd]]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   2*trace[conj[T[Ye]], Tp[T[Ye]]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   6*trace[md2, Yd, Adj[Yd]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   2*trace[me2, Ye, Adj[Ye]]*MatMul[Adj[Ye], Ye][i1, i2] - 
   2*trace[ml2, Adj[Ye], Ye]*MatMul[Adj[Ye], Ye][i1, i2] - 
   6*trace[mq2, Adj[Yd], Yd]*MatMul[Adj[Ye], Ye][i1, i2] + 
   (3*g1^2*conj[MassB]*(3*(69*g1^2*MassB + 5*g2^2*(2*MassB + MassWB))*
       Kronecker[i1, i2] + 40*MassB*MatMul[Adj[Ye], Ye][i1, i2] - 
      20*MatMul[Adj[Ye], T[Ye]][i1, i2]))/25 - 2*\[Lambda]*conj[T[\[Lambda]]]*
    MatMul[Adj[Ye], T[Ye]][i1, i2] - 6*trace[conj[T[Yd]], Tp[Yd]]*
    MatMul[Adj[Ye], T[Ye]][i1, i2] - 2*trace[conj[T[Ye]], Tp[Ye]]*
    MatMul[Adj[Ye], T[Ye]][i1, i2] - 
   (12*g1^2*MassB*MatMul[Adj[T[Ye]], Ye][i1, i2])/5 - 
   2*conj[\[Lambda]]*T[\[Lambda]]*MatMul[Adj[T[Ye]], Ye][i1, i2] - 
   6*trace[Adj[Yd], T[Yd]]*MatMul[Adj[T[Ye]], Ye][i1, i2] - 
   2*trace[Adj[Ye], T[Ye]]*MatMul[Adj[T[Ye]], Ye][i1, i2] + 
   (12*g1^2*MatMul[Adj[T[Ye]], T[Ye]][i1, i2])/5 - 
   2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[T[Ye]], T[Ye]][i1, i2] - 
   6*trace[Yd, Adj[Yd]]*MatMul[Adj[T[Ye]], T[Ye]][i1, i2] - 
   2*trace[Ye, Adj[Ye]]*MatMul[Adj[T[Ye]], T[Ye]][i1, i2] + 
   (6*g1^2*MatMul[ml2, Adj[Ye], Ye][i1, i2])/5 - \[Lambda]*conj[\[Lambda]]*
    MatMul[ml2, Adj[Ye], Ye][i1, i2] - 3*trace[Yd, Adj[Yd]]*
    MatMul[ml2, Adj[Ye], Ye][i1, i2] - trace[Ye, Adj[Ye]]*
    MatMul[ml2, Adj[Ye], Ye][i1, i2] + 
   (12*g1^2*MatMul[Adj[Ye], me2, Ye][i1, i2])/5 - 
   2*\[Lambda]*conj[\[Lambda]]*MatMul[Adj[Ye], me2, Ye][i1, i2] - 
   6*trace[Yd, Adj[Yd]]*MatMul[Adj[Ye], me2, Ye][i1, i2] - 
   2*trace[Ye, Adj[Ye]]*MatMul[Adj[Ye], me2, Ye][i1, i2] + 
   (6*g1^2*MatMul[Adj[Ye], Ye, ml2][i1, i2])/5 - \[Lambda]*conj[\[Lambda]]*
    MatMul[Adj[Ye], Ye, ml2][i1, i2] - 3*trace[Yd, Adj[Yd]]*
    MatMul[Adj[Ye], Ye, ml2][i1, i2] - trace[Ye, Adj[Ye]]*
    MatMul[Adj[Ye], Ye, ml2][i1, i2] - 
   8*mHd2*MatMul[Adj[Ye], Ye, Adj[Ye], Ye][i1, i2] - 
   4*MatMul[Adj[Ye], Ye, Adj[T[Ye]], T[Ye]][i1, i2] - 
   4*MatMul[Adj[Ye], T[Ye], Adj[T[Ye]], Ye][i1, i2] - 
   4*MatMul[Adj[T[Ye]], Ye, Adj[Ye], T[Ye]][i1, i2] - 
   4*MatMul[Adj[T[Ye]], T[Ye], Adj[Ye], Ye][i1, i2] - 
   2*MatMul[ml2, Adj[Ye], Ye, Adj[Ye], Ye][i1, i2] - 
   4*MatMul[Adj[Ye], me2, Ye, Adj[Ye], Ye][i1, i2] - 
   4*MatMul[Adj[Ye], Ye, ml2, Adj[Ye], Ye][i1, i2] - 
   4*MatMul[Adj[Ye], Ye, Adj[Ye], me2, Ye][i1, i2] - 
   2*MatMul[Adj[Ye], Ye, Adj[Ye], Ye, ml2][i1, i2]}, 
 {mHd2, (-6*g1^2*MassB*conj[MassB])/5 - 6*g2^2*MassWB*conj[MassWB] + 
   2*mHd2*\[Lambda]*conj[\[Lambda]] + 2*mHu2*\[Lambda]*conj[\[Lambda]] + 
   2*ms2*\[Lambda]*conj[\[Lambda]] + 2*conj[T[\[Lambda]]]*T[\[Lambda]] - 
   Sqrt[3/5]*g1*Tr1[1] + 6*mHd2*trace[Yd, Adj[Yd]] + 
   2*mHd2*trace[Ye, Adj[Ye]] + 6*trace[conj[T[Yd]], Tp[T[Yd]]] + 
   2*trace[conj[T[Ye]], Tp[T[Ye]]] + 6*trace[md2, Yd, Adj[Yd]] + 
   2*trace[me2, Ye, Adj[Ye]] + 2*trace[ml2, Adj[Ye], Ye] + 
   6*trace[mq2, Adj[Yd], Yd], 
  (g1^2*conj[MassB]*(621*g1^2*MassB + 90*g2^2*MassB + 45*g2^2*MassWB - 
      40*MassB*trace[Yd, Adj[Yd]] + 120*MassB*trace[Ye, Adj[Ye]] + 
      20*trace[Adj[Yd], T[Yd]] - 60*trace[Adj[Ye], T[Ye]]) + 
    5*(3*g2^2*(55*g2^2*MassWB + 3*g1^2*(MassB + 2*MassWB))*conj[MassWB] - 
      2*(30*(mHd2 + mHu2 + ms2)*\[Lambda]^2*conj[\[Lambda]]^2 + 
        10*conj[\[Kappa]]*((mHd2 + mHu2 + 4*ms2)*\[Kappa]*\[Lambda]*
           conj[\[Lambda]] + conj[T[\[Lambda]]]*(\[Lambda]*T[\[Kappa]] + 
            \[Kappa]*T[\[Lambda]])) - 15*g2^4*Tr2[2] - 3*g1^2*Tr2U1[1, 1] + 
        2*Sqrt[15]*g1*Tr3[1] + 2*g1^2*mHd2*trace[Yd, Adj[Yd]] - 
        80*g3^2*mHd2*trace[Yd, Adj[Yd]] - 160*g3^2*MassG*conj[MassG]*
         trace[Yd, Adj[Yd]] - 6*g1^2*mHd2*trace[Ye, Adj[Ye]] + 
        15*conj[T[\[Lambda]]]*T[\[Lambda]]*trace[Yu, Adj[Yu]] + 
        80*g3^2*conj[MassG]*trace[Adj[Yd], T[Yd]] + 
        15*\[Lambda]*conj[T[\[Lambda]]]*trace[Adj[Yu], T[Yu]] - 
        2*g1^2*MassB*trace[conj[T[Yd]], Tp[Yd]] + 80*g3^2*MassG*
         trace[conj[T[Yd]], Tp[Yd]] + 2*g1^2*trace[conj[T[Yd]], Tp[T[Yd]]] - 
        80*g3^2*trace[conj[T[Yd]], Tp[T[Yd]]] + 6*g1^2*MassB*
         trace[conj[T[Ye]], Tp[Ye]] - 6*g1^2*trace[conj[T[Ye]], Tp[T[Ye]]] + 
        2*g1^2*trace[md2, Yd, Adj[Yd]] - 80*g3^2*trace[md2, Yd, Adj[Yd]] - 
        6*g1^2*trace[me2, Ye, Adj[Ye]] - 6*g1^2*trace[ml2, Adj[Ye], Ye] + 
        2*g1^2*trace[mq2, Adj[Yd], Yd] - 80*g3^2*trace[mq2, Adj[Yd], Yd] + 
        5*conj[\[Lambda]]*(2*conj[T[\[Kappa]]]*(\[Lambda]*T[\[Kappa]] + 
            \[Kappa]*T[\[Lambda]]) + 3*(4*\[Lambda]*conj[T[\[Lambda]]]*
             T[\[Lambda]] + (mHd2 + 2*mHu2 + ms2)*\[Lambda]*
             trace[Yu, Adj[Yu]] + T[\[Lambda]]*trace[conj[T[Yu]], Tp[Yu]] + 
            \[Lambda]*trace[conj[T[Yu]], Tp[T[Yu]]] + \[Lambda]*
             trace[mq2, Adj[Yu], Yu] + \[Lambda]*trace[mu2, Yu, Adj[Yu]])) + 
        90*mHd2*trace[Yd, Adj[Yd], Yd, Adj[Yd]] + 
        90*trace[Yd, Adj[Yd], T[Yd], Adj[T[Yd]]] + 
        15*mHd2*trace[Yd, Adj[Yu], Yu, Adj[Yd]] + 
        15*mHu2*trace[Yd, Adj[Yu], Yu, Adj[Yd]] + 
        15*trace[Yd, Adj[Yu], T[Yu], Adj[T[Yd]]] + 
        90*trace[Yd, Adj[T[Yd]], T[Yd], Adj[Yd]] + 
        15*trace[Yd, Adj[T[Yu]], T[Yu], Adj[Yd]] + 
        30*mHd2*trace[Ye, Adj[Ye], Ye, Adj[Ye]] + 
        30*trace[Ye, Adj[Ye], T[Ye], Adj[T[Ye]]] + 
        30*trace[Ye, Adj[T[Ye]], T[Ye], Adj[Ye]] + 
        15*trace[Yu, Adj[Yd], T[Yd], Adj[T[Yu]]] + 
        15*trace[Yu, Adj[T[Yd]], T[Yd], Adj[Yu]] + 
        90*trace[md2, Yd, Adj[Yd], Yd, Adj[Yd]] + 
        15*trace[md2, Yd, Adj[Yu], Yu, Adj[Yd]] + 
        30*trace[me2, Ye, Adj[Ye], Ye, Adj[Ye]] + 
        30*trace[ml2, Adj[Ye], Ye, Adj[Ye], Ye] + 
        90*trace[mq2, Adj[Yd], Yd, Adj[Yd], Yd] + 
        15*trace[mq2, Adj[Yd], Yd, Adj[Yu], Yu] + 
        15*trace[mq2, Adj[Yu], Yu, Adj[Yd], Yd] + 
        15*trace[mu2, Yu, Adj[Yd], Yd, Adj[Yu]])))/25}, 
 {mHu2, (-6*g1^2*MassB*conj[MassB])/5 - 6*g2^2*MassWB*conj[MassWB] + 
   2*mHd2*\[Lambda]*conj[\[Lambda]] + 2*mHu2*\[Lambda]*conj[\[Lambda]] + 
   2*ms2*\[Lambda]*conj[\[Lambda]] + 2*conj[T[\[Lambda]]]*T[\[Lambda]] + 
   Sqrt[3/5]*g1*Tr1[1] + 6*mHu2*trace[Yu, Adj[Yu]] + 
   6*trace[conj[T[Yu]], Tp[T[Yu]]] + 6*trace[mq2, Adj[Yu], Yu] + 
   6*trace[mu2, Yu, Adj[Yu]], 
  (g1^2*conj[MassB]*(621*g1^2*MassB + 90*g2^2*MassB + 45*g2^2*MassWB + 
      80*MassB*trace[Yu, Adj[Yu]] - 40*trace[Adj[Yu], T[Yu]]) + 
    5*(3*g2^2*(55*g2^2*MassWB + 3*g1^2*(MassB + 2*MassWB))*conj[MassWB] - 
      2*(30*(mHd2 + mHu2 + ms2)*\[Lambda]^2*conj[\[Lambda]]^2 + 
        10*conj[\[Kappa]]*((mHd2 + mHu2 + 4*ms2)*\[Kappa]*\[Lambda]*
           conj[\[Lambda]] + conj[T[\[Lambda]]]*(\[Lambda]*T[\[Kappa]] + 
            \[Kappa]*T[\[Lambda]])) - 15*g2^4*Tr2[2] - 3*g1^2*Tr2U1[1, 1] - 
        2*Sqrt[15]*g1*Tr3[1] + 15*conj[T[\[Lambda]]]*T[\[Lambda]]*
         trace[Yd, Adj[Yd]] + 5*conj[T[\[Lambda]]]*T[\[Lambda]]*
         trace[Ye, Adj[Ye]] - 4*g1^2*mHu2*trace[Yu, Adj[Yu]] - 
        80*g3^2*mHu2*trace[Yu, Adj[Yu]] - 160*g3^2*MassG*conj[MassG]*
         trace[Yu, Adj[Yu]] + 15*\[Lambda]*conj[T[\[Lambda]]]*
         trace[Adj[Yd], T[Yd]] + 5*\[Lambda]*conj[T[\[Lambda]]]*
         trace[Adj[Ye], T[Ye]] + 80*g3^2*conj[MassG]*trace[Adj[Yu], T[Yu]] + 
        4*g1^2*MassB*trace[conj[T[Yu]], Tp[Yu]] + 80*g3^2*MassG*
         trace[conj[T[Yu]], Tp[Yu]] - 4*g1^2*trace[conj[T[Yu]], Tp[T[Yu]]] - 
        80*g3^2*trace[conj[T[Yu]], Tp[T[Yu]]] + 5*conj[\[Lambda]]*
         (12*\[Lambda]*conj[T[\[Lambda]]]*T[\[Lambda]] + 2*conj[T[\[Kappa]]]*
           (\[Lambda]*T[\[Kappa]] + \[Kappa]*T[\[Lambda]]) + 
          6*mHd2*\[Lambda]*trace[Yd, Adj[Yd]] + 3*mHu2*\[Lambda]*
           trace[Yd, Adj[Yd]] + 3*ms2*\[Lambda]*trace[Yd, Adj[Yd]] + 
          2*mHd2*\[Lambda]*trace[Ye, Adj[Ye]] + mHu2*\[Lambda]*
           trace[Ye, Adj[Ye]] + ms2*\[Lambda]*trace[Ye, Adj[Ye]] + 
          3*T[\[Lambda]]*trace[conj[T[Yd]], Tp[Yd]] + 3*\[Lambda]*
           trace[conj[T[Yd]], Tp[T[Yd]]] + T[\[Lambda]]*trace[conj[T[Ye]], 
            Tp[Ye]] + \[Lambda]*trace[conj[T[Ye]], Tp[T[Ye]]] + 
          3*\[Lambda]*trace[md2, Yd, Adj[Yd]] + \[Lambda]*
           trace[me2, Ye, Adj[Ye]] + \[Lambda]*trace[ml2, Adj[Ye], Ye] + 
          3*\[Lambda]*trace[mq2, Adj[Yd], Yd]) - 
        4*g1^2*trace[mq2, Adj[Yu], Yu] - 80*g3^2*trace[mq2, Adj[Yu], Yu] - 
        4*g1^2*trace[mu2, Yu, Adj[Yu]] - 80*g3^2*trace[mu2, Yu, Adj[Yu]] + 
        15*mHd2*trace[Yd, Adj[Yu], Yu, Adj[Yd]] + 
        15*mHu2*trace[Yd, Adj[Yu], Yu, Adj[Yd]] + 
        15*trace[Yd, Adj[Yu], T[Yu], Adj[T[Yd]]] + 
        15*trace[Yd, Adj[T[Yu]], T[Yu], Adj[Yd]] + 
        15*trace[Yu, Adj[Yd], T[Yd], Adj[T[Yu]]] + 
        90*mHu2*trace[Yu, Adj[Yu], Yu, Adj[Yu]] + 
        90*trace[Yu, Adj[Yu], T[Yu], Adj[T[Yu]]] + 
        15*trace[Yu, Adj[T[Yd]], T[Yd], Adj[Yu]] + 
        90*trace[Yu, Adj[T[Yu]], T[Yu], Adj[Yu]] + 
        15*trace[md2, Yd, Adj[Yu], Yu, Adj[Yd]] + 
        15*trace[mq2, Adj[Yd], Yd, Adj[Yu], Yu] + 
        15*trace[mq2, Adj[Yu], Yu, Adj[Yd], Yd] + 
        90*trace[mq2, Adj[Yu], Yu, Adj[Yu], Yu] + 
        15*trace[mu2, Yu, Adj[Yd], Yd, Adj[Yu]] + 
        90*trace[mu2, Yu, Adj[Yu], Yu, Adj[Yu]])))/25}, 
 {md2[i1, i2], (-8*g1^2*MassB*conj[MassB]*Kronecker[i1, i2])/15 - 
   (32*g3^2*MassG*conj[MassG]*Kronecker[i1, i2])/3 + 
   (2*g1*Kronecker[i1, i2]*Tr1[1])/Sqrt[15] + 
   4*mHd2*MatMul[Yd, Adj[Yd]][i1, i2] + 4*MatMul[T[Yd], Adj[T[Yd]]][i1, i2] + 
   2*MatMul[md2, Yd, Adj[Yd]][i1, i2] + 4*MatMul[Yd, mq2, Adj[Yd]][i1, i2] + 
   2*MatMul[Yd, Adj[Yd], md2][i1, i2], 
  (64*g3^2*(-30*g3^2*MassG + g1^2*(MassB + 2*MassG))*conj[MassG]*
     Kronecker[i1, i2])/45 + (32*g3^4*Kronecker[i1, i2]*Tr2[3])/3 + 
   (8*g1^2*Kronecker[i1, i2]*Tr2U1[1, 1])/15 + 
   (8*g1*Kronecker[i1, i2]*Tr3[1])/Sqrt[15] + 
   (4*g1^2*mHd2*MatMul[Yd, Adj[Yd]][i1, i2])/5 + 
   12*g2^2*mHd2*MatMul[Yd, Adj[Yd]][i1, i2] + 24*g2^2*MassWB*conj[MassWB]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 8*mHd2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 4*mHu2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 4*ms2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 4*conj[T[\[Lambda]]]*T[\[Lambda]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 24*mHd2*trace[Yd, Adj[Yd]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 8*mHd2*trace[Ye, Adj[Ye]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 12*trace[conj[T[Yd]], Tp[T[Yd]]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 4*trace[conj[T[Ye]], Tp[T[Ye]]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 12*trace[md2, Yd, Adj[Yd]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 4*trace[me2, Ye, Adj[Ye]]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 4*trace[ml2, Adj[Ye], Ye]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 12*trace[mq2, Adj[Yd], Yd]*
    MatMul[Yd, Adj[Yd]][i1, i2] - 
   (4*g1^2*MassB*MatMul[Yd, Adj[T[Yd]]][i1, i2])/5 - 
   12*g2^2*MassWB*MatMul[Yd, Adj[T[Yd]]][i1, i2] - 
   4*conj[\[Lambda]]*T[\[Lambda]]*MatMul[Yd, Adj[T[Yd]]][i1, i2] - 
   12*trace[Adj[Yd], T[Yd]]*MatMul[Yd, Adj[T[Yd]]][i1, i2] - 
   4*trace[Adj[Ye], T[Ye]]*MatMul[Yd, Adj[T[Yd]]][i1, i2] + 
   (4*g1^2*conj[MassB]*(2*(303*g1^2*MassB + 40*g3^2*(2*MassB + MassG))*
       Kronecker[i1, i2] + 90*MassB*MatMul[Yd, Adj[Yd]][i1, i2] - 
      45*MatMul[T[Yd], Adj[Yd]][i1, i2]))/225 - 
   12*g2^2*conj[MassWB]*MatMul[T[Yd], Adj[Yd]][i1, i2] - 
   4*\[Lambda]*conj[T[\[Lambda]]]*MatMul[T[Yd], Adj[Yd]][i1, i2] - 
   12*trace[conj[T[Yd]], Tp[Yd]]*MatMul[T[Yd], Adj[Yd]][i1, i2] - 
   4*trace[conj[T[Ye]], Tp[Ye]]*MatMul[T[Yd], Adj[Yd]][i1, i2] + 
   (4*g1^2*MatMul[T[Yd], Adj[T[Yd]]][i1, i2])/5 + 
   12*g2^2*MatMul[T[Yd], Adj[T[Yd]]][i1, i2] - 4*\[Lambda]*conj[\[Lambda]]*
    MatMul[T[Yd], Adj[T[Yd]]][i1, i2] - 12*trace[Yd, Adj[Yd]]*
    MatMul[T[Yd], Adj[T[Yd]]][i1, i2] - 4*trace[Ye, Adj[Ye]]*
    MatMul[T[Yd], Adj[T[Yd]]][i1, i2] + 
   (2*g1^2*MatMul[md2, Yd, Adj[Yd]][i1, i2])/5 + 
   6*g2^2*MatMul[md2, Yd, Adj[Yd]][i1, i2] - 2*\[Lambda]*conj[\[Lambda]]*
    MatMul[md2, Yd, Adj[Yd]][i1, i2] - 6*trace[Yd, Adj[Yd]]*
    MatMul[md2, Yd, Adj[Yd]][i1, i2] - 2*trace[Ye, Adj[Ye]]*
    MatMul[md2, Yd, Adj[Yd]][i1, i2] + 
   (4*g1^2*MatMul[Yd, mq2, Adj[Yd]][i1, i2])/5 + 
   12*g2^2*MatMul[Yd, mq2, Adj[Yd]][i1, i2] - 4*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yd, mq2, Adj[Yd]][i1, i2] - 12*trace[Yd, Adj[Yd]]*
    MatMul[Yd, mq2, Adj[Yd]][i1, i2] - 4*trace[Ye, Adj[Ye]]*
    MatMul[Yd, mq2, Adj[Yd]][i1, i2] + 
   (2*g1^2*MatMul[Yd, Adj[Yd], md2][i1, i2])/5 + 
   6*g2^2*MatMul[Yd, Adj[Yd], md2][i1, i2] - 2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yd, Adj[Yd], md2][i1, i2] - 6*trace[Yd, Adj[Yd]]*
    MatMul[Yd, Adj[Yd], md2][i1, i2] - 2*trace[Ye, Adj[Ye]]*
    MatMul[Yd, Adj[Yd], md2][i1, i2] - 
   8*mHd2*MatMul[Yd, Adj[Yd], Yd, Adj[Yd]][i1, i2] - 
   4*MatMul[Yd, Adj[Yd], T[Yd], Adj[T[Yd]]][i1, i2] - 
   4*mHd2*MatMul[Yd, Adj[Yu], Yu, Adj[Yd]][i1, i2] - 
   4*mHu2*MatMul[Yd, Adj[Yu], Yu, Adj[Yd]][i1, i2] - 
   4*MatMul[Yd, Adj[Yu], T[Yu], Adj[T[Yd]]][i1, i2] - 
   4*MatMul[Yd, Adj[T[Yd]], T[Yd], Adj[Yd]][i1, i2] - 
   4*MatMul[Yd, Adj[T[Yu]], T[Yu], Adj[Yd]][i1, i2] - 
   4*MatMul[T[Yd], Adj[Yd], Yd, Adj[T[Yd]]][i1, i2] - 
   4*MatMul[T[Yd], Adj[Yu], Yu, Adj[T[Yd]]][i1, i2] - 
   4*MatMul[T[Yd], Adj[T[Yd]], Yd, Adj[Yd]][i1, i2] - 
   4*MatMul[T[Yd], Adj[T[Yu]], Yu, Adj[Yd]][i1, i2] - 
   2*MatMul[md2, Yd, Adj[Yd], Yd, Adj[Yd]][i1, i2] - 
   2*MatMul[md2, Yd, Adj[Yu], Yu, Adj[Yd]][i1, i2] - 
   4*MatMul[Yd, mq2, Adj[Yd], Yd, Adj[Yd]][i1, i2] - 
   4*MatMul[Yd, mq2, Adj[Yu], Yu, Adj[Yd]][i1, i2] - 
   4*MatMul[Yd, Adj[Yd], md2, Yd, Adj[Yd]][i1, i2] - 
   4*MatMul[Yd, Adj[Yd], Yd, mq2, Adj[Yd]][i1, i2] - 
   2*MatMul[Yd, Adj[Yd], Yd, Adj[Yd], md2][i1, i2] - 
   4*MatMul[Yd, Adj[Yu], mu2, Yu, Adj[Yd]][i1, i2] - 
   4*MatMul[Yd, Adj[Yu], Yu, mq2, Adj[Yd]][i1, i2] - 
   2*MatMul[Yd, Adj[Yu], Yu, Adj[Yd], md2][i1, i2]}, 
 {mu2[i1, i2], (-32*g1^2*MassB*conj[MassB]*Kronecker[i1, i2])/15 - 
   (32*g3^2*MassG*conj[MassG]*Kronecker[i1, i2])/3 - 
   (4*g1*Kronecker[i1, i2]*Tr1[1])/Sqrt[15] + 
   4*mHu2*MatMul[Yu, Adj[Yu]][i1, i2] + 4*MatMul[T[Yu], Adj[T[Yu]]][i1, i2] + 
   2*MatMul[mu2, Yu, Adj[Yu]][i1, i2] + 4*MatMul[Yu, mq2, Adj[Yu]][i1, i2] + 
   2*MatMul[Yu, Adj[Yu], mu2][i1, i2], 
  (-128*g3^2*(15*g3^2*MassG - 2*g1^2*(MassB + 2*MassG))*conj[MassG]*
     Kronecker[i1, i2])/45 + (32*g3^4*Kronecker[i1, i2]*Tr2[3])/3 + 
   (32*g1^2*Kronecker[i1, i2]*Tr2U1[1, 1])/15 - 
   (16*g1*Kronecker[i1, i2]*Tr3[1])/Sqrt[15] - 
   (4*g1^2*mHu2*MatMul[Yu, Adj[Yu]][i1, i2])/5 + 
   12*g2^2*mHu2*MatMul[Yu, Adj[Yu]][i1, i2] + 24*g2^2*MassWB*conj[MassWB]*
    MatMul[Yu, Adj[Yu]][i1, i2] - 4*mHd2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yu, Adj[Yu]][i1, i2] - 8*mHu2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yu, Adj[Yu]][i1, i2] - 4*ms2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yu, Adj[Yu]][i1, i2] - 4*conj[T[\[Lambda]]]*T[\[Lambda]]*
    MatMul[Yu, Adj[Yu]][i1, i2] - 24*mHu2*trace[Yu, Adj[Yu]]*
    MatMul[Yu, Adj[Yu]][i1, i2] - 12*trace[conj[T[Yu]], Tp[T[Yu]]]*
    MatMul[Yu, Adj[Yu]][i1, i2] - 12*trace[mq2, Adj[Yu], Yu]*
    MatMul[Yu, Adj[Yu]][i1, i2] - 12*trace[mu2, Yu, Adj[Yu]]*
    MatMul[Yu, Adj[Yu]][i1, i2] + 
   (4*g1^2*MassB*MatMul[Yu, Adj[T[Yu]]][i1, i2])/5 - 
   12*g2^2*MassWB*MatMul[Yu, Adj[T[Yu]]][i1, i2] - 
   4*conj[\[Lambda]]*T[\[Lambda]]*MatMul[Yu, Adj[T[Yu]]][i1, i2] - 
   12*trace[Adj[Yu], T[Yu]]*MatMul[Yu, Adj[T[Yu]]][i1, i2] - 
   12*g2^2*conj[MassWB]*MatMul[T[Yu], Adj[Yu]][i1, i2] - 
   4*\[Lambda]*conj[T[\[Lambda]]]*MatMul[T[Yu], Adj[Yu]][i1, i2] - 
   12*trace[conj[T[Yu]], Tp[Yu]]*MatMul[T[Yu], Adj[Yu]][i1, i2] + 
   (4*g1^2*conj[MassB]*(8*(321*g1^2*MassB + 40*g3^2*(2*MassB + MassG))*
       Kronecker[i1, i2] + 45*(-2*MassB*MatMul[Yu, Adj[Yu]][i1, i2] + 
        MatMul[T[Yu], Adj[Yu]][i1, i2])))/225 - 
   (4*g1^2*MatMul[T[Yu], Adj[T[Yu]]][i1, i2])/5 + 
   12*g2^2*MatMul[T[Yu], Adj[T[Yu]]][i1, i2] - 4*\[Lambda]*conj[\[Lambda]]*
    MatMul[T[Yu], Adj[T[Yu]]][i1, i2] - 12*trace[Yu, Adj[Yu]]*
    MatMul[T[Yu], Adj[T[Yu]]][i1, i2] - 
   (2*g1^2*MatMul[mu2, Yu, Adj[Yu]][i1, i2])/5 + 
   6*g2^2*MatMul[mu2, Yu, Adj[Yu]][i1, i2] - 2*\[Lambda]*conj[\[Lambda]]*
    MatMul[mu2, Yu, Adj[Yu]][i1, i2] - 6*trace[Yu, Adj[Yu]]*
    MatMul[mu2, Yu, Adj[Yu]][i1, i2] - 
   (4*g1^2*MatMul[Yu, mq2, Adj[Yu]][i1, i2])/5 + 
   12*g2^2*MatMul[Yu, mq2, Adj[Yu]][i1, i2] - 4*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yu, mq2, Adj[Yu]][i1, i2] - 12*trace[Yu, Adj[Yu]]*
    MatMul[Yu, mq2, Adj[Yu]][i1, i2] - 
   (2*g1^2*MatMul[Yu, Adj[Yu], mu2][i1, i2])/5 + 
   6*g2^2*MatMul[Yu, Adj[Yu], mu2][i1, i2] - 2*\[Lambda]*conj[\[Lambda]]*
    MatMul[Yu, Adj[Yu], mu2][i1, i2] - 6*trace[Yu, Adj[Yu]]*
    MatMul[Yu, Adj[Yu], mu2][i1, i2] - 
   4*mHd2*MatMul[Yu, Adj[Yd], Yd, Adj[Yu]][i1, i2] - 
   4*mHu2*MatMul[Yu, Adj[Yd], Yd, Adj[Yu]][i1, i2] - 
   4*MatMul[Yu, Adj[Yd], T[Yd], Adj[T[Yu]]][i1, i2] - 
   8*mHu2*MatMul[Yu, Adj[Yu], Yu, Adj[Yu]][i1, i2] - 
   4*MatMul[Yu, Adj[Yu], T[Yu], Adj[T[Yu]]][i1, i2] - 
   4*MatMul[Yu, Adj[T[Yd]], T[Yd], Adj[Yu]][i1, i2] - 
   4*MatMul[Yu, Adj[T[Yu]], T[Yu], Adj[Yu]][i1, i2] - 
   4*MatMul[T[Yu], Adj[Yd], Yd, Adj[T[Yu]]][i1, i2] - 
   4*MatMul[T[Yu], Adj[Yu], Yu, Adj[T[Yu]]][i1, i2] - 
   4*MatMul[T[Yu], Adj[T[Yd]], Yd, Adj[Yu]][i1, i2] - 
   4*MatMul[T[Yu], Adj[T[Yu]], Yu, Adj[Yu]][i1, i2] - 
   2*MatMul[mu2, Yu, Adj[Yd], Yd, Adj[Yu]][i1, i2] - 
   2*MatMul[mu2, Yu, Adj[Yu], Yu, Adj[Yu]][i1, i2] - 
   4*MatMul[Yu, mq2, Adj[Yd], Yd, Adj[Yu]][i1, i2] - 
   4*MatMul[Yu, mq2, Adj[Yu], Yu, Adj[Yu]][i1, i2] - 
   4*MatMul[Yu, Adj[Yd], md2, Yd, Adj[Yu]][i1, i2] - 
   4*MatMul[Yu, Adj[Yd], Yd, mq2, Adj[Yu]][i1, i2] - 
   2*MatMul[Yu, Adj[Yd], Yd, Adj[Yu], mu2][i1, i2] - 
   4*MatMul[Yu, Adj[Yu], mu2, Yu, Adj[Yu]][i1, i2] - 
   4*MatMul[Yu, Adj[Yu], Yu, mq2, Adj[Yu]][i1, i2] - 
   2*MatMul[Yu, Adj[Yu], Yu, Adj[Yu], mu2][i1, i2]}, 
 {me2[i1, i2], (-24*g1^2*MassB*conj[MassB]*Kronecker[i1, i2])/5 + 
   2*Sqrt[3/5]*g1*Kronecker[i1, i2]*Tr1[1] + 
   2*(2*mHd2*MatMul[Ye, Adj[Ye]][i1, i2] + 
     2*MatMul[T[Ye], Adj[T[Ye]]][i1, i2] + MatMul[me2, Ye, Adj[Ye]][i1, i2] + 
     2*MatMul[Ye, ml2, Adj[Ye]][i1, i2] + MatMul[Ye, Adj[Ye], me2][i1, i2]), 
  (2*(20*g1*Kronecker[i1, i2]*(3*g1*Tr2U1[1, 1] + Sqrt[15]*Tr3[1]) + 
     6*g1^2*conj[MassB]*(234*g1^2*MassB*Kronecker[i1, i2] + 
       5*(-2*MassB*MatMul[Ye, Adj[Ye]][i1, i2] + MatMul[T[Ye], Adj[Ye]][i1, 
          i2])) - 5*(2*(3*g1^2*mHd2 - 15*g2^2*mHd2 - 30*g2^2*MassWB*
          conj[MassWB] + 5*(2*mHd2 + mHu2 + ms2)*\[Lambda]*conj[\[Lambda]] + 
         5*conj[T[\[Lambda]]]*T[\[Lambda]] + 30*mHd2*trace[Yd, Adj[Yd]] + 
         10*mHd2*trace[Ye, Adj[Ye]] + 15*trace[conj[T[Yd]], Tp[T[Yd]]] + 
         5*trace[conj[T[Ye]], Tp[T[Ye]]] + 15*trace[md2, Yd, Adj[Yd]] + 
         5*trace[me2, Ye, Adj[Ye]] + 5*trace[ml2, Adj[Ye], Ye] + 
         15*trace[mq2, Adj[Yd], Yd])*MatMul[Ye, Adj[Ye]][i1, i2] + 
       (-6*g1^2*MassB + 30*g2^2*MassWB + 10*conj[\[Lambda]]*T[\[Lambda]] + 
         30*trace[Adj[Yd], T[Yd]] + 10*trace[Adj[Ye], T[Ye]])*
        MatMul[Ye, Adj[T[Ye]]][i1, i2] + 30*g2^2*conj[MassWB]*
        MatMul[T[Ye], Adj[Ye]][i1, i2] + 10*\[Lambda]*conj[T[\[Lambda]]]*
        MatMul[T[Ye], Adj[Ye]][i1, i2] + 30*trace[conj[T[Yd]], Tp[Yd]]*
        MatMul[T[Ye], Adj[Ye]][i1, i2] + 10*trace[conj[T[Ye]], Tp[Ye]]*
        MatMul[T[Ye], Adj[Ye]][i1, i2] + 6*g1^2*MatMul[T[Ye], Adj[T[Ye]]][i1, 
         i2] - 30*g2^2*MatMul[T[Ye], Adj[T[Ye]]][i1, i2] + 
       10*\[Lambda]*conj[\[Lambda]]*MatMul[T[Ye], Adj[T[Ye]]][i1, i2] + 
       30*trace[Yd, Adj[Yd]]*MatMul[T[Ye], Adj[T[Ye]]][i1, i2] + 
       10*trace[Ye, Adj[Ye]]*MatMul[T[Ye], Adj[T[Ye]]][i1, i2] + 
       3*g1^2*MatMul[me2, Ye, Adj[Ye]][i1, i2] - 
       15*g2^2*MatMul[me2, Ye, Adj[Ye]][i1, i2] + 5*\[Lambda]*conj[\[Lambda]]*
        MatMul[me2, Ye, Adj[Ye]][i1, i2] + 15*trace[Yd, Adj[Yd]]*
        MatMul[me2, Ye, Adj[Ye]][i1, i2] + 5*trace[Ye, Adj[Ye]]*
        MatMul[me2, Ye, Adj[Ye]][i1, i2] + 6*g1^2*MatMul[Ye, ml2, Adj[Ye]][
         i1, i2] - 30*g2^2*MatMul[Ye, ml2, Adj[Ye]][i1, i2] + 
       10*\[Lambda]*conj[\[Lambda]]*MatMul[Ye, ml2, Adj[Ye]][i1, i2] + 
       30*trace[Yd, Adj[Yd]]*MatMul[Ye, ml2, Adj[Ye]][i1, i2] + 
       10*trace[Ye, Adj[Ye]]*MatMul[Ye, ml2, Adj[Ye]][i1, i2] + 
       3*g1^2*MatMul[Ye, Adj[Ye], me2][i1, i2] - 
       15*g2^2*MatMul[Ye, Adj[Ye], me2][i1, i2] + 5*\[Lambda]*conj[\[Lambda]]*
        MatMul[Ye, Adj[Ye], me2][i1, i2] + 15*trace[Yd, Adj[Yd]]*
        MatMul[Ye, Adj[Ye], me2][i1, i2] + 5*trace[Ye, Adj[Ye]]*
        MatMul[Ye, Adj[Ye], me2][i1, i2] + 
       20*mHd2*MatMul[Ye, Adj[Ye], Ye, Adj[Ye]][i1, i2] + 
       10*MatMul[Ye, Adj[Ye], T[Ye], Adj[T[Ye]]][i1, i2] + 
       10*MatMul[Ye, Adj[T[Ye]], T[Ye], Adj[Ye]][i1, i2] + 
       10*MatMul[T[Ye], Adj[Ye], Ye, Adj[T[Ye]]][i1, i2] + 
       10*MatMul[T[Ye], Adj[T[Ye]], Ye, Adj[Ye]][i1, i2] + 
       5*MatMul[me2, Ye, Adj[Ye], Ye, Adj[Ye]][i1, i2] + 
       10*MatMul[Ye, ml2, Adj[Ye], Ye, Adj[Ye]][i1, i2] + 
       10*MatMul[Ye, Adj[Ye], me2, Ye, Adj[Ye]][i1, i2] + 
       10*MatMul[Ye, Adj[Ye], Ye, ml2, Adj[Ye]][i1, i2] + 
       5*MatMul[Ye, Adj[Ye], Ye, Adj[Ye], me2][i1, i2])))/25}, 
 {ms2, 4*(3*ms2*\[Kappa]*conj[\[Kappa]] + (mHd2 + mHu2 + ms2)*\[Lambda]*
     conj[\[Lambda]] + conj[T[\[Kappa]]]*T[\[Kappa]] + 
    conj[T[\[Lambda]]]*T[\[Lambda]]), 
  (-4*(120*ms2*\[Kappa]^2*conj[\[Kappa]]^2 + 20*(mHd2 + mHu2 + ms2)*
      \[Lambda]^2*conj[\[Lambda]]^2 + 20*conj[\[Kappa]]*
      ((mHd2 + mHu2 + 4*ms2)*\[Kappa]*\[Lambda]*conj[\[Lambda]] + 
       4*\[Kappa]*conj[T[\[Kappa]]]*T[\[Kappa]] + conj[T[\[Lambda]]]*
        (\[Lambda]*T[\[Kappa]] + \[Kappa]*T[\[Lambda]])) + 
     conj[T[\[Lambda]]]*(T[\[Lambda]]*(15*trace[Yd, Adj[Yd]] + 
         5*trace[Ye, Adj[Ye]] - 3*(g1^2 + 5*g2^2 - 5*trace[Yu, Adj[Yu]])) + 
       \[Lambda]*(15*trace[Adj[Yd], T[Yd]] + 5*trace[Adj[Ye], T[Ye]] + 
         3*(g1^2*MassB + 5*g2^2*MassWB + 5*trace[Adj[Yu], T[Yu]]))) + 
     conj[\[Lambda]]*(-3*g1^2*mHd2*\[Lambda] - 15*g2^2*mHd2*\[Lambda] - 
       3*g1^2*mHu2*\[Lambda] - 15*g2^2*mHu2*\[Lambda] - 
       3*g1^2*ms2*\[Lambda] - 15*g2^2*ms2*\[Lambda] + 
       20*\[Lambda]*conj[T[\[Kappa]]]*T[\[Kappa]] + 
       20*\[Kappa]*conj[T[\[Kappa]]]*T[\[Lambda]] + 
       40*\[Lambda]*conj[T[\[Lambda]]]*T[\[Lambda]] + 
       3*g1^2*conj[MassB]*(-2*MassB*\[Lambda] + T[\[Lambda]]) + 
       15*g2^2*conj[MassWB]*(-2*MassWB*\[Lambda] + T[\[Lambda]]) + 
       30*mHd2*\[Lambda]*trace[Yd, Adj[Yd]] + 15*mHu2*\[Lambda]*
        trace[Yd, Adj[Yd]] + 15*ms2*\[Lambda]*trace[Yd, Adj[Yd]] + 
       10*mHd2*\[Lambda]*trace[Ye, Adj[Ye]] + 5*mHu2*\[Lambda]*
        trace[Ye, Adj[Ye]] + 5*ms2*\[Lambda]*trace[Ye, Adj[Ye]] + 
       15*mHd2*\[Lambda]*trace[Yu, Adj[Yu]] + 30*mHu2*\[Lambda]*
        trace[Yu, Adj[Yu]] + 15*ms2*\[Lambda]*trace[Yu, Adj[Yu]] + 
       15*T[\[Lambda]]*trace[conj[T[Yd]], Tp[Yd]] + 
       15*\[Lambda]*trace[conj[T[Yd]], Tp[T[Yd]]] + 
       5*T[\[Lambda]]*trace[conj[T[Ye]], Tp[Ye]] + 
       5*\[Lambda]*trace[conj[T[Ye]], Tp[T[Ye]]] + 15*T[\[Lambda]]*
        trace[conj[T[Yu]], Tp[Yu]] + 15*\[Lambda]*trace[conj[T[Yu]], 
         Tp[T[Yu]]] + 15*\[Lambda]*trace[md2, Yd, Adj[Yd]] + 
       5*\[Lambda]*trace[me2, Ye, Adj[Ye]] + 5*\[Lambda]*
        trace[ml2, Adj[Ye], Ye] + 15*\[Lambda]*trace[mq2, Adj[Yd], Yd] + 
       15*\[Lambda]*trace[mq2, Adj[Yu], Yu] + 15*\[Lambda]*
        trace[mu2, Yu, Adj[Yu]])))/5}}
