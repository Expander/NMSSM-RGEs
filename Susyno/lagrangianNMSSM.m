{(-("S"*"Hd"[2]*"Hu"[1]) + "S"*"Hd"[1]*"Hu"[2])*y[{"Hu", "Hd", "S"}] + 
  ("S"^3*y[{"S", "S", "S"}])/6 + y[{"e", "L", "Hd"}, {f[1], f[2]}]*
   ("e"[f[1]]*"Hd"[2]*"L"[f[2]][1] - "e"[f[1]]*"Hd"[1]*"L"[f[2]][2]) + 
  y[{"d", "Q", "Hd"}, {f[1], f[2]}]*("Hd"[2]*"d"[f[1]][1]*"Q"[f[2]][1, 1] + 
    "Hd"[2]*"d"[f[1]][2]*"Q"[f[2]][1, 2] + "Hd"[2]*"d"[f[1]][3]*
     "Q"[f[2]][1, 3] - "Hd"[1]*"d"[f[1]][1]*"Q"[f[2]][2, 1] - 
    "Hd"[1]*"d"[f[1]][2]*"Q"[f[2]][2, 2] - "Hd"[1]*"d"[f[1]][3]*
     "Q"[f[2]][2, 3]) + y[{"u", "Q", "Hu"}, {f[1], f[2]}]*
   ("Hu"[2]*"Q"[f[2]][1, 1]*"u"[f[1]][1] - "Hu"[1]*"Q"[f[2]][2, 1]*
     "u"[f[1]][1] + "Hu"[2]*"Q"[f[2]][1, 2]*"u"[f[1]][2] - 
    "Hu"[1]*"Q"[f[2]][2, 2]*"u"[f[1]][2] + "Hu"[2]*"Q"[f[2]][1, 3]*
     "u"[f[1]][3] - "Hu"[1]*"Q"[f[2]][2, 3]*"u"[f[1]][3]), 
 (-("Hd"[2]*"Hu"[1]) + "Hd"[1]*"Hu"[2])*\[Mu][{"Hu", "Hd"}] + 
  ("S"^2*\[Mu][{"S", "S"}])/2, "S"*l[{"S"}], 
 (-("S"*"Hd"[2]*"Hu"[1]) + "S"*"Hd"[1]*"Hu"[2])*h[{"Hu", "Hd", "S"}] + 
  ("S"^3*h[{"S", "S", "S"}])/6 + h[{"e", "L", "Hd"}, {f[1], f[2]}]*
   ("e"[f[1]]*"Hd"[2]*"L"[f[2]][1] - "e"[f[1]]*"Hd"[1]*"L"[f[2]][2]) + 
  h[{"d", "Q", "Hd"}, {f[1], f[2]}]*("Hd"[2]*"d"[f[1]][1]*"Q"[f[2]][1, 1] + 
    "Hd"[2]*"d"[f[1]][2]*"Q"[f[2]][1, 2] + "Hd"[2]*"d"[f[1]][3]*
     "Q"[f[2]][1, 3] - "Hd"[1]*"d"[f[1]][1]*"Q"[f[2]][2, 1] - 
    "Hd"[1]*"d"[f[1]][2]*"Q"[f[2]][2, 2] - "Hd"[1]*"d"[f[1]][3]*
     "Q"[f[2]][2, 3]) + h[{"u", "Q", "Hu"}, {f[1], f[2]}]*
   ("Hu"[2]*"Q"[f[2]][1, 1]*"u"[f[1]][1] - "Hu"[1]*"Q"[f[2]][2, 1]*
     "u"[f[1]][1] + "Hu"[2]*"Q"[f[2]][1, 2]*"u"[f[1]][2] - 
    "Hu"[1]*"Q"[f[2]][2, 2]*"u"[f[1]][2] + "Hu"[2]*"Q"[f[2]][1, 3]*
     "u"[f[1]][3] - "Hu"[1]*"Q"[f[2]][2, 3]*"u"[f[1]][3]), 
 (-("Hd"[2]*"Hu"[1]) + "Hd"[1]*"Hu"[2])*b[{"Hu", "Hd"}] + 
  ("S"^2*b[{"S", "S"}])/2, "S"*s[{"S"}], "S"*Conjugate["S"]*m2[{"S", "S"}] + 
  "e"[f[1]]*m2[{"e", "e"}, {f[1], f[2]}]*Conjugate["e"][f[2]] + 
  m2[{"Hd", "Hd"}]*("Hd"[1]*Conjugate["Hd"][1] + 
    "Hd"[2]*Conjugate["Hd"][2]) + m2[{"Hu", "Hu"}]*
   ("Hu"[1]*Conjugate["Hu"][1] + "Hu"[2]*Conjugate["Hu"][2]) + 
  m2[{"d", "d"}, {f[1], f[2]}]*("d"[f[1]][1]*Conjugate["d"][f[2]][1] + 
    "d"[f[1]][2]*Conjugate["d"][f[2]][2] + 
    "d"[f[1]][3]*Conjugate["d"][f[2]][3]) + m2[{"L", "L"}, {f[1], f[2]}]*
   ("L"[f[1]][1]*Conjugate["L"][f[2]][1] + 
    "L"[f[1]][2]*Conjugate["L"][f[2]][2]) + m2[{"Q", "Q"}, {f[1], f[2]}]*
   ("Q"[f[1]][1, 1]*Conjugate["Q"][f[2]][1, 1] + 
    "Q"[f[1]][1, 2]*Conjugate["Q"][f[2]][1, 2] + 
    "Q"[f[1]][1, 3]*Conjugate["Q"][f[2]][1, 3] + 
    "Q"[f[1]][2, 1]*Conjugate["Q"][f[2]][2, 1] + 
    "Q"[f[1]][2, 2]*Conjugate["Q"][f[2]][2, 2] + 
    "Q"[f[1]][2, 3]*Conjugate["Q"][f[2]][2, 3]) + 
  m2[{"u", "u"}, {f[1], f[2]}]*("u"[f[1]][1]*Conjugate["u"][f[2]][1] + 
    "u"[f[1]][2]*Conjugate["u"][f[2]][2] + 
    "u"[f[1]][3]*Conjugate["u"][f[2]][3])}
