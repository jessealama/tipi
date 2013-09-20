include('../hyperbolic.p',
        [a5,
         a6,
         7,
         30]).

fof(48,conjecture, % inner Pasch
    (! [A,B,C,D,E] :
     ((between(A,B,C) & between(A,D,E))
          => (? [F] : (between(B,F,E) & between(C,F,D)))))).
