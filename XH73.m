// models of XH(73) and X0(73) //

Qq<q>:=PowerSeriesRing(Rationals(),100);
basis := [q - 2*q^10 + 2*q^11 - q^12 - q^13 + q^16 - 2*q^17 - 2*q^19 - q^20 + 2*q^21 + 2*q^23 - 2*q^24 - 3*q^25 + 3*q^26 - q^27 - q^28 + q^29 + q^30 + 3*q^31 + q^34 + 5*q^35 - 3*q^36 + 2*q^37 - 4*q^38 - 2*q^39 + 3*q^40 + q^41 + q^42 + 4*q^44 - q^45 + 6*q^46 + q^47 - q^48 - 6*q^49 + q^50 + q^51 - 2*q^52 - 2*q^55 - 3*q^57 - 5*q^58 - 2*q^59 + 2*q^60 - q^61 - 3*q^63 - q^64 - 2*q^66 - q^67 + 6*q^68 + 6*q^69 + 4*q^70 - 4*q^71 + 2*q^72 - q^73 - q^74 - q^76 - 4*q^77 - q^78 + 4*q^79 - 7*q^80 - 4*q^81 - 2*q^82 + 2*q^83 - 3*q^84 - 6*q^85 + 4*q^87 + 5*q^88 + q^89 + 3*q^90 + 3*q^91 - 7*q^93 + 7*q^95 + 6*q^96 - 2*q^97 - 6*q^98 - 6*q^99 + O(q^100),
q^2 - 2*q^10 + 5/3*q^11 - 4/3*q^12 + 2/3*q^13 - 2*q^14 + q^16 + 2/3*q^17 - 2/3*q^18 - 8/3*q^19 - 1/3*q^20 - 1/3*q^21 + 4/3*q^22 + 8/3*q^23 - 8/3*q^24 + 1/3*q^25 + 5/3*q^26 - 2/3*q^27 + q^28 - 3*q^29 + 8/3*q^30 + 3*q^31 + 2/3*q^32 - 8/3*q^33 + q^34 + 4*q^35 + 1/3*q^36 + 1/3*q^37 - 14/3*q^38 + 4/3*q^39 - 5/3*q^40 - 2/3*q^41 + 1/3*q^42 - 10/3*q^43 + 11/3*q^44 - q^45 + 10/3*q^46 + q^47 + 2*q^48 - 13/3*q^49 - 5/3*q^50 + 2*q^51 + 1/3*q^52 + 2*q^54 + q^55 + 2/3*q^56 + 3*q^57 - 2/3*q^58 + q^60 + 4/3*q^61 + 2/3*q^62 + 1/3*q^63 - 16/3*q^64 - 7/3*q^65 - 4/3*q^66 - 26/3*q^67 - 2*q^68 - 5/3*q^69 + 8/3*q^70 - 1/3*q^71 - 10/3*q^72 - q^73 + 14/3*q^74 - q^75 - 8/3*q^76 - 5/3*q^77 - 7/3*q^78 + 16/3*q^79 + 1/3*q^80 - 3*q^81 - 10/3*q^82 - 2/3*q^83 + 4/3*q^84 - 10/3*q^85 + q^86 - 2*q^87 + 10/3*q^88 + 5*q^89 + 17/3*q^90 + 5/3*q^91 + 16/3*q^92 - q^93 + 13/3*q^94 + 14/3*q^95 + 4*q^96 + 2/3*q^97 - 5*q^98 - 13/3*q^99 + O(q^100),
q^3 - q^10 + 4/3*q^11 - 5/3*q^12 + 1/3*q^13 - q^14 + 1/3*q^17 + 2/3*q^18 - 10/3*q^19 + 1/3*q^20 - 5/3*q^21 - 1/3*q^22 + 7/3*q^23 - 1/3*q^24 - 1/3*q^25 + 7/3*q^26 - 4/3*q^27 + 1/3*q^30 + 4/3*q^32 - 4/3*q^33 + 3*q^35 + 2/3*q^36 + 14/3*q^37 - 7/3*q^38 + 5/3*q^39 + 5/3*q^40 - 4/3*q^41 + 2/3*q^42 + 1/3*q^43 - 2/3*q^44 + q^45 - 1/3*q^46 + q^47 + 2*q^48 - 14/3*q^49 - 1/3*q^50 - 3*q^51 - 1/3*q^52 - 5*q^53 - q^54 + 2*q^55 + 7/3*q^56 - 2*q^57 - 4/3*q^58 + q^59 + 5/3*q^61 + 1/3*q^62 + 8/3*q^63 - 2/3*q^64 + 4/3*q^65 + 1/3*q^66 - 22/3*q^67 - 2*q^68 + 2/3*q^69 + 7/3*q^70 + 4/3*q^71 - 8/3*q^72 - 2*q^73 + 4/3*q^74 - 4*q^75 + 11/3*q^76 - 10/3*q^77 - 2/3*q^78 + 17/3*q^79 - 1/3*q^80 - 4*q^81 - 2/3*q^82 + 14/3*q^83 + 8/3*q^84 - 17/3*q^85 + 2/3*q^88 + 7/3*q^90 + 22/3*q^91 - 10/3*q^92 + 3*q^93 - 7/3*q^94 - 8/3*q^95 + q^96 + 13/3*q^97 - 2*q^98 - 2/3*q^99 + O(q^100),
q^4 - 5/2*q^10 + 19/6*q^11 - 4/3*q^12 - 1/3*q^13 - 3/2*q^14 - 1/2*q^15 + 3/2*q^16 + 1/6*q^17 + 4/3*q^18 - 19/6*q^19 - 5/6*q^20 - 1/3*q^21 - 2/3*q^22 + 1/6*q^23 - 7/6*q^24 + 1/3*q^25 + 17/3*q^26 + 1/3*q^27 - q^28 - 1/2*q^29 + 13/6*q^30 + 3*q^31 - 11/6*q^32 - 8/3*q^33 - q^34 + 2*q^35 - 13/6*q^36 + 17/6*q^37 - 17/3*q^38 + 11/6*q^39 + 29/6*q^40 - 13/6*q^41 + 5/6*q^42 - 7/3*q^43 + 8/3*q^44 - 1/2*q^45 + 13/3*q^46 + q^47 + q^48 - 11/6*q^49 - 2/3*q^50 - 19/6*q^52 - 11/2*q^53 + 1/2*q^54 + 31/6*q^56 + 3/2*q^57 - 14/3*q^58 - 3/2*q^59 + 3/2*q^60 + 17/6*q^61 + 1/6*q^62 + 4/3*q^63 - 10/3*q^64 + 19/6*q^65 + 1/6*q^66 - 20/3*q^67 + 3*q^68 - 7/6*q^69 - 5/6*q^70 - 11/6*q^71 - 11/6*q^72 + 3/2*q^73 + 5/3*q^74 - q^75 + 1/3*q^76 - 7/6*q^77 - 10/3*q^78 + 10/3*q^79 - 43/6*q^80 - 5/2*q^81 - 17/6*q^82 + 16/3*q^83 + 4/3*q^84 - 29/6*q^85 + 3*q^86 - q^87 + 29/6*q^88 + 9/2*q^89 + 37/6*q^90 + 17/3*q^91 - 2/3*q^92 - 3/2*q^93 - 7/6*q^94 + 14/3*q^95 + 5/2*q^96 + 13/6*q^97 - 7/2*q^98 - 22/3*q^99 + O(q^100),
q^5 - q^10 + q^11 - q^13 - q^14 - q^21 - 2*q^22 + 2*q^26 + q^28 + q^30 + 2*q^31 - q^33 + q^34 + q^39 + q^40 - 2*q^43 + q^44 - 2*q^45 + 2*q^47 + q^51 - q^52 - 2*q^53 + 2*q^56 - q^58 - 2*q^59 - q^60 - 2*q^62 - q^63 + q^66 - q^68 + 2*q^73 - q^78 - 3*q^80 + q^83 + q^84 + q^86 - q^87 + 3*q^88 + 3*q^90 - q^94 + 3*q^95 - 3*q^99 + O(q^100),
q^6 - 2*q^10 + 2*q^11 - q^12 + q^13 - 2*q^14 - q^15 + q^16 + 2*q^18 - 2*q^19 + q^20 - q^22 - 3*q^24 - q^25 + 4*q^26 - q^27 + q^28 - 2*q^29 + 2*q^30 + 3*q^31 + q^32 - 2*q^33 - q^34 + 3*q^35 - q^36 + 3*q^37 - 7*q^38 + 2*q^39 + 3*q^40 + q^41 - 3*q^43 + q^44 - q^45 + 2*q^46 - 2*q^47 + 3*q^48 - 3*q^49 + 3*q^51 - 3*q^52 - 4*q^53 - 2*q^54 + 4*q^56 + q^57 + 4*q^61 - 2*q^62 + q^63 - 4*q^64 + 5*q^65 + 2*q^66 - 9*q^67 + q^69 + 3*q^70 - 3*q^71 - 5*q^72 - q^73 + 4*q^74 + 4*q^76 - 3*q^77 - 3*q^78 + 8*q^79 - 3*q^80 - 3*q^81 - 5*q^82 + 4*q^83 - 4*q^85 + 5*q^86 - 4*q^87 + q^88 + q^89 + 7*q^90 + 8*q^91 - q^92 - q^93 - q^94 + 2*q^95 + 4*q^96 + 3*q^97 - 5*q^98 - 7*q^99 + O(q^100),
q^7 - q^10 + q^11 - q^13 - q^15 - q^17 + q^20 + q^21 - q^22 + q^26 - q^28 + q^29 + 2*q^40 + q^42 + q^43 - q^45 - q^47 - q^51 - q^53 - q^56 + q^60 - 2*q^62 - q^63 + q^66 + q^68 + q^73 - q^78 - 3*q^80 + 2*q^83 - 2*q^84 + 2*q^86 + q^87 + q^88 + 2*q^90 - 2*q^93 - 2*q^94 + 4*q^95 - 2*q^99 + O(q^100),
q^8 - q^10 + 2/3*q^11 - 1/3*q^12 + 2/3*q^13 - q^14 - q^16 - 1/3*q^17 + 1/3*q^18 - 2/3*q^19 + 2/3*q^20 - 1/3*q^21 + 1/3*q^22 + 2/3*q^23 - 2/3*q^24 - 2/3*q^25 + 2/3*q^26 + 1/3*q^27 + q^28 - q^29 + 2/3*q^30 + q^31 + 2/3*q^32 - 2/3*q^33 + q^34 - q^35 + 1/3*q^36 + 4/3*q^37 - 8/3*q^38 + 1/3*q^39 - 2/3*q^40 + 1/3*q^41 + 1/3*q^42 - 1/3*q^43 + 2/3*q^44 - 2/3*q^46 + q^48 - 1/3*q^49 + 4/3*q^50 + 1/3*q^52 - q^53 - 2*q^55 - 1/3*q^56 + 1/3*q^58 + q^59 + 1/3*q^61 - 1/3*q^62 + 1/3*q^63 - 4/3*q^64 + 8/3*q^65 - 1/3*q^66 - 5/3*q^67 - 2*q^68 - 2/3*q^69 + 8/3*q^70 - 1/3*q^71 - 10/3*q^72 + 2/3*q^74 + 1/3*q^76 - 5/3*q^77 - 1/3*q^78 + 10/3*q^79 + 4/3*q^80 - q^81 - 10/3*q^82 + 4/3*q^83 + 1/3*q^84 + 2/3*q^85 + 1/3*q^88 - q^89 + 8/3*q^90 + 8/3*q^91 + 10/3*q^92 + 1/3*q^94 - 1/3*q^95 + 2/3*q^97 - q^98 - 4/3*q^99 + O(q^100),
q^9 - 3/2*q^10 + 3/2*q^11 - 1/2*q^14 - 1/2*q^15 + 3/2*q^16 - 3/2*q^17 - 5/2*q^19 + 1/2*q^20 + q^21 - q^22 - 1/2*q^23 - 1/2*q^24 + 3*q^26 - q^27 + 1/2*q^29 + 1/2*q^30 + 2*q^31 - 3/2*q^32 + 2*q^35 - 3/2*q^36 + 1/2*q^37 - q^38 - 1/2*q^39 + 7/2*q^40 - 3/2*q^41 + 1/2*q^42 + q^44 - 1/2*q^45 + q^46 - q^47 - q^48 - 3/2*q^49 - q^50 + q^51 - 5/2*q^52 - 5/2*q^53 + 1/2*q^54 + 3*q^55 + 3/2*q^56 - 1/2*q^57 - 2*q^58 - 1/2*q^59 + 1/2*q^60 + 7/2*q^61 - 3/2*q^62 - q^63 - 2*q^64 - 1/2*q^65 + 1/2*q^66 - 2*q^67 + 3*q^68 + 5/2*q^69 - 3/2*q^70 - 1/2*q^71 + 1/2*q^72 + 1/2*q^73 + 2*q^74 + q^76 + 3/2*q^77 - q^78 - 9/2*q^80 - 7/2*q^81 + 1/2*q^82 + 4*q^83 - 2*q^84 - 7/2*q^85 + 2*q^86 + q^87 + 3/2*q^88 + 7/2*q^89 + 7/2*q^90 + q^91 - 7/2*q^93 - 5/2*q^94 + 2*q^95 + 5/2*q^96 + 3/2*q^97 - 1/2*q^98 - 5*q^99 + O(q^100)] ;

g1 := basis[1] -2*basis[6] -3/2*basis[7] +  3*basis[8] +  2*basis[9];
g2 := basis[2] - basis[6] +  1/2*basis[7] +  2*basis[8];
g3 := basis[3]  -2*basis[6]  -1/2*basis[7] +  4*basis[8] +  2*basis[9];
g4 := basis[4]  + basis[6] + basis[7] - 4*basis[8]  -3*basis[9] ;
g5 := basis[5] + 2*basis[6] + 2*basis[7]  -6*basis[8] -4*basis[9]  ;

G := [ basis[I] : I in [6..9] ] ;
basis := [ g1, g2, g3, g4, g5] cat G ;

Qx<[x]>:=PolynomialRing(Rationals(),9);

mons:=MonomialsOfDegree(Qx,2);
V:=VectorSpace(Rationals(),#mons);
monImages:=[Evaluate(mon,basis) : mon in mons];


W:=VectorSpace(Rationals(),40);
monImages:=[W![Coefficient(m,i) : i in [0..39] ] : m in monImages];

h:=hom<V->W | monImages>;

K:=Kernel(h);

v := [ Eltseq(V ! K.i) : i in [1..21] ] ;

Q := [ &+[ a[i]*mons[i] : i in [1..#mons] ] : a in v ] ;

// rational points on X_0(73) //


Qx<[x]>:=PolynomialRing(Rationals(),5);
Q := 
[
    x[1]*x[3] - x[2]^2 - x[2]*x[5] + 3*x[3]*x[4] - x[3]*x[5] + 3/2*x[4]^2 - 
        1/2*x[5]^2,
    x[1]*x[4] - x[2]*x[3] - x[2]*x[4] + x[3]^2 - x[3]*x[4] + 2*x[3]*x[5] - 
        4*x[4]^2 + 9/2*x[4]*x[5] - 1/2*x[5]^2,
    x[1]*x[5] - x[2]*x[4] + x[2]*x[5] - 3*x[3]*x[4] + 2*x[3]*x[5] - 5*x[4]^2 + 
        4*x[4]*x[5]
] ;

// rational points = [ 1,0,0,0,0], [ 1,1,1,0,0]  - these are the cusps of X0(73)//

//note that this is the model of X0(73) and cusps that are also computed in Box : Quadratic Points on Modular curves with infinite Mordell-Weil group //


// the pullbacks of the above points to X_H(73) //

Qx<[x]> := PolynomialRing(Rationals(),9) ;
Q := [ x[1]*x[3] - x[2]^2 + 2*x[3]*x[4] - 3/2*x[4]^2 - 3*x[4]*x[5] + 29/2*x[5]^2 - 
        73/2*x[5]*x[6] - 219/2*x[5]*x[7] + 219/2*x[5]*x[8] + 73*x[5]*x[9] + 
        73/2*x[6]^2 + 219*x[6]*x[7] - 219*x[6]*x[8] - 146*x[6]*x[9] + 292*x[7]^2
        - 657*x[7]*x[8] - 438*x[7]*x[9] + 657/2*x[8]^2 + 438*x[8]*x[9] + 
        146*x[9]^2,
    x[1]*x[4] - x[2]*x[3] - x[3]*x[4] - 3*x[4]^2 + 7/2*x[4]*x[5] + 15/2*x[5]^2 -
        73/2*x[5]*x[6] - 73/2*x[5]*x[7] + 219/2*x[5]*x[8] + 73*x[5]*x[9] + 
        73/2*x[6]^2 + 73*x[6]*x[7] - 219*x[6]*x[8] - 146*x[6]*x[9] + 73/4*x[7]^2
        - 219*x[7]*x[8] - 146*x[7]*x[9] + 657/2*x[8]^2 + 438*x[8]*x[9] + 
        146*x[9]^2,
    x[1]*x[5] - x[3]^2 - 2*x[3]*x[4] - 2*x[4]^2 + 4*x[4]*x[5] - 3*x[5]^2 + 
        73/2*x[5]*x[7] - 73*x[6]*x[7] - 511/4*x[7]^2 + 219*x[7]*x[8] + 
        146*x[7]*x[9],
    x[1]*x[6] - x[3]*x[4] + x[4]*x[5] - 9*x[4]*x[8] - 3*x[5]^2 + 13/2*x[5]*x[6] 
        + 59/2*x[5]*x[7] + 27/2*x[5]*x[8] + 16*x[5]*x[9] - 15/2*x[6]^2 - 
        177/2*x[6]*x[7] + 21*x[6]*x[8] - 17*x[6]*x[9] - 155*x[7]^2 + 
        411/2*x[7]*x[8] + 86*x[7]*x[9] + 27/2*x[8]^2 + 111*x[8]*x[9] + 
        64*x[9]^2,
    x[1]*x[7] - x[4]^2 + 2*x[5]^2 - 5*x[5]*x[6] - 37/2*x[5]*x[7] + 6*x[5]*x[8] +
        1/2*x[5]*x[9] + 9/2*x[6]^2 + 47*x[6]*x[7] - 18*x[6]*x[8] - 5/2*x[6]*x[9]
        + 70*x[7]^2 - 489/4*x[7]*x[8] - 61*x[7]*x[9] + 27/2*x[8]^2 - 
        21/2*x[8]*x[9] - 13*x[9]^2,
    x[1]*x[8] - x[4]*x[5] - 4*x[4]*x[8] + 2*x[5]^2 - 4*x[5]*x[6] - 
        28/3*x[5]*x[7] + 11*x[5]*x[8] + 3*x[5]*x[9] + 5/3*x[6]^2 + 20*x[6]*x[7] 
        - 8*x[6]*x[8] + 1/3*x[6]*x[9] + 25*x[7]^2 - 57*x[7]*x[8] - 
        56/3*x[7]*x[9] + 12*x[8]^2 - 3*x[8]*x[9] - 26/3*x[9]^2,
    x[1]*x[9] + 3/2*x[4]*x[8] - x[5]^2 + 11/2*x[5]*x[6] - 5/2*x[5]*x[7] + 
        17/2*x[5]*x[9] - 7/2*x[6]^2 - 10*x[6]*x[7] + 6*x[6]*x[8] - 
        33/2*x[6]*x[9] - 9/2*x[7]^2 - 9/4*x[7]*x[8] - 97/2*x[7]*x[9] + 
        27/2*x[8]^2 + 165/2*x[8]*x[9] + 49*x[9]^2,
    x[2]*x[4] - x[3]^2 - x[4]^2 - 5*x[4]*x[5] + 16*x[5]^2 - 73/2*x[5]*x[6] - 
        219/2*x[5]*x[7] + 219/2*x[5]*x[8] + 73*x[5]*x[9] + 73/2*x[6]^2 + 
        219*x[6]*x[7] - 219*x[6]*x[8] - 146*x[6]*x[9] + 1241/4*x[7]^2 - 
        657*x[7]*x[8] - 438*x[7]*x[9] + 657/2*x[8]^2 + 438*x[8]*x[9] + 
        146*x[9]^2,
    x[2]*x[5] - x[3]*x[4] - 2*x[4]^2 - x[4]*x[5] + 11*x[5]^2 - 73/2*x[5]*x[6] - 
        73*x[5]*x[7] + 219/2*x[5]*x[8] + 73*x[5]*x[9] + 73/2*x[6]^2 + 
        146*x[6]*x[7] - 219*x[6]*x[8] - 146*x[6]*x[9] + 146*x[7]^2 - 
        438*x[7]*x[8] - 292*x[7]*x[9] + 657/2*x[8]^2 + 438*x[8]*x[9] + 
        146*x[9]^2,
    x[2]*x[6] - x[4]^2 - 9*x[4]*x[8] + 2*x[5]^2 - 6*x[5]*x[6] - x[5]*x[7] + 
        15*x[5]*x[8] + 4*x[5]*x[9] + 2*x[6]^2 + 1/2*x[6]*x[7] - 6*x[6]*x[8] + 
        4*x[6]*x[9] - 8*x[7]^2 - 6*x[7]*x[8] + 30*x[7]*x[9] - 18*x[8]*x[9] - 
        16*x[9]^2,
    x[2]*x[7] - x[4]*x[5] + 2*x[5]^2 - 5*x[5]*x[6] - 25/2*x[5]*x[7] + 
        6*x[5]*x[8] + 1/2*x[5]*x[9] + 9/2*x[6]^2 + 33*x[6]*x[7] - 18*x[6]*x[8] -
        5/2*x[6]*x[9] + 46*x[7]^2 - 345/4*x[7]*x[8] - 33*x[7]*x[9] + 27/2*x[8]^2
        - 21/2*x[8]*x[9] - 13*x[9]^2,
    x[2]*x[8] - 4*x[4]*x[8] - x[5]^2 + 4*x[5]*x[6] + 34/3*x[5]*x[7] - 
        14*x[5]*x[8] - 13*x[5]*x[9] - 19/3*x[6]^2 - 64/3*x[6]*x[7] + 
        40*x[6]*x[8] + 97/3*x[6]*x[9] - 73/3*x[7]^2 + 67*x[7]*x[8] + 
        64*x[7]*x[9] - 63*x[8]^2 - 99*x[8]*x[9] - 122/3*x[9]^2,
    x[2]*x[9] + 3/2*x[4]*x[8] - x[5]*x[6] + 3/2*x[5]*x[8] + 3*x[5]*x[9] + 
        2*x[6]^2 - x[6]*x[7] - 9*x[6]*x[8] - 9*x[6]*x[9] - 3*x[7]^2 - 
        3/2*x[7]*x[8] - 11/2*x[7]*x[9] + 9*x[8]^2 + 21*x[8]*x[9] + 12*x[9]^2,
    x[3]*x[5] - x[4]^2 - 2*x[4]*x[5] + 4*x[5]^2 - 73/2*x[5]*x[7] + 73*x[6]*x[7] 
        + 146*x[7]^2 - 219*x[7]*x[8] - 146*x[7]*x[9],
    x[3]*x[6] - x[4]*x[5] + 6*x[4]*x[8] + 2*x[5]^2 - 5*x[5]*x[6] - 
        41/2*x[5]*x[7] - 3*x[5]*x[8] - 17/2*x[5]*x[9] + 15/2*x[6]^2 + 
        121/2*x[6]*x[7] - 27*x[6]*x[8] + 13/2*x[6]*x[9] + 185/2*x[7]^2 - 
        591/4*x[7]*x[8] - 59*x[7]*x[9] + 27/2*x[8]^2 - 111/2*x[8]*x[9] - 
        39*x[9]^2,
    x[3]*x[7] - x[5]^2 + 4*x[5]*x[6] + 7/2*x[5]*x[7] - 9*x[5]*x[8] - 
        9/2*x[5]*x[9] - 7/2*x[6]^2 - 7*x[6]*x[7] + 18*x[6]*x[8] + 17/2*x[6]*x[9]
        - 7*x[7]^2 + 69/4*x[7]*x[8] + 5*x[7]*x[9] - 45/2*x[8]^2 - 39/2*x[8]*x[9]
        - 3*x[9]^2,
    x[3]*x[8] + 3*x[4]*x[8] - x[5]*x[6] - 11/3*x[5]*x[7] - 5*x[5]*x[8] - 
        14/3*x[5]*x[9] + 8/3*x[6]^2 + 46/3*x[6]*x[7] - 6*x[6]*x[8] + 2*x[6]*x[9]
        + 70/3*x[7]^2 - 61/2*x[7]*x[8] - 34/3*x[7]*x[9] - 6*x[8]^2 - 
        26*x[8]*x[9] - 40/3*x[9]^2,
    x[3]*x[9] - 3/2*x[4]*x[8] + 2*x[5]*x[7] + 3/2*x[5]*x[8] - 1/2*x[5]*x[9] - 
        3/2*x[6]^2 - 4*x[6]*x[7] + 9/2*x[6]*x[8] + 15/2*x[6]*x[9] - 25/2*x[7]^2 
        + 39/4*x[7]*x[8] + 33/2*x[7]*x[9] - 27/2*x[8]*x[9] - 9*x[9]^2,
    x[4]*x[6] - 3*x[4]*x[8] - x[5]^2 + 4*x[5]*x[6] + 15/2*x[5]*x[7] - 
        3*x[5]*x[8] - 3/2*x[5]*x[9] - 11/2*x[6]^2 - 25*x[6]*x[7] + 21*x[6]*x[8] 
        + 15/2*x[6]*x[9] - 53/2*x[7]^2 + 195/4*x[7]*x[8] + 21*x[7]*x[9] - 
        27/2*x[8]^2 + 3/2*x[8]*x[9] + 7*x[9]^2,
    x[4]*x[7] - x[5]*x[6] + x[5]*x[7] - 3*x[5]*x[8] - 4*x[5]*x[9] + x[6]^2 + 
        2*x[6]*x[7] + 6*x[6]*x[9] + 2*x[7]^2 + 3*x[7]*x[8] + 16*x[7]*x[9] - 
        9*x[8]^2 - 30*x[8]*x[9] - 16*x[9]^2,
    x[4]*x[9] + 1/2*x[5]*x[7] - 3/2*x[5]*x[9] - 1/2*x[6]^2 - 2*x[6]*x[7] + 
        3/2*x[6]*x[8] + 5/2*x[6]*x[9] + 1/2*x[7]^2 - 3/4*x[7]*x[8] + 5*x[7]*x[9]
        - 9/2*x[8]*x[9] - 3*x[9]^2
] ;

ZB<[B]> := PolynomialRing(Rationals(),4) ;
Q1 :=  [ Evaluate(a, [1,0,0,0,0] cat B ) : a in Q ] ;

S1 := Scheme(AffineSpace(ZB), Q1) ;

E1 := Points(S1) ;
E1 := [ Eltseq(a) : a in E1 ]  ;

pt1 := [ [ 1,0,0,0,0] cat b : b in E1 ];

ZB<[B]> := PolynomialRing(QuadraticField(73),4) ;
Q2 :=  [ Evaluate(a, [1,1,1,0,0] cat B ) : a in Q ] ;

S2 := Scheme(AffineSpace(ZB), Q2) ;

E2 := Points(S2) ;
E2 := [ Eltseq(a) : a in E2 ]  ;

pt2 := [ [ 1,1,1,0,0] cat b : b in E2 ];

// cuspidal subgroup of XH(73) //

K := QuadraticField(73) ;
OK := Integers(K) ;
P := PrimesUpTo(50,K) ;

P := P[5] ;
F19, pi := ResidueClassField(P) ;
Zx<[x]> := PolynomialRing(F19,9) ;

eqns := [
    2*x[1]*x[3] - 2*x[2]^2 + 4*x[3]*x[4] - 3*x[4]^2 - 6*x[4]*x[5] + 29*x[5]^2 - 
        73*x[5]*x[6] - 219*x[5]*x[7] + 219*x[5]*x[8] + 146*x[5]*x[9] + 73*x[6]^2
        + 438*x[6]*x[7] - 438*x[6]*x[8] - 292*x[6]*x[9] + 584*x[7]^2 - 
        1314*x[7]*x[8] - 876*x[7]*x[9] + 657*x[8]^2 + 876*x[8]*x[9] + 
        292*x[9]^2,
    2*x[1]*x[4] - 2*x[2]*x[3] - 2*x[3]*x[4] - 6*x[4]^2 + 7*x[4]*x[5] + 15*x[5]^2
        - 73*x[5]*x[6] - 73*x[5]*x[7] + 219*x[5]*x[8] + 146*x[5]*x[9] + 
        73*x[6]^2 + 146*x[6]*x[7] - 438*x[6]*x[8] - 292*x[6]*x[9] + 73/2*x[7]^2 
        - 438*x[7]*x[8] - 292*x[7]*x[9] + 657*x[8]^2 + 876*x[8]*x[9] + 
        292*x[9]^2,
    4*x[1]*x[5] - 4*x[3]^2 - 8*x[3]*x[4] - 8*x[4]^2 + 16*x[4]*x[5] - 12*x[5]^2 +
        146*x[5]*x[7] - 292*x[6]*x[7] - 511*x[7]^2 + 876*x[7]*x[8] + 
        584*x[7]*x[9],
    2*x[1]*x[6] - 2*x[3]*x[4] + 2*x[4]*x[5] - 18*x[4]*x[8] - 6*x[5]^2 + 
        13*x[5]*x[6] + 59*x[5]*x[7] + 27*x[5]*x[8] + 32*x[5]*x[9] - 15*x[6]^2 - 
        177*x[6]*x[7] + 42*x[6]*x[8] - 34*x[6]*x[9] - 310*x[7]^2 + 411*x[7]*x[8]
        + 172*x[7]*x[9] + 27*x[8]^2 + 222*x[8]*x[9] + 128*x[9]^2,
    4*x[1]*x[7] - 4*x[4]^2 + 8*x[5]^2 - 20*x[5]*x[6] - 74*x[5]*x[7] + 
        24*x[5]*x[8] + 2*x[5]*x[9] + 18*x[6]^2 + 188*x[6]*x[7] - 72*x[6]*x[8] - 
        10*x[6]*x[9] + 280*x[7]^2 - 489*x[7]*x[8] - 244*x[7]*x[9] + 54*x[8]^2 - 
        42*x[8]*x[9] - 52*x[9]^2,
    3*x[1]*x[8] - 3*x[4]*x[5] - 12*x[4]*x[8] + 6*x[5]^2 - 12*x[5]*x[6] - 
        28*x[5]*x[7] + 33*x[5]*x[8] + 9*x[5]*x[9] + 5*x[6]^2 + 60*x[6]*x[7] - 
        24*x[6]*x[8] + x[6]*x[9] + 75*x[7]^2 - 171*x[7]*x[8] - 56*x[7]*x[9] + 
        36*x[8]^2 - 9*x[8]*x[9] - 26*x[9]^2,
    4*x[1]*x[9] + 6*x[4]*x[8] - 4*x[5]^2 + 22*x[5]*x[6] - 10*x[5]*x[7] + 
        34*x[5]*x[9] - 14*x[6]^2 - 40*x[6]*x[7] + 24*x[6]*x[8] - 66*x[6]*x[9] - 
        18*x[7]^2 - 9*x[7]*x[8] - 194*x[7]*x[9] + 54*x[8]^2 + 330*x[8]*x[9] + 
        196*x[9]^2,
    2*x[2]*x[4] - 2*x[3]^2 - 2*x[4]^2 - 10*x[4]*x[5] + 32*x[5]^2 - 73*x[5]*x[6] 
        - 219*x[5]*x[7] + 219*x[5]*x[8] + 146*x[5]*x[9] + 73*x[6]^2 + 
        438*x[6]*x[7] - 438*x[6]*x[8] - 292*x[6]*x[9] + 1241/2*x[7]^2 - 
        1314*x[7]*x[8] - 876*x[7]*x[9] + 657*x[8]^2 + 876*x[8]*x[9] + 
        292*x[9]^2,
    x[2]*x[5] - x[3]*x[4] - 2*x[4]^2 - x[4]*x[5] + 11*x[5]^2 - 73/2*x[5]*x[6] - 
        73*x[5]*x[7] + 219/2*x[5]*x[8] + 73*x[5]*x[9] + 73/2*x[6]^2 + 
        146*x[6]*x[7] - 219*x[6]*x[8] - 146*x[6]*x[9] + 146*x[7]^2 - 
        438*x[7]*x[8] - 292*x[7]*x[9] + 657/2*x[8]^2 + 438*x[8]*x[9] + 
        146*x[9]^2,
    x[2]*x[6] - x[4]^2 - 9*x[4]*x[8] + 2*x[5]^2 - 6*x[5]*x[6] - x[5]*x[7] + 
        15*x[5]*x[8] + 4*x[5]*x[9] + 2*x[6]^2 + 1/2*x[6]*x[7] - 6*x[6]*x[8] + 
        4*x[6]*x[9] - 8*x[7]^2 - 6*x[7]*x[8] + 30*x[7]*x[9] - 18*x[8]*x[9] - 
        16*x[9]^2,
    4*x[2]*x[7] - 4*x[4]*x[5] + 8*x[5]^2 - 20*x[5]*x[6] - 50*x[5]*x[7] + 
        24*x[5]*x[8] + 2*x[5]*x[9] + 18*x[6]^2 + 132*x[6]*x[7] - 72*x[6]*x[8] - 
        10*x[6]*x[9] + 184*x[7]^2 - 345*x[7]*x[8] - 132*x[7]*x[9] + 54*x[8]^2 - 
        42*x[8]*x[9] - 52*x[9]^2,
    3*x[2]*x[8] - 12*x[4]*x[8] - 3*x[5]^2 + 12*x[5]*x[6] + 34*x[5]*x[7] - 
        42*x[5]*x[8] - 39*x[5]*x[9] - 19*x[6]^2 - 64*x[6]*x[7] + 120*x[6]*x[8] +
        97*x[6]*x[9] - 73*x[7]^2 + 201*x[7]*x[8] + 192*x[7]*x[9] - 189*x[8]^2 - 
        297*x[8]*x[9] - 122*x[9]^2,
    2*x[2]*x[9] + 3*x[4]*x[8] - 2*x[5]*x[6] + 3*x[5]*x[8] + 6*x[5]*x[9] + 
        4*x[6]^2 - 2*x[6]*x[7] - 18*x[6]*x[8] - 18*x[6]*x[9] - 6*x[7]^2 - 
        3*x[7]*x[8] - 11*x[7]*x[9] + 18*x[8]^2 + 42*x[8]*x[9] + 24*x[9]^2,
    2*x[3]*x[5] - 2*x[4]^2 - 4*x[4]*x[5] + 8*x[5]^2 - 73*x[5]*x[7] + 
        146*x[6]*x[7] + 292*x[7]^2 - 438*x[7]*x[8] - 292*x[7]*x[9],
    2*x[3]*x[6] - 2*x[4]*x[5] + 12*x[4]*x[8] + 4*x[5]^2 - 10*x[5]*x[6] - 
        41*x[5]*x[7] - 6*x[5]*x[8] - 17*x[5]*x[9] + 15*x[6]^2 + 121*x[6]*x[7] - 
        54*x[6]*x[8] + 13*x[6]*x[9] + 185*x[7]^2 - 591/2*x[7]*x[8] - 
        118*x[7]*x[9] + 27*x[8]^2 - 111*x[8]*x[9] - 78*x[9]^2,
    2*x[3]*x[7] - 2*x[5]^2 + 8*x[5]*x[6] + 7*x[5]*x[7] - 18*x[5]*x[8] - 
        9*x[5]*x[9] - 7*x[6]^2 - 14*x[6]*x[7] + 36*x[6]*x[8] + 17*x[6]*x[9] - 
        14*x[7]^2 + 69/2*x[7]*x[8] + 10*x[7]*x[9] - 45*x[8]^2 - 39*x[8]*x[9] - 
        6*x[9]^2,
    6*x[3]*x[8] + 18*x[4]*x[8] - 6*x[5]*x[6] - 22*x[5]*x[7] - 30*x[5]*x[8] - 
        28*x[5]*x[9] + 16*x[6]^2 + 92*x[6]*x[7] - 36*x[6]*x[8] + 12*x[6]*x[9] + 
        140*x[7]^2 - 183*x[7]*x[8] - 68*x[7]*x[9] - 36*x[8]^2 - 156*x[8]*x[9] - 
        80*x[9]^2,
    4*x[3]*x[9] - 6*x[4]*x[8] + 8*x[5]*x[7] + 6*x[5]*x[8] - 2*x[5]*x[9] - 
        6*x[6]^2 - 16*x[6]*x[7] + 18*x[6]*x[8] + 30*x[6]*x[9] - 50*x[7]^2 + 
        39*x[7]*x[8] + 66*x[7]*x[9] - 54*x[8]*x[9] - 36*x[9]^2,
    2*x[4]*x[6] - 6*x[4]*x[8] - 2*x[5]^2 + 8*x[5]*x[6] + 15*x[5]*x[7] - 
        6*x[5]*x[8] - 3*x[5]*x[9] - 11*x[6]^2 - 50*x[6]*x[7] + 42*x[6]*x[8] + 
        15*x[6]*x[9] - 53*x[7]^2 + 195/2*x[7]*x[8] + 42*x[7]*x[9] - 27*x[8]^2 + 
        3*x[8]*x[9] + 14*x[9]^2,
    x[4]*x[7] - x[5]*x[6] + x[5]*x[7] - 3*x[5]*x[8] - 4*x[5]*x[9] + x[6]^2 + 
        2*x[6]*x[7] + 6*x[6]*x[9] + 2*x[7]^2 + 3*x[7]*x[8] + 16*x[7]*x[9] - 
        9*x[8]^2 - 30*x[8]*x[9] - 16*x[9]^2,
    2*x[4]*x[9] + x[5]*x[7] - 3*x[5]*x[9] - x[6]^2 - 4*x[6]*x[7] + 3*x[6]*x[8] +
        5*x[6]*x[9] + x[7]^2 - 3/2*x[7]*x[8] + 10*x[7]*x[9] - 9*x[8]*x[9] - 
        6*x[9]^2
];
X := Curve(ProjectiveSpace(Zx), eqns) ;
XF7 := X ;


Cl, phi, psi := ClassGroup(XF7) ;
Z := FreeAbelianGroup(1) ;
degr := hom<Cl -> Z | [ Degree(phi(g)) : g in OrderedGenerators(Cl)]>;
J := Kernel(degr) ;

d1 := [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0 ],   [ 3, 0, 0, 0, 0, -3, 0, -1, 0 ] ];
a := K.1;

d2 := [
    [ 1, 1, 1, 0, 0, 1/73*(-11*a - 73), -2/73*a, -4/219*a, 1/146*(-11*a 
        - 73) ],
    [ 1, 1, 1, 0, 0, 1/73*(11*a - 73), 2/73*a, 4/219*a, 1/146*(11*a - 
        73) ]
];
d2 := [ [ 6*73*c : c in b ] : b in d2 ];
d := d1 cat d2 ;
dpi := [ [ pi(a) : a in b ] : b in d ] ;
Cdpi := [ XF7  ! a : a in dpi ] ;

C1 := d1[1] ;
C2 := d1[2] ;
C3 := d2[1] ;
C4 := d2[2] ;
C1 := Place( XF7 ! pi(C1) ) ;
C2 := Place(XF7  ! pi(C2) ) ;
C3 := Place(XF7 ! pi(C3));
C4 := Place(XF7 !  pi(C4) ) ;

CC := [ Place(b) : b in Cdpi ] ;
divs := [ CC[2] - CC[1], CC[3] - CC[1], CC[4] - CC[1] ] ;
H := [ psi(a) :a  in divs] ;
ZN := FreeAbelianGroup(#H) ;
hh := hom< ZN -> J | [  a : a in H ] >;

CH := Image(hh) ;
cpt := [ ZN.1, ZN.3, ZN.2] ;

conj := hom< ZN -> ZN | cpt>; 
mu := hom< ZN -> J | [ hh(ZN.i) - hh(conj(ZN.i)) : i in [1..3]]>; 
ker1 := Kernel(mu);
imKer1 := sub<J | [hh(k) : k in Generators(ker1)]>;
CHQ := imKer1;
