// a model of XH(41) and X0(41) and the quotient map XH(41) --> X0(41) //

Qq<q>:=PowerSeriesRing(Rationals(),100);

basis :=
[ q - 2*q^6 + q^7 + q^8 - 2*q^9 - 3*q^10 + q^11 - 2*q^12 - q^13 + 3*q^14 + 3*q^15 - q^16 - q^17 + 4*q^18 + q^19 - 2*q^20 + q^21 + q^22 + 2*q^24 + q^26 - q^27 + 3*q^28 - q^29 + 3*q^30 - 6*q^32 - 7*q^33 - q^34 - 5*q^35 + 3*q^36 - 2*q^37 - q^38 + 9*q^39 + 3*q^40 - 10*q^42 + q^43 + 3*q^44 - 7*q^45 - 2*q^46 - 3*q^47 + 4*q^48 + 2*q^49 + 3*q^50 + q^51 - q^52 + 3*q^53 + 3*q^54 - q^55 - 3*q^56 - 2*q^57 + 3*q^58 + 3*q^60 + q^61 + 10*q^62 + 5*q^63 + 2*q^64 - 2*q^65 - 2*q^67 - q^68 + 4*q^69 - 7*q^70 + 6*q^71 - 7*q^72 + 6*q^73 - 7*q^74 - 4*q^75 - 3*q^76 + 8*q^77 - 7*q^78 + 4*q^79 + 4*q^80 - 4*q^81 + q^82 - 4*q^83 - 9*q^84 + 2*q^85 - 3*q^86 + 5*q^87 - q^88 + q^89 + 2*q^90 - 9*q^91 - 4*q^92 - 10*q^93 - 5*q^94 + q^95 + 2*q^96 - q^97 + 8*q^98 - q^99 + O(q^100),

q^2 - 3*q^6 - q^7 + 3*q^8 - q^9 - 2*q^10 + 5*q^11 - 2*q^12 - 5*q^13 + 6*q^14 + 7*q^15 - 8*q^16 + q^17 + 2*q^18 - 2*q^19 - 6*q^20 + 2*q^22 - 6*q^23 + 7*q^24 + 4*q^25 + q^26 + q^27 + 3*q^28 + q^29 + q^30 - 4*q^31 + q^32 - 2*q^33 - 3*q^34 - 9*q^35 + 7*q^36 - 2*q^37 - 3*q^38 + 6*q^39 + 8*q^40 + 2*q^41 - 10*q^42 + 4*q^43 - 11*q^44 - 6*q^45 - 2*q^46 - 4*q^47 - 4*q^48 + 9*q^49 + q^50 + 4*q^51 + 13*q^52 + 7*q^53 + 3*q^54 - 3*q^55 - 4*q^56 - 10*q^57 + 3*q^58 - 2*q^59 - 3*q^60 + 12*q^62 + 14*q^63 - 4*q^65 - 9*q^67 - q^68 - 6*q^69 - 3*q^70 + 2*q^71 - 14*q^72 + 14*q^73 - 4*q^74 + 8*q^76 + 8*q^77 - 8*q^78 - 8*q^79 - q^82 + 2*q^83 - 10*q^84 + 2*q^85 - 2*q^86 + 12*q^87 + 16*q^88 + q^89 - 2*q^90 - 8*q^91 + 10*q^92 - 30*q^93 + 5*q^94 + 9*q^95 + q^96 - 9*q^97 + 4*q^98 - 6*q^99 + O(q^100),

q^3 - q^6 - q^7 + q^11 - q^12 - 2*q^13 + q^14 + 2*q^15 + q^19 - q^22 + 3*q^24 + 2*q^26 - 2*q^27 + q^28 - 2*q^29 - 2*q^30 - 2*q^35 - q^38 + 2*q^41 - q^44 - q^47 - q^48 + 2*q^52 + 2*q^53 + 2*q^54 + 2*q^55 - 3*q^56 + 2*q^58 - 2*q^60 + 5*q^63 - 4*q^65 - 3*q^67 + 2*q^70 + 3*q^71 - q^75 - q^76 - q^79 - 2*q^82 + 3*q^88 + 4*q^89 - 8*q^93 + q^94 + 2*q^95 - 5*q^96 - 5*q^99 + O(q^100),

q^4 - q^6 - q^8 - q^10 + 2*q^11 - 2*q^13 + q^14 + 2*q^15 + q^18 - q^19 - q^21 - q^22 - 2*q^23 + q^24 + q^25 + 2*q^26 + 2*q^28 - 2*q^32 + q^33 - 2*q^35 - q^36 - q^39 - q^40 + q^41 - 2*q^42 + q^43 - q^45 + q^47 + 2*q^49 + q^50 + q^51 + 2*q^53 + 2*q^54 - 3*q^56 - 2*q^57 + 2*q^58 - 2*q^59 - q^61 + 2*q^62 + 3*q^63 + 3*q^64 - 2*q^65 - 2*q^66 - 3*q^67 - 2*q^68 - 2*q^69 - 2*q^70 + 2*q^72 + 4*q^73 - 3*q^74 + q^76 + 2*q^77 + q^78 + 4*q^80 + q^81 - q^82 + 2*q^83 - q^84 - q^86 + q^87 - q^88 - 2*q^89 - q^91 + 2*q^92 - 8*q^93 - 4*q^94 + 2*q^95 - q^96 - 2*q^97 + 3*q^98 - 3*q^99 + O(q^100),

q^5 - q^6 - q^7 + 2*q^8 - 2*q^9 - q^10 + 3*q^11 - 3*q^13 + 2*q^14 + 3*q^15 - 4*q^16 + q^17 + 2*q^18 - 2*q^19 - 3*q^20 + 3*q^21 - 4*q^23 + 3*q^24 + 2*q^25 + q^26 + q^27 + q^28 + q^29 - q^30 - 6*q^31 - q^33 - q^34 - 3*q^35 + 4*q^36 + q^37 - q^38 + 6*q^39 + 5*q^40 - 5*q^42 + 4*q^43 - 7*q^44 - 5*q^45 - 4*q^48 + 4*q^49 + 2*q^51 + 7*q^52 + 3*q^53 + q^54 - q^55 - 2*q^56 - 9*q^57 + q^58 - 3*q^60 - 2*q^61 + 6*q^62 + 6*q^63 + 4*q^64 - 2*q^65 + 3*q^66 - 5*q^67 - q^68 - 6*q^69 + q^70 - 2*q^71 - 10*q^72 + 13*q^73 - q^74 + 2*q^75 + 6*q^76 + 5*q^77 - 8*q^78 - 6*q^79 - q^80 + 3*q^81 - 2*q^83 - 5*q^84 - 2*q^86 + 12*q^87 + 8*q^88 - q^89 + 3*q^90 - 8*q^91 + 8*q^92 - 14*q^93 + 3*q^94 + 5*q^95 - q^96 - 5*q^97 - 4*q^99 + O(q^100) ];



Qx<[x]>:=PolynomialRing(Rationals(),5);

mons:=MonomialsOfDegree(Qx,2);
V:=VectorSpace(Rationals(),#mons);
monImages:=[Evaluate(mon,basis) : mon in mons];


W:=VectorSpace(Rationals(),40);
monImages:=[W![Coefficient(m,i) : i in [0..39] ] : m in monImages];

h:=hom<V->W | monImages>;

K:=Kernel(h);


v1 :=Eltseq(V!(K.1));

Q1:=&+[v1[i]*mons[i] : i in [1..#mons]];


v2 :=Eltseq(V!(K.2));

Q2:=&+[v2[i]*mons[i] : i in [1..#mons]];

v3 :=Eltseq(V!(K.3));

Q3:=&+[v3[i]*mons[i] : i in [1..#mons]];


print " A model of the curve XH(41) is:";
Q1;
Q2;
Q3;

Qq<q>:=PowerSeriesRing(Rationals(),100);

basis2 := [ q + q^4 - q^5 - 2*q^6 + 2*q^7 - 2*q^8 - 3*q^10 - 2*q^12 + 2*q^14 + 2*q^15 + 3*q^16 - 2*q^17 + 3*q^18 + 2*q^19 + q^20 - 3*q^21 + 2*q^23 - q^25 + 2*q^26 - 2*q^27 + 4*q^28 - 2*q^29 + 4*q^30 + 6*q^31 - 8*q^32 - 5*q^33 - 4*q^35 - 2*q^36 - 3*q^37 + 2*q^39 - 3*q^40 + q^41 - 7*q^42 - 2*q^43 + 10*q^44 - 3*q^45 - 2*q^46 - 2*q^47 + 8*q^48 + 4*q^50 - 8*q^52 + 2*q^53 + 4*q^54 - 4*q^56 + 5*q^57 + 4*q^58 - 2*q^59 + 6*q^60 + 2*q^61 + 6*q^62 + 2*q^63 + q^64 - 2*q^65 - 5*q^66 - 2*q^68 + 8*q^69 - 10*q^70 + 8*q^71 + 5*q^72 - 3*q^73 - 9*q^74 - 6*q^75 - 8*q^76 + 5*q^77 + 2*q^78 + 10*q^79 + 9*q^80 - 6*q^81 - 5*q^84 + 2*q^85 - 2*q^86 - 6*q^87 - 10*q^88 - q^90 - 2*q^91 - 10*q^92 - 4*q^93 - 12*q^94 - 2*q^95 + 2*q^96 + 2*q^97 + 11*q^98 + O(q^100), 

q^2 - 2*q^4 - q^5 + 3*q^8 + q^9 + q^10 - 2*q^11 - 2*q^12 + 2*q^13 + 2*q^14 - 4*q^16 - 2*q^18 + 2*q^19 - 3*q^20 - q^21 + 4*q^22 + 2*q^23 + 2*q^24 - 4*q^26 - 2*q^28 + 2*q^30 + 2*q^31 + 5*q^32 - 3*q^33 - 2*q^34 - 2*q^35 + 5*q^36 - 3*q^37 - 2*q^38 + 2*q^39 + 5*q^40 - q^42 - 2*q^43 - 4*q^44 + q^45 - 2*q^46 - 6*q^47 + q^49 - q^50 + 6*q^52 - 2*q^54 - 2*q^55 + 4*q^56 + 3*q^57 - 2*q^58 + 2*q^59 + 4*q^61 + 2*q^62 + 2*q^63 - 10*q^64 + 2*q^65 + q^66 + 2*q^67 + 4*q^68 + 4*q^69 + 4*q^71 - 8*q^72 - 7*q^73 + 3*q^74 - 2*q^75 - q^77 - 2*q^78 - 2*q^79 - 7*q^80 - 5*q^81 + q^82 - 3*q^84 + 2*q^85 + 2*q^86 - 2*q^87 + 10*q^88 + 6*q^89 - 5*q^90 + 2*q^91 - 2*q^92 + 10*q^94 + 4*q^96 - 2*q^98 + 4*q^99 + O(q^100), 

q^3 - 2*q^4 + q^6 - q^7 + 2*q^8 + 2*q^10 - 3*q^11 - q^12 + 2*q^13 - q^14 - 2*q^15 - 2*q^18 + 3*q^19 + 2*q^21 + q^22 + 4*q^23 + q^24 - 2*q^25 - 2*q^26 - 2*q^27 - 3*q^28 - 2*q^29 - 2*q^30 + 4*q^32 - 2*q^33 + 2*q^35 + 2*q^36 - q^38 + 2*q^39 + 2*q^40 + 4*q^42 - 2*q^43 - q^44 + 2*q^45 - 3*q^47 - q^48 - 4*q^49 - 2*q^50 - 2*q^51 + 2*q^52 - 2*q^53 - 2*q^54 + 2*q^55 + 3*q^56 + 4*q^57 - 2*q^58 + 4*q^59 - 2*q^60 + 2*q^61 - 4*q^62 - q^63 - 6*q^64 + 4*q^66 + 3*q^67 + 4*q^68 + 4*q^69 + 6*q^70 + 3*q^71 - 4*q^72 - 8*q^73 + 6*q^74 - q^75 - 3*q^76 - 4*q^77 - 2*q^78 - q^79 - 8*q^80 - 2*q^81 - 4*q^83 + 2*q^84 + 2*q^86 - 2*q^87 + 5*q^88 + 8*q^89 + 2*q^91 - 4*q^92 + 8*q^93 + 9*q^94 - 2*q^95 - 3*q^96 + 4*q^97 - 6*q^98 + q^99 + O(q^100)];

// can check : basis2[1] = basis[1] + basis[4] - basis[5], basis2[2] = basis[2] - 2basis[4] - basis[5], basis2[3] = basis[3] - 2basis[4] //


// we check whether the curve is Hyperelliptic //

Qx<[x]>:=PolynomialRing(Rationals(),3);

mons:=MonomialsOfDegree(Qx,2);
V:=VectorSpace(Rationals(),#mons);
monImages:=[Evaluate(mon,basis2) : mon in mons];


W:=VectorSpace(Rationals(),40);
monImages:=[W![Coefficient(m,i) : i in [0..39] ] : m in monImages];

h:=hom<V->W | monImages>;

K:=Kernel(h);




// so the curve is Hyperelliptic// 

// a Hyperelliptic model of the curve //

Z<[a]> := PolynomialRing(Rationals(), 9) ;
Q<q> := LaurentSeriesRing(Z) ;


basis2 := [ q + q^4 - q^5 - 2*q^6 + 2*q^7 - 2*q^8 - 3*q^10 - 2*q^12 + 2*q^14 + 2*q^15 + 3*q^16 - 2*q^17 + 3*q^18 + 2*q^19 + q^20 - 3*q^21 + 2*q^23 - q^25 + 2*q^26 - 2*q^27 + 4*q^28 - 2*q^29 + 4*q^30 + 6*q^31 - 8*q^32 - 5*q^33 - 4*q^35 - 2*q^36 - 3*q^37 + 2*q^39 - 3*q^40 + q^41 - 7*q^42 - 2*q^43 + 10*q^44 - 3*q^45 - 2*q^46 - 2*q^47 + 8*q^48 + 4*q^50 - 8*q^52 + 2*q^53 + 4*q^54 - 4*q^56 + 5*q^57 + 4*q^58 - 2*q^59 + 6*q^60 + 2*q^61 + 6*q^62 + 2*q^63 + q^64 - 2*q^65 - 5*q^66 - 2*q^68 + 8*q^69 - 10*q^70 + 8*q^71 + 5*q^72 - 3*q^73 - 9*q^74 - 6*q^75 - 8*q^76 + 5*q^77 + 2*q^78 + 10*q^79 + 9*q^80 - 6*q^81 - 5*q^84 + 2*q^85 - 2*q^86 - 6*q^87 - 10*q^88 - q^90 - 2*q^91 - 10*q^92 - 4*q^93 - 12*q^94 - 2*q^95 + 2*q^96 + 2*q^97 + 11*q^98 + O(q^100), 

q^2 - 2*q^4 - q^5 + 3*q^8 + q^9 + q^10 - 2*q^11 - 2*q^12 + 2*q^13 + 2*q^14 - 4*q^16 - 2*q^18 + 2*q^19 - 3*q^20 - q^21 + 4*q^22 + 2*q^23 + 2*q^24 - 4*q^26 - 2*q^28 + 2*q^30 + 2*q^31 + 5*q^32 - 3*q^33 - 2*q^34 - 2*q^35 + 5*q^36 - 3*q^37 - 2*q^38 + 2*q^39 + 5*q^40 - q^42 - 2*q^43 - 4*q^44 + q^45 - 2*q^46 - 6*q^47 + q^49 - q^50 + 6*q^52 - 2*q^54 - 2*q^55 + 4*q^56 + 3*q^57 - 2*q^58 + 2*q^59 + 4*q^61 + 2*q^62 + 2*q^63 - 10*q^64 + 2*q^65 + q^66 + 2*q^67 + 4*q^68 + 4*q^69 + 4*q^71 - 8*q^72 - 7*q^73 + 3*q^74 - 2*q^75 - q^77 - 2*q^78 - 2*q^79 - 7*q^80 - 5*q^81 + q^82 - 3*q^84 + 2*q^85 + 2*q^86 - 2*q^87 + 10*q^88 + 6*q^89 - 5*q^90 + 2*q^91 - 2*q^92 + 10*q^94 + 4*q^96 - 2*q^98 + 4*q^99 + O(q^100), 

q^3 - 2*q^4 + q^6 - q^7 + 2*q^8 + 2*q^10 - 3*q^11 - q^12 + 2*q^13 - q^14 - 2*q^15 - 2*q^18 + 3*q^19 + 2*q^21 + q^22 + 4*q^23 + q^24 - 2*q^25 - 2*q^26 - 2*q^27 - 3*q^28 - 2*q^29 - 2*q^30 + 4*q^32 - 2*q^33 + 2*q^35 + 2*q^36 - q^38 + 2*q^39 + 2*q^40 + 4*q^42 - 2*q^43 - q^44 + 2*q^45 - 3*q^47 - q^48 - 4*q^49 - 2*q^50 - 2*q^51 + 2*q^52 - 2*q^53 - 2*q^54 + 2*q^55 + 3*q^56 + 4*q^57 - 2*q^58 + 4*q^59 - 2*q^60 + 2*q^61 - 4*q^62 - q^63 - 6*q^64 + 4*q^66 + 3*q^67 + 4*q^68 + 4*q^69 + 6*q^70 + 3*q^71 - 4*q^72 - 8*q^73 + 6*q^74 - q^75 - 3*q^76 - 4*q^77 - 2*q^78 - q^79 - 8*q^80 - 2*q^81 - 4*q^83 + 2*q^84 + 2*q^86 - 2*q^87 + 5*q^88 + 8*q^89 + 2*q^91 - 4*q^92 + 8*q^93 + 9*q^94 - 2*q^95 - 3*q^96 + 4*q^97 - 6*q^98 + q^99 + O(q^100)];



t1 := basis2[2] ;
t2 := basis2[3] ;

X := t1/t2 ;
Y := q*Derivative(X)/t2 ;

F := Y^2  - a[9]*X^8 - a[8]*X^7 - a[7]*X^6 - a[6]*X^5 - a[5]*X^4 - a[4]*X^3 - a[3]*X^2 - a[2]*X - a[1] ;
CF := Coefficients(F) ;
S := Scheme(AffineSpace(Z), CF) ;
Ps := Points(S) ;

X0 := SmallModularCurve(41) ;
c1 := Cusp(X0,41,1) ;
c2 := Cusp(X0,41,41) ;

Zx<x> := PolynomialRing(Rationals());
f := x^8 -12*x^7 + 48*x^6 -82*x^5 +60*x^4 -8*x^3 - 27*x^2 + 16*x - 4 ;
HH := HyperellipticCurve(f) ;

Qx<[x]>:=PolynomialRing(Rationals(),5);

Q1 := 
x[1]*x[3] - x[2]^2 + 2*x[2]*x[4] + 2*x[2]*x[5] - 2*x[3]^2 - x[3]*x[4] + 
    x[3]*x[5] - 2*x[4]^2 - 2*x[4]*x[5] - x[5]^2 ;
Q2 := 
x[1]*x[4] - x[2]*x[3] + x[2]*x[4] - x[3]^2 + x[3]*x[4] + 2*x[3]*x[5] - 2*x[4]^2 
    - 2*x[4]*x[5] ;
Q3 := 
x[1]*x[5] - x[2]*x[5] - x[3]^2 + 2*x[3]*x[4] + 2*x[3]*x[5] - 2*x[4]^2 - 
    x[4]*x[5] + x[5]^2 ;

P := ProjectiveSpace(Qx) ;
C:=Curve(P,[Q1, Q2, Q3]);

K:=FunctionField(C);

g2 := K.2 -2 - K.4 ;
g3 := K.3 -2 ;

X := g2/g3 ;
f := X^8 -12*X^7 + 48*X^6 -82*X^5 +60*X^4 -8*X^3 - 27*X^2 + 16*X - 4 ;
tf, y := IsSquare(f) ;

num := -K.3^8 + 4*K.3^7 - 21*K.3^5 + 892*K.3^4*K.4^4 + 448*K.3^4*K.4^3 - 27*K.3^4*K.4^2 - 35*K.3^4*K.4 + 46*K.3^4 - 644*K.3^3*K.4^5 - 1890*K.3^3*K.4^4 - 
    1746*K.3^3*K.4^3 - 485*K.3^3*K.4^2 + 118*K.3^3*K.4 - 4*K.3^3 + 
    169*K.3^2*K.4^6 + 826*K.3^2*K.4^5 + 2245*K.3^2*K.4^4 + 3107*K.3^2*K.4^3 + 
    1418*K.3^2*K.4^2 - 109*K.3^2*K.4 - 91*K.3^2 - 338*K.3*K.4^6 - 58*K.3*K.4^5 -
    377*K.3*K.4^4 - 2761*K.3*K.4^3 - 1921*K.3*K.4^2 - 15*K.3*K.4 + 138*K.3 + 
    169*K.4^6 - 293*K.4^5 + 67*K.4^4 + 743*K.4^3 + 848*K.4^2 + 126*K.4 - 62 ;

den := K.3^4 - 6*K.3^3 + 14*K.3^2 - 16*K.3 + 8;

Qx<x>:=PolynomialRing(Rationals());
D := HyperellipticCurve( x^8 -12*x^7 + 48*x^6 -82*x^5 +60*x^4 -8*x^3 - 27*x^2 + 16*x - 4 ) ;

m:=map<C->D | [X,num/den,1] >;
D1 := Decomposition( Pullback(m, Place(D ! [ 1,-1,0] ) )) ;
D2 := Decomposition( Pullback(m, Place(D ! [ 1,1,0] ) )) ; 



// cuspidal subgroup of XH(41) //

K := QuadraticField(41) ;
OK := Integers(K);
P := PrimesUpTo(50,K) ;
P := P[4] ;

F7, pi := ResidueClassField(P) ;
Zx<[x]> := PolynomialRing(Integers(),5) ;


q1 := 
x[1]*x[3] - x[2]^2 + 2*x[2]*x[4] + 2*x[2]*x[5] - 2*x[3]^2 - x[3]*x[4] + 
    x[3]*x[5] - 2*x[4]^2 - 2*x[4]*x[5] - x[5]^2 ;
q2 := 
x[1]*x[4] - x[2]*x[3] + x[2]*x[4] - x[3]^2 + x[3]*x[4] + 2*x[3]*x[5] - 2*x[4]^2 
    - 2*x[4]*x[5] ;
q3 := 
x[1]*x[5] - x[2]*x[5] - x[3]^2 + 2*x[3]*x[4] + 2*x[3]*x[5] - 2*x[4]^2 - 
    x[4]*x[5] + x[5]^2 ;

P3 := ProjectiveSpace(Zx) ;
X := Curve(P3, [ q1, q2, q3] ) ;
XF7 := BaseChange(X,F7) ;


Cl, phi, psi := ClassGroup(XF7) ;
Z := FreeAbelianGroup(1) ;
degr := hom<Cl -> Z | [ Degree(phi(g)) : g in OrderedGenerators(Cl)]>;
J := Kernel(degr) ;

d1 := [  [ 1,0,0,0,0] , [ 0,1 ,0,0,1]] ;
r := (1/2)*( - 23 + 3*K.1) ;
r2 := (1/2)*( - 23 - 3*K.1) ;

d2 := [ [ 6*(r + 21), 2*( r + 25), 2*( r + 22), r + 22, 6], [ 6*(r2 + 21), 2*( r2 + 25), 2*( r2 + 22), r2 + 22, 6] ]  ;

print "The cusps on this model of the curve:";
d1[1];
d1[2] ;
d2[1] ;
d2[2] ;

d := d1 cat d2 ;
dpi := [ [ pi(a) : a in b ] : b in d ] ;
Cdpi := [ XF7  ! a : a in dpi ] ;


C1 := d1[1] ;
C2 := d1[2] ;
C3 := d2[1];
C4 :=  d2[2] ;

C1 := Place( XF7 !  pi(C1) );
C2 := Place( XF7 ! pi(C2) ) ;
C3 := Place(XF7 ! pi(C3) ) ;
C4 := Place(XF7 ! pi(C4));

CC := [ Place(b) : b in Cdpi ] ;
divs := [ CC[2] - CC[1], CC[3] - CC[1], CC[4] - CC[1] ] ;
H := [ psi(a) :a  in divs] ;
ZN := FreeAbelianGroup(#H) ;
hh := hom< ZN -> J | [  a : a in H ] >;
 
CH := Image(hh) ;

print "The cuspidal subgroup of JH(41) is:";
CH ;
// rational  part //


cpt := [ ZN.1, ZN.3, ZN.2] ;

conj := hom< ZN -> ZN | cpt>; 
mu := hom< ZN -> J | [ hh(ZN.i) - hh(conj(ZN.i)) : i in [1..3]]>; 
ker1 := Kernel(mu);
imKer1 := sub<J | [hh(k) : k in Generators(ker1)]>;

CHQ := imKer1;
print "The rational cuspidal subgroup of JH(p) is:";
CHQ;


// we compute a bound for the Q(\sqrt(41)) torsion subgroup of the jacobian 

CK := BaseChange(C,K) ;
PP := PrimesUpTo(80,K) ;
PP := [ P : P in PP | Norm(P) ne 41] ;
PP := [ P : P in PP | Norm(P) ne 4] ; 

SPP := Set([ Norm(a) : a in PP] ) ;
ind := [ Minimum([i : i in [1..#PP] | Norm(PP[i]) eq a]) : a in SPP];
PR := [ PP[i] : i in ind];


Bound2 := [];
P := [ p : p in PR | Norm(p) eq 5][1] ;
p := P ;
R,pi := ResidueClassField(p) ;
C5 := ChangeRing(CK, R ) ;
CL5,phi5,pi5 := ClassGroup(C5) ;
 Z := FreeAbelianGroup(1) ;
degr := hom<CL5 -> Z | [ Degree(phi5(g)) : g in OrderedGenerators(CL5)]>; 
t := #Kernel(degr) ;
 Bound2[1] := t;


P := [ p : p in PR | Norm(p) eq 23][1] ;
 p := P ;
R,pi := ResidueClassField(p) ;
C5 := ChangeRing(CK, R ) ;
 CL5,phi5,pi5 := ClassGroup(C5) ;
Z := FreeAbelianGroup(1) ;
degr := hom<CL5 -> Z | [ Degree(phi5(g)) : g in OrderedGenerators(CL5)]>; 
t := #Kernel(degr) ;
Bound2[2] := t;

B2 := GCD(Bound2) ;
print "An upper bound for the Q(\sqrt(41)) torsion subgroup of the Jacobian is";
B2;
assert B2 eq #CH ;

print "Thus it must equal the cuspidal subgroup.";
