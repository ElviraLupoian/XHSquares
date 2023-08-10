
// we start by  finding models of the curves X_H(29) and X0(29) and the quotient map  XH(29) --> X0(29) //

Qq<q>:=PowerSeriesRing(Rationals(),50);

basis:=
[
q - 4*q^6 + 2*q^7 - 3*q^8 - 2*q^9 + 3*q^11 + 3*q^12 + 2*q^13 + 3*q^14 + 5*q^16 - 6*q^17 - 3*q^18 + 9*q^19 - 3*q^20 - 3*q^21 + 4*q^22 - 12*q^23 + q^24 - 8*q^25 + 3*q^26 + 3*q^27 - 6*q^28 + 3*q^29 + 9*q^30 - 3*q^31 + 6*q^32 - 4*q^33 - 8*q^34 + 6*q^36 - 6*q^37 - 3*q^39 + 3*q^40 + 15*q^41 - 2*q^42 + 9*q^43 - 6*q^44 - 6*q^46 + 6*q^47 + 3*q^49 + O(q^50),
q^2 - q^3 - q^8 - 3*q^10 + q^11 + 3*q^12 + 2*q^14 + 3*q^15 - 2*q^17 - 2*q^18 - 2*q^21 - q^26 - q^27 + 2*q^29 + 3*q^31 - 3*q^32 + q^39 + 3*q^40 - 2*q^41 - 3*q^43 - 3*q^44 + 6*q^46 + q^47 + q^48 + O(q^50),
q^4 - q^6 - q^7 - q^8 + q^9 - q^10 + 3*q^12 - q^13 + 2*q^14 + q^15 - 2*q^18 - q^20 - 2*q^21 + 2*q^23 - 2*q^24 + q^26 - q^27 - q^28 + q^29 + q^30 + 4*q^31 - 2*q^32 + q^35 + q^36 - 3*q^38 - q^39 + 2*q^40 - 4*q^41 + 3*q^42 - 2*q^43 - q^44 - q^45 + 2*q^46 - q^47 + 2*q^48 + O(q^50),
q^5 - 2*q^6 + q^7 - q^9 + q^10 + q^11 - 2*q^12 + 2*q^13 - q^14 - q^15 + 2*q^16 - 2*q^17 + q^18 + 3*q^19 - 3*q^20 + q^21 + 3*q^22 - 8*q^23 + 4*q^24 - 4*q^25 + 2*q^27 + q^28 + q^29 + 7*q^30 - 5*q^31 + 4*q^32 - 3*q^33 - 6*q^34 + q^35 - q^36 - 2*q^37 + 3*q^38 - q^40 + 9*q^41 - 7*q^42 + 5*q^43 - q^44 - q^45 - 4*q^46 + 3*q^47 - 2*q^48 + 2*q^49 + O(q^50)
];


Qx<[x]>:=PolynomialRing(Rationals(),4);

mons:=MonomialsOfDegree(Qx,2);
V:=VectorSpace(Rationals(),#mons);
monImages:=[Evaluate(mon,basis) : mon in mons];

W:=VectorSpace(Rationals(),40);
monImages:=[W![Coefficient(m,i) : i in [0..39] ] : m in monImages];

h:=hom<V->W | monImages>;

K:=Kernel(h);

v:=Eltseq(V!(K.1));

Q:=&+[v[i]*mons[i] : i in [1..#mons]];

 //  a  model of XH(29) //

mons:=MonomialsOfDegree(Qx,3);
V:=VectorSpace(Rationals(),#mons);
monImages:=[Evaluate(mon,basis) : mon in mons];

W:=VectorSpace(Rationals(),40);
monImages:=[W![Coefficient(m,i) : i in [0..39] ] : m in monImages];

h:=hom<V->W | monImages>;

K:=Kernel(h);

v:=Eltseq(V!(K.1));

c:=&+[v[i]*mons[i] : i in [1..#mons]];


P:=ProjectiveSpace(Qx);

C:=Curve(P,[Q,c]);

K:=FunctionField(C);


g2:=K.2-2*K.3;
g1:=K.1-K.3-1;

eX:=g1/g2;
f := (eX)^6-4*(eX)^5-12*(eX)^4+2*(eX)^3+8*(eX)^2+8*(eX)-7;


num:=K.2^2*K.3^2 - 5*K.2^2*K.3 - K.2^2 - 2*K.2*K.3^5 + 15*K.2*K.3^4 - 27*K.2*K.3^3 + 15*K.2*K.3^2 + 4*K.2*K.3 + 2*K.3^6 - 10*K.3^5 + 32*K.3^4 - 20*K.3^3 - 29*K.3^2 - 10*K.3 - 1;
den:= K.3^5 - 5*K.3^4 - K.3^3;


QX<X>:=PolynomialRing(Rationals());
D:=HyperellipticCurve(X^6-4*X^5-12*X^4+2*X^3+8*X^2+8*X-7);

m:=map<C->D | [eX,num/den,1] >;

d1 := Decomposition(Pullback(m,Place(D![1,1,0])));

d2 := Decomposition(Pullback(m,Place(D![1,-1,0])));

// d1 & d2 are the cusps of  XH(29) //

print " A model of XH(29) is:";
Q;
c;



// we compute the cuspidal subgroup of  XH(29) //

K := QuadraticField(29) ;
OK := Integers(K);
P := PrimesUpTo(50,K) ;
P := P[2] ;

F5, pi := ResidueClassField(P) ;


Zx<[x]> := PolynomialRing(Integers(),4) ;
c := x[1]^2*x[3] - 3*x[1]*x[2]*x[3] - x[1]*x[3]^2 - x[2]^3 + x[2]^2*x[3] +  x[2]*x[3]^2 + 5*x[2]*x[3]*x[4] - x[2]*x[4]^2 + 4*x[3]^3 - 5*x[3]^2*x[4] - 3*x[3]*x[4]^2;
q := x[1]*x[4] - x[2]*x[3] + x[2]*x[4] + x[3]^2 - 3*x[3]*x[4] - 2*x[4]^2;
P3 := ProjectiveSpace(Zx) ;
X := Curve(P3, [c,q] ) ;
XF5 := BaseChange(X,F5) ;

Cl, phi, psi := ClassGroup(XF5) ;
Z := FreeAbelianGroup(1) ;
degr := hom<Cl -> Z | [Degree(phi(g)) : g in OrderedGenerators(Cl)]>;
J := Kernel(degr) ;

d1 := [ [1,0,0,0] , [ 2,0,0,1] ] ;

r := (1/2)*(5 + K.1) ;
r2 := (1/2)*(5 - K.1) ;
d2 := [ [6*r +3, 2*r , r, 1] , [ 6*r2 + 3 , 2*r2, r2, 1] ] ;
d := d1 cat d2;

print " The cusps on this model are:";
for i in [1..4] do ;
d[i];
end for ;
dpi := [ [ pi(a) : a in b ] : b in d ] ;
Cdpi := [ XF5  ! a : a in dpi ] ;

C1 := d1[1] ;
C2 := d1[2] ;
C3 := d2[1] ;
C4 := d2[2] ;
C1 := XF5 ! pi(C1) ;
C2 := XF5 ! pi (C2) ;
C3 := XF5 ! pi(C3) ;
C4 := XF5 ! pi(C4) ;
C1 := Place(C1) ;
C2 := Place(C2) ;
C3 := Place(C3) ;
C4 := Place(C4) ;

CC := [ Place(b) : b in Cdpi ] ;
divs := [ CC[i] - CC[1] : i in [2..4] ] ;
H := [ psi(a) :a  in divs] ;
ZN := FreeAbelianGroup(#H) ;

hh := hom< ZN -> J | [  a : a in H ] >;
CH := Image(hh) ;

print "The cuspidal subgroup of JH(p) is:";
CH;

// taking the rational part of the cuspidal subgroup//

cpt := [ ZN.1, ZN.3, ZN.2] ;
conj := hom< ZN -> ZN | cpt>; mu := hom< ZN -> J | [ hh(ZN.i) - hh(conj(ZN.i)) : i in [1..3]]>; 
ker1 := Kernel(mu);
imKer1 := sub<J | [hh(k) : k in Generators(ker1)]>;
CHQ  := imKer1;


print "The rational cuspdial subgroup of JH(p) is:";

CHQ;


// we compute the upper bounds for rational and Q(\sqrt(p)) torsion 

CK := BaseChange(C,K) ;
PP := PrimesUpTo(50,K) ;
PP := [ P : P in PP | Norm(P) ne 29] ;
PP := [ P : P in PP | Norm(P) ne 4] ; 

SPP := Set([ Norm(a) : a in PP] ) ;
ind := [ Minimum([i : i in [1..#PP] | Norm(PP[i]) eq a]) : a in SPP];
PR := [ PP[i] : i in ind];

Bounds2 := [] ;

for i in [1..#PR] do ;
p := PR[i] ;
R,pi := ResidueClassField(p) ;
C5 := ChangeRing(CK, R ) ;
CL5,phi5,pi5 := ClassGroup(C5) ;
Z := FreeAbelianGroup(1) ;
degr := hom<CL5 -> Z | [ Degree(phi5(g)) : g in OrderedGenerators(CL5)]>; 
t := #Kernel(degr) ;
Bounds2[i] := t ;
end for ;

B2 := GCD(Bounds2) ;

print "An upper bound for the Q(\sqrt(29)) torsion subgroup of the Jacobian is:";
B2;
assert B2 eq #CH ;

print "Thus it must equal the cuspidal subgroup.";


