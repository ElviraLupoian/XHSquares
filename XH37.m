// model of the curve XH(37) & cusps 


T := [ [a,b,c,d] : a, b,c,d in [0,1,2,3] | a + b + c + d eq 3 ] ;
Z<[a]> := PolynomialRing(CyclotomicField(37), 20) ;
Q<q> := LaurentSeriesRing(Z) ;

f1 := q - 2*q^5 + 2*q^6 - 3*q^7 + 2*q^9 + 2*q^11 - 6*q^12 - 8*q^13 + 4*q^14 + 4*q^15 + 4*q^16 + 7*q^17 - 8*q^18 + 5*q^19 + 2*q^21 + 2*q^22 + 9*q^23 - 6*q^25 - 4*q^26 - 12*q^27 + 4*q^28 - 5*q^29 - 4*q^30 - 6*q^31 + 9*q^33 + 2*q^34 - 2*q^35 + 8*q^36 + 4*q^37 + 6*q^38 + 2*q^39 - 12*q^41 - 6*q^42 + 6*q^43 - 14*q^44 - 4*q^45 + 2*q^46 - 3*q^47 + 8*q^48 - 10*q^49 + O(q^50);
f2 := q^2 - q^5 - q^6 - 3*q^13 + 3*q^14 + q^15 + q^17 - 2*q^18 + 3*q^19 + 2*q^20 - 3*q^22 + 2*q^23 - 2*q^29 - 4*q^32 - 3*q^35 + 3*q^37 + 3*q^39 - 3*q^42 - 3*q^43 + 2*q^45 + O(q^50);
f3 := q^3 - 2*q^7 - 2*q^10 + 3*q^11 - 2*q^12 - 2*q^13 + 4*q^16 + 3*q^17 + q^19 + q^21 + 3*q^23 - 3*q^25 - 6*q^26 - 5*q^27 + 4*q^28 - 3*q^29 + 2*q^30 - 2*q^31 + 2*q^34 + q^37 + 6*q^38 - 2*q^39 - 3*q^41 + 4*q^43 - 6*q^44 + 4*q^46 - 4*q^49 + O(q^50);
f4 := q^4 - q^5 + q^6 - 2*q^7 + 2*q^9 - q^10 + q^11 - 3*q^12 - 3*q^13 + 2*q^14 + 2*q^15 + 2*q^16 + 2*q^17 - 4*q^18 + 2*q^19 + 2*q^21 + q^22 + 3*q^23 - 2*q^25 - 5*q^26 - 6*q^27 + 3*q^28 - q^29 - q^30 - 2*q^31 + 3*q^33 + 2*q^34 - q^35 + 2*q^36 + 2*q^37 + 6*q^38 + 2*q^39 - 3*q^41 - 3*q^42 + q^43 - 7*q^44 - 2*q^45 + 3*q^46 - 3*q^47 + 2*q^48 - 4*q^49 + O(q^50);

F := a[1]*f1^3 + a[2]*f1^2*f2 + a[3]*f1^2*f3 + a[4]*f1^2*f4 + a[5]*f1*f2^2 + a[6]*f1*f3^2 + a[7]*f1*f4^2 + a[8]*f1*f2*f3 + a[9]*f1*f2*f4 + a[10]*f1*f3*f4 + a[11]*f2^3 + a[12]*f2^2*f3 + a[13]*f2^2*f4 + a[14]*f2*f3^2 + a[15]*f2*f4^2 + a[16]*f2*f3*f4 + a[17]*f3^3 + a[18]*f3^2*f4 + a[19]*f3*f4^2 + a[20]*f4^3 ;

CF := Coefficients(F) ;


S := Scheme(ProjectiveSpace(Z), CF ) ;  
I := Ideal(CF) ;
GB := GroebnerBasis(I) ;

S := Scheme(AffineSpace(Z), GB cat [ a[10] -1, a[19] -1, a[20] -1, a[18] -1, a[16] -1] ) ;
pt := Points(S) ;
pt := pt[1] ;
pt := Eltseq(pt) ;
pt := [ 24*a : a in pt ] ;

Zb<[b]> := PolynomialRing(Z, 16) ;
ZABCD< A, B, C, D> := PolynomialRing(Zb, 4) ;                                                                                      
Zx<[x]> := PolynomialRing(ZABCD,4) ;
s := [ x[1]^3, x[1]^2*x[2] , x[1]^2*x[3], x[1]^2*x[4], x[1]*x[2]^2, x[1]*x[3]^2, x[1]*x[4]^2, x[1]*x[2]*x[3], x[1]*x[2]*x[4], 
x[1]*x[3]*x[4] ,  x[2]^3, x[2]^2*x[3], x[2]^2*x[4], x[2]*x[3]^2, x[2]*x\
[4]^2, x[2]*x[3]*x[4], x[3]^3, x[3]^2*x[4], x[3]*x[4]^2, x[4]^3] ;
Q := -x[1]*x[3] + x[2]^2 + 2*x[3]*x[4] ;
F := &+[ pt[i]*s[i] : i in [1..20] ] ;
MF := Monomials(F) ;

T := &+ [ b[i]*MF[i] : i in [1..16] ] ;
q1 := x[1]*Q ;
q2 := x[2]*Q ;
q3 := x[3]*Q;
q4 := x[4]*Q ;
C := F - A*q1 - B*q2 - C*q3 - D*q4 ;


// cuspidal subgroup of XH(37) //


K := QuadraticField(37) ;
OK := Integers(K);
P := PrimesUpTo(50,K) ;
P := P[4] ;

F7, pi := ResidueClassField(P) ;
Zx<[x]> := PolynomialRing(Integers(),4) ;

c := 2*x[1]^2*x[4] - 5*x[1]*x[4]^2 - 2*x[2]^3 + 2*x[2]^2*x[3] - 2*x[2]*x[3]^2  +  6*x[2]*x[3]*x[4] - 6*x[2]*x[4]^2 - 3*x[3]^3 + 8*x[3]^2*x[4] - 9*x[3]*x[4]^2 + 6*x[4]^3;
q := x[1]*x[3] - x[2]^2 - 2*x[3]*x[4];

print "A model of the curve is:";
c;
q;

ZX<[X]> := PolynomialRing(Rationals(),4) ;
e1 := Evaluate(c, X) ;
e2 := Evaluate(q, X) ;
C := Curve(ProjectiveSpace(ZX), [e1,e2]) ;



P3 := ProjectiveSpace(Zx) ;
X := Curve(P3, [c,q] ) ;
XF7 := BaseChange(X,F7) ;


Cl, phi, psi := ClassGroup(XF7) ;
Z := FreeAbelianGroup(1) ;
degr := hom<Cl -> Z | [ Degree(phi(g)) : g in OrderedGenerators(Cl)]>;
J := Kernel(degr) ;


d1 := [[1,0,0,0], [2,0,1,1] ] ;
r := (1/6)*(1 + K.1) ;
r2 := (1/6)*(1 - K.1) ;
d2 := [[3*r + 2,3,3*r -1,1], [ 3*r2 +2, 3, 3*r2 -1, 1] ] ;

d := d1 cat d2 ;

print "The cusps of XH(37) on this model are:";
d[1] ;
d[2] ;
d[3] ;
d[4];

dpi := [ [ pi(a) : a in b ] : b in d ] ;
Cdpi := [ XF7  ! a : a in dpi ] ;

C1 := d1[1] ;
C2 := d1[2] ;
C3 := d2[1] ;
C4 := d2[2] ;

C1 := Place(XF7 !  pi(C1) ) ;
C2 := Place(XF7 ! pi(C2)) ;
C3 := Place(XF7 ! pi(C3) ) ;
C4 := Place(XF7 ! pi(C4)) ;

CC := [ Place(b) : b in Cdpi ] ;
divs := [ CC[2] - CC[1], CC[3] - CC[1], CC[4] - CC[1] ] ;
H := [ psi(a) :a  in divs] ;


ZN := FreeAbelianGroup(#H) ;
hh := hom< ZN -> J | [  a : a in H ] >;

CH := Image(hh) ;

print "The Cuspidal Subgroup of XH(37):";
CH;

// rational cuspidal subgroup  //
cpt := [  ZN.1, ZN.3, ZN.2] ;

conj := hom< ZN -> ZN | cpt>; 
mu := hom< ZN -> J | [ hh(ZN.i) - hh(conj(ZN.i)) : i in [1..3]]>; 
ker1 := Kernel(mu);
imKer1 := sub<J | [hh(k) : k in Generators(ker1)]>;
CHQ := imKer1;


print "The rational torsion subgroup of XH(37) is:";
CHQ;


CK := BaseChange(C,K) ;
PP := PrimesUpTo(80,K) ;
PP := [ P : P in PP | Norm(P) ne 37] ;
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

print "An upper bound for the Q(\sqrt(37)) torsion subgroup of the Jacobian is:";
B2;
assert B2 eq #CH ;

print "Thus it must equal the cuspidal subgroup.";


