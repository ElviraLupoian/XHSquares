
// model of XH(61), X0(61) and the map XH(61) --> X0(61) //

Qq<q>:=PowerSeriesRing(Rationals(),200);

basis := [q - 6*q^9 - 2*q^10 + q^11 + 4*q^12 + 5*q^13 + 5*q^14 - 3*q^15 - 8*q^16 - 3*q^17 - 3*q^18 + 9*q^19 - 5*q^20 + 8*q^21 + 3*q^22 + 5*q^23 + 2*q^24 - 7*q^25 - 2*q^26 - 13*q^27 + q^28 + 2*q^29 + 7*q^30 - 2*q^31 + q^32 - 13*q^33 + 11*q^34 - 4*q^35 + 2*q^36 + q^37 - 3*q^38 + q^39 + 4*q^40 - 10*q^41 + 13*q^42 + 3*q^43 + 18*q^45 - 18*q^46 + 17*q^47 - 13*q^48 - q^49 + 3*q^50 + 3*q^51 - 3*q^52 - 3*q^53 + 3*q^54 - 2*q^55 + 13*q^56 + 3*q^57 - 13*q^58 - 9*q^59 + 2*q^60 - 4*q^61 - 21*q^62 + 4*q^63 - 10*q^64 - 3*q^65 + q^66 + 2*q^67 + 5*q^68 - 15*q^69 - 19*q^70 + 5*q^71 + 6*q^72 + 24*q^73 + 26*q^74 + q^75 - 13*q^76 + 5*q^77 + 3*q^78 - 6*q^79 + 18*q^80 + 10*q^81 + 4*q^82 + 11*q^83 - 19*q^84 + 8*q^85 - 17*q^86 - 12*q^87 + 14*q^88 + 4*q^89 - 4*q^90 - q^91 - 8*q^92 + q^94 - 16*q^95 - 11*q^96 + 7*q^97 + q^98 - 6*q^99 + 6*q^100 + 6*q^102 + 23*q^103 + 3*q^104 - 10*q^105 + 33*q^106 - 16*q^107 + 15*q^108 - 15*q^109 + 5*q^110 + 18*q^111 + 4*q^112 - 26*q^113 + 13*q^114 - 10*q^115 - q^116 - 14*q^117 + 11*q^118 - 20*q^119 - 18*q^120 + 7*q^121 - 25*q^122 + 7*q^123 + 3*q^124 + 12*q^125 - 26*q^126 - 29*q^127 - 2*q^128 + 3*q^129 - 5*q^130 + 3*q^131 + 21*q^132 - 13*q^133 + 33*q^134 + 25*q^135 + 16*q^136 - 10*q^137 - 15*q^138 + 4*q^139 + 12*q^140 + 11*q^141 - 2*q^142 + q^143 + 13*q^144 - 4*q^145 - 3*q^146 - 2*q^147 - 6*q^148 - 14*q^149 - 7*q^150 - 13*q^151 + 7*q^152 - 8*q^153 - 6*q^154 + 11*q^156 + 6*q^157 - 33*q^158 + 6*q^159 - 2*q^160 + 44*q^161 - 5*q^162 - 9*q^163 - 2*q^164 + 28*q^165 - q^166 + 22*q^167 - 7*q^168 + 2*q^169 - 27*q^170 + 20*q^171 + 3*q^172 - 3*q^173 - 7*q^174 + 2*q^175 - 8*q^176 - 14*q^177 + 27*q^178 + 8*q^179 - 24*q^180 + 3*q^181 + 13*q^182 - 10*q^183 - 23*q^184 + 3*q^185 - 7*q^186 - 2*q^187 + 4*q^188 + 13*q^189 - 2*q^190 - 19*q^191 - q^192 + 7*q^193 - 7*q^194 - 8*q^195 + 10*q^196 - 10*q^197 - 9*q^198 + 7*q^199 + O(q^200),
q^2 - 4*q^9 - 3*q^11 + 3*q^12 + 4*q^13 + 4*q^14 - q^15 - 4*q^16 - 3*q^17 - 2*q^18 + 4*q^19 - 2*q^20 + 2*q^21 - q^22 + 4*q^23 + 5*q^24 - 4*q^25 + 3*q^26 - 8*q^27 - 3*q^28 + 2*q^29 + 4*q^31 - 6*q^33 + 5*q^34 + 4*q^36 - 3*q^37 - q^38 - 3*q^40 - 6*q^41 + 5*q^42 + 7*q^43 + 12*q^45 - 10*q^46 + 10*q^47 - 8*q^48 + 2*q^49 + q^50 - q^51 + 2*q^52 - 7*q^53 - 7*q^54 + 2*q^55 + 5*q^56 + 8*q^57 - 9*q^58 - 11*q^59 - 4*q^60 + q^61 - 5*q^62 + 12*q^63 - 4*q^64 + 6*q^66 - 8*q^67 + 2*q^68 - 3*q^69 - 9*q^70 - q^71 - 2*q^72 + 13*q^73 + 11*q^74 - 2*q^75 - 11*q^76 + 4*q^77 - 5*q^78 + q^79 + 12*q^80 + 6*q^81 - 2*q^82 + 6*q^83 - 4*q^84 + 11*q^85 - 7*q^86 - 4*q^87 + 6*q^88 - 2*q^89 + 4*q^90 - q^91 - 4*q^93 + 7*q^94 - 9*q^95 - 9*q^96 + 2*q^97 - 3*q^98 - 6*q^99 + 4*q^100 - 5*q^101 + 2*q^102 + 11*q^103 + 7*q^104 - 11*q^105 + 20*q^106 - 12*q^107 + 6*q^108 - 10*q^109 + 8*q^110 + 14*q^111 + 12*q^112 - 12*q^113 + 7*q^114 - 14*q^115 - 12*q^117 + 6*q^118 - 9*q^119 - 10*q^120 - 2*q^121 - 12*q^122 + 7*q^123 + 4*q^124 + 4*q^125 - 10*q^126 - 14*q^127 + 2*q^128 - 3*q^129 - 11*q^130 - q^131 + 15*q^132 - 7*q^133 + 22*q^134 + 20*q^135 + 10*q^136 - 5*q^137 - 5*q^138 + 2*q^139 + 2*q^140 + 4*q^141 - 9*q^142 + 3*q^143 + 2*q^144 - 3*q^145 + 7*q^146 - 6*q^147 - 5*q^148 - 8*q^149 - 3*q^150 - 9*q^151 - 2*q^152 + q^154 - 11*q^155 + q^156 + 18*q^157 - 16*q^158 - 2*q^159 - 2*q^160 + 22*q^161 + 4*q^162 - 4*q^163 - 5*q^164 + 18*q^165 + 3*q^166 + 18*q^167 - 2*q^168 - 2*q^169 - 16*q^170 + 14*q^171 - 6*q^172 - 5*q^173 - 2*q^174 + 2*q^175 - 9*q^177 + 16*q^178 - 4*q^179 - 14*q^180 - q^181 + 8*q^182 - 3*q^183 - 12*q^184 + 6*q^185 - 12*q^186 - 2*q^187 + 4*q^188 + 13*q^189 + 5*q^190 - 5*q^191 - q^192 + 15*q^193 - 9*q^194 + 3*q^195 + 7*q^196 - 9*q^198 + 12*q^199 + O(q^200),
q^3 - q^9 - q^11 - q^12 + 2*q^14 - q^16 - q^17 + q^18 - 2*q^19 + 2*q^20 - 2*q^21 - 3*q^22 + q^23 + 2*q^24 + q^25 + 4*q^26 - q^27 - q^28 - 3*q^30 + 4*q^31 + q^32 + 3*q^33 + 2*q^34 + 2*q^35 + 2*q^36 - 3*q^37 - q^38 - q^41 - 4*q^42 + q^43 - 2*q^44 - 2*q^45 - 4*q^46 + q^47 + 2*q^49 - q^50 - q^51 + 3*q^52 - q^53 - 3*q^54 + 2*q^55 - q^56 + 3*q^57 - 2*q^58 + q^59 - 3*q^60 + 2*q^62 + 2*q^63 - 5*q^65 + 6*q^66 - q^68 + 3*q^69 + q^70 - 3*q^71 - 4*q^72 + 2*q^73 - q^74 - q^75 - 2*q^77 - 3*q^78 + 4*q^79 - 2*q^80 + 3*q^81 - 4*q^82 - 3*q^83 + 5*q^84 + 6*q^86 + 2*q^87 + q^88 - 2*q^89 + 4*q^90 - q^91 + 2*q^92 - 2*q^93 + 5*q^94 + q^95 - q^96 - q^97 + q^98 - 2*q^101 + 2*q^103 - q^104 + 3*q^106 - 5*q^108 - 2*q^109 + 2*q^112 + q^113 + q^114 - 2*q^115 - q^116 - q^117 - 6*q^118 - q^119 + 2*q^120 - 4*q^121 + 3*q^124 + 2*q^125 + 5*q^126 - 6*q^127 - 5*q^129 + q^130 - 2*q^131 + q^132 + 3*q^133 + q^135 - q^136 + 4*q^137 + 4*q^138 - 4*q^140 + q^141 - 7*q^142 + q^143 + 2*q^144 - 2*q^145 + q^146 - 6*q^147 + q^149 - q^150 - 3*q^151 - 3*q^152 + 4*q^153 + 2*q^154 - 2*q^155 - 8*q^156 + 8*q^157 - 5*q^158 - 2*q^159 - 2*q^160 - q^161 + 3*q^162 + 6*q^163 - 2*q^165 + q^166 + q^167 + 8*q^168 + 2*q^169 + 2*q^170 - 5*q^171 - 3*q^172 - 3*q^173 + 3*q^174 + 2*q^176 + 6*q^177 + 5*q^178 + 5*q^180 + q^181 + 6*q^182 + 3*q^183 + 2*q^184 + 3*q^185 - 11*q^186 - 3*q^187 - 2*q^188 - q^189 + 6*q^190 + 5*q^191 + q^193 - q^194 + q^195 - 3*q^196 + 2*q^197 - 3*q^198 + q^199 + O(q^200),
q^4 - 3*q^9 - q^10 + q^11 + q^13 + 3*q^14 - 3*q^16 - q^17 - 2*q^18 + 4*q^19 - 2*q^20 + 5*q^21 + q^22 + 2*q^23 - 3*q^25 - 3*q^26 - 4*q^27 + q^28 + q^29 + 5*q^30 - 3*q^31 - 8*q^33 + 4*q^34 - 3*q^35 + 4*q^36 + 2*q^37 - q^38 + 2*q^39 + 2*q^40 - 3*q^41 + q^42 + q^43 + q^44 + 7*q^45 - 8*q^46 + 5*q^47 - 4*q^48 + q^49 + 2*q^50 + 2*q^51 - q^53 + 3*q^54 - 2*q^55 + q^56 + 3*q^57 - 5*q^58 - 5*q^59 - 2*q^60 - 3*q^61 - 7*q^62 + q^63 - 6*q^64 + q^65 + q^66 + q^67 + 3*q^68 - 9*q^69 - 4*q^70 + 4*q^71 + 5*q^72 + 9*q^73 + 9*q^74 - 2*q^76 + 2*q^77 + 3*q^78 - 5*q^79 + 7*q^80 + 3*q^81 + 4*q^82 + 4*q^83 - 12*q^84 + 4*q^85 - 4*q^86 - 7*q^87 + 8*q^88 + 3*q^89 - 4*q^90 - 5*q^92 + q^93 - 2*q^94 - 10*q^95 - 5*q^96 + 4*q^97 - 3*q^99 + q^100 + q^101 - q^102 + 9*q^103 + 2*q^104 - 5*q^105 + 10*q^106 - 2*q^107 + 6*q^108 - q^109 + q^110 + 9*q^111 + q^112 - 9*q^113 + 6*q^114 - 4*q^115 - 8*q^117 + 3*q^118 - 5*q^119 - 10*q^120 + 2*q^121 - 8*q^122 + 5*q^123 + 5*q^125 - 2*q^126 - 10*q^127 - q^128 + 4*q^129 - 3*q^130 + q^131 + 10*q^132 - 8*q^133 + 6*q^134 + 12*q^135 + 4*q^136 - 7*q^137 + 2*q^138 + 2*q^139 + 8*q^140 + 2*q^141 - q^142 - q^145 - 2*q^146 - 5*q^147 - 3*q^148 - 6*q^149 - 3*q^150 - 5*q^151 + 5*q^152 - 6*q^153 - 4*q^154 + q^155 + 2*q^156 - q^157 - 14*q^158 + 4*q^159 + 12*q^161 - 4*q^162 - 6*q^163 - q^164 + 15*q^165 - q^166 + 15*q^167 + 3*q^168 + 2*q^169 - 10*q^170 + 8*q^171 + 3*q^172 - q^174 + q^175 - 5*q^176 - 10*q^177 + 13*q^178 - 2*q^179 - 7*q^180 + q^181 + 8*q^182 - q^183 - 2*q^184 - q^186 + q^187 + 3*q^188 + 7*q^189 - 4*q^190 - 12*q^191 + 8*q^192 + 3*q^193 - 3*q^194 - 3*q^196 - 3*q^197 - 8*q^198 + 2*q^199 + O(q^200),
q^5 - 2*q^9 - q^10 + 2*q^12 + q^13 + q^14 - q^15 - 2*q^16 - 2*q^17 - q^18 + 3*q^19 - 2*q^20 + 3*q^21 - q^22 + 3*q^23 + 2*q^24 - 2*q^25 + q^26 - 5*q^27 + q^29 + 2*q^30 + q^31 + q^32 - 5*q^33 + 3*q^34 - q^35 + q^36 - q^37 - 2*q^38 - q^39 + 2*q^40 - 2*q^41 + 4*q^42 + 2*q^43 - q^44 + 6*q^45 - 5*q^46 + 2*q^47 - 5*q^48 + q^50 + q^51 - 2*q^53 + 3*q^56 + 2*q^57 - 4*q^58 - 4*q^59 - 4*q^62 + 3*q^63 - q^64 - q^65 + 2*q^66 + q^67 + 2*q^68 - 6*q^69 - 6*q^70 + q^71 + q^72 + 8*q^73 + 7*q^74 + q^75 - 7*q^76 + 4*q^77 - q^79 + 4*q^80 + 4*q^81 + 3*q^83 - 7*q^84 + 4*q^85 - 5*q^86 - 5*q^87 + 5*q^88 + q^89 - q^91 - 3*q^92 - q^93 + 3*q^94 - 6*q^95 - 6*q^96 + q^97 + q^98 - 3*q^99 + 3*q^100 - q^101 + q^102 + 4*q^103 + q^104 - 5*q^105 + 9*q^106 - 4*q^107 + 5*q^108 - 3*q^109 + 5*q^110 + 9*q^111 + 3*q^112 - 4*q^113 + 7*q^114 - 6*q^115 - q^116 - 3*q^117 + 3*q^118 - 5*q^119 - 8*q^120 - 3*q^121 - 7*q^122 + 6*q^123 + 3*q^124 - 7*q^126 - 7*q^127 - q^128 - q^129 - 2*q^130 + 4*q^131 + 11*q^132 - 5*q^133 + 8*q^134 + 9*q^135 + 2*q^136 - 2*q^137 - 3*q^138 + 2*q^139 + 4*q^140 + q^141 - 3*q^142 + q^143 + 2*q^144 - 3*q^145 - q^146 - 3*q^147 - 3*q^148 - 7*q^149 - 4*q^150 - 8*q^151 + 2*q^152 - 2*q^153 - 2*q^154 - q^155 + 3*q^156 + 7*q^157 - 6*q^158 + 2*q^159 - 2*q^160 + 13*q^161 - q^162 - 7*q^163 - 2*q^164 + 13*q^165 + 7*q^167 - 4*q^168 + 4*q^169 - 7*q^170 + 10*q^171 - 3*q^173 + q^175 - 3*q^176 - 4*q^177 + 4*q^178 + 4*q^179 - 10*q^180 + 2*q^181 + 2*q^182 - 3*q^183 - 2*q^184 + 3*q^185 - 4*q^186 + q^187 + 3*q^188 + 6*q^189 + 2*q^190 - 7*q^191 + q^192 + 4*q^193 - 4*q^194 - 2*q^195 + 5*q^196 - 2*q^198 + 3*q^199 + O(q^200),
q^6 - 4*q^9 - 2*q^11 + 3*q^12 + 4*q^13 + 4*q^14 - q^15 - 4*q^16 - 2*q^17 - 3*q^18 + 4*q^19 - 2*q^20 + 5*q^21 - q^22 + 3*q^23 + 2*q^24 - 4*q^25 - 8*q^27 + q^29 + 3*q^30 + q^31 + q^32 - 7*q^33 + 5*q^34 - 3*q^35 + 4*q^36 - 4*q^38 - 6*q^41 + 5*q^42 + 4*q^43 - 2*q^44 + 12*q^45 - 10*q^46 + 10*q^47 - 8*q^48 + 2*q^49 + 3*q^50 + q^51 + 2*q^52 - 3*q^53 - 3*q^54 + 2*q^55 + 5*q^56 + 8*q^57 - 9*q^58 - 9*q^59 - 4*q^60 - 2*q^61 - 5*q^62 + 6*q^63 - 4*q^64 + 6*q^66 - 2*q^67 + 3*q^68 - 8*q^69 - 9*q^70 + 4*q^72 + 13*q^73 + 11*q^74 - 2*q^75 - 11*q^76 + 4*q^77 - 2*q^78 - 2*q^79 + 12*q^80 + 6*q^81 + q^82 + 6*q^83 - 13*q^84 + 8*q^85 - 7*q^86 - 6*q^87 + 6*q^88 - 2*q^90 - q^91 - 4*q^92 - 4*q^93 + q^94 - 9*q^95 - 10*q^96 + 2*q^97 + 2*q^98 - 5*q^99 + 4*q^100 + 2*q^102 + 11*q^103 + 7*q^104 - 8*q^105 + 20*q^106 - 12*q^107 + 6*q^108 - 10*q^109 + 8*q^110 + 14*q^111 + 6*q^112 - 12*q^113 + 13*q^114 - 8*q^115 - q^116 - 12*q^117 + 6*q^118 - 9*q^119 - 13*q^120 - 2*q^121 - 14*q^122 + 7*q^123 + 7*q^124 + 4*q^125 - 10*q^126 - 14*q^127 + 2*q^128 + 3*q^129 - 11*q^130 - q^131 + 20*q^132 - 10*q^133 + 22*q^134 + 20*q^135 + 10*q^136 - 5*q^137 - 5*q^138 + 2*q^139 + 8*q^140 + 4*q^141 - 9*q^142 + 6*q^143 + 2*q^144 + 3*q^146 - 6*q^147 - 8*q^148 - 8*q^149 - 5*q^150 - 9*q^151 + q^152 - 5*q^153 - 2*q^154 - 8*q^155 + q^156 + 9*q^157 - 16*q^158 - 2*q^160 + 22*q^161 + 3*q^162 - 4*q^163 - 5*q^164 + 15*q^165 - 3*q^166 + 18*q^167 - 2*q^168 - 2*q^169 - 16*q^170 + 14*q^171 + 3*q^172 - q^173 - 2*q^174 + 2*q^175 - q^176 - 14*q^177 + 16*q^178 - 4*q^179 - 14*q^180 + 5*q^181 + 8*q^182 - 3*q^183 - 12*q^184 + 3*q^185 - 12*q^186 - 2*q^187 + 4*q^188 + 13*q^189 + 2*q^190 - 13*q^191 - q^192 + 9*q^193 - 9*q^194 + 3*q^195 + 7*q^196 - 9*q^198 + 12*q^199 + O(q^200),
q^7 - 3*q^9 + q^10 - 3*q^11 + 2*q^12 + 2*q^13 + 3*q^14 + q^15 - 3*q^16 - q^17 + q^19 + q^21 - 2*q^22 + 3*q^24 - q^25 + 3*q^26 - 5*q^27 - 3*q^28 - q^29 - 2*q^30 + 3*q^31 - q^33 + 3*q^34 - q^35 + 4*q^36 - q^38 - q^39 - 2*q^40 - 2*q^41 + q^42 + 3*q^43 + 6*q^45 - 6*q^46 + 5*q^47 - 5*q^48 + q^49 - 2*q^51 + 3*q^52 - q^53 - 5*q^54 + 5*q^55 + 2*q^56 + 9*q^57 - 5*q^58 - 5*q^59 - 6*q^60 - q^61 - q^62 + 8*q^63 - 3*q^65 + 5*q^66 - 3*q^67 + q^69 - 4*q^70 - 4*q^71 - q^72 + 7*q^73 + 4*q^74 - 3*q^75 - 7*q^76 + 2*q^77 - 5*q^78 - q^79 + 6*q^80 + 3*q^81 - 2*q^82 + 3*q^83 - q^84 + 3*q^85 + q^86 - q^87 + 2*q^88 + q^89 + 4*q^90 + q^91 + 3*q^92 - 5*q^93 + 2*q^94 - 4*q^95 - 5*q^96 - 3*q^97 - 7*q^99 + 2*q^100 - 2*q^101 + 2*q^102 + 7*q^103 + 2*q^104 - 8*q^105 + 11*q^106 - 6*q^107 + q^108 - 9*q^109 + 3*q^110 + 9*q^111 + 6*q^112 - 4*q^113 + 4*q^114 - 2*q^115 + q^116 - 7*q^117 - 2*q^118 - 4*q^119 - 2*q^120 - 3*q^121 - 5*q^122 + 5*q^123 + 5*q^124 + 2*q^125 - 2*q^126 - 9*q^127 + 5*q^128 - 2*q^129 - 3*q^130 - 3*q^131 + 10*q^132 - 4*q^133 + 12*q^134 + 11*q^135 + 6*q^136 + q^137 - 3*q^138 + q^140 - q^141 - 8*q^142 + 2*q^143 + 2*q^145 + 3*q^146 - 6*q^147 - 6*q^148 - 2*q^149 - q^150 - 5*q^151 - 6*q^152 + 4*q^154 - 4*q^155 - 3*q^156 + 11*q^157 - 11*q^158 - 2*q^159 + 10*q^161 + 4*q^162 + q^163 - 5*q^164 + 7*q^165 + q^166 + 10*q^167 + q^168 - 5*q^170 + 8*q^171 - 4*q^172 - 2*q^173 - q^174 + q^175 - q^176 - 7*q^177 + 11*q^178 - 6*q^179 - 7*q^180 + 3*q^181 + 7*q^182 - q^183 - 8*q^184 - 3*q^185 - 13*q^186 - 6*q^187 + 2*q^188 + 7*q^189 + 5*q^190 - 4*q^191 - q^192 + 5*q^193 - 5*q^194 + 4*q^195 + 4*q^196 + 2*q^197 - 6*q^198 + 9*q^199 + O(q^200),
q^8 + q^9 - q^11 - q^12 - 2*q^13 - q^14 + 2*q^15 + q^16 + q^18 - 3*q^19 + 2*q^20 - 3*q^21 - q^22 - q^23 + 3*q^25 + 3*q^26 + 3*q^27 - q^28 - q^29 - 4*q^30 + 3*q^31 - q^32 + 5*q^33 - 2*q^34 + 2*q^35 - q^37 + 2*q^38 - q^39 - 2*q^40 + 4*q^41 - 4*q^42 - 2*q^43 + q^44 - 6*q^45 + 4*q^46 - 5*q^47 + 3*q^48 - q^49 - 3*q^50 - 3*q^51 + q^52 - 2*q^54 + 4*q^55 - 3*q^56 + q^57 + 4*q^58 + 5*q^59 - 2*q^60 + 2*q^61 + 4*q^62 + q^63 + 4*q^64 - 3*q^65 - q^66 - 2*q^67 - 2*q^68 + 8*q^69 + 5*q^70 - 3*q^71 - 4*q^72 - 6*q^73 - 7*q^74 - q^75 + 4*q^76 - 2*q^77 + 5*q^79 - 6*q^80 - 3*q^81 - 5*q^82 - 3*q^83 + 7*q^84 - 4*q^85 + 8*q^86 + 5*q^87 - 4*q^88 - 3*q^89 + 5*q^90 - q^91 + 4*q^92 + q^93 + 5*q^94 + 5*q^95 + 4*q^96 - 5*q^97 + q^98 - 2*q^100 - 3*q^101 - 4*q^103 - 2*q^104 + 3*q^105 - 9*q^106 + 6*q^107 - 5*q^108 + q^109 - 5*q^110 - 7*q^111 + q^112 + 8*q^113 - 5*q^114 + 5*q^115 + q^116 + 5*q^117 - 8*q^118 + 5*q^119 + 8*q^120 - q^121 + 6*q^122 - 2*q^123 - q^124 - 2*q^125 + 8*q^126 + 5*q^127 - 2*q^128 - q^129 + 5*q^130 - 2*q^131 - 7*q^132 + 3*q^133 - 10*q^134 - 9*q^135 - 4*q^136 + 6*q^137 + 2*q^138 - 3*q^139 - 4*q^140 - 5*q^141 + q^142 - 4*q^143 - 2*q^144 + q^145 + 3*q^146 + q^148 + 6*q^149 + 2*q^150 + q^151 - 4*q^152 + 6*q^153 + 2*q^154 + q^155 - 4*q^156 + 5*q^157 + 5*q^158 - 4*q^159 + 3*q^160 - 12*q^161 + 3*q^162 + 5*q^163 - 9*q^165 + 2*q^166 - 8*q^167 + 3*q^168 + 2*q^169 + 11*q^170 - 6*q^171 - 2*q^172 - q^173 + q^174 + q^175 + 2*q^176 + 4*q^177 - 5*q^178 - 2*q^179 + 7*q^180 - q^182 + 4*q^183 + 4*q^184 - 3*q^185 - q^186 - 4*q^187 - 2*q^188 - 2*q^189 + 8*q^191 - 2*q^193 + 6*q^194 + q^195 - 3*q^196 + 2*q^197 + 3*q^198 - 3*q^199 + O(q^200)
];


Qx<[x]>:=PolynomialRing(Rationals(),8);

mons:=MonomialsOfDegree(Qx,2);

V:=VectorSpace(Rationals(),#mons);

monImages:=[Evaluate(mon,basis) : mon in mons];

W:=VectorSpace(Rationals(),40);

monImages:=[W![Coefficient(m,i) : i in [0..39] ] : m in monImages];

h:=hom<V->W | monImages>;

K:=Kernel(h);

v := [ Eltseq(V ! K.i) : i in [1..15] ] ;
Q := [ &+[ a[i]*mons[i] : i in [1..#mons] ] : a in v ] ;

mons:=MonomialsOfDegree(Qx,3);

V:=VectorSpace(Rationals(),#mons);

monImages:=[Evaluate(mon,basis) : mon in mons];

W:=VectorSpace(Rationals(),50);

monImages:=[W![Coefficient(m,i) : i in [0..49] ] : m in monImages];

h:=hom<V->W | monImages>;

K:=Kernel(h);
v := [ Eltseq(V ! K.i) : i in [1..85] ] ;
C := [ &+[ a[i]*mons[i] : i in [1..#mons] ] : a in v ] ;
CC := Curve(ProjectiveSpace(Qx), Q cat [ C[1] ] ) ;


basis2 := [
q - q^5 - 1/2*q^6 - 1/2*q^7 + 3/2*q^8 + q^9 - 3/2*q^10 + 2*q^11 - 2*q^12 - 2*q^13 - q^14 + q^15 - q^16 + 1/2*q^17 + q^18 - q^19 + q^20 - 5/2*q^21 + 4*q^22 - q^23 - 5/2*q^24 + 2*q^25 + 3*q^27 + q^28 - 1/2*q^29 - 3/2*q^30 - 1/2*q^31 - 2*q^32 + 7/2*q^33 + q^34 + 2*q^35 - 3*q^36 + 1/2*q^37 + 9/2*q^38 + q^39 + 2*q^41 - 11/2*q^43 + 7/2*q^44 - 6*q^45 + q^46 + 3*q^48 - 4*q^49 - 4*q^50 - 2*q^51 - 4*q^52 + q^53 + 4*q^54 + 1/2*q^55 + 2*q^56 - 6*q^57 + 4*q^58 + 19/2*q^59 + 4*q^60 + 1/2*q^61 - 8*q^62 - 9/2*q^63 - q^64 - 5*q^65 - 8*q^66 + 1/2*q^67 - 3/2*q^68 + 13/2*q^69 + q^70 + 3/2*q^71 - 5/2*q^72 - 3*q^73 + q^74 + q^75 + 9*q^76 - 5*q^77 + 13/2*q^78 + 4*q^79 - 4*q^80 - 3*q^81 - 3*q^82 - q^83 + 11/2*q^84 - 15/2*q^85 + 3*q^86 + 4*q^87 - q^88 - 2*q^89 + 5/2*q^90 - 3/2*q^91 + 3/2*q^92 + 7*q^93 + 4*q^94 + 4*q^95 + 17/2*q^96 - q^97 + 1/2*q^98 + 3*q^99 - 3*q^100 - 5/2*q^101 + 3*q^102 + 4*q^103 - 11/2*q^104 + 15/2*q^105 - 5*q^106 + 6*q^107 - q^108 - q^109 - 13*q^110 - 13*q^111 - 7/2*q^112 - 2*q^113 - 10*q^114 + 17/2*q^115 + 3/2*q^116 + 6*q^117 - 6*q^118 - q^119 + 19/2*q^120 + 11*q^121 + 1/2*q^122 - 8*q^123 - 15/2*q^124 + 6*q^125 - q^126 - 3*q^127 - 15/2*q^128 + 2*q^129 + 23/2*q^130 - 2*q^131 - 31/2*q^132 + 7/2*q^133 - 7*q^134 - 13*q^135 + 3*q^137 - 5*q^138 - 7/2*q^139 - 5/2*q^140 + q^141 + 11*q^142 - 10*q^143 + 7*q^144 - 1/2*q^145 - 1/2*q^146 + 7*q^147 + 11/2*q^148 + 7*q^149 + 3*q^150 + 7/2*q^151 + 3/2*q^152 + 11/2*q^153 - 2*q^154 + 17/2*q^155 + 3*q^156 - 7/2*q^157 - 6*q^158 - q^159 + 11/2*q^160 - 3*q^161 - 3*q^162 + 7*q^163 + 5*q^164 - 19/2*q^165 + 3*q^166 - 11*q^167 + 2*q^168 + 2*q^169 + 7*q^170 - 10*q^171 + 1/2*q^172 - 4*q^174 + q^175 - q^176 + 13/2*q^177 + 2*q^178 + 6*q^179 + 7*q^180 - 3*q^181 + 2*q^182 + q^183 - 5*q^184 - 9/2*q^185 + 8*q^186 - 5*q^187 - 5*q^188 - 6*q^189 - 15/2*q^190 + 17/2*q^191 - q^192 - 7*q^193 + 13*q^194 - 8*q^195 - 5*q^196 - 8*q^197 + 5*q^198 - 11*q^199 + O(q^200),
q^2 - 2*q^5 - q^7 - q^8 + 2*q^9 + q^10 + q^11 - 2*q^12 + 2*q^13 - 2*q^15 + 2*q^16 + 2*q^17 - q^18 - 2*q^21 + 4*q^22 - q^23 - 2*q^24 - 2*q^25 - 5*q^26 + 4*q^27 + q^28 + 2*q^29 + 2*q^30 - 4*q^31 - q^32 - 2*q^34 + q^35 - 2*q^36 + 2*q^38 + 4*q^39 - 3*q^40 - 4*q^41 + 2*q^43 + q^44 + 2*q^46 + 6*q^47 + 4*q^48 + 2*q^49 + 2*q^50 + 2*q^51 - 2*q^52 - 2*q^53 - 7*q^55 - 6*q^57 - 3*q^59 + 4*q^60 - 3*q^63 - 6*q^64 + 8*q^65 - 2*q^66 - 5*q^67 + 2*q^70 + 4*q^71 + q^72 - 4*q^73 + 6*q^76 - 4*q^77 - q^79 + 4*q^80 - 2*q^81 + 5*q^82 + 4*q^84 + 4*q^85 - 6*q^86 + 2*q^87 - 2*q^88 - 2*q^89 - 5*q^90 + q^91 - q^92 + 2*q^93 - 6*q^94 + 2*q^95 + 4*q^96 + 8*q^97 - 6*q^98 + 7*q^99 - 2*q^100 + 2*q^101 - 2*q^102 + 5*q^104 + 4*q^105 - 4*q^107 + 4*q^109 - 6*q^111 - q^112 - 8*q^113 - 6*q^114 - 5*q^115 - 4*q^117 + 10*q^118 + 8*q^121 + q^122 - 8*q^123 - 6*q^124 + 4*q^125 - 2*q^126 + 4*q^127 + q^128 + 2*q^129 - 9*q^130 - 4*q^131 - 10*q^132 + 4*q^133 + 4*q^134 + 4*q^136 - 8*q^137 + 2*q^138 + q^139 - 3*q^140 + 8*q^141 + 4*q^142 + 3*q^143 + 3*q^146 + 6*q^147 + 6*q^148 + 2*q^149 + 4*q^150 + 11*q^151 + 4*q^152 - 2*q^153 - q^154 - 6*q^155 + 2*q^156 - 12*q^157 + 2*q^158 - q^160 - 2*q^161 - q^162 + 4*q^163 + 4*q^164 - 6*q^165 + 2*q^167 + 2*q^168 - 12*q^169 - 8*q^170 - 8*q^171 + 4*q^173 - 2*q^174 - 2*q^175 + 5*q^176 + 2*q^177 + 2*q^178 - 4*q^179 + 6*q^180 - 8*q^181 - 2*q^182 - 4*q^184 + 6*q^185 + 10*q^186 + 6*q^187 - 2*q^188 - 4*q^189 - 4*q^190 + 5*q^191 - 2*q^192 + 4*q^193 - 2*q^194 + 2*q^195 - 4*q^196 - 4*q^197 - 2*q^198 + O(q^200),
q^3 + q^5 - 1/2*q^6 - 1/2*q^7 - 1/2*q^8 - 3/2*q^10 + 2*q^11 - q^12 - q^13 - 2*q^15 - 3/2*q^17 + q^18 - 1/2*q^21 - 2*q^22 + 3*q^23 + 3/2*q^24 + 2*q^26 - q^27 + q^28 + 3/2*q^29 + 1/2*q^30 + 3/2*q^31 + 2*q^32 - 1/2*q^33 + 2*q^34 + 2*q^35 - q^36 - 7/2*q^37 - 3/2*q^38 + 4*q^40 - q^41 - q^42 + 1/2*q^43 - 5/2*q^44 - 2*q^45 - 3*q^46 - 2*q^47 + q^49 + 2*q^51 - q^53 + 2*q^54 - 7/2*q^55 - 4*q^57 - q^58 + 3/2*q^59 + 3*q^60 + 1/2*q^61 - q^62 - 5/2*q^63 - q^64 - 3*q^65 + 3*q^66 + 9/2*q^67 + 1/2*q^68 - 7/2*q^69 - q^70 + 3/2*q^71 - 5/2*q^72 + 3*q^73 + 2*q^74 + 3*q^75 + 1/2*q^78 + 2*q^79 - 4*q^80 + 4*q^81 - q^82 - 3*q^83 + 3/2*q^84 + 1/2*q^85 - 2*q^87 + 4*q^88 + 1/2*q^90 - 3/2*q^91 - 5/2*q^92 + q^93 + 4*q^94 - q^95 - 3/2*q^96 + 3*q^97 + 1/2*q^98 + 3*q^99 + q^100 - 1/2*q^101 - q^102 - q^103 - 7/2*q^104 + 3/2*q^105 + q^106 + 2*q^107 - q^108 + 4*q^109 + 2*q^110 + q^111 - 3/2*q^112 + q^113 + 2*q^114 - 11/2*q^115 - 5/2*q^116 + 3*q^117 - q^118 - 2*q^119 - 5/2*q^120 - 4*q^121 - 1/2*q^122 + q^123 + 1/2*q^124 - 4*q^127 - 7/2*q^128 - 6*q^129 + 7/2*q^130 + 5*q^131 + 1/2*q^132 + 7/2*q^133 - 4*q^134 - q^135 - 5*q^136 + q^137 + 4*q^138 + 5/2*q^139 - 5/2*q^140 + 3*q^141 - 2*q^142 + 4*q^144 - 13/2*q^145 - 9/2*q^146 - 3*q^147 + 7/2*q^148 - 4*q^149 - 3*q^150 - 9/2*q^151 + 7/2*q^152 + 3/2*q^153 - 2*q^154 + 5/2*q^155 - 2*q^156 + 5/2*q^157 + 3*q^159 - 9/2*q^160 + 2*q^161 - 3*q^162 - 2*q^163 + 3*q^164 + 9/2*q^165 + q^166 - 2*q^167 + 3*q^168 + 6*q^169 - 3*q^171 - 3/2*q^172 - 4*q^173 + 4*q^174 - q^175 - q^176 + 21/2*q^177 - 2*q^178 + 10*q^179 + 2*q^180 - q^181 + q^182 + 8*q^184 + 15/2*q^185 - 2*q^186 + 4*q^187 - q^188 - 4*q^189 + 9/2*q^190 + 5/2*q^191 + 2*q^192 - q^193 - q^194 - 5*q^195 - 2*q^196 + q^198 - 5*q^199 + O(q^200),
q^4 + 2*q^5 - 3/2*q^6 + 1/2*q^7 + 1/2*q^8 - 2*q^9 - 5/2*q^10 + 2*q^11 - 3*q^13 + q^15 - 2*q^16 - 5/2*q^17 + q^18 + 3*q^19 - 2*q^20 + 5/2*q^21 - q^22 + 3*q^23 + 5/2*q^24 + 2*q^26 - 3*q^27 - q^28 + 1/2*q^29 + 3/2*q^30 + 1/2*q^31 - 11/2*q^33 + 3*q^34 + 2*q^36 - 1/2*q^37 + 3/2*q^38 - q^39 + 4*q^40 + 3*q^41 - 1/2*q^43 + 5/2*q^44 + q^45 - 4*q^46 - 6*q^47 - 3*q^48 - 2*q^49 - 2*q^50 - q^52 - q^53 + 4*q^54 - 1/2*q^55 - q^56 + 1/2*q^59 + 1/2*q^61 - 6*q^62 + 5/2*q^63 - 4*q^65 - 2*q^66 + 7/2*q^67 + 3/2*q^68 - 9/2*q^69 - 2*q^70 + 5/2*q^71 - 3/2*q^72 + 6*q^73 + 5*q^74 + 3*q^75 - q^76 + 4*q^77 + 7/2*q^78 - 2*q^79 - 3*q^80 + 2*q^81 - q^82 + q^83 - 7/2*q^84 - 1/2*q^85 + q^86 - 6*q^87 + 8*q^88 + 4*q^89 + 7/2*q^90 - 1/2*q^91 - 3/2*q^92 + 3*q^93 + 6*q^94 - 8*q^95 - 5/2*q^96 - q^97 - 1/2*q^98 - 5*q^99 + q^100 - 7/2*q^101 - q^102 + 2*q^103 - 13/2*q^104 - 11/2*q^105 - q^106 + 8*q^107 + 5*q^108 + 4*q^109 - 2*q^110 + 7*q^111 + 3/2*q^112 + 3*q^113 - 5/2*q^115 + 1/2*q^116 + 3*q^117 - 5*q^118 - q^119 - 7/2*q^120 - 3*q^121 - 1/2*q^122 + 8*q^123 - 5/2*q^124 - q^125 + 2*q^126 - 5*q^127 - 9/2*q^128 - 4*q^129 + 21/2*q^130 + 8*q^131 + 7/2*q^132 - 7/2*q^133 - 10*q^134 + q^135 - 6*q^136 + 3*q^138 + 3/2*q^139 + 5/2*q^140 - 5*q^141 + 3*q^142 - 8*q^143 - 11/2*q^145 - 11/2*q^146 - 5*q^147 + 1/2*q^148 - 6*q^149 - 3*q^150 - 19/2*q^151 + 5/2*q^152 + 1/2*q^153 - 2*q^154 + 19/2*q^155 + 3*q^156 + 15/2*q^157 - 5*q^158 + 5*q^159 + 1/2*q^160 + 4*q^161 - 7*q^162 - 11*q^163 + 35/2*q^165 + 5*q^166 + 3*q^167 + 14*q^169 + 3*q^170 + 8*q^171 - 9/2*q^172 - 6*q^173 + 2*q^174 + q^175 - 9*q^176 + 3/2*q^177 + 8*q^179 - 6*q^180 - q^181 + 3*q^182 - q^183 + 10*q^184 - 3/2*q^185 + 2*q^186 + q^187 + 3*q^188 + 2*q^189 - 1/2*q^190 - 9/2*q^191 + 11*q^192 - q^193 + 3*q^194 - 6*q^195 - 3*q^196 - q^197 - 7*q^199 + O(q^200)
];


Qx<[x]>:=PolynomialRing(Rationals(),4);

mons:=MonomialsOfDegree(Qx,2);

V:=VectorSpace(Rationals(),#mons);

monImages:=[Evaluate(mon,basis2) : mon in mons];

W:=VectorSpace(Rationals(),40);

monImages:=[W![Coefficient(m,i) : i in [0..39] ] : m in monImages];

h:=hom<V->W | monImages>;

K:=Kernel(h);
v := Eltseq(V ! K.1) ;
q1 :=  &+[ v[i]*mons[i] : i in [1..#mons] ] ;
mons:=MonomialsOfDegree(Qx,3);
V:=VectorSpace(Rationals(),#mons);
monImages:=[Evaluate(mon,basis2) : mon in mons];

W:=VectorSpace(Rationals(),40);
monImages:=[W![Coefficient(m,i) : i in [0..39] ] : m in monImages]; 
h:=hom<V->W | monImages>;

K:=Kernel(h);

v:=Eltseq(V!(K.2));

c:=&+[v[i]*mons[i] : i in [1..#mons]];
 
CC := Curve(ProjectiveSpace(Qx), [ q1, c] ) ;


// the map  XH(61) --> X0(61) //
Qx<[x]>:=PolynomialRing(Rationals(),8);
eqns := [
    x[1]*x[3] - x[2]^2 - 101*x[4]*x[6] + 427/4*x[4]*x[7] - 329/4*x[4]*x[8] + 102*x[5]^2 - 459/4*x[5]*x[6] + 117/4*x[5]*x[7] + 
        219/4*x[5]*x[8] + 60*x[6]^2 - 237/4*x[6]*x[7] - 539/4*x[6]*x[8] + 15*x[7]^2 + 469/4*x[7]*x[8] - 475/4*x[8]^2,
    x[1]*x[4] - x[2]*x[3] + 4*x[4]*x[6] - 9/4*x[4]*x[7] + 23/4*x[4]*x[8] - x[5]^2 + 9/4*x[5]*x[6] - 15/4*x[5]*x[7] - 5/4*x[5]*x[8] - 
        7*x[6]^2 + 39/4*x[6]*x[7] - 15/4*x[6]*x[8] - 2*x[7]^2 + 5/4*x[7]*x[8] + 1/4*x[8]^2,
    x[1]*x[5] - x[3]^2 - 79*x[4]*x[6] + 351/4*x[4]*x[7] - 249/4*x[4]*x[8] + 81*x[5]^2 - 347/4*x[5]*x[6] + 73/4*x[5]*x[7] + 
        199/4*x[5]*x[8] + 42*x[6]^2 - 169/4*x[6]*x[7] - 455/4*x[6]*x[8] + 11*x[7]^2 + 393/4*x[7]*x[8] - 379/4*x[8]^2,
    x[1]*x[6] - x[3]*x[4] + 8*x[4]*x[6] - 11/2*x[4]*x[7] + 17/2*x[4]*x[8] - 4*x[5]^2 + 11/2*x[5]*x[6] - 5/2*x[5]*x[7] - 9/2*x[5]*x[8] -
        7*x[6]^2 + 13/2*x[6]*x[7] + 13/2*x[6]*x[8] - x[7]^2 - 9/2*x[7]*x[8] + 15/2*x[8]^2,
    x[1]*x[7] - x[4]^2 - 54*x[4]*x[6] + 237/4*x[4]*x[7] - 175/4*x[4]*x[8] + 57*x[5]^2 - 241/4*x[5]*x[6] + 59/4*x[5]*x[7] + 
        129/4*x[5]*x[8] + 32*x[6]^2 - 139/4*x[6]*x[7] - 293/4*x[6]*x[8] + 9*x[7]^2 + 267/4*x[7]*x[8] - 253/4*x[8]^2,
    x[1]*x[8] - x[4]*x[5] + 17*x[4]*x[6] - 39/2*x[4]*x[7] + 25/2*x[4]*x[8] - 18*x[5]^2 + 39/2*x[5]*x[6] - 7/2*x[5]*x[7] - 
        23/2*x[5]*x[8] - 8*x[6]^2 + 15/2*x[6]*x[7] + 53/2*x[6]*x[8] - 2*x[7]^2 - 45/2*x[7]*x[8] + 45/2*x[8]^2,
    x[2]*x[4] - x[3]^2 - 58*x[4]*x[6] + 249/4*x[4]*x[7] - 183/4*x[4]*x[8] + 58*x[5]^2 - 237/4*x[5]*x[6] + 51/4*x[5]*x[7] + 
        149/4*x[5]*x[8] + 32*x[6]^2 - 135/4*x[6]*x[7] - 309/4*x[6]*x[8] + 9*x[7]^2 + 271/4*x[7]*x[8] - 261/4*x[8]^2,
    x[2]*x[5] - x[3]*x[4] + 45*x[4]*x[6] - 189/4*x[4]*x[7] + 147/4*x[4]*x[8] - 45*x[5]^2 + 197/4*x[5]*x[6] - 51/4*x[5]*x[7] - 
        105/4*x[5]*x[8] - 26*x[6]^2 + 103/4*x[6]*x[7] + 245/4*x[6]*x[8] - 6*x[7]^2 - 211/4*x[7]*x[8] + 213/4*x[8]^2,
    x[2]*x[6] - x[4]^2 - 22*x[4]*x[6] + 49/2*x[4]*x[7] - 37/2*x[4]*x[8] + 22*x[5]^2 - 41/2*x[5]*x[6] + 11/2*x[5]*x[7] + 33/2*x[5]*x[8] 
        + 13*x[6]^2 - 33/2*x[6]*x[7] - 55/2*x[6]*x[8] + 4*x[7]^2 + 53/2*x[7]*x[8] - 47/2*x[8]^2,
    x[2]*x[7] - x[4]*x[5] + 16*x[4]*x[6] - 17*x[4]*x[7] + 13*x[4]*x[8] - 16*x[5]^2 + 20*x[5]*x[6] - 5*x[5]*x[7] - 7*x[5]*x[8] - 
        9*x[6]^2 + 8*x[6]*x[7] + 22*x[6]*x[8] - 3*x[7]^2 - 18*x[7]*x[8] + 20*x[8]^2,
    x[2]*x[8] - 4*x[4]*x[6] + 15/4*x[4]*x[7] - 13/4*x[4]*x[8] + 3*x[5]^2 - 19/4*x[5]*x[6] + 5/4*x[5]*x[7] + 3/4*x[5]*x[8] + 2*x[6]^2 - 
        5/4*x[6]*x[7] - 27/4*x[6]*x[8] + 17/4*x[7]*x[8] - 23/4*x[8]^2,
    x[3]*x[5] - x[4]^2 - 45*x[4]*x[6] + 97/2*x[4]*x[7] - 75/2*x[4]*x[8] + 45*x[5]^2 - 97/2*x[5]*x[6] + 25/2*x[5]*x[7] + 53/2*x[5]*x[8] 
        + 27*x[6]^2 - 57/2*x[6]*x[7] - 119/2*x[6]*x[8] + 7*x[7]^2 + 107/2*x[7]*x[8] - 105/2*x[8]^2,
    x[3]*x[6] - x[4]*x[5] - 22*x[4]*x[6] + 23*x[4]*x[7] - 18*x[4]*x[8] + 22*x[5]^2 - 23*x[5]*x[6] + 7*x[5]*x[7] + 14*x[5]*x[8] + 
        15*x[6]^2 - 17*x[6]*x[7] - 26*x[6]*x[8] + 4*x[7]^2 + 24*x[7]*x[8] - 23*x[8]^2,
    x[3]*x[7] + 8*x[4]*x[6] - 39/4*x[4]*x[7] + 25/4*x[4]*x[8] - 9*x[5]^2 + 39/4*x[5]*x[6] - 5/4*x[5]*x[7] - 19/4*x[5]*x[8] - 2*x[6]^2 +
        1/4*x[6]*x[7] + 67/4*x[6]*x[8] - 57/4*x[7]*x[8] + 51/4*x[8]^2,
    x[3]*x[8] + 40*x[4]*x[6] - 43*x[4]*x[7] + 32*x[4]*x[8] - 40*x[5]^2 + 42*x[5]*x[6] - 10*x[5]*x[7] - 25*x[5]*x[8] - 23*x[6]^2 + 
        24*x[6]*x[7] + 53*x[6]*x[8] - 6*x[7]^2 - 47*x[7]*x[8] + 45*x[8]^2,
    x[1]^2*x[3] - x[1]*x[2]^2 + x[3]*x[4]^2 - 8*x[4]^3 + 7*x[4]^2*x[5] - 3*x[4]*x[5]^2 + 113905/4064*x[4]*x[7]^2 + 
        934015/16256*x[4]*x[7]*x[8] + 378671/16256*x[4]*x[8]^2 + 5*x[5]^3 + 8*x[5]^2*x[6] + 17729/508*x[5]^2*x[7] + 
        452673/4064*x[5]^2*x[8] - 44145/508*x[5]*x[6]^2 - 88769/2032*x[5]*x[6]*x[7] - 3915151/16256*x[5]*x[6]*x[8] + 
        62489/2032*x[5]*x[7]^2 + 353577/16256*x[5]*x[7]*x[8] - 1485701/16256*x[5]*x[8]^2 - 23045/254*x[6]^3 + 942633/4064*x[6]^2*x[7] -
        723667/2032*x[6]^2*x[8] - 140725/1016*x[6]*x[7]^2 + 5914379/16256*x[6]*x[7]*x[8] - 7757003/16256*x[6]*x[8]^2 + 
        36821/2032*x[7]^3 - 116875/4064*x[7]^2*x[8] + 3280949/16256*x[7]*x[8]^2 - 3204251/16256*x[8]^3
] ;


print "A model of the curve is:";
eqns;
C := Curve(ProjectiveSpace(Qx), eqns) ;

K:=FunctionField(C);


g1 := K.1 - K.5 -1/2*K.6 -1/2*K.7 + 3/2;
g2 := K.2 -2*K.5 - K.7 - 1 ;
g3 := K.3 + K.5 -1/2*K.6 -1/2*K.7 -1/2;
g4 := K.4 +2*K.5 -3/2*K.6 +1/2*K.7 + 1/2 ;


Qy<[y]>:=PolynomialRing(Rationals(),4);
eqns2 :=  [ y[1]*y[3] + y[1]*y[4] - y[2]^2 - y[2]*y[3] - y[2]*y[4] - 2*y[3]^2 + y[3]*y[4] - y[4]^2,
y[1]^2*y[4] + y[1]*y[2]*y[4] - 2*y[1]*y[4]^2 - y[2]^3 - 3*y[2]^2*y[3] + 2*y[2]^2*y[4] - 5/2*y[2]*y[3]^2 - y[2]*y[3]*y[4] + 
    5/2*y[2]*y[4]^2 - 3*y[3]^3 + 5*y[3]^2*y[4] - 3*y[3]*y[4]^2 + 2*y[4]^3 ] ;


D := Curve(ProjectiveSpace(Qy), eqns2 ) ;

m:=map<C->D | [g1, g2, g3, g4] >;
p1 := Pullback(m , Place(D ! [ 1,0,1,1]) ) ;
p2 :=  Pullback(m , Place(D ! [ 1,0,0,0]) ) ;

// the  cuspidal subgroup of XH(61) //

K := QuadraticField(61) ;
OK := Integers(K);
P := PrimesUpTo(50,K) ;
P := P[4] ;

F7, pi := ResidueClassField(P) ;
Zx<[x]> := PolynomialRing(F7,8) ;
eqns := [
    4*x[1]*x[3] - 4*x[2]^2 - 404*x[4]*x[6] + 427*x[4]*x[7] - 329*x[4]*x[8] + 
        408*x[5]^2 - 459*x[5]*x[6] + 117*x[5]*x[7] + 219*x[5]*x[8] + 240*x[6]^2 
        - 237*x[6]*x[7] - 539*x[6]*x[8] + 60*x[7]^2 + 469*x[7]*x[8] - 
        475*x[8]^2,
    4*x[1]*x[4] - 4*x[2]*x[3] + 16*x[4]*x[6] - 9*x[4]*x[7] + 23*x[4]*x[8] - 
        4*x[5]^2 + 9*x[5]*x[6] - 15*x[5]*x[7] - 5*x[5]*x[8] - 28*x[6]^2 + 
        39*x[6]*x[7] - 15*x[6]*x[8] - 8*x[7]^2 + 5*x[7]*x[8] + x[8]^2,
    4*x[1]*x[5] - 4*x[3]^2 - 316*x[4]*x[6] + 351*x[4]*x[7] - 249*x[4]*x[8] + 
        324*x[5]^2 - 347*x[5]*x[6] + 73*x[5]*x[7] + 199*x[5]*x[8] + 168*x[6]^2 -
        169*x[6]*x[7] - 455*x[6]*x[8] + 44*x[7]^2 + 393*x[7]*x[8] - 379*x[8]^2,
    2*x[1]*x[6] - 2*x[3]*x[4] + 16*x[4]*x[6] - 11*x[4]*x[7] + 17*x[4]*x[8] - 
        8*x[5]^2 + 11*x[5]*x[6] - 5*x[5]*x[7] - 9*x[5]*x[8] - 14*x[6]^2 + 
        13*x[6]*x[7] + 13*x[6]*x[8] - 2*x[7]^2 - 9*x[7]*x[8] + 15*x[8]^2,
    4*x[1]*x[7] - 4*x[4]^2 - 216*x[4]*x[6] + 237*x[4]*x[7] - 175*x[4]*x[8] + 
        228*x[5]^2 - 241*x[5]*x[6] + 59*x[5]*x[7] + 129*x[5]*x[8] + 128*x[6]^2 -
        139*x[6]*x[7] - 293*x[6]*x[8] + 36*x[7]^2 + 267*x[7]*x[8] - 253*x[8]^2,
    2*x[1]*x[8] - 2*x[4]*x[5] + 34*x[4]*x[6] - 39*x[4]*x[7] + 25*x[4]*x[8] - 
        36*x[5]^2 + 39*x[5]*x[6] - 7*x[5]*x[7] - 23*x[5]*x[8] - 16*x[6]^2 + 
        15*x[6]*x[7] + 53*x[6]*x[8] - 4*x[7]^2 - 45*x[7]*x[8] + 45*x[8]^2,
    4*x[2]*x[4] - 4*x[3]^2 - 232*x[4]*x[6] + 249*x[4]*x[7] - 183*x[4]*x[8] + 
        232*x[5]^2 - 237*x[5]*x[6] + 51*x[5]*x[7] + 149*x[5]*x[8] + 128*x[6]^2 -
        135*x[6]*x[7] - 309*x[6]*x[8] + 36*x[7]^2 + 271*x[7]*x[8] - 261*x[8]^2,
    4*x[2]*x[5] - 4*x[3]*x[4] + 180*x[4]*x[6] - 189*x[4]*x[7] + 147*x[4]*x[8] - 
        180*x[5]^2 + 197*x[5]*x[6] - 51*x[5]*x[7] - 105*x[5]*x[8] - 104*x[6]^2 +
        103*x[6]*x[7] + 245*x[6]*x[8] - 24*x[7]^2 - 211*x[7]*x[8] + 213*x[8]^2,
    2*x[2]*x[6] - 2*x[4]^2 - 44*x[4]*x[6] + 49*x[4]*x[7] - 37*x[4]*x[8] + 
        44*x[5]^2 - 41*x[5]*x[6] + 11*x[5]*x[7] + 33*x[5]*x[8] + 26*x[6]^2 - 
        33*x[6]*x[7] - 55*x[6]*x[8] + 8*x[7]^2 + 53*x[7]*x[8] - 47*x[8]^2,
    x[2]*x[7] - x[4]*x[5] + 16*x[4]*x[6] - 17*x[4]*x[7] + 13*x[4]*x[8] - 
        16*x[5]^2 + 20*x[5]*x[6] - 5*x[5]*x[7] - 7*x[5]*x[8] - 9*x[6]^2 + 
        8*x[6]*x[7] + 22*x[6]*x[8] - 3*x[7]^2 - 18*x[7]*x[8] + 20*x[8]^2,
    4*x[2]*x[8] - 16*x[4]*x[6] + 15*x[4]*x[7] - 13*x[4]*x[8] + 12*x[5]^2 - 
        19*x[5]*x[6] + 5*x[5]*x[7] + 3*x[5]*x[8] + 8*x[6]^2 - 5*x[6]*x[7] - 
        27*x[6]*x[8] + 17*x[7]*x[8] - 23*x[8]^2,
    2*x[3]*x[5] - 2*x[4]^2 - 90*x[4]*x[6] + 97*x[4]*x[7] - 75*x[4]*x[8] + 
        90*x[5]^2 - 97*x[5]*x[6] + 25*x[5]*x[7] + 53*x[5]*x[8] + 54*x[6]^2 - 
        57*x[6]*x[7] - 119*x[6]*x[8] + 14*x[7]^2 + 107*x[7]*x[8] - 105*x[8]^2,
    x[3]*x[6] - x[4]*x[5] - 22*x[4]*x[6] + 23*x[4]*x[7] - 18*x[4]*x[8] + 
        22*x[5]^2 - 23*x[5]*x[6] + 7*x[5]*x[7] + 14*x[5]*x[8] + 15*x[6]^2 - 
        17*x[6]*x[7] - 26*x[6]*x[8] + 4*x[7]^2 + 24*x[7]*x[8] - 23*x[8]^2,
    4*x[3]*x[7] + 32*x[4]*x[6] - 39*x[4]*x[7] + 25*x[4]*x[8] - 36*x[5]^2 + 
        39*x[5]*x[6] - 5*x[5]*x[7] - 19*x[5]*x[8] - 8*x[6]^2 + x[6]*x[7] + 
        67*x[6]*x[8] - 57*x[7]*x[8] + 51*x[8]^2,
    x[3]*x[8] + 40*x[4]*x[6] - 43*x[4]*x[7] + 32*x[4]*x[8] - 40*x[5]^2 + 
        42*x[5]*x[6] - 10*x[5]*x[7] - 25*x[5]*x[8] - 23*x[6]^2 + 24*x[6]*x[7] + 
        53*x[6]*x[8] - 6*x[7]^2 - 47*x[7]*x[8] + 45*x[8]^2,
    16256*x[1]^2*x[3] - 16256*x[1]*x[2]^2 + 16256*x[3]*x[4]^2 - 130048*x[4]^3 + 
        113792*x[4]^2*x[5] - 48768*x[4]*x[5]^2 + 455620*x[4]*x[7]^2 + 
        934015*x[4]*x[7]*x[8] + 378671*x[4]*x[8]^2 + 81280*x[5]^3 + 
        130048*x[5]^2*x[6] + 567328*x[5]^2*x[7] + 1810692*x[5]^2*x[8] - 
        1412640*x[5]*x[6]^2 - 710152*x[5]*x[6]*x[7] - 3915151*x[5]*x[6]*x[8] + 
        499912*x[5]*x[7]^2 + 353577*x[5]*x[7]*x[8] - 1485701*x[5]*x[8]^2 - 
        1474880*x[6]^3 + 3770532*x[6]^2*x[7] - 5789336*x[6]^2*x[8] - 
        2251600*x[6]*x[7]^2 + 5914379*x[6]*x[7]*x[8] - 7757003*x[6]*x[8]^2 + 
        294568*x[7]^3 - 467500*x[7]^2*x[8] + 3280949*x[7]*x[8]^2 - 
        3204251*x[8]^3
];

X := Curve(ProjectiveSpace(Zx), eqns) ;

XF7 := X ;


Cl, phi, psi := ClassGroup(XF7) ;
Z := FreeAbelianGroup(1) ;
degr := hom<Cl -> Z | [ Degree(phi(g)) : g in OrderedGenerators(Cl)]>;
J := Kernel(degr) ;


d1 := [  [ 1 , 0 , 0 , 0 , 0 , 0 , 0 , 0], [-3 , -2 , 0 , -1 , -1 , -2 , -1 , 1] ] ;
r1 := 7/2 + (1/2)*K.1;
r2 := 7/2 - (1/2)*K.1;

d2 := [ [3 , r1 + 3 , 3 , 1 , 1 , r1 , r1 , 1], [3 , r2 + 3 , 3 , 1 , 1 , r2 , r2 , 1]];
d := d1 cat d2 ;

print "The cusps on this model of the curve are:";
d[1] ;
d[2] ;
d[3] ;
d[4] ;

dpi := [ [ pi(a) : a in b ] : b in d ] ;
Cdpi := [ XF7  ! a : a in dpi ] ;




C1 := d1[1] ;
C2 := d1[2] ;
C3 := d2[1] ;
C4 := d2[2] ;

C1 := XF7 ! pi(C1) ;
C2 := XF7 ! pi(C2) ;
C3 := XF7 ! pi(C3) ;
C4 := XF7 ! pi(C4) ;

C1 := Place(C1) ;
C2 := Place(C2) ;
C3 := Place(C3) ;
C4 := Place(C4) ;

CC := [ Place(b) : b in Cdpi ] ;
divs := [ CC[2] - CC[1], CC[3] - CC[1], CC[4] - CC[1] ] ;
H := [ psi(a) :a  in divs] ;
ZN := FreeAbelianGroup(#H) ;
hh := hom< ZN -> J | [  a : a in H ] >;

CH := Image(hh) ;

print "The cuspidal group of XH(61) is:";
CH;

// rational part //

cpt := [ ZN.1, ZN.3, ZN.2] ;

conj := hom< ZN -> ZN | cpt>; 
mu := hom< ZN -> J | [ hh(ZN.i) - hh(conj(ZN.i)) : i in [1..3]]>; 
ker1 := Kernel(mu);
imKer1 := sub<J | [hh(k) : k in Generators(ker1)]>;
CHQ := imKer1;

print "The rational cuspidal subgroup of XH(61) is:";
CHQ;
// a bound for Q(\sqrt(61) torsion 


K := QuadraticField(61) ;
OK := Integers(K);
Zx<[x]> := PolynomialRing(K,8) ;
eqns := [
    4*x[1]*x[3] - 4*x[2]^2 - 404*x[4]*x[6] + 427*x[4]*x[7] - 329*x[4]*x[8] + 
        408*x[5]^2 - 459*x[5]*x[6] + 117*x[5]*x[7] + 219*x[5]*x[8] + 240*x[6]^2 
        - 237*x[6]*x[7] - 539*x[6]*x[8] + 60*x[7]^2 + 469*x[7]*x[8] - 
        475*x[8]^2,
    4*x[1]*x[4] - 4*x[2]*x[3] + 16*x[4]*x[6] - 9*x[4]*x[7] + 23*x[4]*x[8] - 
        4*x[5]^2 + 9*x[5]*x[6] - 15*x[5]*x[7] - 5*x[5]*x[8] - 28*x[6]^2 + 
        39*x[6]*x[7] - 15*x[6]*x[8] - 8*x[7]^2 + 5*x[7]*x[8] + x[8]^2,
    4*x[1]*x[5] - 4*x[3]^2 - 316*x[4]*x[6] + 351*x[4]*x[7] - 249*x[4]*x[8] + 
        324*x[5]^2 - 347*x[5]*x[6] + 73*x[5]*x[7] + 199*x[5]*x[8] + 168*x[6]^2 -
        169*x[6]*x[7] - 455*x[6]*x[8] + 44*x[7]^2 + 393*x[7]*x[8] - 379*x[8]^2,
    2*x[1]*x[6] - 2*x[3]*x[4] + 16*x[4]*x[6] - 11*x[4]*x[7] + 17*x[4]*x[8] - 
        8*x[5]^2 + 11*x[5]*x[6] - 5*x[5]*x[7] - 9*x[5]*x[8] - 14*x[6]^2 + 
        13*x[6]*x[7] + 13*x[6]*x[8] - 2*x[7]^2 - 9*x[7]*x[8] + 15*x[8]^2,
    4*x[1]*x[7] - 4*x[4]^2 - 216*x[4]*x[6] + 237*x[4]*x[7] - 175*x[4]*x[8] + 
        228*x[5]^2 - 241*x[5]*x[6] + 59*x[5]*x[7] + 129*x[5]*x[8] + 128*x[6]^2 -
        139*x[6]*x[7] - 293*x[6]*x[8] + 36*x[7]^2 + 267*x[7]*x[8] - 253*x[8]^2,
    2*x[1]*x[8] - 2*x[4]*x[5] + 34*x[4]*x[6] - 39*x[4]*x[7] + 25*x[4]*x[8] - 
        36*x[5]^2 + 39*x[5]*x[6] - 7*x[5]*x[7] - 23*x[5]*x[8] - 16*x[6]^2 + 
        15*x[6]*x[7] + 53*x[6]*x[8] - 4*x[7]^2 - 45*x[7]*x[8] + 45*x[8]^2,
    4*x[2]*x[4] - 4*x[3]^2 - 232*x[4]*x[6] + 249*x[4]*x[7] - 183*x[4]*x[8] + 
        232*x[5]^2 - 237*x[5]*x[6] + 51*x[5]*x[7] + 149*x[5]*x[8] + 128*x[6]^2 -
        135*x[6]*x[7] - 309*x[6]*x[8] + 36*x[7]^2 + 271*x[7]*x[8] - 261*x[8]^2,
    4*x[2]*x[5] - 4*x[3]*x[4] + 180*x[4]*x[6] - 189*x[4]*x[7] + 147*x[4]*x[8] - 
        180*x[5]^2 + 197*x[5]*x[6] - 51*x[5]*x[7] - 105*x[5]*x[8] - 104*x[6]^2 +
        103*x[6]*x[7] + 245*x[6]*x[8] - 24*x[7]^2 - 211*x[7]*x[8] + 213*x[8]^2,
    2*x[2]*x[6] - 2*x[4]^2 - 44*x[4]*x[6] + 49*x[4]*x[7] - 37*x[4]*x[8] + 
        44*x[5]^2 - 41*x[5]*x[6] + 11*x[5]*x[7] + 33*x[5]*x[8] + 26*x[6]^2 - 
        33*x[6]*x[7] - 55*x[6]*x[8] + 8*x[7]^2 + 53*x[7]*x[8] - 47*x[8]^2,
    x[2]*x[7] - x[4]*x[5] + 16*x[4]*x[6] - 17*x[4]*x[7] + 13*x[4]*x[8] - 
        16*x[5]^2 + 20*x[5]*x[6] - 5*x[5]*x[7] - 7*x[5]*x[8] - 9*x[6]^2 + 
        8*x[6]*x[7] + 22*x[6]*x[8] - 3*x[7]^2 - 18*x[7]*x[8] + 20*x[8]^2,
    4*x[2]*x[8] - 16*x[4]*x[6] + 15*x[4]*x[7] - 13*x[4]*x[8] + 12*x[5]^2 - 
        19*x[5]*x[6] + 5*x[5]*x[7] + 3*x[5]*x[8] + 8*x[6]^2 - 5*x[6]*x[7] - 
        27*x[6]*x[8] + 17*x[7]*x[8] - 23*x[8]^2,
    2*x[3]*x[5] - 2*x[4]^2 - 90*x[4]*x[6] + 97*x[4]*x[7] - 75*x[4]*x[8] + 
        90*x[5]^2 - 97*x[5]*x[6] + 25*x[5]*x[7] + 53*x[5]*x[8] + 54*x[6]^2 - 
        57*x[6]*x[7] - 119*x[6]*x[8] + 14*x[7]^2 + 107*x[7]*x[8] - 105*x[8]^2,
    x[3]*x[6] - x[4]*x[5] - 22*x[4]*x[6] + 23*x[4]*x[7] - 18*x[4]*x[8] + 
        22*x[5]^2 - 23*x[5]*x[6] + 7*x[5]*x[7] + 14*x[5]*x[8] + 15*x[6]^2 - 
        17*x[6]*x[7] - 26*x[6]*x[8] + 4*x[7]^2 + 24*x[7]*x[8] - 23*x[8]^2,
    4*x[3]*x[7] + 32*x[4]*x[6] - 39*x[4]*x[7] + 25*x[4]*x[8] - 36*x[5]^2 + 
        39*x[5]*x[6] - 5*x[5]*x[7] - 19*x[5]*x[8] - 8*x[6]^2 + x[6]*x[7] + 
        67*x[6]*x[8] - 57*x[7]*x[8] + 51*x[8]^2,
    x[3]*x[8] + 40*x[4]*x[6] - 43*x[4]*x[7] + 32*x[4]*x[8] - 40*x[5]^2 + 
        42*x[5]*x[6] - 10*x[5]*x[7] - 25*x[5]*x[8] - 23*x[6]^2 + 24*x[6]*x[7] + 
        53*x[6]*x[8] - 6*x[7]^2 - 47*x[7]*x[8] + 45*x[8]^2,
    16256*x[1]^2*x[3] - 16256*x[1]*x[2]^2 + 16256*x[3]*x[4]^2 - 130048*x[4]^3 + 
        113792*x[4]^2*x[5] - 48768*x[4]*x[5]^2 + 455620*x[4]*x[7]^2 + 
        934015*x[4]*x[7]*x[8] + 378671*x[4]*x[8]^2 + 81280*x[5]^3 + 
        130048*x[5]^2*x[6] + 567328*x[5]^2*x[7] + 1810692*x[5]^2*x[8] - 
        1412640*x[5]*x[6]^2 - 710152*x[5]*x[6]*x[7] - 3915151*x[5]*x[6]*x[8] + 
        499912*x[5]*x[7]^2 + 353577*x[5]*x[7]*x[8] - 1485701*x[5]*x[8]^2 - 
        1474880*x[6]^3 + 3770532*x[6]^2*x[7] - 5789336*x[6]^2*x[8] - 
        2251600*x[6]*x[7]^2 + 5914379*x[6]*x[7]*x[8] - 7757003*x[6]*x[8]^2 + 
        294568*x[7]^3 - 467500*x[7]^2*x[8] + 3280949*x[7]*x[8]^2 - 
        3204251*x[8]^3
];

X := Curve(ProjectiveSpace(Zx), eqns) ;
PP := PrimesUpTo(50,K) ;

bound := [] ;
P := [ p : p in PP | Norm(p) eq 5][1] ;
p := P;
R,pi := ResidueClassField(p) ;
C5 := ChangeRing(X, R) ;
CL5, phi5, pi5 := ClassGroup(C5) ;
Z := FreeAbelianGroup(1) ;
degr := hom<CL5 -> Z | [ Degree(phi5(g)) : g in OrderedGenerators(CL5)]>; 
t := #Kernel(degr) ;
bound[1] := t ;

P := [ p : p in PP | Norm(p) eq 13][1] ;
p := P;
R,pi := ResidueClassField(p) ;
C5 := ChangeRing(X, R) ;
CL5, phi5, pi5 := ClassGroup(C5) ;
Z := FreeAbelianGroup(1) ;
degr := hom<CL5 -> Z | [ Degree(phi5(g)) : g in OrderedGenerators(CL5)]>; 
t := #Kernel(degr) ;
bound[2] := t ;

P := [ p : p in PP | Norm(p) eq 19][1] ;
p := P;
R,pi := ResidueClassField(p) ;
C5 := ChangeRing(X, R) ;
CL5, phi5, pi5 := ClassGroup(C5) ;
Z := FreeAbelianGroup(1) ;
degr := hom<CL5 -> Z | [ Degree(phi5(g)) : g in OrderedGenerators(CL5)]>; 
t := #Kernel(degr) ;
bound[3] := t ;

B := GCD(bound) ;

print "An upper bound for the Q(\sqrt(61)) rational torsion subgroup is:";
B ;

assert B eq #CH;
print "Thus the two groups must be equal";



