;;;; File: jit-code-t.scm
;;;; 
;;;; Generated from "/usr/include/lightning.h"

;;;
;;; Enum values of jit_code_t
;;;

(define jit-code-data 0)
(define jit-code-live 1)
(define jit-code-align 2)
(define jit-code-save 3)
(define jit-code-load 4)
(define jit-code-name 5)
(define jit-code-note 6)
(define jit-code-label 7)
(define jit-code-prolog 8)
(define jit-code-arg 9)
(define jit-code-addr 10)
(define jit-code-addi 11)
(define jit-code-addcr 12)
(define jit-code-addci 13)
(define jit-code-addxr 14)
(define jit-code-addxi 15)
(define jit-code-subr 16)
(define jit-code-subi 17)
(define jit-code-subcr 18)
(define jit-code-subci 19)
(define jit-code-subxr 20)
(define jit-code-subxi 21)
(define jit-code-rsbi 22)
(define jit-code-mulr 23)
(define jit-code-muli 24)
(define jit-code-qmulr 25)
(define jit-code-qmuli 26)
(define jit-code-qmulr-u 27)
(define jit-code-qmuli-u 28)
(define jit-code-divr 29)
(define jit-code-divi 30)
(define jit-code-divr-u 31)
(define jit-code-divi-u 32)
(define jit-code-qdivr 33)
(define jit-code-qdivi 34)
(define jit-code-qdivr-u 35)
(define jit-code-qdivi-u 36)
(define jit-code-remr 37)
(define jit-code-remi 38)
(define jit-code-remr-u 39)
(define jit-code-remi-u 40)
(define jit-code-andr 41)
(define jit-code-andi 42)
(define jit-code-orr 43)
(define jit-code-ori 44)
(define jit-code-xorr 45)
(define jit-code-xori 46)
(define jit-code-lshr 47)
(define jit-code-lshi 48)
(define jit-code-rshr 49)
(define jit-code-rshi 50)
(define jit-code-rshr-u 51)
(define jit-code-rshi-u 52)
(define jit-code-negr 53)
(define jit-code-comr 54)
(define jit-code-ltr 55)
(define jit-code-lti 56)
(define jit-code-ltr-u 57)
(define jit-code-lti-u 58)
(define jit-code-ler 59)
(define jit-code-lei 60)
(define jit-code-ler-u 61)
(define jit-code-lei-u 62)
(define jit-code-eqr 63)
(define jit-code-eqi 64)
(define jit-code-ger 65)
(define jit-code-gei 66)
(define jit-code-ger-u 67)
(define jit-code-gei-u 68)
(define jit-code-gtr 69)
(define jit-code-gti 70)
(define jit-code-gtr-u 71)
(define jit-code-gti-u 72)
(define jit-code-ner 73)
(define jit-code-nei 74)
(define jit-code-movr 75)
(define jit-code-movi 76)
(define jit-code-extr-c 77)
(define jit-code-extr-uc 78)
(define jit-code-extr-s 79)
(define jit-code-extr-us 80)
(define jit-code-extr-i 81)
(define jit-code-extr-ui 82)
(define jit-code-htonr-us 83)
(define jit-code-htonr-ui 84)
(define jit-code-htonr-ul 85)
(define jit-code-ldr-c 86)
(define jit-code-ldi-c 87)
(define jit-code-ldr-uc 88)
(define jit-code-ldi-uc 89)
(define jit-code-ldr-s 90)
(define jit-code-ldi-s 91)
(define jit-code-ldr-us 92)
(define jit-code-ldi-us 93)
(define jit-code-ldr-i 94)
(define jit-code-ldi-i 95)
(define jit-code-ldr-ui 96)
(define jit-code-ldi-ui 97)
(define jit-code-ldr-l 98)
(define jit-code-ldi-l 99)
(define jit-code-ldxr-c 100)
(define jit-code-ldxi-c 101)
(define jit-code-ldxr-uc 102)
(define jit-code-ldxi-uc 103)
(define jit-code-ldxr-s 104)
(define jit-code-ldxi-s 105)
(define jit-code-ldxr-us 106)
(define jit-code-ldxi-us 107)
(define jit-code-ldxr-i 108)
(define jit-code-ldxi-i 109)
(define jit-code-ldxr-ui 110)
(define jit-code-ldxi-ui 111)
(define jit-code-ldxr-l 112)
(define jit-code-ldxi-l 113)
(define jit-code-str-c 114)
(define jit-code-sti-c 115)
(define jit-code-str-s 116)
(define jit-code-sti-s 117)
(define jit-code-str-i 118)
(define jit-code-sti-i 119)
(define jit-code-str-l 120)
(define jit-code-sti-l 121)
(define jit-code-stxr-c 122)
(define jit-code-stxi-c 123)
(define jit-code-stxr-s 124)
(define jit-code-stxi-s 125)
(define jit-code-stxr-i 126)
(define jit-code-stxi-i 127)
(define jit-code-stxr-l 128)
(define jit-code-stxi-l 129)
(define jit-code-bltr 130)
(define jit-code-blti 131)
(define jit-code-bltr-u 132)
(define jit-code-blti-u 133)
(define jit-code-bler 134)
(define jit-code-blei 135)
(define jit-code-bler-u 136)
(define jit-code-blei-u 137)
(define jit-code-beqr 138)
(define jit-code-beqi 139)
(define jit-code-bger 140)
(define jit-code-bgei 141)
(define jit-code-bger-u 142)
(define jit-code-bgei-u 143)
(define jit-code-bgtr 144)
(define jit-code-bgti 145)
(define jit-code-bgtr-u 146)
(define jit-code-bgti-u 147)
(define jit-code-bner 148)
(define jit-code-bnei 149)
(define jit-code-bmsr 150)
(define jit-code-bmsi 151)
(define jit-code-bmcr 152)
(define jit-code-bmci 153)
(define jit-code-boaddr 154)
(define jit-code-boaddi 155)
(define jit-code-boaddr-u 156)
(define jit-code-boaddi-u 157)
(define jit-code-bxaddr 158)
(define jit-code-bxaddi 159)
(define jit-code-bxaddr-u 160)
(define jit-code-bxaddi-u 161)
(define jit-code-bosubr 162)
(define jit-code-bosubi 163)
(define jit-code-bosubr-u 164)
(define jit-code-bosubi-u 165)
(define jit-code-bxsubr 166)
(define jit-code-bxsubi 167)
(define jit-code-bxsubr-u 168)
(define jit-code-bxsubi-u 169)
(define jit-code-jmpr 170)
(define jit-code-jmpi 171)
(define jit-code-callr 172)
(define jit-code-calli 173)
(define jit-code-epilog 174)
(define jit-code-arg-f 175)
(define jit-code-addr-f 176)
(define jit-code-addi-f 177)
(define jit-code-subr-f 178)
(define jit-code-subi-f 179)
(define jit-code-rsbi-f 180)
(define jit-code-mulr-f 181)
(define jit-code-muli-f 182)
(define jit-code-divr-f 183)
(define jit-code-divi-f 184)
(define jit-code-negr-f 185)
(define jit-code-absr-f 186)
(define jit-code-sqrtr-f 187)
(define jit-code-ltr-f 188)
(define jit-code-lti-f 189)
(define jit-code-ler-f 190)
(define jit-code-lei-f 191)
(define jit-code-eqr-f 192)
(define jit-code-eqi-f 193)
(define jit-code-ger-f 194)
(define jit-code-gei-f 195)
(define jit-code-gtr-f 196)
(define jit-code-gti-f 197)
(define jit-code-ner-f 198)
(define jit-code-nei-f 199)
(define jit-code-unltr-f 200)
(define jit-code-unlti-f 201)
(define jit-code-unler-f 202)
(define jit-code-unlei-f 203)
(define jit-code-uneqr-f 204)
(define jit-code-uneqi-f 205)
(define jit-code-unger-f 206)
(define jit-code-ungei-f 207)
(define jit-code-ungtr-f 208)
(define jit-code-ungti-f 209)
(define jit-code-ltgtr-f 210)
(define jit-code-ltgti-f 211)
(define jit-code-ordr-f 212)
(define jit-code-ordi-f 213)
(define jit-code-unordr-f 214)
(define jit-code-unordi-f 215)
(define jit-code-truncr-f-i 216)
(define jit-code-truncr-f-l 217)
(define jit-code-extr-f 218)
(define jit-code-extr-d-f 219)
(define jit-code-movr-f 220)
(define jit-code-movi-f 221)
(define jit-code-ldr-f 222)
(define jit-code-ldi-f 223)
(define jit-code-ldxr-f 224)
(define jit-code-ldxi-f 225)
(define jit-code-str-f 226)
(define jit-code-sti-f 227)
(define jit-code-stxr-f 228)
(define jit-code-stxi-f 229)
(define jit-code-bltr-f 230)
(define jit-code-blti-f 231)
(define jit-code-bler-f 232)
(define jit-code-blei-f 233)
(define jit-code-beqr-f 234)
(define jit-code-beqi-f 235)
(define jit-code-bger-f 236)
(define jit-code-bgei-f 237)
(define jit-code-bgtr-f 238)
(define jit-code-bgti-f 239)
(define jit-code-bner-f 240)
(define jit-code-bnei-f 241)
(define jit-code-bunltr-f 242)
(define jit-code-bunlti-f 243)
(define jit-code-bunler-f 244)
(define jit-code-bunlei-f 245)
(define jit-code-buneqr-f 246)
(define jit-code-buneqi-f 247)
(define jit-code-bunger-f 248)
(define jit-code-bungei-f 249)
(define jit-code-bungtr-f 250)
(define jit-code-bungti-f 251)
(define jit-code-bltgtr-f 252)
(define jit-code-bltgti-f 253)
(define jit-code-bordr-f 254)
(define jit-code-bordi-f 255)
(define jit-code-bunordr-f 256)
(define jit-code-bunordi-f 257)
(define jit-code-arg-d 258)
(define jit-code-addr-d 259)
(define jit-code-addi-d 260)
(define jit-code-subr-d 261)
(define jit-code-subi-d 262)
(define jit-code-rsbi-d 263)
(define jit-code-mulr-d 264)
(define jit-code-muli-d 265)
(define jit-code-divr-d 266)
(define jit-code-divi-d 267)
(define jit-code-negr-d 268)
(define jit-code-absr-d 269)
(define jit-code-sqrtr-d 270)
(define jit-code-ltr-d 271)
(define jit-code-lti-d 272)
(define jit-code-ler-d 273)
(define jit-code-lei-d 274)
(define jit-code-eqr-d 275)
(define jit-code-eqi-d 276)
(define jit-code-ger-d 277)
(define jit-code-gei-d 278)
(define jit-code-gtr-d 279)
(define jit-code-gti-d 280)
(define jit-code-ner-d 281)
(define jit-code-nei-d 282)
(define jit-code-unltr-d 283)
(define jit-code-unlti-d 284)
(define jit-code-unler-d 285)
(define jit-code-unlei-d 286)
(define jit-code-uneqr-d 287)
(define jit-code-uneqi-d 288)
(define jit-code-unger-d 289)
(define jit-code-ungei-d 290)
(define jit-code-ungtr-d 291)
(define jit-code-ungti-d 292)
(define jit-code-ltgtr-d 293)
(define jit-code-ltgti-d 294)
(define jit-code-ordr-d 295)
(define jit-code-ordi-d 296)
(define jit-code-unordr-d 297)
(define jit-code-unordi-d 298)
(define jit-code-truncr-d-i 299)
(define jit-code-truncr-d-l 300)
(define jit-code-extr-d 301)
(define jit-code-extr-f-d 302)
(define jit-code-movr-d 303)
(define jit-code-movi-d 304)
(define jit-code-ldr-d 305)
(define jit-code-ldi-d 306)
(define jit-code-ldxr-d 307)
(define jit-code-ldxi-d 308)
(define jit-code-str-d 309)
(define jit-code-sti-d 310)
(define jit-code-stxr-d 311)
(define jit-code-stxi-d 312)
(define jit-code-bltr-d 313)
(define jit-code-blti-d 314)
(define jit-code-bler-d 315)
(define jit-code-blei-d 316)
(define jit-code-beqr-d 317)
(define jit-code-beqi-d 318)
(define jit-code-bger-d 319)
(define jit-code-bgei-d 320)
(define jit-code-bgtr-d 321)
(define jit-code-bgti-d 322)
(define jit-code-bner-d 323)
(define jit-code-bnei-d 324)
(define jit-code-bunltr-d 325)
(define jit-code-bunlti-d 326)
(define jit-code-bunler-d 327)
(define jit-code-bunlei-d 328)
(define jit-code-buneqr-d 329)
(define jit-code-buneqi-d 330)
(define jit-code-bunger-d 331)
(define jit-code-bungei-d 332)
(define jit-code-bungtr-d 333)
(define jit-code-bungti-d 334)
(define jit-code-bltgtr-d 335)
(define jit-code-bltgti-d 336)
(define jit-code-bordr-d 337)
(define jit-code-bordi-d 338)
(define jit-code-bunordr-d 339)
(define jit-code-bunordi-d 340)
(define jit-code-movr-w-f 341)
(define jit-code-movr-ww-d 342)
(define jit-code-movr-w-d 343)
(define jit-code-movr-f-w 344)
(define jit-code-movi-f-w 345)
(define jit-code-movr-d-ww 346)
(define jit-code-movi-d-ww 347)
(define jit-code-movr-d-w 348)
(define jit-code-movi-d-w 349)
(define jit-code-x86-retval-f 350)
(define jit-code-x86-retval-d 351)


;;;
;;; C macro -> Scheme procedure
;;;

(define (jit-addr u v w)
  (jit-new-node-www (make-pointer jit-code-addr) u v w))

(define (jit-addi u v w)
  (jit-new-node-www (make-pointer jit-code-addi) u v w))

(define (jit-addcr u v w)
  (jit-new-node-www (make-pointer jit-code-addcr) u v w))

(define (jit-addci u v w)
  (jit-new-node-www (make-pointer jit-code-addci) u v w))

(define (jit-addxr u v w)
  (jit-new-node-www (make-pointer jit-code-addxr) u v w))

(define (jit-addxi u v w)
  (jit-new-node-www (make-pointer jit-code-addxi) u v w))

(define (jit-subr u v w)
  (jit-new-node-www (make-pointer jit-code-subr) u v w))

(define (jit-subi u v w)
  (jit-new-node-www (make-pointer jit-code-subi) u v w))

(define (jit-subcr u v w)
  (jit-new-node-www (make-pointer jit-code-subcr) u v w))

(define (jit-subci u v w)
  (jit-new-node-www (make-pointer jit-code-subci) u v w))

(define (jit-subxr u v w)
  (jit-new-node-www (make-pointer jit-code-subxr) u v w))

(define (jit-subxi u v w)
  (jit-new-node-www (make-pointer jit-code-subxi) u v w))

(define (jit-rsbi u v w)
  (jit-new-node-www (make-pointer jit-code-rsbi) u v w))

(define (jit-mulr u v w)
  (jit-new-node-www (make-pointer jit-code-mulr) u v w))

(define (jit-muli u v w)
  (jit-new-node-www (make-pointer jit-code-muli) u v w))

(define (jit-qmulr l h v w)
  (jit-new-node-qww (make-pointer jit-code-qmulr) l h v w))

(define (jit-qmuli l h v w)
  (jit-new-node-qww (make-pointer jit-code-qmuli) l h v w))

(define (jit-qmulr-u l h v w)
  (jit-new-node-qww (make-pointer jit-code-qmulr-u) l h v w))

(define (jit-qmuli-u l h v w)
  (jit-new-node-qww (make-pointer jit-code-qmuli-u) l h v w))

(define (jit-divr u v w)
  (jit-new-node-www (make-pointer jit-code-divr) u v w))

(define (jit-divi u v w)
  (jit-new-node-www (make-pointer jit-code-divi) u v w))

(define (jit-divr-u u v w)
  (jit-new-node-www (make-pointer jit-code-divr-u) u v w))

(define (jit-divi-u u v w)
  (jit-new-node-www (make-pointer jit-code-divi-u) u v w))

(define (jit-qdivr l h v w)
  (jit-new-node-qww (make-pointer jit-code-qdivr) l h v w))

(define (jit-qdivi l h v w)
  (jit-new-node-qww (make-pointer jit-code-qdivi) l h v w))

(define (jit-qdivr-u l h v w)
  (jit-new-node-qww (make-pointer jit-code-qdivr-u) l h v w))

(define (jit-qdivi-u l h v w)
  (jit-new-node-qww (make-pointer jit-code-qdivi-u) l h v w))

(define (jit-remr u v w)
  (jit-new-node-www (make-pointer jit-code-remr) u v w))

(define (jit-remi u v w)
  (jit-new-node-www (make-pointer jit-code-remi) u v w))

(define (jit-remr-u u v w)
  (jit-new-node-www (make-pointer jit-code-remr-u) u v w))

(define (jit-remi-u u v w)
  (jit-new-node-www (make-pointer jit-code-remi-u) u v w))

(define (jit-andr u v w)
  (jit-new-node-www (make-pointer jit-code-andr) u v w))

(define (jit-andi u v w)
  (jit-new-node-www (make-pointer jit-code-andi) u v w))

(define (jit-orr u v w)
  (jit-new-node-www (make-pointer jit-code-orr) u v w))

(define (jit-ori u v w)
  (jit-new-node-www (make-pointer jit-code-ori) u v w))

(define (jit-xorr u v w)
  (jit-new-node-www (make-pointer jit-code-xorr) u v w))

(define (jit-xori u v w)
  (jit-new-node-www (make-pointer jit-code-xori) u v w))

(define (jit-lshr u v w)
  (jit-new-node-www (make-pointer jit-code-lshr) u v w))

(define (jit-lshi u v w)
  (jit-new-node-www (make-pointer jit-code-lshi) u v w))

(define (jit-rshr u v w)
  (jit-new-node-www (make-pointer jit-code-rshr) u v w))

(define (jit-rshi u v w)
  (jit-new-node-www (make-pointer jit-code-rshi) u v w))

(define (jit-rshr-u u v w)
  (jit-new-node-www (make-pointer jit-code-rshr-u) u v w))

(define (jit-rshi-u u v w)
  (jit-new-node-www (make-pointer jit-code-rshi-u) u v w))

(define (jit-negr u v)
  (jit-new-node-ww (make-pointer jit-code-negr) u v))

(define (jit-comr u v)
  (jit-new-node-ww (make-pointer jit-code-comr) u v))

(define (jit-ltr u v w)
  (jit-new-node-www (make-pointer jit-code-ltr) u v w))

(define (jit-lti u v w)
  (jit-new-node-www (make-pointer jit-code-lti) u v w))

(define (jit-ltr-u u v w)
  (jit-new-node-www (make-pointer jit-code-ltr-u) u v w))

(define (jit-lti-u u v w)
  (jit-new-node-www (make-pointer jit-code-lti-u) u v w))

(define (jit-ler u v w)
  (jit-new-node-www (make-pointer jit-code-ler) u v w))

(define (jit-lei u v w)
  (jit-new-node-www (make-pointer jit-code-lei) u v w))

(define (jit-ler-u u v w)
  (jit-new-node-www (make-pointer jit-code-ler-u) u v w))

(define (jit-lei-u u v w)
  (jit-new-node-www (make-pointer jit-code-lei-u) u v w))

(define (jit-eqr u v w)
  (jit-new-node-www (make-pointer jit-code-eqr) u v w))

(define (jit-eqi u v w)
  (jit-new-node-www (make-pointer jit-code-eqi) u v w))

(define (jit-ger u v w)
  (jit-new-node-www (make-pointer jit-code-ger) u v w))

(define (jit-gei u v w)
  (jit-new-node-www (make-pointer jit-code-gei) u v w))

(define (jit-ger-u u v w)
  (jit-new-node-www (make-pointer jit-code-ger-u) u v w))

(define (jit-gei-u u v w)
  (jit-new-node-www (make-pointer jit-code-gei-u) u v w))

(define (jit-gtr u v w)
  (jit-new-node-www (make-pointer jit-code-gtr) u v w))

(define (jit-gti u v w)
  (jit-new-node-www (make-pointer jit-code-gti) u v w))

(define (jit-gtr-u u v w)
  (jit-new-node-www (make-pointer jit-code-gtr-u) u v w))

(define (jit-gti-u u v w)
  (jit-new-node-www (make-pointer jit-code-gti-u) u v w))

(define (jit-ner u v w)
  (jit-new-node-www (make-pointer jit-code-ner) u v w))

(define (jit-nei u v w)
  (jit-new-node-www (make-pointer jit-code-nei) u v w))

(define (jit-movr u v)
  (jit-new-node-ww (make-pointer jit-code-movr) u v))

(define (jit-movi u v)
  (jit-new-node-ww (make-pointer jit-code-movi) u v))

(define (jit-extr-c u v)
  (jit-new-node-ww (make-pointer jit-code-extr-c) u v))

(define (jit-extr-uc u v)
  (jit-new-node-ww (make-pointer jit-code-extr-uc) u v))

(define (jit-extr-s u v)
  (jit-new-node-ww (make-pointer jit-code-extr-s) u v))

(define (jit-extr-us u v)
  (jit-new-node-ww (make-pointer jit-code-extr-us) u v))

(define (jit-extr-i u v)
  (jit-new-node-ww (make-pointer jit-code-extr-i) u v))

(define (jit-extr-ui u v)
  (jit-new-node-ww (make-pointer jit-code-extr-ui) u v))

(define (jit-htonr-us u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-us) u v))

(define (jit-htonr-us u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-us) u v))

(define (jit-htonr-ui u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-ui) u v))

(define (jit-htonr-ui u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-ui) u v))

(define (jit-htonr-ui u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-ui) u v))

(define (jit-htonr-ui u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-ui) u v))

(define (jit-htonr-ul u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-ul) u v))

(define (jit-htonr-ul u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-ul) u v))

(define (jit-htonr-ul u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-ul) u v))

(define (jit-htonr-ul u v)
  (jit-new-node-ww (make-pointer jit-code-htonr-ul) u v))

(define (jit-ldr-c u v)
  (jit-new-node-ww (make-pointer jit-code-ldr-c) u v))

(define (jit-ldi-c u v)
  (jit-new-node-wp (make-pointer jit-code-ldi-c) u v))

(define (jit-ldr-uc u v)
  (jit-new-node-ww (make-pointer jit-code-ldr-uc) u v))

(define (jit-ldi-uc u v)
  (jit-new-node-wp (make-pointer jit-code-ldi-uc) u v))

(define (jit-ldr-s u v)
  (jit-new-node-ww (make-pointer jit-code-ldr-s) u v))

(define (jit-ldi-s u v)
  (jit-new-node-wp (make-pointer jit-code-ldi-s) u v))

(define (jit-ldr-us u v)
  (jit-new-node-ww (make-pointer jit-code-ldr-us) u v))

(define (jit-ldi-us u v)
  (jit-new-node-wp (make-pointer jit-code-ldi-us) u v))

(define (jit-ldr-i u v)
  (jit-new-node-ww (make-pointer jit-code-ldr-i) u v))

(define (jit-ldi-i u v)
  (jit-new-node-wp (make-pointer jit-code-ldi-i) u v))

(define (jit-ldr-ui u v)
  (jit-new-node-ww (make-pointer jit-code-ldr-ui) u v))

(define (jit-ldi-ui u v)
  (jit-new-node-wp (make-pointer jit-code-ldi-ui) u v))

(define (jit-ldr-l u v)
  (jit-new-node-ww (make-pointer jit-code-ldr-l) u v))

(define (jit-ldi-l u v)
  (jit-new-node-wp (make-pointer jit-code-ldi-l) u v))

(define (jit-ldxr-c u v w)
  (jit-new-node-www (make-pointer jit-code-ldxr-c) u v w))

(define (jit-ldxi-c u v w)
  (jit-new-node-www (make-pointer jit-code-ldxi-c) u v w))

(define (jit-ldxr-uc u v w)
  (jit-new-node-www (make-pointer jit-code-ldxr-uc) u v w))

(define (jit-ldxi-uc u v w)
  (jit-new-node-www (make-pointer jit-code-ldxi-uc) u v w))

(define (jit-ldxr-s u v w)
  (jit-new-node-www (make-pointer jit-code-ldxr-s) u v w))

(define (jit-ldxi-s u v w)
  (jit-new-node-www (make-pointer jit-code-ldxi-s) u v w))

(define (jit-ldxr-us u v w)
  (jit-new-node-www (make-pointer jit-code-ldxr-us) u v w))

(define (jit-ldxi-us u v w)
  (jit-new-node-www (make-pointer jit-code-ldxi-us) u v w))

(define (jit-ldxr-i u v w)
  (jit-new-node-www (make-pointer jit-code-ldxr-i) u v w))

(define (jit-ldxi-i u v w)
  (jit-new-node-www (make-pointer jit-code-ldxi-i) u v w))

(define (jit-ldxr-ui u v w)
  (jit-new-node-www (make-pointer jit-code-ldxr-ui) u v w))

(define (jit-ldxi-ui u v w)
  (jit-new-node-www (make-pointer jit-code-ldxi-ui) u v w))

(define (jit-ldxr-l u v w)
  (jit-new-node-www (make-pointer jit-code-ldxr-l) u v w))

(define (jit-ldxi-l u v w)
  (jit-new-node-www (make-pointer jit-code-ldxi-l) u v w))

(define (jit-str-c u v)
  (jit-new-node-ww (make-pointer jit-code-str-c) u v))

(define (jit-sti-c u v)
  (jit-new-node-pw (make-pointer jit-code-sti-c) u v))

(define (jit-str-s u v)
  (jit-new-node-ww (make-pointer jit-code-str-s) u v))

(define (jit-sti-s u v)
  (jit-new-node-pw (make-pointer jit-code-sti-s) u v))

(define (jit-str-i u v)
  (jit-new-node-ww (make-pointer jit-code-str-i) u v))

(define (jit-sti-i u v)
  (jit-new-node-pw (make-pointer jit-code-sti-i) u v))

(define (jit-str-l u v)
  (jit-new-node-ww (make-pointer jit-code-str-l) u v))

(define (jit-sti-l u v)
  (jit-new-node-pw (make-pointer jit-code-sti-l) u v))

(define (jit-stxr-c u v w)
  (jit-new-node-www (make-pointer jit-code-stxr-c) u v w))

(define (jit-stxi-c u v w)
  (jit-new-node-www (make-pointer jit-code-stxi-c) u v w))

(define (jit-stxr-s u v w)
  (jit-new-node-www (make-pointer jit-code-stxr-s) u v w))

(define (jit-stxi-s u v w)
  (jit-new-node-www (make-pointer jit-code-stxi-s) u v w))

(define (jit-stxr-i u v w)
  (jit-new-node-www (make-pointer jit-code-stxr-i) u v w))

(define (jit-stxi-i u v w)
  (jit-new-node-www (make-pointer jit-code-stxi-i) u v w))

(define (jit-stxr-l u v w)
  (jit-new-node-www (make-pointer jit-code-stxr-l) u v w))

(define (jit-stxi-l u v w)
  (jit-new-node-www (make-pointer jit-code-stxi-l) u v w))

(define (jit-bltr v w)
  (jit-new-node-pww (make-pointer jit-code-bltr) %null-pointer v w))

(define (jit-blti v w)
  (jit-new-node-pww (make-pointer jit-code-blti) %null-pointer v w))

(define (jit-bltr-u v w)
  (jit-new-node-pww (make-pointer jit-code-bltr-u) %null-pointer v w))

(define (jit-blti-u v w)
  (jit-new-node-pww (make-pointer jit-code-blti-u) %null-pointer v w))

(define (jit-bler v w)
  (jit-new-node-pww (make-pointer jit-code-bler) %null-pointer v w))

(define (jit-blei v w)
  (jit-new-node-pww (make-pointer jit-code-blei) %null-pointer v w))

(define (jit-bler-u v w)
  (jit-new-node-pww (make-pointer jit-code-bler-u) %null-pointer v w))

(define (jit-blei-u v w)
  (jit-new-node-pww (make-pointer jit-code-blei-u) %null-pointer v w))

(define (jit-beqr v w)
  (jit-new-node-pww (make-pointer jit-code-beqr) %null-pointer v w))

(define (jit-beqi v w)
  (jit-new-node-pww (make-pointer jit-code-beqi) %null-pointer v w))

(define (jit-bger v w)
  (jit-new-node-pww (make-pointer jit-code-bger) %null-pointer v w))

(define (jit-bgei v w)
  (jit-new-node-pww (make-pointer jit-code-bgei) %null-pointer v w))

(define (jit-bger-u v w)
  (jit-new-node-pww (make-pointer jit-code-bger-u) %null-pointer v w))

(define (jit-bgei-u v w)
  (jit-new-node-pww (make-pointer jit-code-bgei-u) %null-pointer v w))

(define (jit-bgtr v w)
  (jit-new-node-pww (make-pointer jit-code-bgtr) %null-pointer v w))

(define (jit-bgti v w)
  (jit-new-node-pww (make-pointer jit-code-bgti) %null-pointer v w))

(define (jit-bgtr-u v w)
  (jit-new-node-pww (make-pointer jit-code-bgtr-u) %null-pointer v w))

(define (jit-bgti-u v w)
  (jit-new-node-pww (make-pointer jit-code-bgti-u) %null-pointer v w))

(define (jit-bner v w)
  (jit-new-node-pww (make-pointer jit-code-bner) %null-pointer v w))

(define (jit-bnei v w)
  (jit-new-node-pww (make-pointer jit-code-bnei) %null-pointer v w))

(define (jit-bmsr v w)
  (jit-new-node-pww (make-pointer jit-code-bmsr) %null-pointer v w))

(define (jit-bmsi v w)
  (jit-new-node-pww (make-pointer jit-code-bmsi) %null-pointer v w))

(define (jit-bmcr v w)
  (jit-new-node-pww (make-pointer jit-code-bmcr) %null-pointer v w))

(define (jit-bmci v w)
  (jit-new-node-pww (make-pointer jit-code-bmci) %null-pointer v w))

(define (jit-boaddr v w)
  (jit-new-node-pww (make-pointer jit-code-boaddr) %null-pointer v w))

(define (jit-boaddi v w)
  (jit-new-node-pww (make-pointer jit-code-boaddi) %null-pointer v w))

(define (jit-boaddr-u v w)
  (jit-new-node-pww (make-pointer jit-code-boaddr-u) %null-pointer v w))

(define (jit-boaddi-u v w)
  (jit-new-node-pww (make-pointer jit-code-boaddi-u) %null-pointer v w))

(define (jit-bxaddr v w)
  (jit-new-node-pww (make-pointer jit-code-bxaddr) %null-pointer v w))

(define (jit-bxaddi v w)
  (jit-new-node-pww (make-pointer jit-code-bxaddi) %null-pointer v w))

(define (jit-bxaddr-u v w)
  (jit-new-node-pww (make-pointer jit-code-bxaddr-u) %null-pointer v w))

(define (jit-bxaddi-u v w)
  (jit-new-node-pww (make-pointer jit-code-bxaddi-u) %null-pointer v w))

(define (jit-bosubr v w)
  (jit-new-node-pww (make-pointer jit-code-bosubr) %null-pointer v w))

(define (jit-bosubi v w)
  (jit-new-node-pww (make-pointer jit-code-bosubi) %null-pointer v w))

(define (jit-bosubr-u v w)
  (jit-new-node-pww (make-pointer jit-code-bosubr-u) %null-pointer v w))

(define (jit-bosubi-u v w)
  (jit-new-node-pww (make-pointer jit-code-bosubi-u) %null-pointer v w))

(define (jit-bxsubr v w)
  (jit-new-node-pww (make-pointer jit-code-bxsubr) %null-pointer v w))

(define (jit-bxsubi v w)
  (jit-new-node-pww (make-pointer jit-code-bxsubi) %null-pointer v w))

(define (jit-bxsubr-u v w)
  (jit-new-node-pww (make-pointer jit-code-bxsubr-u) %null-pointer v w))

(define (jit-bxsubi-u v w)
  (jit-new-node-pww (make-pointer jit-code-bxsubi-u) %null-pointer v w))

(define (jit-jmpr u)
  (jit-new-node-w (make-pointer jit-code-jmpr) u))

(define (jit-jmpi )
  (jit-new-node-p (make-pointer jit-code-jmpi) %null-pointer))

(define (jit-callr u)
  (jit-new-node-w (make-pointer jit-code-callr) u))

(define (jit-calli u)
  (jit-new-node-p (make-pointer jit-code-calli) u))

(define (jit-addr-f u v w)
  (jit-new-node-www (make-pointer jit-code-addr-f) u v w))

(define (jit-addi-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-addi-f) u v w))

(define (jit-subr-f u v w)
  (jit-new-node-www (make-pointer jit-code-subr-f) u v w))

(define (jit-subi-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-subi-f) u v w))

(define (jit-rsbi-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-rsbi-f) u v w))

(define (jit-mulr-f u v w)
  (jit-new-node-www (make-pointer jit-code-mulr-f) u v w))

(define (jit-muli-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-muli-f) u v w))

(define (jit-divr-f u v w)
  (jit-new-node-www (make-pointer jit-code-divr-f) u v w))

(define (jit-divi-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-divi-f) u v w))

(define (jit-negr-f u v)
  (jit-new-node-ww (make-pointer jit-code-negr-f) u v))

(define (jit-absr-f u v)
  (jit-new-node-ww (make-pointer jit-code-absr-f) u v))

(define (jit-sqrtr-f u v)
  (jit-new-node-ww (make-pointer jit-code-sqrtr-f) u v))

(define (jit-ltr-f u v w)
  (jit-new-node-www (make-pointer jit-code-ltr-f) u v w))

(define (jit-lti-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-lti-f) u v w))

(define (jit-ler-f u v w)
  (jit-new-node-www (make-pointer jit-code-ler-f) u v w))

(define (jit-lei-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-lei-f) u v w))

(define (jit-eqr-f u v w)
  (jit-new-node-www (make-pointer jit-code-eqr-f) u v w))

(define (jit-eqi-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-eqi-f) u v w))

(define (jit-ger-f u v w)
  (jit-new-node-www (make-pointer jit-code-ger-f) u v w))

(define (jit-gei-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-gei-f) u v w))

(define (jit-gtr-f u v w)
  (jit-new-node-www (make-pointer jit-code-gtr-f) u v w))

(define (jit-gti-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-gti-f) u v w))

(define (jit-ner-f u v w)
  (jit-new-node-www (make-pointer jit-code-ner-f) u v w))

(define (jit-nei-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-nei-f) u v w))

(define (jit-unltr-f u v w)
  (jit-new-node-www (make-pointer jit-code-unltr-f) u v w))

(define (jit-unlti-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-unlti-f) u v w))

(define (jit-unler-f u v w)
  (jit-new-node-www (make-pointer jit-code-unler-f) u v w))

(define (jit-unlei-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-unlei-f) u v w))

(define (jit-uneqr-f u v w)
  (jit-new-node-www (make-pointer jit-code-uneqr-f) u v w))

(define (jit-uneqi-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-uneqi-f) u v w))

(define (jit-unger-f u v w)
  (jit-new-node-www (make-pointer jit-code-unger-f) u v w))

(define (jit-ungei-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-ungei-f) u v w))

(define (jit-ungtr-f u v w)
  (jit-new-node-www (make-pointer jit-code-ungtr-f) u v w))

(define (jit-ungti-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-ungti-f) u v w))

(define (jit-ltgtr-f u v w)
  (jit-new-node-www (make-pointer jit-code-ltgtr-f) u v w))

(define (jit-ltgti-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-ltgti-f) u v w))

(define (jit-ordr-f u v w)
  (jit-new-node-www (make-pointer jit-code-ordr-f) u v w))

(define (jit-ordi-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-ordi-f) u v w))

(define (jit-unordr-f u v w)
  (jit-new-node-www (make-pointer jit-code-unordr-f) u v w))

(define (jit-unordi-f u v w)
  (jit-new-node-wwf (make-pointer jit-code-unordi-f) u v w))

(define (jit-truncr-f-i u v)
  (jit-new-node-ww (make-pointer jit-code-truncr-f-i) u v))

(define (jit-truncr-f-l u v)
  (jit-new-node-ww (make-pointer jit-code-truncr-f-l) u v))

(define (jit-extr-f u v)
  (jit-new-node-ww (make-pointer jit-code-extr-f) u v))

(define (jit-extr-d-f u v)
  (jit-new-node-ww (make-pointer jit-code-extr-d-f) u v))

(define (jit-movr-f u v)
  (jit-new-node-ww (make-pointer jit-code-movr-f) u v))

(define (jit-movi-f u v)
  (jit-new-node-wf (make-pointer jit-code-movi-f) u v))

(define (jit-ldr-f u v)
  (jit-new-node-ww (make-pointer jit-code-ldr-f) u v))

(define (jit-ldi-f u v)
  (jit-new-node-wp (make-pointer jit-code-ldi-f) u v))

(define (jit-ldxr-f u v w)
  (jit-new-node-www (make-pointer jit-code-ldxr-f) u v w))

(define (jit-ldxi-f u v w)
  (jit-new-node-www (make-pointer jit-code-ldxi-f) u v w))

(define (jit-str-f u v)
  (jit-new-node-ww (make-pointer jit-code-str-f) u v))

(define (jit-sti-f u v)
  (jit-new-node-pw (make-pointer jit-code-sti-f) u v))

(define (jit-stxr-f u v w)
  (jit-new-node-www (make-pointer jit-code-stxr-f) u v w))

(define (jit-stxi-f u v w)
  (jit-new-node-www (make-pointer jit-code-stxi-f) u v w))

(define (jit-bltr-f v w)
  (jit-new-node-pww (make-pointer jit-code-bltr-f) %null-pointer v w))

(define (jit-blti-f v w)
  (jit-new-node-pwf (make-pointer jit-code-blti-f) %null-pointer v w))

(define (jit-bler-f v w)
  (jit-new-node-pww (make-pointer jit-code-bler-f) %null-pointer v w))

(define (jit-blei-f v w)
  (jit-new-node-pwf (make-pointer jit-code-blei-f) %null-pointer v w))

(define (jit-beqr-f v w)
  (jit-new-node-pww (make-pointer jit-code-beqr-f) %null-pointer v w))

(define (jit-beqi-f v w)
  (jit-new-node-pwf (make-pointer jit-code-beqi-f) %null-pointer v w))

(define (jit-bger-f v w)
  (jit-new-node-pww (make-pointer jit-code-bger-f) %null-pointer v w))

(define (jit-bgei-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bgei-f) %null-pointer v w))

(define (jit-bgtr-f v w)
  (jit-new-node-pww (make-pointer jit-code-bgtr-f) %null-pointer v w))

(define (jit-bgti-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bgti-f) %null-pointer v w))

(define (jit-bner-f v w)
  (jit-new-node-pww (make-pointer jit-code-bner-f) %null-pointer v w))

(define (jit-bnei-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bnei-f) %null-pointer v w))

(define (jit-bunltr-f v w)
  (jit-new-node-pww (make-pointer jit-code-bunltr-f) %null-pointer v w))

(define (jit-bunlti-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bunlti-f) %null-pointer v w))

(define (jit-bunler-f v w)
  (jit-new-node-pww (make-pointer jit-code-bunler-f) %null-pointer v w))

(define (jit-bunlei-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bunlei-f) %null-pointer v w))

(define (jit-buneqr-f v w)
  (jit-new-node-pww (make-pointer jit-code-buneqr-f) %null-pointer v w))

(define (jit-buneqi-f v w)
  (jit-new-node-pwf (make-pointer jit-code-buneqi-f) %null-pointer v w))

(define (jit-bunger-f v w)
  (jit-new-node-pww (make-pointer jit-code-bunger-f) %null-pointer v w))

(define (jit-bungei-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bungei-f) %null-pointer v w))

(define (jit-bungtr-f v w)
  (jit-new-node-pww (make-pointer jit-code-bungtr-f) %null-pointer v w))

(define (jit-bungti-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bungti-f) %null-pointer v w))

(define (jit-bltgtr-f v w)
  (jit-new-node-pww (make-pointer jit-code-bltgtr-f) %null-pointer v w))

(define (jit-bltgti-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bltgti-f) %null-pointer v w))

(define (jit-bordr-f v w)
  (jit-new-node-pww (make-pointer jit-code-bordr-f) %null-pointer v w))

(define (jit-bordi-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bordi-f) %null-pointer v w))

(define (jit-bunordr-f v w)
  (jit-new-node-pww (make-pointer jit-code-bunordr-f) %null-pointer v w))

(define (jit-bunordi-f v w)
  (jit-new-node-pwf (make-pointer jit-code-bunordi-f) %null-pointer v w))

(define (jit-addr-d u v w)
  (jit-new-node-www (make-pointer jit-code-addr-d) u v w))

(define (jit-addi-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-addi-d) u v w))

(define (jit-subr-d u v w)
  (jit-new-node-www (make-pointer jit-code-subr-d) u v w))

(define (jit-subi-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-subi-d) u v w))

(define (jit-rsbi-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-rsbi-d) u v w))

(define (jit-mulr-d u v w)
  (jit-new-node-www (make-pointer jit-code-mulr-d) u v w))

(define (jit-muli-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-muli-d) u v w))

(define (jit-divr-d u v w)
  (jit-new-node-www (make-pointer jit-code-divr-d) u v w))

(define (jit-divi-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-divi-d) u v w))

(define (jit-negr-d u v)
  (jit-new-node-ww (make-pointer jit-code-negr-d) u v))

(define (jit-absr-d u v)
  (jit-new-node-ww (make-pointer jit-code-absr-d) u v))

(define (jit-sqrtr-d u v)
  (jit-new-node-ww (make-pointer jit-code-sqrtr-d) u v))

(define (jit-ltr-d u v w)
  (jit-new-node-www (make-pointer jit-code-ltr-d) u v w))

(define (jit-lti-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-lti-d) u v w))

(define (jit-ler-d u v w)
  (jit-new-node-www (make-pointer jit-code-ler-d) u v w))

(define (jit-lei-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-lei-d) u v w))

(define (jit-eqr-d u v w)
  (jit-new-node-www (make-pointer jit-code-eqr-d) u v w))

(define (jit-eqi-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-eqi-d) u v w))

(define (jit-ger-d u v w)
  (jit-new-node-www (make-pointer jit-code-ger-d) u v w))

(define (jit-gei-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-gei-d) u v w))

(define (jit-gtr-d u v w)
  (jit-new-node-www (make-pointer jit-code-gtr-d) u v w))

(define (jit-gti-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-gti-d) u v w))

(define (jit-ner-d u v w)
  (jit-new-node-www (make-pointer jit-code-ner-d) u v w))

(define (jit-nei-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-nei-d) u v w))

(define (jit-unltr-d u v w)
  (jit-new-node-www (make-pointer jit-code-unltr-d) u v w))

(define (jit-unlti-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-unlti-d) u v w))

(define (jit-unler-d u v w)
  (jit-new-node-www (make-pointer jit-code-unler-d) u v w))

(define (jit-unlei-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-unlei-d) u v w))

(define (jit-uneqr-d u v w)
  (jit-new-node-www (make-pointer jit-code-uneqr-d) u v w))

(define (jit-uneqi-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-uneqi-d) u v w))

(define (jit-unger-d u v w)
  (jit-new-node-www (make-pointer jit-code-unger-d) u v w))

(define (jit-ungei-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-ungei-d) u v w))

(define (jit-ungtr-d u v w)
  (jit-new-node-www (make-pointer jit-code-ungtr-d) u v w))

(define (jit-ungti-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-ungti-d) u v w))

(define (jit-ltgtr-d u v w)
  (jit-new-node-www (make-pointer jit-code-ltgtr-d) u v w))

(define (jit-ltgti-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-ltgti-d) u v w))

(define (jit-ordr-d u v w)
  (jit-new-node-www (make-pointer jit-code-ordr-d) u v w))

(define (jit-ordi-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-ordi-d) u v w))

(define (jit-unordr-d u v w)
  (jit-new-node-www (make-pointer jit-code-unordr-d) u v w))

(define (jit-unordi-d u v w)
  (jit-new-node-wwd (make-pointer jit-code-unordi-d) u v w))

(define (jit-truncr-d-i u v)
  (jit-new-node-ww (make-pointer jit-code-truncr-d-i) u v))

(define (jit-truncr-d-l u v)
  (jit-new-node-ww (make-pointer jit-code-truncr-d-l) u v))

(define (jit-extr-d u v)
  (jit-new-node-ww (make-pointer jit-code-extr-d) u v))

(define (jit-extr-f-d u v)
  (jit-new-node-ww (make-pointer jit-code-extr-f-d) u v))

(define (jit-movr-d u v)
  (jit-new-node-ww (make-pointer jit-code-movr-d) u v))

(define (jit-movi-d u v)
  (jit-new-node-wd (make-pointer jit-code-movi-d) u v))

(define (jit-ldr-d u v)
  (jit-new-node-ww (make-pointer jit-code-ldr-d) u v))

(define (jit-ldi-d u v)
  (jit-new-node-wp (make-pointer jit-code-ldi-d) u v))

(define (jit-ldxr-d u v w)
  (jit-new-node-www (make-pointer jit-code-ldxr-d) u v w))

(define (jit-ldxi-d u v w)
  (jit-new-node-www (make-pointer jit-code-ldxi-d) u v w))

(define (jit-str-d u v)
  (jit-new-node-ww (make-pointer jit-code-str-d) u v))

(define (jit-sti-d u v)
  (jit-new-node-pw (make-pointer jit-code-sti-d) u v))

(define (jit-stxr-d u v w)
  (jit-new-node-www (make-pointer jit-code-stxr-d) u v w))

(define (jit-stxi-d u v w)
  (jit-new-node-www (make-pointer jit-code-stxi-d) u v w))

(define (jit-bltr-d v w)
  (jit-new-node-pww (make-pointer jit-code-bltr-d) %null-pointer v w))

(define (jit-blti-d v w)
  (jit-new-node-pwd (make-pointer jit-code-blti-d) %null-pointer v w))

(define (jit-bler-d v w)
  (jit-new-node-pww (make-pointer jit-code-bler-d) %null-pointer v w))

(define (jit-blei-d v w)
  (jit-new-node-pwd (make-pointer jit-code-blei-d) %null-pointer v w))

(define (jit-beqr-d v w)
  (jit-new-node-pww (make-pointer jit-code-beqr-d) %null-pointer v w))

(define (jit-beqi-d v w)
  (jit-new-node-pwd (make-pointer jit-code-beqi-d) %null-pointer v w))

(define (jit-bger-d v w)
  (jit-new-node-pww (make-pointer jit-code-bger-d) %null-pointer v w))

(define (jit-bgei-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bgei-d) %null-pointer v w))

(define (jit-bgtr-d v w)
  (jit-new-node-pww (make-pointer jit-code-bgtr-d) %null-pointer v w))

(define (jit-bgti-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bgti-d) %null-pointer v w))

(define (jit-bner-d v w)
  (jit-new-node-pww (make-pointer jit-code-bner-d) %null-pointer v w))

(define (jit-bnei-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bnei-d) %null-pointer v w))

(define (jit-bunltr-d v w)
  (jit-new-node-pww (make-pointer jit-code-bunltr-d) %null-pointer v w))

(define (jit-bunlti-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bunlti-d) %null-pointer v w))

(define (jit-bunler-d v w)
  (jit-new-node-pww (make-pointer jit-code-bunler-d) %null-pointer v w))

(define (jit-bunlei-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bunlei-d) %null-pointer v w))

(define (jit-buneqr-d v w)
  (jit-new-node-pww (make-pointer jit-code-buneqr-d) %null-pointer v w))

(define (jit-buneqi-d v w)
  (jit-new-node-pwd (make-pointer jit-code-buneqi-d) %null-pointer v w))

(define (jit-bunger-d v w)
  (jit-new-node-pww (make-pointer jit-code-bunger-d) %null-pointer v w))

(define (jit-bungei-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bungei-d) %null-pointer v w))

(define (jit-bungtr-d v w)
  (jit-new-node-pww (make-pointer jit-code-bungtr-d) %null-pointer v w))

(define (jit-bungti-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bungti-d) %null-pointer v w))

(define (jit-bltgtr-d v w)
  (jit-new-node-pww (make-pointer jit-code-bltgtr-d) %null-pointer v w))

(define (jit-bltgti-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bltgti-d) %null-pointer v w))

(define (jit-bordr-d v w)
  (jit-new-node-pww (make-pointer jit-code-bordr-d) %null-pointer v w))

(define (jit-bordi-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bordi-d) %null-pointer v w))

(define (jit-bunordr-d v w)
  (jit-new-node-pww (make-pointer jit-code-bunordr-d) %null-pointer v w))

(define (jit-bunordi-d v w)
  (jit-new-node-pwd (make-pointer jit-code-bunordi-d) %null-pointer v w))


;;;
;;; Word size specific aliases
;;;

(define jit-getarg jit-getarg-l)
(define jit-htonr jit-htonr-ul)
(define jit-ntohr jit-htonr-ul)
(define jit-ldr jit-ldr-l)
(define jit-ldi jit-ldi-l)
(define jit-ldxr jit-ldxr-l)
(define jit-ldxi jit-ldxi-l)
(define jit-str jit-str-l)
(define jit-sti jit-sti-l)
(define jit-stxr jit-stxr-l)
(define jit-stxi jit-stxi-l)
(define jit-retval jit-retval-l)
(define jit-truncr-f jit-truncr-f-l)
(define jit-truncr-d jit-truncr-d-l)

;;; EOF