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
(define jit-code-ellipsis 9)
(define jit-code-allocai 10)
(define jit-code-allocar 11)
(define jit-code-arg 12)
(define jit-code-getarg-c 13)
(define jit-code-getarg-uc 14)
(define jit-code-getarg-s 15)
(define jit-code-getarg-us 16)
(define jit-code-getarg-i 17)
(define jit-code-getarg-ui 18)
(define jit-code-getarg-l 19)
(define jit-code-putargr 20)
(define jit-code-putargi 21)
(define jit-code-va-start 22)
(define jit-code-va-arg 23)
(define jit-code-va-arg-d 24)
(define jit-code-va-end 25)
(define jit-code-addr 26)
(define jit-code-addi 27)
(define jit-code-addcr 28)
(define jit-code-addci 29)
(define jit-code-addxr 30)
(define jit-code-addxi 31)
(define jit-code-subr 32)
(define jit-code-subi 33)
(define jit-code-subcr 34)
(define jit-code-subci 35)
(define jit-code-subxr 36)
(define jit-code-subxi 37)
(define jit-code-rsbi 38)
(define jit-code-mulr 39)
(define jit-code-muli 40)
(define jit-code-qmulr 41)
(define jit-code-qmuli 42)
(define jit-code-qmulr-u 43)
(define jit-code-qmuli-u 44)
(define jit-code-divr 45)
(define jit-code-divi 46)
(define jit-code-divr-u 47)
(define jit-code-divi-u 48)
(define jit-code-qdivr 49)
(define jit-code-qdivi 50)
(define jit-code-qdivr-u 51)
(define jit-code-qdivi-u 52)
(define jit-code-remr 53)
(define jit-code-remi 54)
(define jit-code-remr-u 55)
(define jit-code-remi-u 56)
(define jit-code-andr 57)
(define jit-code-andi 58)
(define jit-code-orr 59)
(define jit-code-ori 60)
(define jit-code-xorr 61)
(define jit-code-xori 62)
(define jit-code-lshr 63)
(define jit-code-lshi 64)
(define jit-code-rshr 65)
(define jit-code-rshi 66)
(define jit-code-rshr-u 67)
(define jit-code-rshi-u 68)
(define jit-code-negr 69)
(define jit-code-comr 70)
(define jit-code-ltr 71)
(define jit-code-lti 72)
(define jit-code-ltr-u 73)
(define jit-code-lti-u 74)
(define jit-code-ler 75)
(define jit-code-lei 76)
(define jit-code-ler-u 77)
(define jit-code-lei-u 78)
(define jit-code-eqr 79)
(define jit-code-eqi 80)
(define jit-code-ger 81)
(define jit-code-gei 82)
(define jit-code-ger-u 83)
(define jit-code-gei-u 84)
(define jit-code-gtr 85)
(define jit-code-gti 86)
(define jit-code-gtr-u 87)
(define jit-code-gti-u 88)
(define jit-code-ner 89)
(define jit-code-nei 90)
(define jit-code-movr 91)
(define jit-code-movi 92)
(define jit-code-extr-c 93)
(define jit-code-extr-uc 94)
(define jit-code-extr-s 95)
(define jit-code-extr-us 96)
(define jit-code-extr-i 97)
(define jit-code-extr-ui 98)
(define jit-code-htonr-us 99)
(define jit-code-htonr-ui 100)
(define jit-code-htonr-ul 101)
(define jit-code-ldr-c 102)
(define jit-code-ldi-c 103)
(define jit-code-ldr-uc 104)
(define jit-code-ldi-uc 105)
(define jit-code-ldr-s 106)
(define jit-code-ldi-s 107)
(define jit-code-ldr-us 108)
(define jit-code-ldi-us 109)
(define jit-code-ldr-i 110)
(define jit-code-ldi-i 111)
(define jit-code-ldr-ui 112)
(define jit-code-ldi-ui 113)
(define jit-code-ldr-l 114)
(define jit-code-ldi-l 115)
(define jit-code-ldxr-c 116)
(define jit-code-ldxi-c 117)
(define jit-code-ldxr-uc 118)
(define jit-code-ldxi-uc 119)
(define jit-code-ldxr-s 120)
(define jit-code-ldxi-s 121)
(define jit-code-ldxr-us 122)
(define jit-code-ldxi-us 123)
(define jit-code-ldxr-i 124)
(define jit-code-ldxi-i 125)
(define jit-code-ldxr-ui 126)
(define jit-code-ldxi-ui 127)
(define jit-code-ldxr-l 128)
(define jit-code-ldxi-l 129)
(define jit-code-str-c 130)
(define jit-code-sti-c 131)
(define jit-code-str-s 132)
(define jit-code-sti-s 133)
(define jit-code-str-i 134)
(define jit-code-sti-i 135)
(define jit-code-str-l 136)
(define jit-code-sti-l 137)
(define jit-code-stxr-c 138)
(define jit-code-stxi-c 139)
(define jit-code-stxr-s 140)
(define jit-code-stxi-s 141)
(define jit-code-stxr-i 142)
(define jit-code-stxi-i 143)
(define jit-code-stxr-l 144)
(define jit-code-stxi-l 145)
(define jit-code-bltr 146)
(define jit-code-blti 147)
(define jit-code-bltr-u 148)
(define jit-code-blti-u 149)
(define jit-code-bler 150)
(define jit-code-blei 151)
(define jit-code-bler-u 152)
(define jit-code-blei-u 153)
(define jit-code-beqr 154)
(define jit-code-beqi 155)
(define jit-code-bger 156)
(define jit-code-bgei 157)
(define jit-code-bger-u 158)
(define jit-code-bgei-u 159)
(define jit-code-bgtr 160)
(define jit-code-bgti 161)
(define jit-code-bgtr-u 162)
(define jit-code-bgti-u 163)
(define jit-code-bner 164)
(define jit-code-bnei 165)
(define jit-code-bmsr 166)
(define jit-code-bmsi 167)
(define jit-code-bmcr 168)
(define jit-code-bmci 169)
(define jit-code-boaddr 170)
(define jit-code-boaddi 171)
(define jit-code-boaddr-u 172)
(define jit-code-boaddi-u 173)
(define jit-code-bxaddr 174)
(define jit-code-bxaddi 175)
(define jit-code-bxaddr-u 176)
(define jit-code-bxaddi-u 177)
(define jit-code-bosubr 178)
(define jit-code-bosubi 179)
(define jit-code-bosubr-u 180)
(define jit-code-bosubi-u 181)
(define jit-code-bxsubr 182)
(define jit-code-bxsubi 183)
(define jit-code-bxsubr-u 184)
(define jit-code-bxsubi-u 185)
(define jit-code-jmpr 186)
(define jit-code-jmpi 187)
(define jit-code-callr 188)
(define jit-code-calli 189)
(define jit-code-prepare 190)
(define jit-code-pushargr 191)
(define jit-code-pushargi 192)
(define jit-code-finishr 193)
(define jit-code-finishi 194)
(define jit-code-ret 195)
(define jit-code-retr 196)
(define jit-code-reti 197)
(define jit-code-retval-c 198)
(define jit-code-retval-uc 199)
(define jit-code-retval-s 200)
(define jit-code-retval-us 201)
(define jit-code-retval-i 202)
(define jit-code-retval-ui 203)
(define jit-code-retval-l 204)
(define jit-code-epilog 205)
(define jit-code-arg-f 206)
(define jit-code-getarg-f 207)
(define jit-code-putargr-f 208)
(define jit-code-putargi-f 209)
(define jit-code-addr-f 210)
(define jit-code-addi-f 211)
(define jit-code-subr-f 212)
(define jit-code-subi-f 213)
(define jit-code-rsbi-f 214)
(define jit-code-mulr-f 215)
(define jit-code-muli-f 216)
(define jit-code-divr-f 217)
(define jit-code-divi-f 218)
(define jit-code-negr-f 219)
(define jit-code-absr-f 220)
(define jit-code-sqrtr-f 221)
(define jit-code-ltr-f 222)
(define jit-code-lti-f 223)
(define jit-code-ler-f 224)
(define jit-code-lei-f 225)
(define jit-code-eqr-f 226)
(define jit-code-eqi-f 227)
(define jit-code-ger-f 228)
(define jit-code-gei-f 229)
(define jit-code-gtr-f 230)
(define jit-code-gti-f 231)
(define jit-code-ner-f 232)
(define jit-code-nei-f 233)
(define jit-code-unltr-f 234)
(define jit-code-unlti-f 235)
(define jit-code-unler-f 236)
(define jit-code-unlei-f 237)
(define jit-code-uneqr-f 238)
(define jit-code-uneqi-f 239)
(define jit-code-unger-f 240)
(define jit-code-ungei-f 241)
(define jit-code-ungtr-f 242)
(define jit-code-ungti-f 243)
(define jit-code-ltgtr-f 244)
(define jit-code-ltgti-f 245)
(define jit-code-ordr-f 246)
(define jit-code-ordi-f 247)
(define jit-code-unordr-f 248)
(define jit-code-unordi-f 249)
(define jit-code-truncr-f-i 250)
(define jit-code-truncr-f-l 251)
(define jit-code-extr-f 252)
(define jit-code-extr-d-f 253)
(define jit-code-movr-f 254)
(define jit-code-movi-f 255)
(define jit-code-ldr-f 256)
(define jit-code-ldi-f 257)
(define jit-code-ldxr-f 258)
(define jit-code-ldxi-f 259)
(define jit-code-str-f 260)
(define jit-code-sti-f 261)
(define jit-code-stxr-f 262)
(define jit-code-stxi-f 263)
(define jit-code-bltr-f 264)
(define jit-code-blti-f 265)
(define jit-code-bler-f 266)
(define jit-code-blei-f 267)
(define jit-code-beqr-f 268)
(define jit-code-beqi-f 269)
(define jit-code-bger-f 270)
(define jit-code-bgei-f 271)
(define jit-code-bgtr-f 272)
(define jit-code-bgti-f 273)
(define jit-code-bner-f 274)
(define jit-code-bnei-f 275)
(define jit-code-bunltr-f 276)
(define jit-code-bunlti-f 277)
(define jit-code-bunler-f 278)
(define jit-code-bunlei-f 279)
(define jit-code-buneqr-f 280)
(define jit-code-buneqi-f 281)
(define jit-code-bunger-f 282)
(define jit-code-bungei-f 283)
(define jit-code-bungtr-f 284)
(define jit-code-bungti-f 285)
(define jit-code-bltgtr-f 286)
(define jit-code-bltgti-f 287)
(define jit-code-bordr-f 288)
(define jit-code-bordi-f 289)
(define jit-code-bunordr-f 290)
(define jit-code-bunordi-f 291)
(define jit-code-pushargr-f 292)
(define jit-code-pushargi-f 293)
(define jit-code-retr-f 294)
(define jit-code-reti-f 295)
(define jit-code-retval-f 296)
(define jit-code-arg-d 297)
(define jit-code-getarg-d 298)
(define jit-code-putargr-d 299)
(define jit-code-putargi-d 300)
(define jit-code-addr-d 301)
(define jit-code-addi-d 302)
(define jit-code-subr-d 303)
(define jit-code-subi-d 304)
(define jit-code-rsbi-d 305)
(define jit-code-mulr-d 306)
(define jit-code-muli-d 307)
(define jit-code-divr-d 308)
(define jit-code-divi-d 309)
(define jit-code-negr-d 310)
(define jit-code-absr-d 311)
(define jit-code-sqrtr-d 312)
(define jit-code-ltr-d 313)
(define jit-code-lti-d 314)
(define jit-code-ler-d 315)
(define jit-code-lei-d 316)
(define jit-code-eqr-d 317)
(define jit-code-eqi-d 318)
(define jit-code-ger-d 319)
(define jit-code-gei-d 320)
(define jit-code-gtr-d 321)
(define jit-code-gti-d 322)
(define jit-code-ner-d 323)
(define jit-code-nei-d 324)
(define jit-code-unltr-d 325)
(define jit-code-unlti-d 326)
(define jit-code-unler-d 327)
(define jit-code-unlei-d 328)
(define jit-code-uneqr-d 329)
(define jit-code-uneqi-d 330)
(define jit-code-unger-d 331)
(define jit-code-ungei-d 332)
(define jit-code-ungtr-d 333)
(define jit-code-ungti-d 334)
(define jit-code-ltgtr-d 335)
(define jit-code-ltgti-d 336)
(define jit-code-ordr-d 337)
(define jit-code-ordi-d 338)
(define jit-code-unordr-d 339)
(define jit-code-unordi-d 340)
(define jit-code-truncr-d-i 341)
(define jit-code-truncr-d-l 342)
(define jit-code-extr-d 343)
(define jit-code-extr-f-d 344)
(define jit-code-movr-d 345)
(define jit-code-movi-d 346)
(define jit-code-ldr-d 347)
(define jit-code-ldi-d 348)
(define jit-code-ldxr-d 349)
(define jit-code-ldxi-d 350)
(define jit-code-str-d 351)
(define jit-code-sti-d 352)
(define jit-code-stxr-d 353)
(define jit-code-stxi-d 354)
(define jit-code-bltr-d 355)
(define jit-code-blti-d 356)
(define jit-code-bler-d 357)
(define jit-code-blei-d 358)
(define jit-code-beqr-d 359)
(define jit-code-beqi-d 360)
(define jit-code-bger-d 361)
(define jit-code-bgei-d 362)
(define jit-code-bgtr-d 363)
(define jit-code-bgti-d 364)
(define jit-code-bner-d 365)
(define jit-code-bnei-d 366)
(define jit-code-bunltr-d 367)
(define jit-code-bunlti-d 368)
(define jit-code-bunler-d 369)
(define jit-code-bunlei-d 370)
(define jit-code-buneqr-d 371)
(define jit-code-buneqi-d 372)
(define jit-code-bunger-d 373)
(define jit-code-bungei-d 374)
(define jit-code-bungtr-d 375)
(define jit-code-bungti-d 376)
(define jit-code-bltgtr-d 377)
(define jit-code-bltgti-d 378)
(define jit-code-bordr-d 379)
(define jit-code-bordi-d 380)
(define jit-code-bunordr-d 381)
(define jit-code-bunordi-d 382)
(define jit-code-pushargr-d 383)
(define jit-code-pushargi-d 384)
(define jit-code-retr-d 385)
(define jit-code-reti-d 386)
(define jit-code-retval-d 387)
(define jit-code-movr-w-f 388)
(define jit-code-movr-ww-d 389)
(define jit-code-movr-w-d 390)
(define jit-code-movr-f-w 391)
(define jit-code-movi-f-w 392)
(define jit-code-movr-d-ww 393)
(define jit-code-movi-d-ww 394)
(define jit-code-movr-d-w 395)
(define jit-code-movi-d-w 396)


;;;
;;; C macro -> Scheme procedure
;;;

(define (jit-addr u v w)
  (jit-new-node-www jit-code-addr u v w))

(define (jit-addi u v w)
  (jit-new-node-www jit-code-addi u v w))

(define (jit-addcr u v w)
  (jit-new-node-www jit-code-addcr u v w))

(define (jit-addci u v w)
  (jit-new-node-www jit-code-addci u v w))

(define (jit-addxr u v w)
  (jit-new-node-www jit-code-addxr u v w))

(define (jit-addxi u v w)
  (jit-new-node-www jit-code-addxi u v w))

(define (jit-subr u v w)
  (jit-new-node-www jit-code-subr u v w))

(define (jit-subi u v w)
  (jit-new-node-www jit-code-subi u v w))

(define (jit-subcr u v w)
  (jit-new-node-www jit-code-subcr u v w))

(define (jit-subci u v w)
  (jit-new-node-www jit-code-subci u v w))

(define (jit-subxr u v w)
  (jit-new-node-www jit-code-subxr u v w))

(define (jit-subxi u v w)
  (jit-new-node-www jit-code-subxi u v w))

(define (jit-rsbi u v w)
  (jit-new-node-www jit-code-rsbi u v w))

(define (jit-mulr u v w)
  (jit-new-node-www jit-code-mulr u v w))

(define (jit-muli u v w)
  (jit-new-node-www jit-code-muli u v w))

(define (jit-qmulr l h v w)
  (jit-new-node-qww jit-code-qmulr l h v w))

(define (jit-qmuli l h v w)
  (jit-new-node-qww jit-code-qmuli l h v w))

(define (jit-qmulr-u l h v w)
  (jit-new-node-qww jit-code-qmulr-u l h v w))

(define (jit-qmuli-u l h v w)
  (jit-new-node-qww jit-code-qmuli-u l h v w))

(define (jit-divr u v w)
  (jit-new-node-www jit-code-divr u v w))

(define (jit-divi u v w)
  (jit-new-node-www jit-code-divi u v w))

(define (jit-divr-u u v w)
  (jit-new-node-www jit-code-divr-u u v w))

(define (jit-divi-u u v w)
  (jit-new-node-www jit-code-divi-u u v w))

(define (jit-qdivr l h v w)
  (jit-new-node-qww jit-code-qdivr l h v w))

(define (jit-qdivi l h v w)
  (jit-new-node-qww jit-code-qdivi l h v w))

(define (jit-qdivr-u l h v w)
  (jit-new-node-qww jit-code-qdivr-u l h v w))

(define (jit-qdivi-u l h v w)
  (jit-new-node-qww jit-code-qdivi-u l h v w))

(define (jit-remr u v w)
  (jit-new-node-www jit-code-remr u v w))

(define (jit-remi u v w)
  (jit-new-node-www jit-code-remi u v w))

(define (jit-remr-u u v w)
  (jit-new-node-www jit-code-remr-u u v w))

(define (jit-remi-u u v w)
  (jit-new-node-www jit-code-remi-u u v w))

(define (jit-andr u v w)
  (jit-new-node-www jit-code-andr u v w))

(define (jit-andi u v w)
  (jit-new-node-www jit-code-andi u v w))

(define (jit-orr u v w)
  (jit-new-node-www jit-code-orr u v w))

(define (jit-ori u v w)
  (jit-new-node-www jit-code-ori u v w))

(define (jit-xorr u v w)
  (jit-new-node-www jit-code-xorr u v w))

(define (jit-xori u v w)
  (jit-new-node-www jit-code-xori u v w))

(define (jit-lshr u v w)
  (jit-new-node-www jit-code-lshr u v w))

(define (jit-lshi u v w)
  (jit-new-node-www jit-code-lshi u v w))

(define (jit-rshr u v w)
  (jit-new-node-www jit-code-rshr u v w))

(define (jit-rshi u v w)
  (jit-new-node-www jit-code-rshi u v w))

(define (jit-rshr-u u v w)
  (jit-new-node-www jit-code-rshr-u u v w))

(define (jit-rshi-u u v w)
  (jit-new-node-www jit-code-rshi-u u v w))

(define (jit-negr u v)
  (jit-new-node-ww jit-code-negr u v))

(define (jit-comr u v)
  (jit-new-node-ww jit-code-comr u v))

(define (jit-ltr u v w)
  (jit-new-node-www jit-code-ltr u v w))

(define (jit-lti u v w)
  (jit-new-node-www jit-code-lti u v w))

(define (jit-ltr-u u v w)
  (jit-new-node-www jit-code-ltr-u u v w))

(define (jit-lti-u u v w)
  (jit-new-node-www jit-code-lti-u u v w))

(define (jit-ler u v w)
  (jit-new-node-www jit-code-ler u v w))

(define (jit-lei u v w)
  (jit-new-node-www jit-code-lei u v w))

(define (jit-ler-u u v w)
  (jit-new-node-www jit-code-ler-u u v w))

(define (jit-lei-u u v w)
  (jit-new-node-www jit-code-lei-u u v w))

(define (jit-eqr u v w)
  (jit-new-node-www jit-code-eqr u v w))

(define (jit-eqi u v w)
  (jit-new-node-www jit-code-eqi u v w))

(define (jit-ger u v w)
  (jit-new-node-www jit-code-ger u v w))

(define (jit-gei u v w)
  (jit-new-node-www jit-code-gei u v w))

(define (jit-ger-u u v w)
  (jit-new-node-www jit-code-ger-u u v w))

(define (jit-gei-u u v w)
  (jit-new-node-www jit-code-gei-u u v w))

(define (jit-gtr u v w)
  (jit-new-node-www jit-code-gtr u v w))

(define (jit-gti u v w)
  (jit-new-node-www jit-code-gti u v w))

(define (jit-gtr-u u v w)
  (jit-new-node-www jit-code-gtr-u u v w))

(define (jit-gti-u u v w)
  (jit-new-node-www jit-code-gti-u u v w))

(define (jit-ner u v w)
  (jit-new-node-www jit-code-ner u v w))

(define (jit-nei u v w)
  (jit-new-node-www jit-code-nei u v w))

(define (jit-movr u v)
  (jit-new-node-ww jit-code-movr u v))

(define (jit-movi u v)
  (jit-new-node-ww jit-code-movi u v))

(define (jit-extr-c u v)
  (jit-new-node-ww jit-code-extr-c u v))

(define (jit-extr-uc u v)
  (jit-new-node-ww jit-code-extr-uc u v))

(define (jit-extr-s u v)
  (jit-new-node-ww jit-code-extr-s u v))

(define (jit-extr-us u v)
  (jit-new-node-ww jit-code-extr-us u v))

(define (jit-extr-i u v)
  (jit-new-node-ww jit-code-extr-i u v))

(define (jit-extr-ui u v)
  (jit-new-node-ww jit-code-extr-ui u v))

(define (jit-htonr-us u v)
  (jit-new-node-ww jit-code-htonr-us u v))

(define (jit-htonr-us u v)
  (jit-new-node-ww jit-code-htonr-us u v))

(define (jit-htonr-ui u v)
  (jit-new-node-ww jit-code-htonr-ui u v))

(define (jit-htonr-ui u v)
  (jit-new-node-ww jit-code-htonr-ui u v))

(define (jit-htonr-ui u v)
  (jit-new-node-ww jit-code-htonr-ui u v))

(define (jit-htonr-ui u v)
  (jit-new-node-ww jit-code-htonr-ui u v))

(define (jit-htonr-ul u v)
  (jit-new-node-ww jit-code-htonr-ul u v))

(define (jit-htonr-ul u v)
  (jit-new-node-ww jit-code-htonr-ul u v))

(define (jit-htonr-ul u v)
  (jit-new-node-ww jit-code-htonr-ul u v))

(define (jit-htonr-ul u v)
  (jit-new-node-ww jit-code-htonr-ul u v))

(define (jit-ldr-c u v)
  (jit-new-node-ww jit-code-ldr-c u v))

(define (jit-ldi-c u v)
  (jit-new-node-wp jit-code-ldi-c u v))

(define (jit-ldr-uc u v)
  (jit-new-node-ww jit-code-ldr-uc u v))

(define (jit-ldi-uc u v)
  (jit-new-node-wp jit-code-ldi-uc u v))

(define (jit-ldr-s u v)
  (jit-new-node-ww jit-code-ldr-s u v))

(define (jit-ldi-s u v)
  (jit-new-node-wp jit-code-ldi-s u v))

(define (jit-ldr-us u v)
  (jit-new-node-ww jit-code-ldr-us u v))

(define (jit-ldi-us u v)
  (jit-new-node-wp jit-code-ldi-us u v))

(define (jit-ldr-i u v)
  (jit-new-node-ww jit-code-ldr-i u v))

(define (jit-ldi-i u v)
  (jit-new-node-wp jit-code-ldi-i u v))

(define (jit-ldr-ui u v)
  (jit-new-node-ww jit-code-ldr-ui u v))

(define (jit-ldi-ui u v)
  (jit-new-node-wp jit-code-ldi-ui u v))

(define (jit-ldr-l u v)
  (jit-new-node-ww jit-code-ldr-l u v))

(define (jit-ldi-l u v)
  (jit-new-node-wp jit-code-ldi-l u v))

(define (jit-ldxr-c u v w)
  (jit-new-node-www jit-code-ldxr-c u v w))

(define (jit-ldxi-c u v w)
  (jit-new-node-www jit-code-ldxi-c u v w))

(define (jit-ldxr-uc u v w)
  (jit-new-node-www jit-code-ldxr-uc u v w))

(define (jit-ldxi-uc u v w)
  (jit-new-node-www jit-code-ldxi-uc u v w))

(define (jit-ldxr-s u v w)
  (jit-new-node-www jit-code-ldxr-s u v w))

(define (jit-ldxi-s u v w)
  (jit-new-node-www jit-code-ldxi-s u v w))

(define (jit-ldxr-us u v w)
  (jit-new-node-www jit-code-ldxr-us u v w))

(define (jit-ldxi-us u v w)
  (jit-new-node-www jit-code-ldxi-us u v w))

(define (jit-ldxr-i u v w)
  (jit-new-node-www jit-code-ldxr-i u v w))

(define (jit-ldxi-i u v w)
  (jit-new-node-www jit-code-ldxi-i u v w))

(define (jit-ldxr-ui u v w)
  (jit-new-node-www jit-code-ldxr-ui u v w))

(define (jit-ldxi-ui u v w)
  (jit-new-node-www jit-code-ldxi-ui u v w))

(define (jit-ldxr-l u v w)
  (jit-new-node-www jit-code-ldxr-l u v w))

(define (jit-ldxi-l u v w)
  (jit-new-node-www jit-code-ldxi-l u v w))

(define (jit-str-c u v)
  (jit-new-node-ww jit-code-str-c u v))

(define (jit-sti-c u v)
  (jit-new-node-pw jit-code-sti-c u v))

(define (jit-str-s u v)
  (jit-new-node-ww jit-code-str-s u v))

(define (jit-sti-s u v)
  (jit-new-node-pw jit-code-sti-s u v))

(define (jit-str-i u v)
  (jit-new-node-ww jit-code-str-i u v))

(define (jit-sti-i u v)
  (jit-new-node-pw jit-code-sti-i u v))

(define (jit-str-l u v)
  (jit-new-node-ww jit-code-str-l u v))

(define (jit-sti-l u v)
  (jit-new-node-pw jit-code-sti-l u v))

(define (jit-stxr-c u v w)
  (jit-new-node-www jit-code-stxr-c u v w))

(define (jit-stxi-c u v w)
  (jit-new-node-www jit-code-stxi-c u v w))

(define (jit-stxr-s u v w)
  (jit-new-node-www jit-code-stxr-s u v w))

(define (jit-stxi-s u v w)
  (jit-new-node-www jit-code-stxi-s u v w))

(define (jit-stxr-i u v w)
  (jit-new-node-www jit-code-stxr-i u v w))

(define (jit-stxi-i u v w)
  (jit-new-node-www jit-code-stxi-i u v w))

(define (jit-stxr-l u v w)
  (jit-new-node-www jit-code-stxr-l u v w))

(define (jit-stxi-l u v w)
  (jit-new-node-www jit-code-stxi-l u v w))

(define (jit-bltr v w)
  (jit-new-node-pww jit-code-bltr %null-pointer v w))

(define (jit-blti v w)
  (jit-new-node-pww jit-code-blti %null-pointer v w))

(define (jit-bltr-u v w)
  (jit-new-node-pww jit-code-bltr-u %null-pointer v w))

(define (jit-blti-u v w)
  (jit-new-node-pww jit-code-blti-u %null-pointer v w))

(define (jit-bler v w)
  (jit-new-node-pww jit-code-bler %null-pointer v w))

(define (jit-blei v w)
  (jit-new-node-pww jit-code-blei %null-pointer v w))

(define (jit-bler-u v w)
  (jit-new-node-pww jit-code-bler-u %null-pointer v w))

(define (jit-blei-u v w)
  (jit-new-node-pww jit-code-blei-u %null-pointer v w))

(define (jit-beqr v w)
  (jit-new-node-pww jit-code-beqr %null-pointer v w))

(define (jit-beqi v w)
  (jit-new-node-pww jit-code-beqi %null-pointer v w))

(define (jit-bger v w)
  (jit-new-node-pww jit-code-bger %null-pointer v w))

(define (jit-bgei v w)
  (jit-new-node-pww jit-code-bgei %null-pointer v w))

(define (jit-bger-u v w)
  (jit-new-node-pww jit-code-bger-u %null-pointer v w))

(define (jit-bgei-u v w)
  (jit-new-node-pww jit-code-bgei-u %null-pointer v w))

(define (jit-bgtr v w)
  (jit-new-node-pww jit-code-bgtr %null-pointer v w))

(define (jit-bgti v w)
  (jit-new-node-pww jit-code-bgti %null-pointer v w))

(define (jit-bgtr-u v w)
  (jit-new-node-pww jit-code-bgtr-u %null-pointer v w))

(define (jit-bgti-u v w)
  (jit-new-node-pww jit-code-bgti-u %null-pointer v w))

(define (jit-bner v w)
  (jit-new-node-pww jit-code-bner %null-pointer v w))

(define (jit-bnei v w)
  (jit-new-node-pww jit-code-bnei %null-pointer v w))

(define (jit-bmsr v w)
  (jit-new-node-pww jit-code-bmsr %null-pointer v w))

(define (jit-bmsi v w)
  (jit-new-node-pww jit-code-bmsi %null-pointer v w))

(define (jit-bmcr v w)
  (jit-new-node-pww jit-code-bmcr %null-pointer v w))

(define (jit-bmci v w)
  (jit-new-node-pww jit-code-bmci %null-pointer v w))

(define (jit-boaddr v w)
  (jit-new-node-pww jit-code-boaddr %null-pointer v w))

(define (jit-boaddi v w)
  (jit-new-node-pww jit-code-boaddi %null-pointer v w))

(define (jit-boaddr-u v w)
  (jit-new-node-pww jit-code-boaddr-u %null-pointer v w))

(define (jit-boaddi-u v w)
  (jit-new-node-pww jit-code-boaddi-u %null-pointer v w))

(define (jit-bxaddr v w)
  (jit-new-node-pww jit-code-bxaddr %null-pointer v w))

(define (jit-bxaddi v w)
  (jit-new-node-pww jit-code-bxaddi %null-pointer v w))

(define (jit-bxaddr-u v w)
  (jit-new-node-pww jit-code-bxaddr-u %null-pointer v w))

(define (jit-bxaddi-u v w)
  (jit-new-node-pww jit-code-bxaddi-u %null-pointer v w))

(define (jit-bosubr v w)
  (jit-new-node-pww jit-code-bosubr %null-pointer v w))

(define (jit-bosubi v w)
  (jit-new-node-pww jit-code-bosubi %null-pointer v w))

(define (jit-bosubr-u v w)
  (jit-new-node-pww jit-code-bosubr-u %null-pointer v w))

(define (jit-bosubi-u v w)
  (jit-new-node-pww jit-code-bosubi-u %null-pointer v w))

(define (jit-bxsubr v w)
  (jit-new-node-pww jit-code-bxsubr %null-pointer v w))

(define (jit-bxsubi v w)
  (jit-new-node-pww jit-code-bxsubi %null-pointer v w))

(define (jit-bxsubr-u v w)
  (jit-new-node-pww jit-code-bxsubr-u %null-pointer v w))

(define (jit-bxsubi-u v w)
  (jit-new-node-pww jit-code-bxsubi-u %null-pointer v w))

(define (jit-jmpr u)
  (jit-new-node-w jit-code-jmpr u))

(define (jit-jmpi )
  (jit-new-node-p jit-code-jmpi %null-pointer))

(define (jit-callr u)
  (jit-new-node-w jit-code-callr u))

(define (jit-calli u)
  (jit-new-node-p jit-code-calli u))

(define (jit-addr-f u v w)
  (jit-new-node-www jit-code-addr-f u v w))

(define (jit-addi-f u v w)
  (jit-new-node-wwf jit-code-addi-f u v w))

(define (jit-subr-f u v w)
  (jit-new-node-www jit-code-subr-f u v w))

(define (jit-subi-f u v w)
  (jit-new-node-wwf jit-code-subi-f u v w))

(define (jit-rsbi-f u v w)
  (jit-new-node-wwf jit-code-rsbi-f u v w))

(define (jit-mulr-f u v w)
  (jit-new-node-www jit-code-mulr-f u v w))

(define (jit-muli-f u v w)
  (jit-new-node-wwf jit-code-muli-f u v w))

(define (jit-divr-f u v w)
  (jit-new-node-www jit-code-divr-f u v w))

(define (jit-divi-f u v w)
  (jit-new-node-wwf jit-code-divi-f u v w))

(define (jit-negr-f u v)
  (jit-new-node-ww jit-code-negr-f u v))

(define (jit-absr-f u v)
  (jit-new-node-ww jit-code-absr-f u v))

(define (jit-sqrtr-f u v)
  (jit-new-node-ww jit-code-sqrtr-f u v))

(define (jit-ltr-f u v w)
  (jit-new-node-www jit-code-ltr-f u v w))

(define (jit-lti-f u v w)
  (jit-new-node-wwf jit-code-lti-f u v w))

(define (jit-ler-f u v w)
  (jit-new-node-www jit-code-ler-f u v w))

(define (jit-lei-f u v w)
  (jit-new-node-wwf jit-code-lei-f u v w))

(define (jit-eqr-f u v w)
  (jit-new-node-www jit-code-eqr-f u v w))

(define (jit-eqi-f u v w)
  (jit-new-node-wwf jit-code-eqi-f u v w))

(define (jit-ger-f u v w)
  (jit-new-node-www jit-code-ger-f u v w))

(define (jit-gei-f u v w)
  (jit-new-node-wwf jit-code-gei-f u v w))

(define (jit-gtr-f u v w)
  (jit-new-node-www jit-code-gtr-f u v w))

(define (jit-gti-f u v w)
  (jit-new-node-wwf jit-code-gti-f u v w))

(define (jit-ner-f u v w)
  (jit-new-node-www jit-code-ner-f u v w))

(define (jit-nei-f u v w)
  (jit-new-node-wwf jit-code-nei-f u v w))

(define (jit-unltr-f u v w)
  (jit-new-node-www jit-code-unltr-f u v w))

(define (jit-unlti-f u v w)
  (jit-new-node-wwf jit-code-unlti-f u v w))

(define (jit-unler-f u v w)
  (jit-new-node-www jit-code-unler-f u v w))

(define (jit-unlei-f u v w)
  (jit-new-node-wwf jit-code-unlei-f u v w))

(define (jit-uneqr-f u v w)
  (jit-new-node-www jit-code-uneqr-f u v w))

(define (jit-uneqi-f u v w)
  (jit-new-node-wwf jit-code-uneqi-f u v w))

(define (jit-unger-f u v w)
  (jit-new-node-www jit-code-unger-f u v w))

(define (jit-ungei-f u v w)
  (jit-new-node-wwf jit-code-ungei-f u v w))

(define (jit-ungtr-f u v w)
  (jit-new-node-www jit-code-ungtr-f u v w))

(define (jit-ungti-f u v w)
  (jit-new-node-wwf jit-code-ungti-f u v w))

(define (jit-ltgtr-f u v w)
  (jit-new-node-www jit-code-ltgtr-f u v w))

(define (jit-ltgti-f u v w)
  (jit-new-node-wwf jit-code-ltgti-f u v w))

(define (jit-ordr-f u v w)
  (jit-new-node-www jit-code-ordr-f u v w))

(define (jit-ordi-f u v w)
  (jit-new-node-wwf jit-code-ordi-f u v w))

(define (jit-unordr-f u v w)
  (jit-new-node-www jit-code-unordr-f u v w))

(define (jit-unordi-f u v w)
  (jit-new-node-wwf jit-code-unordi-f u v w))

(define (jit-truncr-f-i u v)
  (jit-new-node-ww jit-code-truncr-f-i u v))

(define (jit-truncr-f-l u v)
  (jit-new-node-ww jit-code-truncr-f-l u v))

(define (jit-extr-f u v)
  (jit-new-node-ww jit-code-extr-f u v))

(define (jit-extr-d-f u v)
  (jit-new-node-ww jit-code-extr-d-f u v))

(define (jit-movr-f u v)
  (jit-new-node-ww jit-code-movr-f u v))

(define (jit-movi-f u v)
  (jit-new-node-wf jit-code-movi-f u v))

(define (jit-ldr-f u v)
  (jit-new-node-ww jit-code-ldr-f u v))

(define (jit-ldi-f u v)
  (jit-new-node-wp jit-code-ldi-f u v))

(define (jit-ldxr-f u v w)
  (jit-new-node-www jit-code-ldxr-f u v w))

(define (jit-ldxi-f u v w)
  (jit-new-node-www jit-code-ldxi-f u v w))

(define (jit-str-f u v)
  (jit-new-node-ww jit-code-str-f u v))

(define (jit-sti-f u v)
  (jit-new-node-pw jit-code-sti-f u v))

(define (jit-stxr-f u v w)
  (jit-new-node-www jit-code-stxr-f u v w))

(define (jit-stxi-f u v w)
  (jit-new-node-www jit-code-stxi-f u v w))

(define (jit-bltr-f v w)
  (jit-new-node-pww jit-code-bltr-f %null-pointer v w))

(define (jit-blti-f v w)
  (jit-new-node-pwf jit-code-blti-f %null-pointer v w))

(define (jit-bler-f v w)
  (jit-new-node-pww jit-code-bler-f %null-pointer v w))

(define (jit-blei-f v w)
  (jit-new-node-pwf jit-code-blei-f %null-pointer v w))

(define (jit-beqr-f v w)
  (jit-new-node-pww jit-code-beqr-f %null-pointer v w))

(define (jit-beqi-f v w)
  (jit-new-node-pwf jit-code-beqi-f %null-pointer v w))

(define (jit-bger-f v w)
  (jit-new-node-pww jit-code-bger-f %null-pointer v w))

(define (jit-bgei-f v w)
  (jit-new-node-pwf jit-code-bgei-f %null-pointer v w))

(define (jit-bgtr-f v w)
  (jit-new-node-pww jit-code-bgtr-f %null-pointer v w))

(define (jit-bgti-f v w)
  (jit-new-node-pwf jit-code-bgti-f %null-pointer v w))

(define (jit-bner-f v w)
  (jit-new-node-pww jit-code-bner-f %null-pointer v w))

(define (jit-bnei-f v w)
  (jit-new-node-pwf jit-code-bnei-f %null-pointer v w))

(define (jit-bunltr-f v w)
  (jit-new-node-pww jit-code-bunltr-f %null-pointer v w))

(define (jit-bunlti-f v w)
  (jit-new-node-pwf jit-code-bunlti-f %null-pointer v w))

(define (jit-bunler-f v w)
  (jit-new-node-pww jit-code-bunler-f %null-pointer v w))

(define (jit-bunlei-f v w)
  (jit-new-node-pwf jit-code-bunlei-f %null-pointer v w))

(define (jit-buneqr-f v w)
  (jit-new-node-pww jit-code-buneqr-f %null-pointer v w))

(define (jit-buneqi-f v w)
  (jit-new-node-pwf jit-code-buneqi-f %null-pointer v w))

(define (jit-bunger-f v w)
  (jit-new-node-pww jit-code-bunger-f %null-pointer v w))

(define (jit-bungei-f v w)
  (jit-new-node-pwf jit-code-bungei-f %null-pointer v w))

(define (jit-bungtr-f v w)
  (jit-new-node-pww jit-code-bungtr-f %null-pointer v w))

(define (jit-bungti-f v w)
  (jit-new-node-pwf jit-code-bungti-f %null-pointer v w))

(define (jit-bltgtr-f v w)
  (jit-new-node-pww jit-code-bltgtr-f %null-pointer v w))

(define (jit-bltgti-f v w)
  (jit-new-node-pwf jit-code-bltgti-f %null-pointer v w))

(define (jit-bordr-f v w)
  (jit-new-node-pww jit-code-bordr-f %null-pointer v w))

(define (jit-bordi-f v w)
  (jit-new-node-pwf jit-code-bordi-f %null-pointer v w))

(define (jit-bunordr-f v w)
  (jit-new-node-pww jit-code-bunordr-f %null-pointer v w))

(define (jit-bunordi-f v w)
  (jit-new-node-pwf jit-code-bunordi-f %null-pointer v w))

(define (jit-addr-d u v w)
  (jit-new-node-www jit-code-addr-d u v w))

(define (jit-addi-d u v w)
  (jit-new-node-wwd jit-code-addi-d u v w))

(define (jit-subr-d u v w)
  (jit-new-node-www jit-code-subr-d u v w))

(define (jit-subi-d u v w)
  (jit-new-node-wwd jit-code-subi-d u v w))

(define (jit-rsbi-d u v w)
  (jit-new-node-wwd jit-code-rsbi-d u v w))

(define (jit-mulr-d u v w)
  (jit-new-node-www jit-code-mulr-d u v w))

(define (jit-muli-d u v w)
  (jit-new-node-wwd jit-code-muli-d u v w))

(define (jit-divr-d u v w)
  (jit-new-node-www jit-code-divr-d u v w))

(define (jit-divi-d u v w)
  (jit-new-node-wwd jit-code-divi-d u v w))

(define (jit-negr-d u v)
  (jit-new-node-ww jit-code-negr-d u v))

(define (jit-absr-d u v)
  (jit-new-node-ww jit-code-absr-d u v))

(define (jit-sqrtr-d u v)
  (jit-new-node-ww jit-code-sqrtr-d u v))

(define (jit-ltr-d u v w)
  (jit-new-node-www jit-code-ltr-d u v w))

(define (jit-lti-d u v w)
  (jit-new-node-wwd jit-code-lti-d u v w))

(define (jit-ler-d u v w)
  (jit-new-node-www jit-code-ler-d u v w))

(define (jit-lei-d u v w)
  (jit-new-node-wwd jit-code-lei-d u v w))

(define (jit-eqr-d u v w)
  (jit-new-node-www jit-code-eqr-d u v w))

(define (jit-eqi-d u v w)
  (jit-new-node-wwd jit-code-eqi-d u v w))

(define (jit-ger-d u v w)
  (jit-new-node-www jit-code-ger-d u v w))

(define (jit-gei-d u v w)
  (jit-new-node-wwd jit-code-gei-d u v w))

(define (jit-gtr-d u v w)
  (jit-new-node-www jit-code-gtr-d u v w))

(define (jit-gti-d u v w)
  (jit-new-node-wwd jit-code-gti-d u v w))

(define (jit-ner-d u v w)
  (jit-new-node-www jit-code-ner-d u v w))

(define (jit-nei-d u v w)
  (jit-new-node-wwd jit-code-nei-d u v w))

(define (jit-unltr-d u v w)
  (jit-new-node-www jit-code-unltr-d u v w))

(define (jit-unlti-d u v w)
  (jit-new-node-wwd jit-code-unlti-d u v w))

(define (jit-unler-d u v w)
  (jit-new-node-www jit-code-unler-d u v w))

(define (jit-unlei-d u v w)
  (jit-new-node-wwd jit-code-unlei-d u v w))

(define (jit-uneqr-d u v w)
  (jit-new-node-www jit-code-uneqr-d u v w))

(define (jit-uneqi-d u v w)
  (jit-new-node-wwd jit-code-uneqi-d u v w))

(define (jit-unger-d u v w)
  (jit-new-node-www jit-code-unger-d u v w))

(define (jit-ungei-d u v w)
  (jit-new-node-wwd jit-code-ungei-d u v w))

(define (jit-ungtr-d u v w)
  (jit-new-node-www jit-code-ungtr-d u v w))

(define (jit-ungti-d u v w)
  (jit-new-node-wwd jit-code-ungti-d u v w))

(define (jit-ltgtr-d u v w)
  (jit-new-node-www jit-code-ltgtr-d u v w))

(define (jit-ltgti-d u v w)
  (jit-new-node-wwd jit-code-ltgti-d u v w))

(define (jit-ordr-d u v w)
  (jit-new-node-www jit-code-ordr-d u v w))

(define (jit-ordi-d u v w)
  (jit-new-node-wwd jit-code-ordi-d u v w))

(define (jit-unordr-d u v w)
  (jit-new-node-www jit-code-unordr-d u v w))

(define (jit-unordi-d u v w)
  (jit-new-node-wwd jit-code-unordi-d u v w))

(define (jit-truncr-d-i u v)
  (jit-new-node-ww jit-code-truncr-d-i u v))

(define (jit-truncr-d-l u v)
  (jit-new-node-ww jit-code-truncr-d-l u v))

(define (jit-extr-d u v)
  (jit-new-node-ww jit-code-extr-d u v))

(define (jit-extr-f-d u v)
  (jit-new-node-ww jit-code-extr-f-d u v))

(define (jit-movr-d u v)
  (jit-new-node-ww jit-code-movr-d u v))

(define (jit-movi-d u v)
  (jit-new-node-wd jit-code-movi-d u v))

(define (jit-ldr-d u v)
  (jit-new-node-ww jit-code-ldr-d u v))

(define (jit-ldi-d u v)
  (jit-new-node-wp jit-code-ldi-d u v))

(define (jit-ldxr-d u v w)
  (jit-new-node-www jit-code-ldxr-d u v w))

(define (jit-ldxi-d u v w)
  (jit-new-node-www jit-code-ldxi-d u v w))

(define (jit-str-d u v)
  (jit-new-node-ww jit-code-str-d u v))

(define (jit-sti-d u v)
  (jit-new-node-pw jit-code-sti-d u v))

(define (jit-stxr-d u v w)
  (jit-new-node-www jit-code-stxr-d u v w))

(define (jit-stxi-d u v w)
  (jit-new-node-www jit-code-stxi-d u v w))

(define (jit-bltr-d v w)
  (jit-new-node-pww jit-code-bltr-d %null-pointer v w))

(define (jit-blti-d v w)
  (jit-new-node-pwd jit-code-blti-d %null-pointer v w))

(define (jit-bler-d v w)
  (jit-new-node-pww jit-code-bler-d %null-pointer v w))

(define (jit-blei-d v w)
  (jit-new-node-pwd jit-code-blei-d %null-pointer v w))

(define (jit-beqr-d v w)
  (jit-new-node-pww jit-code-beqr-d %null-pointer v w))

(define (jit-beqi-d v w)
  (jit-new-node-pwd jit-code-beqi-d %null-pointer v w))

(define (jit-bger-d v w)
  (jit-new-node-pww jit-code-bger-d %null-pointer v w))

(define (jit-bgei-d v w)
  (jit-new-node-pwd jit-code-bgei-d %null-pointer v w))

(define (jit-bgtr-d v w)
  (jit-new-node-pww jit-code-bgtr-d %null-pointer v w))

(define (jit-bgti-d v w)
  (jit-new-node-pwd jit-code-bgti-d %null-pointer v w))

(define (jit-bner-d v w)
  (jit-new-node-pww jit-code-bner-d %null-pointer v w))

(define (jit-bnei-d v w)
  (jit-new-node-pwd jit-code-bnei-d %null-pointer v w))

(define (jit-bunltr-d v w)
  (jit-new-node-pww jit-code-bunltr-d %null-pointer v w))

(define (jit-bunlti-d v w)
  (jit-new-node-pwd jit-code-bunlti-d %null-pointer v w))

(define (jit-bunler-d v w)
  (jit-new-node-pww jit-code-bunler-d %null-pointer v w))

(define (jit-bunlei-d v w)
  (jit-new-node-pwd jit-code-bunlei-d %null-pointer v w))

(define (jit-buneqr-d v w)
  (jit-new-node-pww jit-code-buneqr-d %null-pointer v w))

(define (jit-buneqi-d v w)
  (jit-new-node-pwd jit-code-buneqi-d %null-pointer v w))

(define (jit-bunger-d v w)
  (jit-new-node-pww jit-code-bunger-d %null-pointer v w))

(define (jit-bungei-d v w)
  (jit-new-node-pwd jit-code-bungei-d %null-pointer v w))

(define (jit-bungtr-d v w)
  (jit-new-node-pww jit-code-bungtr-d %null-pointer v w))

(define (jit-bungti-d v w)
  (jit-new-node-pwd jit-code-bungti-d %null-pointer v w))

(define (jit-bltgtr-d v w)
  (jit-new-node-pww jit-code-bltgtr-d %null-pointer v w))

(define (jit-bltgti-d v w)
  (jit-new-node-pwd jit-code-bltgti-d %null-pointer v w))

(define (jit-bordr-d v w)
  (jit-new-node-pww jit-code-bordr-d %null-pointer v w))

(define (jit-bordi-d v w)
  (jit-new-node-pwd jit-code-bordi-d %null-pointer v w))

(define (jit-bunordr-d v w)
  (jit-new-node-pww jit-code-bunordr-d %null-pointer v w))

(define (jit-bunordi-d v w)
  (jit-new-node-pwd jit-code-bunordi-d %null-pointer v w))


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