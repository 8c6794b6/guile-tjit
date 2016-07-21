;;;; File: jit-code-t.scm
;;;; 
;;;; Generated from "/usr/include/lightning.h"

;;;
;;; Enum values of jit_code_t
;;;

(define-syntax jit-code-data (identifier-syntax 0))
(define-syntax jit-code-live (identifier-syntax 1))
(define-syntax jit-code-align (identifier-syntax 2))
(define-syntax jit-code-save (identifier-syntax 3))
(define-syntax jit-code-load (identifier-syntax 4))
(define-syntax jit-code-name (identifier-syntax 5))
(define-syntax jit-code-note (identifier-syntax 6))
(define-syntax jit-code-label (identifier-syntax 7))
(define-syntax jit-code-prolog (identifier-syntax 8))
(define-syntax jit-code-ellipsis (identifier-syntax 9))
(define-syntax jit-code-allocai (identifier-syntax 10))
(define-syntax jit-code-allocar (identifier-syntax 11))
(define-syntax jit-code-arg (identifier-syntax 12))
(define-syntax jit-code-getarg-c (identifier-syntax 13))
(define-syntax jit-code-getarg-uc (identifier-syntax 14))
(define-syntax jit-code-getarg-s (identifier-syntax 15))
(define-syntax jit-code-getarg-us (identifier-syntax 16))
(define-syntax jit-code-getarg-i (identifier-syntax 17))
(define-syntax jit-code-getarg-ui (identifier-syntax 18))
(define-syntax jit-code-getarg-l (identifier-syntax 19))
(define-syntax jit-code-putargr (identifier-syntax 20))
(define-syntax jit-code-putargi (identifier-syntax 21))
(define-syntax jit-code-va-start (identifier-syntax 22))
(define-syntax jit-code-va-arg (identifier-syntax 23))
(define-syntax jit-code-va-arg-d (identifier-syntax 24))
(define-syntax jit-code-va-end (identifier-syntax 25))
(define-syntax jit-code-addr (identifier-syntax 26))
(define-syntax jit-code-addi (identifier-syntax 27))
(define-syntax jit-code-addcr (identifier-syntax 28))
(define-syntax jit-code-addci (identifier-syntax 29))
(define-syntax jit-code-addxr (identifier-syntax 30))
(define-syntax jit-code-addxi (identifier-syntax 31))
(define-syntax jit-code-subr (identifier-syntax 32))
(define-syntax jit-code-subi (identifier-syntax 33))
(define-syntax jit-code-subcr (identifier-syntax 34))
(define-syntax jit-code-subci (identifier-syntax 35))
(define-syntax jit-code-subxr (identifier-syntax 36))
(define-syntax jit-code-subxi (identifier-syntax 37))
(define-syntax jit-code-rsbi (identifier-syntax 38))
(define-syntax jit-code-mulr (identifier-syntax 39))
(define-syntax jit-code-muli (identifier-syntax 40))
(define-syntax jit-code-qmulr (identifier-syntax 41))
(define-syntax jit-code-qmuli (identifier-syntax 42))
(define-syntax jit-code-qmulr-u (identifier-syntax 43))
(define-syntax jit-code-qmuli-u (identifier-syntax 44))
(define-syntax jit-code-divr (identifier-syntax 45))
(define-syntax jit-code-divi (identifier-syntax 46))
(define-syntax jit-code-divr-u (identifier-syntax 47))
(define-syntax jit-code-divi-u (identifier-syntax 48))
(define-syntax jit-code-qdivr (identifier-syntax 49))
(define-syntax jit-code-qdivi (identifier-syntax 50))
(define-syntax jit-code-qdivr-u (identifier-syntax 51))
(define-syntax jit-code-qdivi-u (identifier-syntax 52))
(define-syntax jit-code-remr (identifier-syntax 53))
(define-syntax jit-code-remi (identifier-syntax 54))
(define-syntax jit-code-remr-u (identifier-syntax 55))
(define-syntax jit-code-remi-u (identifier-syntax 56))
(define-syntax jit-code-andr (identifier-syntax 57))
(define-syntax jit-code-andi (identifier-syntax 58))
(define-syntax jit-code-orr (identifier-syntax 59))
(define-syntax jit-code-ori (identifier-syntax 60))
(define-syntax jit-code-xorr (identifier-syntax 61))
(define-syntax jit-code-xori (identifier-syntax 62))
(define-syntax jit-code-lshr (identifier-syntax 63))
(define-syntax jit-code-lshi (identifier-syntax 64))
(define-syntax jit-code-rshr (identifier-syntax 65))
(define-syntax jit-code-rshi (identifier-syntax 66))
(define-syntax jit-code-rshr-u (identifier-syntax 67))
(define-syntax jit-code-rshi-u (identifier-syntax 68))
(define-syntax jit-code-negr (identifier-syntax 69))
(define-syntax jit-code-comr (identifier-syntax 70))
(define-syntax jit-code-ltr (identifier-syntax 71))
(define-syntax jit-code-lti (identifier-syntax 72))
(define-syntax jit-code-ltr-u (identifier-syntax 73))
(define-syntax jit-code-lti-u (identifier-syntax 74))
(define-syntax jit-code-ler (identifier-syntax 75))
(define-syntax jit-code-lei (identifier-syntax 76))
(define-syntax jit-code-ler-u (identifier-syntax 77))
(define-syntax jit-code-lei-u (identifier-syntax 78))
(define-syntax jit-code-eqr (identifier-syntax 79))
(define-syntax jit-code-eqi (identifier-syntax 80))
(define-syntax jit-code-ger (identifier-syntax 81))
(define-syntax jit-code-gei (identifier-syntax 82))
(define-syntax jit-code-ger-u (identifier-syntax 83))
(define-syntax jit-code-gei-u (identifier-syntax 84))
(define-syntax jit-code-gtr (identifier-syntax 85))
(define-syntax jit-code-gti (identifier-syntax 86))
(define-syntax jit-code-gtr-u (identifier-syntax 87))
(define-syntax jit-code-gti-u (identifier-syntax 88))
(define-syntax jit-code-ner (identifier-syntax 89))
(define-syntax jit-code-nei (identifier-syntax 90))
(define-syntax jit-code-movr (identifier-syntax 91))
(define-syntax jit-code-movi (identifier-syntax 92))
(define-syntax jit-code-extr-c (identifier-syntax 93))
(define-syntax jit-code-extr-uc (identifier-syntax 94))
(define-syntax jit-code-extr-s (identifier-syntax 95))
(define-syntax jit-code-extr-us (identifier-syntax 96))
(define-syntax jit-code-extr-i (identifier-syntax 97))
(define-syntax jit-code-extr-ui (identifier-syntax 98))
(define-syntax jit-code-htonr-us (identifier-syntax 99))
(define-syntax jit-code-htonr-ui (identifier-syntax 100))
(define-syntax jit-code-htonr-ul (identifier-syntax 101))
(define-syntax jit-code-ldr-c (identifier-syntax 102))
(define-syntax jit-code-ldi-c (identifier-syntax 103))
(define-syntax jit-code-ldr-uc (identifier-syntax 104))
(define-syntax jit-code-ldi-uc (identifier-syntax 105))
(define-syntax jit-code-ldr-s (identifier-syntax 106))
(define-syntax jit-code-ldi-s (identifier-syntax 107))
(define-syntax jit-code-ldr-us (identifier-syntax 108))
(define-syntax jit-code-ldi-us (identifier-syntax 109))
(define-syntax jit-code-ldr-i (identifier-syntax 110))
(define-syntax jit-code-ldi-i (identifier-syntax 111))
(define-syntax jit-code-ldr-ui (identifier-syntax 112))
(define-syntax jit-code-ldi-ui (identifier-syntax 113))
(define-syntax jit-code-ldr-l (identifier-syntax 114))
(define-syntax jit-code-ldi-l (identifier-syntax 115))
(define-syntax jit-code-ldxr-c (identifier-syntax 116))
(define-syntax jit-code-ldxi-c (identifier-syntax 117))
(define-syntax jit-code-ldxr-uc (identifier-syntax 118))
(define-syntax jit-code-ldxi-uc (identifier-syntax 119))
(define-syntax jit-code-ldxr-s (identifier-syntax 120))
(define-syntax jit-code-ldxi-s (identifier-syntax 121))
(define-syntax jit-code-ldxr-us (identifier-syntax 122))
(define-syntax jit-code-ldxi-us (identifier-syntax 123))
(define-syntax jit-code-ldxr-i (identifier-syntax 124))
(define-syntax jit-code-ldxi-i (identifier-syntax 125))
(define-syntax jit-code-ldxr-ui (identifier-syntax 126))
(define-syntax jit-code-ldxi-ui (identifier-syntax 127))
(define-syntax jit-code-ldxr-l (identifier-syntax 128))
(define-syntax jit-code-ldxi-l (identifier-syntax 129))
(define-syntax jit-code-str-c (identifier-syntax 130))
(define-syntax jit-code-sti-c (identifier-syntax 131))
(define-syntax jit-code-str-s (identifier-syntax 132))
(define-syntax jit-code-sti-s (identifier-syntax 133))
(define-syntax jit-code-str-i (identifier-syntax 134))
(define-syntax jit-code-sti-i (identifier-syntax 135))
(define-syntax jit-code-str-l (identifier-syntax 136))
(define-syntax jit-code-sti-l (identifier-syntax 137))
(define-syntax jit-code-stxr-c (identifier-syntax 138))
(define-syntax jit-code-stxi-c (identifier-syntax 139))
(define-syntax jit-code-stxr-s (identifier-syntax 140))
(define-syntax jit-code-stxi-s (identifier-syntax 141))
(define-syntax jit-code-stxr-i (identifier-syntax 142))
(define-syntax jit-code-stxi-i (identifier-syntax 143))
(define-syntax jit-code-stxr-l (identifier-syntax 144))
(define-syntax jit-code-stxi-l (identifier-syntax 145))
(define-syntax jit-code-bltr (identifier-syntax 146))
(define-syntax jit-code-blti (identifier-syntax 147))
(define-syntax jit-code-bltr-u (identifier-syntax 148))
(define-syntax jit-code-blti-u (identifier-syntax 149))
(define-syntax jit-code-bler (identifier-syntax 150))
(define-syntax jit-code-blei (identifier-syntax 151))
(define-syntax jit-code-bler-u (identifier-syntax 152))
(define-syntax jit-code-blei-u (identifier-syntax 153))
(define-syntax jit-code-beqr (identifier-syntax 154))
(define-syntax jit-code-beqi (identifier-syntax 155))
(define-syntax jit-code-bger (identifier-syntax 156))
(define-syntax jit-code-bgei (identifier-syntax 157))
(define-syntax jit-code-bger-u (identifier-syntax 158))
(define-syntax jit-code-bgei-u (identifier-syntax 159))
(define-syntax jit-code-bgtr (identifier-syntax 160))
(define-syntax jit-code-bgti (identifier-syntax 161))
(define-syntax jit-code-bgtr-u (identifier-syntax 162))
(define-syntax jit-code-bgti-u (identifier-syntax 163))
(define-syntax jit-code-bner (identifier-syntax 164))
(define-syntax jit-code-bnei (identifier-syntax 165))
(define-syntax jit-code-bmsr (identifier-syntax 166))
(define-syntax jit-code-bmsi (identifier-syntax 167))
(define-syntax jit-code-bmcr (identifier-syntax 168))
(define-syntax jit-code-bmci (identifier-syntax 169))
(define-syntax jit-code-boaddr (identifier-syntax 170))
(define-syntax jit-code-boaddi (identifier-syntax 171))
(define-syntax jit-code-boaddr-u (identifier-syntax 172))
(define-syntax jit-code-boaddi-u (identifier-syntax 173))
(define-syntax jit-code-bxaddr (identifier-syntax 174))
(define-syntax jit-code-bxaddi (identifier-syntax 175))
(define-syntax jit-code-bxaddr-u (identifier-syntax 176))
(define-syntax jit-code-bxaddi-u (identifier-syntax 177))
(define-syntax jit-code-bosubr (identifier-syntax 178))
(define-syntax jit-code-bosubi (identifier-syntax 179))
(define-syntax jit-code-bosubr-u (identifier-syntax 180))
(define-syntax jit-code-bosubi-u (identifier-syntax 181))
(define-syntax jit-code-bxsubr (identifier-syntax 182))
(define-syntax jit-code-bxsubi (identifier-syntax 183))
(define-syntax jit-code-bxsubr-u (identifier-syntax 184))
(define-syntax jit-code-bxsubi-u (identifier-syntax 185))
(define-syntax jit-code-jmpr (identifier-syntax 186))
(define-syntax jit-code-jmpi (identifier-syntax 187))
(define-syntax jit-code-callr (identifier-syntax 188))
(define-syntax jit-code-calli (identifier-syntax 189))
(define-syntax jit-code-prepare (identifier-syntax 190))
(define-syntax jit-code-pushargr (identifier-syntax 191))
(define-syntax jit-code-pushargi (identifier-syntax 192))
(define-syntax jit-code-finishr (identifier-syntax 193))
(define-syntax jit-code-finishi (identifier-syntax 194))
(define-syntax jit-code-ret (identifier-syntax 195))
(define-syntax jit-code-retr (identifier-syntax 196))
(define-syntax jit-code-reti (identifier-syntax 197))
(define-syntax jit-code-retval-c (identifier-syntax 198))
(define-syntax jit-code-retval-uc (identifier-syntax 199))
(define-syntax jit-code-retval-s (identifier-syntax 200))
(define-syntax jit-code-retval-us (identifier-syntax 201))
(define-syntax jit-code-retval-i (identifier-syntax 202))
(define-syntax jit-code-retval-ui (identifier-syntax 203))
(define-syntax jit-code-retval-l (identifier-syntax 204))
(define-syntax jit-code-epilog (identifier-syntax 205))
(define-syntax jit-code-arg-f (identifier-syntax 206))
(define-syntax jit-code-getarg-f (identifier-syntax 207))
(define-syntax jit-code-putargr-f (identifier-syntax 208))
(define-syntax jit-code-putargi-f (identifier-syntax 209))
(define-syntax jit-code-addr-f (identifier-syntax 210))
(define-syntax jit-code-addi-f (identifier-syntax 211))
(define-syntax jit-code-subr-f (identifier-syntax 212))
(define-syntax jit-code-subi-f (identifier-syntax 213))
(define-syntax jit-code-rsbi-f (identifier-syntax 214))
(define-syntax jit-code-mulr-f (identifier-syntax 215))
(define-syntax jit-code-muli-f (identifier-syntax 216))
(define-syntax jit-code-divr-f (identifier-syntax 217))
(define-syntax jit-code-divi-f (identifier-syntax 218))
(define-syntax jit-code-negr-f (identifier-syntax 219))
(define-syntax jit-code-absr-f (identifier-syntax 220))
(define-syntax jit-code-sqrtr-f (identifier-syntax 221))
(define-syntax jit-code-ltr-f (identifier-syntax 222))
(define-syntax jit-code-lti-f (identifier-syntax 223))
(define-syntax jit-code-ler-f (identifier-syntax 224))
(define-syntax jit-code-lei-f (identifier-syntax 225))
(define-syntax jit-code-eqr-f (identifier-syntax 226))
(define-syntax jit-code-eqi-f (identifier-syntax 227))
(define-syntax jit-code-ger-f (identifier-syntax 228))
(define-syntax jit-code-gei-f (identifier-syntax 229))
(define-syntax jit-code-gtr-f (identifier-syntax 230))
(define-syntax jit-code-gti-f (identifier-syntax 231))
(define-syntax jit-code-ner-f (identifier-syntax 232))
(define-syntax jit-code-nei-f (identifier-syntax 233))
(define-syntax jit-code-unltr-f (identifier-syntax 234))
(define-syntax jit-code-unlti-f (identifier-syntax 235))
(define-syntax jit-code-unler-f (identifier-syntax 236))
(define-syntax jit-code-unlei-f (identifier-syntax 237))
(define-syntax jit-code-uneqr-f (identifier-syntax 238))
(define-syntax jit-code-uneqi-f (identifier-syntax 239))
(define-syntax jit-code-unger-f (identifier-syntax 240))
(define-syntax jit-code-ungei-f (identifier-syntax 241))
(define-syntax jit-code-ungtr-f (identifier-syntax 242))
(define-syntax jit-code-ungti-f (identifier-syntax 243))
(define-syntax jit-code-ltgtr-f (identifier-syntax 244))
(define-syntax jit-code-ltgti-f (identifier-syntax 245))
(define-syntax jit-code-ordr-f (identifier-syntax 246))
(define-syntax jit-code-ordi-f (identifier-syntax 247))
(define-syntax jit-code-unordr-f (identifier-syntax 248))
(define-syntax jit-code-unordi-f (identifier-syntax 249))
(define-syntax jit-code-truncr-f-i (identifier-syntax 250))
(define-syntax jit-code-truncr-f-l (identifier-syntax 251))
(define-syntax jit-code-extr-f (identifier-syntax 252))
(define-syntax jit-code-extr-d-f (identifier-syntax 253))
(define-syntax jit-code-movr-f (identifier-syntax 254))
(define-syntax jit-code-movi-f (identifier-syntax 255))
(define-syntax jit-code-ldr-f (identifier-syntax 256))
(define-syntax jit-code-ldi-f (identifier-syntax 257))
(define-syntax jit-code-ldxr-f (identifier-syntax 258))
(define-syntax jit-code-ldxi-f (identifier-syntax 259))
(define-syntax jit-code-str-f (identifier-syntax 260))
(define-syntax jit-code-sti-f (identifier-syntax 261))
(define-syntax jit-code-stxr-f (identifier-syntax 262))
(define-syntax jit-code-stxi-f (identifier-syntax 263))
(define-syntax jit-code-bltr-f (identifier-syntax 264))
(define-syntax jit-code-blti-f (identifier-syntax 265))
(define-syntax jit-code-bler-f (identifier-syntax 266))
(define-syntax jit-code-blei-f (identifier-syntax 267))
(define-syntax jit-code-beqr-f (identifier-syntax 268))
(define-syntax jit-code-beqi-f (identifier-syntax 269))
(define-syntax jit-code-bger-f (identifier-syntax 270))
(define-syntax jit-code-bgei-f (identifier-syntax 271))
(define-syntax jit-code-bgtr-f (identifier-syntax 272))
(define-syntax jit-code-bgti-f (identifier-syntax 273))
(define-syntax jit-code-bner-f (identifier-syntax 274))
(define-syntax jit-code-bnei-f (identifier-syntax 275))
(define-syntax jit-code-bunltr-f (identifier-syntax 276))
(define-syntax jit-code-bunlti-f (identifier-syntax 277))
(define-syntax jit-code-bunler-f (identifier-syntax 278))
(define-syntax jit-code-bunlei-f (identifier-syntax 279))
(define-syntax jit-code-buneqr-f (identifier-syntax 280))
(define-syntax jit-code-buneqi-f (identifier-syntax 281))
(define-syntax jit-code-bunger-f (identifier-syntax 282))
(define-syntax jit-code-bungei-f (identifier-syntax 283))
(define-syntax jit-code-bungtr-f (identifier-syntax 284))
(define-syntax jit-code-bungti-f (identifier-syntax 285))
(define-syntax jit-code-bltgtr-f (identifier-syntax 286))
(define-syntax jit-code-bltgti-f (identifier-syntax 287))
(define-syntax jit-code-bordr-f (identifier-syntax 288))
(define-syntax jit-code-bordi-f (identifier-syntax 289))
(define-syntax jit-code-bunordr-f (identifier-syntax 290))
(define-syntax jit-code-bunordi-f (identifier-syntax 291))
(define-syntax jit-code-pushargr-f (identifier-syntax 292))
(define-syntax jit-code-pushargi-f (identifier-syntax 293))
(define-syntax jit-code-retr-f (identifier-syntax 294))
(define-syntax jit-code-reti-f (identifier-syntax 295))
(define-syntax jit-code-retval-f (identifier-syntax 296))
(define-syntax jit-code-arg-d (identifier-syntax 297))
(define-syntax jit-code-getarg-d (identifier-syntax 298))
(define-syntax jit-code-putargr-d (identifier-syntax 299))
(define-syntax jit-code-putargi-d (identifier-syntax 300))
(define-syntax jit-code-addr-d (identifier-syntax 301))
(define-syntax jit-code-addi-d (identifier-syntax 302))
(define-syntax jit-code-subr-d (identifier-syntax 303))
(define-syntax jit-code-subi-d (identifier-syntax 304))
(define-syntax jit-code-rsbi-d (identifier-syntax 305))
(define-syntax jit-code-mulr-d (identifier-syntax 306))
(define-syntax jit-code-muli-d (identifier-syntax 307))
(define-syntax jit-code-divr-d (identifier-syntax 308))
(define-syntax jit-code-divi-d (identifier-syntax 309))
(define-syntax jit-code-negr-d (identifier-syntax 310))
(define-syntax jit-code-absr-d (identifier-syntax 311))
(define-syntax jit-code-sqrtr-d (identifier-syntax 312))
(define-syntax jit-code-ltr-d (identifier-syntax 313))
(define-syntax jit-code-lti-d (identifier-syntax 314))
(define-syntax jit-code-ler-d (identifier-syntax 315))
(define-syntax jit-code-lei-d (identifier-syntax 316))
(define-syntax jit-code-eqr-d (identifier-syntax 317))
(define-syntax jit-code-eqi-d (identifier-syntax 318))
(define-syntax jit-code-ger-d (identifier-syntax 319))
(define-syntax jit-code-gei-d (identifier-syntax 320))
(define-syntax jit-code-gtr-d (identifier-syntax 321))
(define-syntax jit-code-gti-d (identifier-syntax 322))
(define-syntax jit-code-ner-d (identifier-syntax 323))
(define-syntax jit-code-nei-d (identifier-syntax 324))
(define-syntax jit-code-unltr-d (identifier-syntax 325))
(define-syntax jit-code-unlti-d (identifier-syntax 326))
(define-syntax jit-code-unler-d (identifier-syntax 327))
(define-syntax jit-code-unlei-d (identifier-syntax 328))
(define-syntax jit-code-uneqr-d (identifier-syntax 329))
(define-syntax jit-code-uneqi-d (identifier-syntax 330))
(define-syntax jit-code-unger-d (identifier-syntax 331))
(define-syntax jit-code-ungei-d (identifier-syntax 332))
(define-syntax jit-code-ungtr-d (identifier-syntax 333))
(define-syntax jit-code-ungti-d (identifier-syntax 334))
(define-syntax jit-code-ltgtr-d (identifier-syntax 335))
(define-syntax jit-code-ltgti-d (identifier-syntax 336))
(define-syntax jit-code-ordr-d (identifier-syntax 337))
(define-syntax jit-code-ordi-d (identifier-syntax 338))
(define-syntax jit-code-unordr-d (identifier-syntax 339))
(define-syntax jit-code-unordi-d (identifier-syntax 340))
(define-syntax jit-code-truncr-d-i (identifier-syntax 341))
(define-syntax jit-code-truncr-d-l (identifier-syntax 342))
(define-syntax jit-code-extr-d (identifier-syntax 343))
(define-syntax jit-code-extr-f-d (identifier-syntax 344))
(define-syntax jit-code-movr-d (identifier-syntax 345))
(define-syntax jit-code-movi-d (identifier-syntax 346))
(define-syntax jit-code-ldr-d (identifier-syntax 347))
(define-syntax jit-code-ldi-d (identifier-syntax 348))
(define-syntax jit-code-ldxr-d (identifier-syntax 349))
(define-syntax jit-code-ldxi-d (identifier-syntax 350))
(define-syntax jit-code-str-d (identifier-syntax 351))
(define-syntax jit-code-sti-d (identifier-syntax 352))
(define-syntax jit-code-stxr-d (identifier-syntax 353))
(define-syntax jit-code-stxi-d (identifier-syntax 354))
(define-syntax jit-code-bltr-d (identifier-syntax 355))
(define-syntax jit-code-blti-d (identifier-syntax 356))
(define-syntax jit-code-bler-d (identifier-syntax 357))
(define-syntax jit-code-blei-d (identifier-syntax 358))
(define-syntax jit-code-beqr-d (identifier-syntax 359))
(define-syntax jit-code-beqi-d (identifier-syntax 360))
(define-syntax jit-code-bger-d (identifier-syntax 361))
(define-syntax jit-code-bgei-d (identifier-syntax 362))
(define-syntax jit-code-bgtr-d (identifier-syntax 363))
(define-syntax jit-code-bgti-d (identifier-syntax 364))
(define-syntax jit-code-bner-d (identifier-syntax 365))
(define-syntax jit-code-bnei-d (identifier-syntax 366))
(define-syntax jit-code-bunltr-d (identifier-syntax 367))
(define-syntax jit-code-bunlti-d (identifier-syntax 368))
(define-syntax jit-code-bunler-d (identifier-syntax 369))
(define-syntax jit-code-bunlei-d (identifier-syntax 370))
(define-syntax jit-code-buneqr-d (identifier-syntax 371))
(define-syntax jit-code-buneqi-d (identifier-syntax 372))
(define-syntax jit-code-bunger-d (identifier-syntax 373))
(define-syntax jit-code-bungei-d (identifier-syntax 374))
(define-syntax jit-code-bungtr-d (identifier-syntax 375))
(define-syntax jit-code-bungti-d (identifier-syntax 376))
(define-syntax jit-code-bltgtr-d (identifier-syntax 377))
(define-syntax jit-code-bltgti-d (identifier-syntax 378))
(define-syntax jit-code-bordr-d (identifier-syntax 379))
(define-syntax jit-code-bordi-d (identifier-syntax 380))
(define-syntax jit-code-bunordr-d (identifier-syntax 381))
(define-syntax jit-code-bunordi-d (identifier-syntax 382))
(define-syntax jit-code-pushargr-d (identifier-syntax 383))
(define-syntax jit-code-pushargi-d (identifier-syntax 384))
(define-syntax jit-code-retr-d (identifier-syntax 385))
(define-syntax jit-code-reti-d (identifier-syntax 386))
(define-syntax jit-code-retval-d (identifier-syntax 387))
(define-syntax jit-code-movr-w-f (identifier-syntax 388))
(define-syntax jit-code-movr-ww-d (identifier-syntax 389))
(define-syntax jit-code-movr-w-d (identifier-syntax 390))
(define-syntax jit-code-movr-f-w (identifier-syntax 391))
(define-syntax jit-code-movi-f-w (identifier-syntax 392))
(define-syntax jit-code-movr-d-ww (identifier-syntax 393))
(define-syntax jit-code-movi-d-ww (identifier-syntax 394))
(define-syntax jit-code-movr-d-w (identifier-syntax 395))
(define-syntax jit-code-movi-d-w (identifier-syntax 396))


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