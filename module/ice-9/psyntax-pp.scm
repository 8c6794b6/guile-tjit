(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec*
  ((#{make-void 203}#
     (lambda (#{src 765}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 0)
         #{src 765}#)))
   (#{make-const 205}#
     (lambda (#{src 767}# #{exp 768}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 1)
         #{src 767}#
         #{exp 768}#)))
   (#{make-primitive-ref 207}#
     (lambda (#{src 771}# #{name 772}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 2)
         #{src 771}#
         #{name 772}#)))
   (#{make-lexical-ref 209}#
     (lambda (#{src 775}# #{name 776}# #{gensym 777}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 3)
         #{src 775}#
         #{name 776}#
         #{gensym 777}#)))
   (#{make-lexical-set 211}#
     (lambda (#{src 781}#
              #{name 782}#
              #{gensym 783}#
              #{exp 784}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 4)
         #{src 781}#
         #{name 782}#
         #{gensym 783}#
         #{exp 784}#)))
   (#{make-module-ref 213}#
     (lambda (#{src 789}#
              #{mod 790}#
              #{name 791}#
              #{public? 792}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 5)
         #{src 789}#
         #{mod 790}#
         #{name 791}#
         #{public? 792}#)))
   (#{make-module-set 215}#
     (lambda (#{src 797}#
              #{mod 798}#
              #{name 799}#
              #{public? 800}#
              #{exp 801}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 6)
         #{src 797}#
         #{mod 798}#
         #{name 799}#
         #{public? 800}#
         #{exp 801}#)))
   (#{make-toplevel-ref 217}#
     (lambda (#{src 807}# #{name 808}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 7)
         #{src 807}#
         #{name 808}#)))
   (#{make-toplevel-set 219}#
     (lambda (#{src 811}# #{name 812}# #{exp 813}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 8)
         #{src 811}#
         #{name 812}#
         #{exp 813}#)))
   (#{make-toplevel-define 221}#
     (lambda (#{src 817}# #{name 818}# #{exp 819}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 9)
         #{src 817}#
         #{name 818}#
         #{exp 819}#)))
   (#{make-conditional 223}#
     (lambda (#{src 823}#
              #{test 824}#
              #{consequent 825}#
              #{alternate 826}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 10)
         #{src 823}#
         #{test 824}#
         #{consequent 825}#
         #{alternate 826}#)))
   (#{make-call 225}#
     (lambda (#{src 831}# #{proc 832}# #{args 833}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 11)
         #{src 831}#
         #{proc 832}#
         #{args 833}#)))
   (#{make-primcall 227}#
     (lambda (#{src 837}# #{name 838}# #{args 839}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 12)
         #{src 837}#
         #{name 838}#
         #{args 839}#)))
   (#{make-seq 229}#
     (lambda (#{src 843}# #{head 844}# #{tail 845}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 13)
         #{src 843}#
         #{head 844}#
         #{tail 845}#)))
   (#{make-lambda 231}#
     (lambda (#{src 849}# #{meta 850}# #{body 851}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 14)
         #{src 849}#
         #{meta 850}#
         #{body 851}#)))
   (#{make-lambda-case 233}#
     (lambda (#{src 855}#
              #{req 856}#
              #{opt 857}#
              #{rest 858}#
              #{kw 859}#
              #{inits 860}#
              #{gensyms 861}#
              #{body 862}#
              #{alternate 863}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 15)
         #{src 855}#
         #{req 856}#
         #{opt 857}#
         #{rest 858}#
         #{kw 859}#
         #{inits 860}#
         #{gensyms 861}#
         #{body 862}#
         #{alternate 863}#)))
   (#{make-let 235}#
     (lambda (#{src 873}#
              #{names 874}#
              #{gensyms 875}#
              #{vals 876}#
              #{body 877}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 16)
         #{src 873}#
         #{names 874}#
         #{gensyms 875}#
         #{vals 876}#
         #{body 877}#)))
   (#{make-letrec 237}#
     (lambda (#{src 883}#
              #{in-order? 884}#
              #{names 885}#
              #{gensyms 886}#
              #{vals 887}#
              #{body 888}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 17)
         #{src 883}#
         #{in-order? 884}#
         #{names 885}#
         #{gensyms 886}#
         #{vals 887}#
         #{body 888}#)))
   (#{make-dynlet 239}#
     (lambda (#{src 895}#
              #{fluids 896}#
              #{vals 897}#
              #{body 898}#)
       (make-struct/no-tail
         (vector-ref %expanded-vtables 18)
         #{src 895}#
         #{fluids 896}#
         #{vals 897}#
         #{body 898}#)))
   (#{lambda? 242}#
     (lambda (#{x 903}#)
       (if (struct? #{x 903}#)
         (eq? (struct-vtable #{x 903}#)
              (vector-ref %expanded-vtables 14))
         #f)))
   (#{lambda-meta 244}#
     (lambda (#{x 907}#) (struct-ref #{x 907}# 1)))
   (#{set-lambda-meta! 246}#
     (lambda (#{x 909}# #{v 910}#)
       (struct-set! #{x 909}# 1 #{v 910}#)))
   (#{top-level-eval-hook 252}#
     (lambda (#{x 913}# #{mod 914}#)
       (primitive-eval #{x 913}#)))
   (#{local-eval-hook 254}#
     (lambda (#{x 917}# #{mod 918}#)
       (primitive-eval #{x 917}#)))
   (#{put-global-definition-hook 257}#
     (lambda (#{symbol 921}# #{type 922}# #{val 923}#)
       (module-define!
         (current-module)
         #{symbol 921}#
         (make-syntax-transformer
           #{symbol 921}#
           #{type 922}#
           #{val 923}#))))
   (#{get-global-definition-hook 259}#
     (lambda (#{symbol 927}# #{module 928}#)
       (begin
         (if (if (not #{module 928}#) (current-module) #f)
           (warn "module system is booted, we should have a module"
                 #{symbol 927}#))
         (let ((#{v 934}# (module-variable
                            (if #{module 928}#
                              (resolve-module (cdr #{module 928}#))
                              (current-module))
                            #{symbol 927}#)))
           (if #{v 934}#
             (if (variable-bound? #{v 934}#)
               (let ((#{val 939}# (variable-ref #{v 934}#)))
                 (if (macro? #{val 939}#)
                   (if (macro-type #{val 939}#)
                     (cons (macro-type #{val 939}#)
                           (macro-binding #{val 939}#))
                     #f)
                   #f))
               #f)
             #f)))))
   (#{decorate-source 261}#
     (lambda (#{e 943}# #{s 944}#)
       (begin
         (if (if (pair? #{e 943}#) #{s 944}# #f)
           (set-source-properties! #{e 943}# #{s 944}#))
         #{e 943}#)))
   (#{maybe-name-value! 263}#
     (lambda (#{name 949}# #{val 950}#)
       (if (#{lambda? 242}# #{val 950}#)
         (let ((#{meta 954}# (#{lambda-meta 244}# #{val 950}#)))
           (if (not (assq 'name #{meta 954}#))
             (#{set-lambda-meta! 246}#
               #{val 950}#
               (cons (cons 'name #{name 949}#) #{meta 954}#)))))))
   (#{build-void 265}#
     (lambda (#{source 955}#)
       (#{make-void 203}# #{source 955}#)))
   (#{build-call 267}#
     (lambda (#{source 957}# #{fun-exp 958}# #{arg-exps 959}#)
       (#{make-call 225}#
         #{source 957}#
         #{fun-exp 958}#
         #{arg-exps 959}#)))
   (#{build-conditional 269}#
     (lambda (#{source 963}#
              #{test-exp 964}#
              #{then-exp 965}#
              #{else-exp 966}#)
       (#{make-conditional 223}#
         #{source 963}#
         #{test-exp 964}#
         #{then-exp 965}#
         #{else-exp 966}#)))
   (#{build-dynlet 271}#
     (lambda (#{source 971}#
              #{fluids 972}#
              #{vals 973}#
              #{body 974}#)
       (#{make-dynlet 239}#
         #{source 971}#
         #{fluids 972}#
         #{vals 973}#
         #{body 974}#)))
   (#{build-lexical-reference 273}#
     (lambda (#{type 979}#
              #{source 980}#
              #{name 981}#
              #{var 982}#)
       (#{make-lexical-ref 209}#
         #{source 980}#
         #{name 981}#
         #{var 982}#)))
   (#{build-lexical-assignment 275}#
     (lambda (#{source 987}#
              #{name 988}#
              #{var 989}#
              #{exp 990}#)
       (begin
         (#{maybe-name-value! 263}#
           #{name 988}#
           #{exp 990}#)
         (#{make-lexical-set 211}#
           #{source 987}#
           #{name 988}#
           #{var 989}#
           #{exp 990}#))))
   (#{analyze-variable 277}#
     (lambda (#{mod 995}#
              #{var 996}#
              #{modref-cont 997}#
              #{bare-cont 998}#)
       (if (not #{mod 995}#)
         (#{bare-cont 998}# #{var 996}#)
         (let ((#{kind 1005}# (car #{mod 995}#))
               (#{mod 1006}# (cdr #{mod 995}#)))
           (if (memv #{kind 1005}# '(public))
             (#{modref-cont 997}# #{mod 1006}# #{var 996}# #t)
             (if (memv #{kind 1005}# '(private))
               (if (not (equal?
                          #{mod 1006}#
                          (module-name (current-module))))
                 (#{modref-cont 997}# #{mod 1006}# #{var 996}# #f)
                 (#{bare-cont 998}# #{var 996}#))
               (if (memv #{kind 1005}# '(bare))
                 (#{bare-cont 998}# #{var 996}#)
                 (if (memv #{kind 1005}# '(hygiene))
                   (if (if (not (equal?
                                  #{mod 1006}#
                                  (module-name (current-module))))
                         (module-variable
                           (resolve-module #{mod 1006}#)
                           #{var 996}#)
                         #f)
                     (#{modref-cont 997}# #{mod 1006}# #{var 996}# #f)
                     (#{bare-cont 998}# #{var 996}#))
                   (syntax-violation
                     #f
                     "bad module kind"
                     #{var 996}#
                     #{mod 1006}#)))))))))
   (#{build-global-reference 279}#
     (lambda (#{source 1014}# #{var 1015}# #{mod 1016}#)
       (#{analyze-variable 277}#
         #{mod 1016}#
         #{var 1015}#
         (lambda (#{mod 1020}# #{var 1021}# #{public? 1022}#)
           (#{make-module-ref 213}#
             #{source 1014}#
             #{mod 1020}#
             #{var 1021}#
             #{public? 1022}#))
         (lambda (#{var 1026}#)
           (#{make-toplevel-ref 217}#
             #{source 1014}#
             #{var 1026}#)))))
   (#{build-global-assignment 281}#
     (lambda (#{source 1028}#
              #{var 1029}#
              #{exp 1030}#
              #{mod 1031}#)
       (begin
         (#{maybe-name-value! 263}#
           #{var 1029}#
           #{exp 1030}#)
         (#{analyze-variable 277}#
           #{mod 1031}#
           #{var 1029}#
           (lambda (#{mod 1036}# #{var 1037}# #{public? 1038}#)
             (#{make-module-set 215}#
               #{source 1028}#
               #{mod 1036}#
               #{var 1037}#
               #{public? 1038}#
               #{exp 1030}#))
           (lambda (#{var 1042}#)
             (#{make-toplevel-set 219}#
               #{source 1028}#
               #{var 1042}#
               #{exp 1030}#))))))
   (#{build-global-definition 283}#
     (lambda (#{source 1044}# #{var 1045}# #{exp 1046}#)
       (begin
         (#{maybe-name-value! 263}#
           #{var 1045}#
           #{exp 1046}#)
         (#{make-toplevel-define 221}#
           #{source 1044}#
           #{var 1045}#
           #{exp 1046}#))))
   (#{build-simple-lambda 285}#
     (lambda (#{src 1050}#
              #{req 1051}#
              #{rest 1052}#
              #{vars 1053}#
              #{meta 1054}#
              #{exp 1055}#)
       (#{make-lambda 231}#
         #{src 1050}#
         #{meta 1054}#
         (#{make-lambda-case 233}#
           #{src 1050}#
           #{req 1051}#
           #f
           #{rest 1052}#
           #f
           '()
           #{vars 1053}#
           #{exp 1055}#
           #f))))
   (#{build-case-lambda 287}#
     (lambda (#{src 1062}# #{meta 1063}# #{body 1064}#)
       (#{make-lambda 231}#
         #{src 1062}#
         #{meta 1063}#
         #{body 1064}#)))
   (#{build-lambda-case 289}#
     (lambda (#{src 1068}#
              #{req 1069}#
              #{opt 1070}#
              #{rest 1071}#
              #{kw 1072}#
              #{inits 1073}#
              #{vars 1074}#
              #{body 1075}#
              #{else-case 1076}#)
       (#{make-lambda-case 233}#
         #{src 1068}#
         #{req 1069}#
         #{opt 1070}#
         #{rest 1071}#
         #{kw 1072}#
         #{inits 1073}#
         #{vars 1074}#
         #{body 1075}#
         #{else-case 1076}#)))
   (#{build-primcall 291}#
     (lambda (#{src 1086}# #{name 1087}# #{args 1088}#)
       (#{make-primcall 227}#
         #{src 1086}#
         #{name 1087}#
         #{args 1088}#)))
   (#{build-primref 293}#
     (lambda (#{src 1092}# #{name 1093}#)
       (#{make-primitive-ref 207}#
         #{src 1092}#
         #{name 1093}#)))
   (#{build-data 295}#
     (lambda (#{src 1096}# #{exp 1097}#)
       (#{make-const 205}# #{src 1096}# #{exp 1097}#)))
   (#{build-sequence 297}#
     (lambda (#{src 1100}# #{exps 1101}#)
       (if (null? (cdr #{exps 1101}#))
         (car #{exps 1101}#)
         (#{make-seq 229}#
           #{src 1100}#
           (car #{exps 1101}#)
           (#{build-sequence 297}# #f (cdr #{exps 1101}#))))))
   (#{build-let 299}#
     (lambda (#{src 1104}#
              #{ids 1105}#
              #{vars 1106}#
              #{val-exps 1107}#
              #{body-exp 1108}#)
       (begin
         (for-each
           #{maybe-name-value! 263}#
           #{ids 1105}#
           #{val-exps 1107}#)
         (if (null? #{vars 1106}#)
           #{body-exp 1108}#
           (#{make-let 235}#
             #{src 1104}#
             #{ids 1105}#
             #{vars 1106}#
             #{val-exps 1107}#
             #{body-exp 1108}#)))))
   (#{build-named-let 301}#
     (lambda (#{src 1114}#
              #{ids 1115}#
              #{vars 1116}#
              #{val-exps 1117}#
              #{body-exp 1118}#)
       (let ((#{f 1128}# (car #{vars 1116}#))
             (#{f-name 1129}# (car #{ids 1115}#))
             (#{vars 1130}# (cdr #{vars 1116}#))
             (#{ids 1131}# (cdr #{ids 1115}#)))
         (let ((#{proc 1133}#
                 (#{build-simple-lambda 285}#
                   #{src 1114}#
                   #{ids 1131}#
                   #f
                   #{vars 1130}#
                   '()
                   #{body-exp 1118}#)))
           (begin
             (#{maybe-name-value! 263}#
               #{f-name 1129}#
               #{proc 1133}#)
             (for-each
               #{maybe-name-value! 263}#
               #{ids 1131}#
               #{val-exps 1117}#)
             (#{make-letrec 237}#
               #{src 1114}#
               #f
               (list #{f-name 1129}#)
               (list #{f 1128}#)
               (list #{proc 1133}#)
               (#{build-call 267}#
                 #{src 1114}#
                 (#{build-lexical-reference 273}#
                   'fun
                   #{src 1114}#
                   #{f-name 1129}#
                   #{f 1128}#)
                 #{val-exps 1117}#)))))))
   (#{build-letrec 303}#
     (lambda (#{src 1134}#
              #{in-order? 1135}#
              #{ids 1136}#
              #{vars 1137}#
              #{val-exps 1138}#
              #{body-exp 1139}#)
       (if (null? #{vars 1137}#)
         #{body-exp 1139}#
         (begin
           (for-each
             #{maybe-name-value! 263}#
             #{ids 1136}#
             #{val-exps 1138}#)
           (#{make-letrec 237}#
             #{src 1134}#
             #{in-order? 1135}#
             #{ids 1136}#
             #{vars 1137}#
             #{val-exps 1138}#
             #{body-exp 1139}#)))))
   (#{make-syntax-object 307}#
     (lambda (#{expression 1146}#
              #{wrap 1147}#
              #{module 1148}#)
       (vector
         'syntax-object
         #{expression 1146}#
         #{wrap 1147}#
         #{module 1148}#)))
   (#{syntax-object? 309}#
     (lambda (#{x 1152}#)
       (if (vector? #{x 1152}#)
         (if (= (vector-length #{x 1152}#) 4)
           (eq? (vector-ref #{x 1152}# 0) 'syntax-object)
           #f)
         #f)))
   (#{syntax-object-expression 311}#
     (lambda (#{x 1157}#) (vector-ref #{x 1157}# 1)))
   (#{syntax-object-wrap 313}#
     (lambda (#{x 1159}#) (vector-ref #{x 1159}# 2)))
   (#{syntax-object-module 315}#
     (lambda (#{x 1161}#) (vector-ref #{x 1161}# 3)))
   (#{source-annotation 324}#
     (lambda (#{x 1175}#)
       (if (#{syntax-object? 309}# #{x 1175}#)
         (#{source-annotation 324}#
           (#{syntax-object-expression 311}# #{x 1175}#))
         (if (pair? #{x 1175}#)
           (let ((#{props 1182}# (source-properties #{x 1175}#)))
             (if (pair? #{props 1182}#) #{props 1182}# #f))
           #f))))
   (#{extend-env 331}#
     (lambda (#{labels 1184}# #{bindings 1185}# #{r 1186}#)
       (if (null? #{labels 1184}#)
         #{r 1186}#
         (#{extend-env 331}#
           (cdr #{labels 1184}#)
           (cdr #{bindings 1185}#)
           (cons (cons (car #{labels 1184}#)
                       (car #{bindings 1185}#))
                 #{r 1186}#)))))
   (#{extend-var-env 333}#
     (lambda (#{labels 1190}# #{vars 1191}# #{r 1192}#)
       (if (null? #{labels 1190}#)
         #{r 1192}#
         (#{extend-var-env 333}#
           (cdr #{labels 1190}#)
           (cdr #{vars 1191}#)
           (cons (cons (car #{labels 1190}#)
                       (cons 'lexical (car #{vars 1191}#)))
                 #{r 1192}#)))))
   (#{macros-only-env 335}#
     (lambda (#{r 1197}#)
       (if (null? #{r 1197}#)
         '()
         (let ((#{a 1200}# (car #{r 1197}#)))
           (if (eq? (car (cdr #{a 1200}#)) 'macro)
             (cons #{a 1200}#
                   (#{macros-only-env 335}# (cdr #{r 1197}#)))
             (#{macros-only-env 335}# (cdr #{r 1197}#)))))))
   (#{lookup 337}#
     (lambda (#{x 1201}# #{r 1202}# #{mod 1203}#)
       (let ((#{t 1209}# (assq #{x 1201}# #{r 1202}#)))
         (if #{t 1209}#
           (cdr #{t 1209}#)
           (if (symbol? #{x 1201}#)
             (let ((#{t 1215}#
                     (#{get-global-definition-hook 259}#
                       #{x 1201}#
                       #{mod 1203}#)))
               (if #{t 1215}# #{t 1215}# '(global)))
             '(displaced-lexical))))))
   (#{global-extend 339}#
     (lambda (#{type 1220}# #{sym 1221}# #{val 1222}#)
       (#{put-global-definition-hook 257}#
         #{sym 1221}#
         #{type 1220}#
         #{val 1222}#)))
   (#{nonsymbol-id? 341}#
     (lambda (#{x 1226}#)
       (if (#{syntax-object? 309}# #{x 1226}#)
         (symbol?
           (#{syntax-object-expression 311}# #{x 1226}#))
         #f)))
   (#{id? 343}#
     (lambda (#{x 1230}#)
       (if (symbol? #{x 1230}#)
         #t
         (if (#{syntax-object? 309}# #{x 1230}#)
           (symbol?
             (#{syntax-object-expression 311}# #{x 1230}#))
           #f))))
   (#{id-sym-name&marks 346}#
     (lambda (#{x 1237}# #{w 1238}#)
       (if (#{syntax-object? 309}# #{x 1237}#)
         (values
           (#{syntax-object-expression 311}# #{x 1237}#)
           (#{join-marks 393}#
             (car #{w 1238}#)
             (car (#{syntax-object-wrap 313}# #{x 1237}#))))
         (values #{x 1237}# (car #{w 1238}#)))))
   (#{gen-label 356}#
     (lambda () (symbol->string (gensym "i"))))
   (#{gen-labels 358}#
     (lambda (#{ls 1244}#)
       (if (null? #{ls 1244}#)
         '()
         (cons (#{gen-label 356}#)
               (#{gen-labels 358}# (cdr #{ls 1244}#))))))
   (#{make-ribcage 361}#
     (lambda (#{symnames 1246}#
              #{marks 1247}#
              #{labels 1248}#)
       (vector
         'ribcage
         #{symnames 1246}#
         #{marks 1247}#
         #{labels 1248}#)))
   (#{ribcage-symnames 365}#
     (lambda (#{x 1257}#) (vector-ref #{x 1257}# 1)))
   (#{ribcage-marks 367}#
     (lambda (#{x 1259}#) (vector-ref #{x 1259}# 2)))
   (#{ribcage-labels 369}#
     (lambda (#{x 1261}#) (vector-ref #{x 1261}# 3)))
   (#{set-ribcage-symnames! 371}#
     (lambda (#{x 1263}# #{update 1264}#)
       (vector-set! #{x 1263}# 1 #{update 1264}#)))
   (#{set-ribcage-marks! 373}#
     (lambda (#{x 1267}# #{update 1268}#)
       (vector-set! #{x 1267}# 2 #{update 1268}#)))
   (#{set-ribcage-labels! 375}#
     (lambda (#{x 1271}# #{update 1272}#)
       (vector-set! #{x 1271}# 3 #{update 1272}#)))
   (#{anti-mark 381}#
     (lambda (#{w 1275}#)
       (cons (cons #f (car #{w 1275}#))
             (cons 'shift (cdr #{w 1275}#)))))
   (#{extend-ribcage! 385}#
     (lambda (#{ribcage 1281}# #{id 1282}# #{label 1283}#)
       (begin
         (#{set-ribcage-symnames! 371}#
           #{ribcage 1281}#
           (cons (#{syntax-object-expression 311}# #{id 1282}#)
                 (#{ribcage-symnames 365}# #{ribcage 1281}#)))
         (#{set-ribcage-marks! 373}#
           #{ribcage 1281}#
           (cons (car (#{syntax-object-wrap 313}# #{id 1282}#))
                 (#{ribcage-marks 367}# #{ribcage 1281}#)))
         (#{set-ribcage-labels! 375}#
           #{ribcage 1281}#
           (cons #{label 1283}#
                 (#{ribcage-labels 369}# #{ribcage 1281}#))))))
   (#{make-binding-wrap 387}#
     (lambda (#{ids 1288}# #{labels 1289}# #{w 1290}#)
       (if (null? #{ids 1288}#)
         #{w 1290}#
         (cons (car #{w 1290}#)
               (cons (let ((#{labelvec 1297}#
                             (list->vector #{labels 1289}#)))
                       (let ((#{n 1299}# (vector-length #{labelvec 1297}#)))
                         (let ((#{symnamevec 1302}# (make-vector #{n 1299}#))
                               (#{marksvec 1303}# (make-vector #{n 1299}#)))
                           (begin
                             (letrec*
                               ((#{f 1307}#
                                  (lambda (#{ids 1308}# #{i 1309}#)
                                    (if (not (null? #{ids 1308}#))
                                      (call-with-values
                                        (lambda ()
                                          (#{id-sym-name&marks 346}#
                                            (car #{ids 1308}#)
                                            #{w 1290}#))
                                        (lambda (#{symname 1310}#
                                                 #{marks 1311}#)
                                          (begin
                                            (vector-set!
                                              #{symnamevec 1302}#
                                              #{i 1309}#
                                              #{symname 1310}#)
                                            (vector-set!
                                              #{marksvec 1303}#
                                              #{i 1309}#
                                              #{marks 1311}#)
                                            (#{f 1307}#
                                              (cdr #{ids 1308}#)
                                              (#{1+}# #{i 1309}#)))))))))
                               (#{f 1307}# #{ids 1288}# 0))
                             (#{make-ribcage 361}#
                               #{symnamevec 1302}#
                               #{marksvec 1303}#
                               #{labelvec 1297}#)))))
                     (cdr #{w 1290}#))))))
   (#{smart-append 389}#
     (lambda (#{m1 1316}# #{m2 1317}#)
       (if (null? #{m2 1317}#)
         #{m1 1316}#
         (append #{m1 1316}# #{m2 1317}#))))
   (#{join-wraps 391}#
     (lambda (#{w1 1320}# #{w2 1321}#)
       (let ((#{m1 1326}# (car #{w1 1320}#))
             (#{s1 1327}# (cdr #{w1 1320}#)))
         (if (null? #{m1 1326}#)
           (if (null? #{s1 1327}#)
             #{w2 1321}#
             (cons (car #{w2 1321}#)
                   (#{smart-append 389}#
                     #{s1 1327}#
                     (cdr #{w2 1321}#))))
           (cons (#{smart-append 389}#
                   #{m1 1326}#
                   (car #{w2 1321}#))
                 (#{smart-append 389}#
                   #{s1 1327}#
                   (cdr #{w2 1321}#)))))))
   (#{join-marks 393}#
     (lambda (#{m1 1336}# #{m2 1337}#)
       (#{smart-append 389}# #{m1 1336}# #{m2 1337}#)))
   (#{same-marks? 395}#
     (lambda (#{x 1340}# #{y 1341}#)
       (let ((#{t 1346}# (eq? #{x 1340}# #{y 1341}#)))
         (if #{t 1346}#
           #{t 1346}#
           (if (not (null? #{x 1340}#))
             (if (not (null? #{y 1341}#))
               (if (eq? (car #{x 1340}#) (car #{y 1341}#))
                 (#{same-marks? 395}#
                   (cdr #{x 1340}#)
                   (cdr #{y 1341}#))
                 #f)
               #f)
             #f)))))
   (#{id-var-name 397}#
     (lambda (#{id 1352}# #{w 1353}#)
       (letrec*
         ((#{search 1358}#
            (lambda (#{sym 1374}# #{subst 1375}# #{marks 1376}#)
              (if (null? #{subst 1375}#)
                (values #f #{marks 1376}#)
                (let ((#{fst 1381}# (car #{subst 1375}#)))
                  (if (eq? #{fst 1381}# 'shift)
                    (#{search 1358}#
                      #{sym 1374}#
                      (cdr #{subst 1375}#)
                      (cdr #{marks 1376}#))
                    (let ((#{symnames 1383}#
                            (#{ribcage-symnames 365}# #{fst 1381}#)))
                      (if (vector? #{symnames 1383}#)
                        (#{search-vector-rib 1362}#
                          #{sym 1374}#
                          #{subst 1375}#
                          #{marks 1376}#
                          #{symnames 1383}#
                          #{fst 1381}#)
                        (#{search-list-rib 1360}#
                          #{sym 1374}#
                          #{subst 1375}#
                          #{marks 1376}#
                          #{symnames 1383}#
                          #{fst 1381}#))))))))
          (#{search-list-rib 1360}#
            (lambda (#{sym 1384}#
                     #{subst 1385}#
                     #{marks 1386}#
                     #{symnames 1387}#
                     #{ribcage 1388}#)
              (letrec*
                ((#{f 1397}#
                   (lambda (#{symnames 1398}# #{i 1399}#)
                     (if (null? #{symnames 1398}#)
                       (#{search 1358}#
                         #{sym 1384}#
                         (cdr #{subst 1385}#)
                         #{marks 1386}#)
                       (if (if (eq? (car #{symnames 1398}#) #{sym 1384}#)
                             (#{same-marks? 395}#
                               #{marks 1386}#
                               (list-ref
                                 (#{ribcage-marks 367}# #{ribcage 1388}#)
                                 #{i 1399}#))
                             #f)
                         (values
                           (list-ref
                             (#{ribcage-labels 369}# #{ribcage 1388}#)
                             #{i 1399}#)
                           #{marks 1386}#)
                         (#{f 1397}#
                           (cdr #{symnames 1398}#)
                           (#{1+}# #{i 1399}#)))))))
                (#{f 1397}# #{symnames 1387}# 0))))
          (#{search-vector-rib 1362}#
            (lambda (#{sym 1408}#
                     #{subst 1409}#
                     #{marks 1410}#
                     #{symnames 1411}#
                     #{ribcage 1412}#)
              (let ((#{n 1419}# (vector-length #{symnames 1411}#)))
                (letrec*
                  ((#{f 1422}#
                     (lambda (#{i 1423}#)
                       (if (= #{i 1423}# #{n 1419}#)
                         (#{search 1358}#
                           #{sym 1408}#
                           (cdr #{subst 1409}#)
                           #{marks 1410}#)
                         (if (if (eq? (vector-ref #{symnames 1411}# #{i 1423}#)
                                      #{sym 1408}#)
                               (#{same-marks? 395}#
                                 #{marks 1410}#
                                 (vector-ref
                                   (#{ribcage-marks 367}# #{ribcage 1412}#)
                                   #{i 1423}#))
                               #f)
                           (values
                             (vector-ref
                               (#{ribcage-labels 369}# #{ribcage 1412}#)
                               #{i 1423}#)
                             #{marks 1410}#)
                           (#{f 1422}# (#{1+}# #{i 1423}#)))))))
                  (#{f 1422}# 0))))))
         (if (symbol? #{id 1352}#)
           (let ((#{t 1435}#
                   (#{search 1358}#
                     #{id 1352}#
                     (cdr #{w 1353}#)
                     (car #{w 1353}#))))
             (if #{t 1435}# #{t 1435}# #{id 1352}#))
           (if (#{syntax-object? 309}# #{id 1352}#)
             (let ((#{id 1444}#
                     (#{syntax-object-expression 311}# #{id 1352}#))
                   (#{w1 1445}#
                     (#{syntax-object-wrap 313}# #{id 1352}#)))
               (let ((#{marks 1447}#
                       (#{join-marks 393}#
                         (car #{w 1353}#)
                         (car #{w1 1445}#))))
                 (call-with-values
                   (lambda ()
                     (#{search 1358}#
                       #{id 1444}#
                       (cdr #{w 1353}#)
                       #{marks 1447}#))
                   (lambda (#{new-id 1451}# #{marks 1452}#)
                     (let ((#{t 1457}# #{new-id 1451}#))
                       (if #{t 1457}#
                         #{t 1457}#
                         (let ((#{t 1460}#
                                 (#{search 1358}#
                                   #{id 1444}#
                                   (cdr #{w1 1445}#)
                                   #{marks 1452}#)))
                           (if #{t 1460}# #{t 1460}# #{id 1444}#))))))))
             (syntax-violation
               'id-var-name
               "invalid id"
               #{id 1352}#))))))
   (#{free-id=? 399}#
     (lambda (#{i 1465}# #{j 1466}#)
       (let ((#{ni 1471}#
               (#{id-var-name 397}# #{i 1465}# '(())))
             (#{nj 1472}#
               (#{id-var-name 397}# #{j 1466}# '(()))))
         (letrec*
           ((#{id-module-binding 1476}#
              (lambda (#{id 1477}#)
                (let ((#{mod 1480}#
                        (if (#{syntax-object? 309}# #{id 1477}#)
                          (#{syntax-object-module 315}# #{id 1477}#)
                          #f)))
                  (module-variable
                    (if #{mod 1480}#
                      (resolve-module (cdr #{mod 1480}#))
                      (current-module))
                    (let ((#{x 1485}# #{id 1477}#))
                      (if (#{syntax-object? 309}# #{x 1485}#)
                        (#{syntax-object-expression 311}# #{x 1485}#)
                        #{x 1485}#)))))))
           (if (eq? #{ni 1471}#
                    (let ((#{x 1488}# #{i 1465}#))
                      (if (#{syntax-object? 309}# #{x 1488}#)
                        (#{syntax-object-expression 311}# #{x 1488}#)
                        #{x 1488}#)))
             (if (eq? #{nj 1472}#
                      (let ((#{x 1492}# #{j 1466}#))
                        (if (#{syntax-object? 309}# #{x 1492}#)
                          (#{syntax-object-expression 311}# #{x 1492}#)
                          #{x 1492}#)))
               (if (let ((#{bi 1495}#
                           (#{id-module-binding 1476}# #{i 1465}#)))
                     (if #{bi 1495}#
                       (eq? #{bi 1495}#
                            (#{id-module-binding 1476}# #{j 1466}#))
                       (if (not (#{id-module-binding 1476}# #{j 1466}#))
                         (eq? #{ni 1471}# #{nj 1472}#)
                         #f)))
                 (eq? (#{id-module-binding 1476}# #{i 1465}#)
                      (#{id-module-binding 1476}# #{j 1466}#))
                 #f)
               #f)
             (if (eq? #{ni 1471}# #{nj 1472}#)
               (not (eq? #{nj 1472}#
                         (let ((#{x 1503}# #{j 1466}#))
                           (if (#{syntax-object? 309}# #{x 1503}#)
                             (#{syntax-object-expression 311}# #{x 1503}#)
                             #{x 1503}#))))
               #f))))))
   (#{bound-id=? 401}#
     (lambda (#{i 1504}# #{j 1505}#)
       (if (if (#{syntax-object? 309}# #{i 1504}#)
             (#{syntax-object? 309}# #{j 1505}#)
             #f)
         (if (eq? (#{syntax-object-expression 311}# #{i 1504}#)
                  (#{syntax-object-expression 311}# #{j 1505}#))
           (#{same-marks? 395}#
             (car (#{syntax-object-wrap 313}# #{i 1504}#))
             (car (#{syntax-object-wrap 313}# #{j 1505}#)))
           #f)
         (eq? #{i 1504}# #{j 1505}#))))
   (#{valid-bound-ids? 403}#
     (lambda (#{ids 1514}#)
       (if (letrec*
             ((#{all-ids? 1519}#
                (lambda (#{ids 1520}#)
                  (let ((#{t 1523}# (null? #{ids 1520}#)))
                    (if #{t 1523}#
                      #{t 1523}#
                      (if (#{id? 343}# (car #{ids 1520}#))
                        (#{all-ids? 1519}# (cdr #{ids 1520}#))
                        #f))))))
             (#{all-ids? 1519}# #{ids 1514}#))
         (#{distinct-bound-ids? 405}# #{ids 1514}#)
         #f)))
   (#{distinct-bound-ids? 405}#
     (lambda (#{ids 1528}#)
       (letrec*
         ((#{distinct? 1532}#
            (lambda (#{ids 1533}#)
              (let ((#{t 1536}# (null? #{ids 1533}#)))
                (if #{t 1536}#
                  #{t 1536}#
                  (if (not (#{bound-id-member? 407}#
                             (car #{ids 1533}#)
                             (cdr #{ids 1533}#)))
                    (#{distinct? 1532}# (cdr #{ids 1533}#))
                    #f))))))
         (#{distinct? 1532}# #{ids 1528}#))))
   (#{bound-id-member? 407}#
     (lambda (#{x 1540}# #{list 1541}#)
       (if (not (null? #{list 1541}#))
         (let ((#{t 1548}#
                 (#{bound-id=? 401}#
                   #{x 1540}#
                   (car #{list 1541}#))))
           (if #{t 1548}#
             #{t 1548}#
             (#{bound-id-member? 407}#
               #{x 1540}#
               (cdr #{list 1541}#))))
         #f)))
   (#{wrap 409}#
     (lambda (#{x 1550}# #{w 1551}# #{defmod 1552}#)
       (if (if (null? (car #{w 1551}#))
             (null? (cdr #{w 1551}#))
             #f)
         #{x 1550}#
         (if (#{syntax-object? 309}# #{x 1550}#)
           (#{make-syntax-object 307}#
             (#{syntax-object-expression 311}# #{x 1550}#)
             (#{join-wraps 391}#
               #{w 1551}#
               (#{syntax-object-wrap 313}# #{x 1550}#))
             (#{syntax-object-module 315}# #{x 1550}#))
           (if (null? #{x 1550}#)
             #{x 1550}#
             (#{make-syntax-object 307}#
               #{x 1550}#
               #{w 1551}#
               #{defmod 1552}#))))))
   (#{source-wrap 411}#
     (lambda (#{x 1567}#
              #{w 1568}#
              #{s 1569}#
              #{defmod 1570}#)
       (#{wrap 409}#
         (#{decorate-source 261}# #{x 1567}# #{s 1569}#)
         #{w 1568}#
         #{defmod 1570}#)))
   (#{chi-sequence 413}#
     (lambda (#{body 1575}#
              #{r 1576}#
              #{w 1577}#
              #{s 1578}#
              #{mod 1579}#)
       (#{build-sequence 297}#
         #{s 1578}#
         (letrec*
           ((#{dobody 1590}#
              (lambda (#{body 1591}#
                       #{r 1592}#
                       #{w 1593}#
                       #{mod 1594}#)
                (if (null? #{body 1591}#)
                  '()
                  (let ((#{first 1596}#
                          (#{chi 423}#
                            (car #{body 1591}#)
                            #{r 1592}#
                            #{w 1593}#
                            #{mod 1594}#)))
                    (cons #{first 1596}#
                          (#{dobody 1590}#
                            (cdr #{body 1591}#)
                            #{r 1592}#
                            #{w 1593}#
                            #{mod 1594}#)))))))
           (#{dobody 1590}#
             #{body 1575}#
             #{r 1576}#
             #{w 1577}#
             #{mod 1579}#)))))
   (#{chi-top-sequence 415}#
     (lambda (#{body 1597}#
              #{r 1598}#
              #{w 1599}#
              #{s 1600}#
              #{m 1601}#
              #{esew 1602}#
              #{mod 1603}#)
       (letrec*
         ((#{scan 1612}#
            (lambda (#{body 1613}#
                     #{r 1614}#
                     #{w 1615}#
                     #{s 1616}#
                     #{m 1617}#
                     #{esew 1618}#
                     #{mod 1619}#
                     #{exps 1620}#)
              (if (null? #{body 1613}#)
                #{exps 1620}#
                (call-with-values
                  (lambda ()
                    (call-with-values
                      (lambda ()
                        (let ((#{e 1633}# (car #{body 1613}#)))
                          (#{syntax-type 421}#
                            #{e 1633}#
                            #{r 1614}#
                            #{w 1615}#
                            (let ((#{t 1636}#
                                    (#{source-annotation 324}# #{e 1633}#)))
                              (if #{t 1636}# #{t 1636}# #{s 1616}#))
                            #f
                            #{mod 1619}#
                            #f)))
                      (lambda (#{type 1638}#
                               #{value 1639}#
                               #{e 1640}#
                               #{w 1641}#
                               #{s 1642}#
                               #{mod 1643}#)
                        (if (memv #{type 1638}# '(begin-form))
                          (let ((#{tmp 1651}# #{e 1640}#))
                            (let ((#{tmp 1652}#
                                    ($sc-dispatch #{tmp 1651}# '(_))))
                              (if #{tmp 1652}#
                                (@apply (lambda () #{exps 1620}#) #{tmp 1652}#)
                                (let ((#{tmp 1653}#
                                        ($sc-dispatch
                                          #{tmp 1651}#
                                          '(_ any . each-any))))
                                  (if #{tmp 1653}#
                                    (@apply
                                      (lambda (#{e1 1656}# #{e2 1657}#)
                                        (#{scan 1612}#
                                          (cons #{e1 1656}# #{e2 1657}#)
                                          #{r 1614}#
                                          #{w 1641}#
                                          #{s 1642}#
                                          #{m 1617}#
                                          #{esew 1618}#
                                          #{mod 1643}#
                                          #{exps 1620}#))
                                      #{tmp 1653}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp 1651}#))))))
                          (if (memv #{type 1638}# '(local-syntax-form))
                            (#{chi-local-syntax 433}#
                              #{value 1639}#
                              #{e 1640}#
                              #{r 1614}#
                              #{w 1641}#
                              #{s 1642}#
                              #{mod 1643}#
                              (lambda (#{body 1660}#
                                       #{r 1661}#
                                       #{w 1662}#
                                       #{s 1663}#
                                       #{mod 1664}#)
                                (#{scan 1612}#
                                  #{body 1660}#
                                  #{r 1661}#
                                  #{w 1662}#
                                  #{s 1663}#
                                  #{m 1617}#
                                  #{esew 1618}#
                                  #{mod 1664}#
                                  #{exps 1620}#)))
                            (if (memv #{type 1638}# '(eval-when-form))
                              (let ((#{tmp 1671}# #{e 1640}#))
                                (let ((#{tmp 1672}#
                                        ($sc-dispatch
                                          #{tmp 1671}#
                                          '(_ each-any any . each-any))))
                                  (if #{tmp 1672}#
                                    (@apply
                                      (lambda (#{x 1676}#
                                               #{e1 1677}#
                                               #{e2 1678}#)
                                        (let ((#{when-list 1681}#
                                                (#{chi-when-list 419}#
                                                  #{e 1640}#
                                                  #{x 1676}#
                                                  #{w 1641}#))
                                              (#{body 1682}#
                                                (cons #{e1 1677}#
                                                      #{e2 1678}#)))
                                          (if (eq? #{m 1617}# 'e)
                                            (if (memq 'eval #{when-list 1681}#)
                                              (#{scan 1612}#
                                                #{body 1682}#
                                                #{r 1614}#
                                                #{w 1641}#
                                                #{s 1642}#
                                                (if (memq 'expand
                                                          #{when-list 1681}#)
                                                  'c&e
                                                  'e)
                                                '(eval)
                                                #{mod 1643}#
                                                #{exps 1620}#)
                                              (begin
                                                (if (memq 'expand
                                                          #{when-list 1681}#)
                                                  (#{top-level-eval-hook 252}#
                                                    (#{chi-top-sequence 415}#
                                                      #{body 1682}#
                                                      #{r 1614}#
                                                      #{w 1641}#
                                                      #{s 1642}#
                                                      'e
                                                      '(eval)
                                                      #{mod 1643}#)
                                                    #{mod 1643}#))
                                                (values #{exps 1620}#)))
                                            (if (memq 'load #{when-list 1681}#)
                                              (if (let ((#{t 1691}#
                                                          (memq 'compile
                                                                #{when-list 1681}#)))
                                                    (if #{t 1691}#
                                                      #{t 1691}#
                                                      (let ((#{t 1694}#
                                                              (memq 'expand
                                                                    #{when-list 1681}#)))
                                                        (if #{t 1694}#
                                                          #{t 1694}#
                                                          (if (eq? #{m 1617}#
                                                                   'c&e)
                                                            (memq 'eval
                                                                  #{when-list 1681}#)
                                                            #f)))))
                                                (#{scan 1612}#
                                                  #{body 1682}#
                                                  #{r 1614}#
                                                  #{w 1641}#
                                                  #{s 1642}#
                                                  'c&e
                                                  '(compile load)
                                                  #{mod 1643}#
                                                  #{exps 1620}#)
                                                (if (memq #{m 1617}# '(c c&e))
                                                  (#{scan 1612}#
                                                    #{body 1682}#
                                                    #{r 1614}#
                                                    #{w 1641}#
                                                    #{s 1642}#
                                                    'c
                                                    '(load)
                                                    #{mod 1643}#
                                                    #{exps 1620}#)
                                                  (values #{exps 1620}#)))
                                              (if (let ((#{t 1702}#
                                                          (memq 'compile
                                                                #{when-list 1681}#)))
                                                    (if #{t 1702}#
                                                      #{t 1702}#
                                                      (let ((#{t 1705}#
                                                              (memq 'expand
                                                                    #{when-list 1681}#)))
                                                        (if #{t 1705}#
                                                          #{t 1705}#
                                                          (if (eq? #{m 1617}#
                                                                   'c&e)
                                                            (memq 'eval
                                                                  #{when-list 1681}#)
                                                            #f)))))
                                                (begin
                                                  (#{top-level-eval-hook 252}#
                                                    (#{chi-top-sequence 415}#
                                                      #{body 1682}#
                                                      #{r 1614}#
                                                      #{w 1641}#
                                                      #{s 1642}#
                                                      'e
                                                      '(eval)
                                                      #{mod 1643}#)
                                                    #{mod 1643}#)
                                                  (values #{exps 1620}#))
                                                (values #{exps 1620}#))))))
                                      #{tmp 1672}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp 1671}#))))
                              (if (memv #{type 1638}# '(define-syntax-form))
                                (let ((#{n 1713}#
                                        (#{id-var-name 397}#
                                          #{value 1639}#
                                          #{w 1641}#))
                                      (#{r 1714}#
                                        (#{macros-only-env 335}# #{r 1614}#)))
                                  (if (memv #{m 1617}# '(c))
                                    (if (memq 'compile #{esew 1618}#)
                                      (let ((#{e 1717}#
                                              (#{chi-install-global 417}#
                                                #{n 1713}#
                                                (#{chi 423}#
                                                  #{e 1640}#
                                                  #{r 1714}#
                                                  #{w 1641}#
                                                  #{mod 1643}#))))
                                        (begin
                                          (#{top-level-eval-hook 252}#
                                            #{e 1717}#
                                            #{mod 1643}#)
                                          (if (memq 'load #{esew 1618}#)
                                            (values
                                              (cons #{e 1717}# #{exps 1620}#))
                                            (values #{exps 1620}#))))
                                      (if (memq 'load #{esew 1618}#)
                                        (values
                                          (cons (#{chi-install-global 417}#
                                                  #{n 1713}#
                                                  (#{chi 423}#
                                                    #{e 1640}#
                                                    #{r 1714}#
                                                    #{w 1641}#
                                                    #{mod 1643}#))
                                                #{exps 1620}#))
                                        (values #{exps 1620}#)))
                                    (if (memv #{m 1617}# '(c&e))
                                      (let ((#{e 1720}#
                                              (#{chi-install-global 417}#
                                                #{n 1713}#
                                                (#{chi 423}#
                                                  #{e 1640}#
                                                  #{r 1714}#
                                                  #{w 1641}#
                                                  #{mod 1643}#))))
                                        (begin
                                          (#{top-level-eval-hook 252}#
                                            #{e 1720}#
                                            #{mod 1643}#)
                                          (values
                                            (cons #{e 1720}# #{exps 1620}#))))
                                      (begin
                                        (if (memq 'eval #{esew 1618}#)
                                          (#{top-level-eval-hook 252}#
                                            (#{chi-install-global 417}#
                                              #{n 1713}#
                                              (#{chi 423}#
                                                #{e 1640}#
                                                #{r 1714}#
                                                #{w 1641}#
                                                #{mod 1643}#))
                                            #{mod 1643}#))
                                        (values #{exps 1620}#)))))
                                (if (memv #{type 1638}# '(define-form))
                                  (let ((#{n 1725}#
                                          (#{id-var-name 397}#
                                            #{value 1639}#
                                            #{w 1641}#)))
                                    (let ((#{type 1727}#
                                            (car (#{lookup 337}#
                                                   #{n 1725}#
                                                   #{r 1614}#
                                                   #{mod 1643}#))))
                                      (if (memv #{type 1727}#
                                                '(global
                                                   core
                                                   macro
                                                   module-ref))
                                        (begin
                                          (if (if (memq #{m 1617}# '(c c&e))
                                                (if (not (module-local-variable
                                                           (current-module)
                                                           #{n 1725}#))
                                                  (current-module)
                                                  #f)
                                                #f)
                                            (let ((#{old 1734}#
                                                    (module-variable
                                                      (current-module)
                                                      #{n 1725}#)))
                                              (if (if (variable? #{old 1734}#)
                                                    (variable-bound?
                                                      #{old 1734}#)
                                                    #f)
                                                (module-define!
                                                  (current-module)
                                                  #{n 1725}#
                                                  (variable-ref #{old 1734}#))
                                                (module-add!
                                                  (current-module)
                                                  #{n 1725}#
                                                  (make-undefined-variable)))))
                                          (values
                                            (cons (if (eq? #{m 1617}# 'c&e)
                                                    (let ((#{x 1738}#
                                                            (#{build-global-definition 283}#
                                                              #{s 1642}#
                                                              #{n 1725}#
                                                              (#{chi 423}#
                                                                #{e 1640}#
                                                                #{r 1614}#
                                                                #{w 1641}#
                                                                #{mod 1643}#))))
                                                      (begin
                                                        (#{top-level-eval-hook 252}#
                                                          #{x 1738}#
                                                          #{mod 1643}#)
                                                        #{x 1738}#))
                                                    (lambda ()
                                                      (#{build-global-definition 283}#
                                                        #{s 1642}#
                                                        #{n 1725}#
                                                        (#{chi 423}#
                                                          #{e 1640}#
                                                          #{r 1614}#
                                                          #{w 1641}#
                                                          #{mod 1643}#))))
                                                  #{exps 1620}#)))
                                        (if (memv #{type 1727}#
                                                  '(displaced-lexical))
                                          (syntax-violation
                                            #f
                                            "identifier out of context"
                                            #{e 1640}#
                                            (#{wrap 409}#
                                              #{value 1639}#
                                              #{w 1641}#
                                              #{mod 1643}#))
                                          (syntax-violation
                                            #f
                                            "cannot define keyword at top level"
                                            #{e 1640}#
                                            (#{wrap 409}#
                                              #{value 1639}#
                                              #{w 1641}#
                                              #{mod 1643}#))))))
                                  (values
                                    (cons (if (eq? #{m 1617}# 'c&e)
                                            (let ((#{x 1743}#
                                                    (#{chi-expr 425}#
                                                      #{type 1638}#
                                                      #{value 1639}#
                                                      #{e 1640}#
                                                      #{r 1614}#
                                                      #{w 1641}#
                                                      #{s 1642}#
                                                      #{mod 1643}#)))
                                              (begin
                                                (#{top-level-eval-hook 252}#
                                                  #{x 1743}#
                                                  #{mod 1643}#)
                                                #{x 1743}#))
                                            (lambda ()
                                              (#{chi-expr 425}#
                                                #{type 1638}#
                                                #{value 1639}#
                                                #{e 1640}#
                                                #{r 1614}#
                                                #{w 1641}#
                                                #{s 1642}#
                                                #{mod 1643}#)))
                                          #{exps 1620}#))))))))))
                  (lambda (#{exps 1744}#)
                    (#{scan 1612}#
                      (cdr #{body 1613}#)
                      #{r 1614}#
                      #{w 1615}#
                      #{s 1616}#
                      #{m 1617}#
                      #{esew 1618}#
                      #{mod 1619}#
                      #{exps 1744}#)))))))
         (call-with-values
           (lambda ()
             (#{scan 1612}#
               #{body 1597}#
               #{r 1598}#
               #{w 1599}#
               #{s 1600}#
               #{m 1601}#
               #{esew 1602}#
               #{mod 1603}#
               '()))
           (lambda (#{exps 1746}#)
             (if (null? #{exps 1746}#)
               (#{build-void 265}# #{s 1600}#)
               (#{build-sequence 297}#
                 #{s 1600}#
                 (letrec*
                   ((#{lp 1751}#
                      (lambda (#{in 1752}# #{out 1753}#)
                        (if (null? #{in 1752}#)
                          #{out 1753}#
                          (let ((#{e 1755}# (car #{in 1752}#)))
                            (#{lp 1751}#
                              (cdr #{in 1752}#)
                              (cons (if (procedure? #{e 1755}#)
                                      (#{e 1755}#)
                                      #{e 1755}#)
                                    #{out 1753}#)))))))
                   (#{lp 1751}# #{exps 1746}# '())))))))))
   (#{chi-install-global 417}#
     (lambda (#{name 1756}# #{e 1757}#)
       (#{build-global-definition 283}#
         #f
         #{name 1756}#
         (#{build-primcall 291}#
           #f
           'make-syntax-transformer
           (list (#{build-data 295}# #f #{name 1756}#)
                 (#{build-data 295}# #f 'macro)
                 #{e 1757}#)))))
   (#{chi-when-list 419}#
     (lambda (#{e 1764}# #{when-list 1765}# #{w 1766}#)
       (letrec*
         ((#{f 1773}#
            (lambda (#{when-list 1774}# #{situations 1775}#)
              (if (null? #{when-list 1774}#)
                #{situations 1775}#
                (#{f 1773}#
                  (cdr #{when-list 1774}#)
                  (cons (let ((#{x 1777}#
                                (syntax->datum (car #{when-list 1774}#))))
                          (if (memq #{x 1777}# '(compile load eval expand))
                            #{x 1777}#
                            (syntax-violation
                              'eval-when
                              "invalid situation"
                              #{e 1764}#
                              (#{wrap 409}#
                                (car #{when-list 1774}#)
                                #{w 1766}#
                                #f))))
                        #{situations 1775}#))))))
         (#{f 1773}# #{when-list 1765}# '()))))
   (#{syntax-type 421}#
     (lambda (#{e 1778}#
              #{r 1779}#
              #{w 1780}#
              #{s 1781}#
              #{rib 1782}#
              #{mod 1783}#
              #{for-car? 1784}#)
       (if (symbol? #{e 1778}#)
         (let ((#{n 1796}#
                 (#{id-var-name 397}# #{e 1778}# #{w 1780}#)))
           (let ((#{b 1798}#
                   (#{lookup 337}#
                     #{n 1796}#
                     #{r 1779}#
                     #{mod 1783}#)))
             (let ((#{type 1800}# (car #{b 1798}#)))
               (if (memv #{type 1800}# '(lexical))
                 (values
                   #{type 1800}#
                   (cdr #{b 1798}#)
                   #{e 1778}#
                   #{w 1780}#
                   #{s 1781}#
                   #{mod 1783}#)
                 (if (memv #{type 1800}# '(global))
                   (values
                     #{type 1800}#
                     #{n 1796}#
                     #{e 1778}#
                     #{w 1780}#
                     #{s 1781}#
                     #{mod 1783}#)
                   (if (memv #{type 1800}# '(macro))
                     (if #{for-car? 1784}#
                       (values
                         #{type 1800}#
                         (cdr #{b 1798}#)
                         #{e 1778}#
                         #{w 1780}#
                         #{s 1781}#
                         #{mod 1783}#)
                       (#{syntax-type 421}#
                         (#{chi-macro 429}#
                           (cdr #{b 1798}#)
                           #{e 1778}#
                           #{r 1779}#
                           #{w 1780}#
                           #{s 1781}#
                           #{rib 1782}#
                           #{mod 1783}#)
                         #{r 1779}#
                         '(())
                         #{s 1781}#
                         #{rib 1782}#
                         #{mod 1783}#
                         #f))
                     (values
                       #{type 1800}#
                       (cdr #{b 1798}#)
                       #{e 1778}#
                       #{w 1780}#
                       #{s 1781}#
                       #{mod 1783}#)))))))
         (if (pair? #{e 1778}#)
           (let ((#{first 1814}# (car #{e 1778}#)))
             (call-with-values
               (lambda ()
                 (#{syntax-type 421}#
                   #{first 1814}#
                   #{r 1779}#
                   #{w 1780}#
                   #{s 1781}#
                   #{rib 1782}#
                   #{mod 1783}#
                   #t))
               (lambda (#{ftype 1815}#
                        #{fval 1816}#
                        #{fe 1817}#
                        #{fw 1818}#
                        #{fs 1819}#
                        #{fmod 1820}#)
                 (if (memv #{ftype 1815}# '(lexical))
                   (values
                     'lexical-call
                     #{fval 1816}#
                     #{e 1778}#
                     #{w 1780}#
                     #{s 1781}#
                     #{mod 1783}#)
                   (if (memv #{ftype 1815}# '(global))
                     (values
                       'global-call
                       (#{make-syntax-object 307}#
                         #{fval 1816}#
                         #{w 1780}#
                         #{fmod 1820}#)
                       #{e 1778}#
                       #{w 1780}#
                       #{s 1781}#
                       #{mod 1783}#)
                     (if (memv #{ftype 1815}# '(macro))
                       (#{syntax-type 421}#
                         (#{chi-macro 429}#
                           #{fval 1816}#
                           #{e 1778}#
                           #{r 1779}#
                           #{w 1780}#
                           #{s 1781}#
                           #{rib 1782}#
                           #{mod 1783}#)
                         #{r 1779}#
                         '(())
                         #{s 1781}#
                         #{rib 1782}#
                         #{mod 1783}#
                         #{for-car? 1784}#)
                       (if (memv #{ftype 1815}# '(module-ref))
                         (call-with-values
                           (lambda ()
                             (#{fval 1816}# #{e 1778}# #{r 1779}# #{w 1780}#))
                           (lambda (#{e 1832}#
                                    #{r 1833}#
                                    #{w 1834}#
                                    #{s 1835}#
                                    #{mod 1836}#)
                             (#{syntax-type 421}#
                               #{e 1832}#
                               #{r 1833}#
                               #{w 1834}#
                               #{s 1835}#
                               #{rib 1782}#
                               #{mod 1836}#
                               #{for-car? 1784}#)))
                         (if (memv #{ftype 1815}# '(core))
                           (values
                             'core-form
                             #{fval 1816}#
                             #{e 1778}#
                             #{w 1780}#
                             #{s 1781}#
                             #{mod 1783}#)
                           (if (memv #{ftype 1815}# '(local-syntax))
                             (values
                               'local-syntax-form
                               #{fval 1816}#
                               #{e 1778}#
                               #{w 1780}#
                               #{s 1781}#
                               #{mod 1783}#)
                             (if (memv #{ftype 1815}# '(begin))
                               (values
                                 'begin-form
                                 #f
                                 #{e 1778}#
                                 #{w 1780}#
                                 #{s 1781}#
                                 #{mod 1783}#)
                               (if (memv #{ftype 1815}# '(eval-when))
                                 (values
                                   'eval-when-form
                                   #f
                                   #{e 1778}#
                                   #{w 1780}#
                                   #{s 1781}#
                                   #{mod 1783}#)
                                 (if (memv #{ftype 1815}# '(define))
                                   (let ((#{tmp 1847}# #{e 1778}#))
                                     (let ((#{tmp 1848}#
                                             ($sc-dispatch
                                               #{tmp 1847}#
                                               '(_ any any))))
                                       (if (if #{tmp 1848}#
                                             (@apply
                                               (lambda (#{name 1851}#
                                                        #{val 1852}#)
                                                 (#{id? 343}# #{name 1851}#))
                                               #{tmp 1848}#)
                                             #f)
                                         (@apply
                                           (lambda (#{name 1855}# #{val 1856}#)
                                             (values
                                               'define-form
                                               #{name 1855}#
                                               #{val 1856}#
                                               #{w 1780}#
                                               #{s 1781}#
                                               #{mod 1783}#))
                                           #{tmp 1848}#)
                                         (let ((#{tmp 1857}#
                                                 ($sc-dispatch
                                                   #{tmp 1847}#
                                                   '(_ (any . any)
                                                       any
                                                       .
                                                       each-any))))
                                           (if (if #{tmp 1857}#
                                                 (@apply
                                                   (lambda (#{name 1862}#
                                                            #{args 1863}#
                                                            #{e1 1864}#
                                                            #{e2 1865}#)
                                                     (if (#{id? 343}#
                                                           #{name 1862}#)
                                                       (#{valid-bound-ids? 403}#
                                                         (#{lambda-var-list 453}#
                                                           #{args 1863}#))
                                                       #f))
                                                   #{tmp 1857}#)
                                                 #f)
                                             (@apply
                                               (lambda (#{name 1872}#
                                                        #{args 1873}#
                                                        #{e1 1874}#
                                                        #{e2 1875}#)
                                                 (values
                                                   'define-form
                                                   (#{wrap 409}#
                                                     #{name 1872}#
                                                     #{w 1780}#
                                                     #{mod 1783}#)
                                                   (#{decorate-source 261}#
                                                     (cons '#(syntax-object
                                                              lambda
                                                              ((top)
                                                               #(ribcage
                                                                 #(name
                                                                   args
                                                                   e1
                                                                   e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i1868"
                                                                   "i1869"
                                                                   "i1870"
                                                                   "i1871"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(ftype
                                                                   fval
                                                                   fe
                                                                   fw
                                                                   fs
                                                                   fmod)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i1821"
                                                                   "i1822"
                                                                   "i1823"
                                                                   "i1824"
                                                                   "i1825"
                                                                   "i1826"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(first)
                                                                 #((top))
                                                                 #("i1813"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(e
                                                                   r
                                                                   w
                                                                   s
                                                                   rib
                                                                   mod
                                                                   for-car?)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i1785"
                                                                   "i1786"
                                                                   "i1787"
                                                                   "i1788"
                                                                   "i1789"
                                                                   "i1790"
                                                                   "i1791"))
                                                               #(ribcage
                                                                 (lambda-var-list
                                                                   gen-var
                                                                   strip
                                                                   chi-lambda-case
                                                                   lambda*-formals
                                                                   chi-simple-lambda
                                                                   lambda-formals
                                                                   ellipsis?
                                                                   chi-void
                                                                   eval-local-transformer
                                                                   chi-local-syntax
                                                                   chi-body
                                                                   chi-macro
                                                                   chi-call
                                                                   chi-expr
                                                                   chi
                                                                   syntax-type
                                                                   chi-when-list
                                                                   chi-install-global
                                                                   chi-top-sequence
                                                                   chi-sequence
                                                                   source-wrap
                                                                   wrap
                                                                   bound-id-member?
                                                                   distinct-bound-ids?
                                                                   valid-bound-ids?
                                                                   bound-id=?
                                                                   free-id=?
                                                                   id-var-name
                                                                   same-marks?
                                                                   join-marks
                                                                   join-wraps
                                                                   smart-append
                                                                   make-binding-wrap
                                                                   extend-ribcage!
                                                                   make-empty-ribcage
                                                                   new-mark
                                                                   anti-mark
                                                                   the-anti-mark
                                                                   top-marked?
                                                                   top-wrap
                                                                   empty-wrap
                                                                   set-ribcage-labels!
                                                                   set-ribcage-marks!
                                                                   set-ribcage-symnames!
                                                                   ribcage-labels
                                                                   ribcage-marks
                                                                   ribcage-symnames
                                                                   ribcage?
                                                                   make-ribcage
                                                                   gen-labels
                                                                   gen-label
                                                                   make-rename
                                                                   rename-marks
                                                                   rename-new
                                                                   rename-old
                                                                   subst-rename?
                                                                   wrap-subst
                                                                   wrap-marks
                                                                   make-wrap
                                                                   id-sym-name&marks
                                                                   id-sym-name
                                                                   id?
                                                                   nonsymbol-id?
                                                                   global-extend
                                                                   lookup
                                                                   macros-only-env
                                                                   extend-var-env
                                                                   extend-env
                                                                   null-env
                                                                   binding-value
                                                                   binding-type
                                                                   make-binding
                                                                   arg-check
                                                                   source-annotation
                                                                   no-source
                                                                   set-syntax-object-module!
                                                                   set-syntax-object-wrap!
                                                                   set-syntax-object-expression!
                                                                   syntax-object-module
                                                                   syntax-object-wrap
                                                                   syntax-object-expression
                                                                   syntax-object?
                                                                   make-syntax-object
                                                                   build-lexical-var
                                                                   build-letrec
                                                                   build-named-let
                                                                   build-let
                                                                   build-sequence
                                                                   build-data
                                                                   build-primref
                                                                   build-primcall
                                                                   build-lambda-case
                                                                   build-case-lambda
                                                                   build-simple-lambda
                                                                   build-global-definition
                                                                   build-global-assignment
                                                                   build-global-reference
                                                                   analyze-variable
                                                                   build-lexical-assignment
                                                                   build-lexical-reference
                                                                   build-dynlet
                                                                   build-conditional
                                                                   build-call
                                                                   build-void
                                                                   maybe-name-value!
                                                                   decorate-source
                                                                   get-global-definition-hook
                                                                   put-global-definition-hook
                                                                   gensym-hook
                                                                   local-eval-hook
                                                                   top-level-eval-hook
                                                                   fx<
                                                                   fx=
                                                                   fx-
                                                                   fx+
                                                                   set-lambda-meta!
                                                                   lambda-meta
                                                                   lambda?
                                                                   make-dynlet
                                                                   make-letrec
                                                                   make-let
                                                                   make-lambda-case
                                                                   make-lambda
                                                                   make-seq
                                                                   make-primcall
                                                                   make-call
                                                                   make-conditional
                                                                   make-toplevel-define
                                                                   make-toplevel-set
                                                                   make-toplevel-ref
                                                                   make-module-set
                                                                   make-module-ref
                                                                   make-lexical-set
                                                                   make-lexical-ref
                                                                   make-primitive-ref
                                                                   make-const
                                                                   make-void)
                                                                 ((top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top))
                                                                 ("i452"
                                                                  "i450"
                                                                  "i448"
                                                                  "i446"
                                                                  "i444"
                                                                  "i442"
                                                                  "i440"
                                                                  "i438"
                                                                  "i436"
                                                                  "i434"
                                                                  "i432"
                                                                  "i430"
                                                                  "i428"
                                                                  "i426"
                                                                  "i424"
                                                                  "i422"
                                                                  "i420"
                                                                  "i418"
                                                                  "i416"
                                                                  "i414"
                                                                  "i412"
                                                                  "i410"
                                                                  "i408"
                                                                  "i406"
                                                                  "i404"
                                                                  "i402"
                                                                  "i400"
                                                                  "i398"
                                                                  "i396"
                                                                  "i394"
                                                                  "i392"
                                                                  "i390"
                                                                  "i388"
                                                                  "i386"
                                                                  "i384"
                                                                  "i383"
                                                                  "i382"
                                                                  "i380"
                                                                  "i379"
                                                                  "i378"
                                                                  "i377"
                                                                  "i376"
                                                                  "i374"
                                                                  "i372"
                                                                  "i370"
                                                                  "i368"
                                                                  "i366"
                                                                  "i364"
                                                                  "i362"
                                                                  "i360"
                                                                  "i357"
                                                                  "i355"
                                                                  "i354"
                                                                  "i353"
                                                                  "i352"
                                                                  "i351"
                                                                  "i350"
                                                                  "i349"
                                                                  "i348"
                                                                  "i347"
                                                                  "i345"
                                                                  "i344"
                                                                  "i342"
                                                                  "i340"
                                                                  "i338"
                                                                  "i336"
                                                                  "i334"
                                                                  "i332"
                                                                  "i330"
                                                                  "i329"
                                                                  "i328"
                                                                  "i327"
                                                                  "i326"
                                                                  "i325"
                                                                  "i323"
                                                                  "i322"
                                                                  "i320"
                                                                  "i318"
                                                                  "i316"
                                                                  "i314"
                                                                  "i312"
                                                                  "i310"
                                                                  "i308"
                                                                  "i306"
                                                                  "i304"
                                                                  "i302"
                                                                  "i300"
                                                                  "i298"
                                                                  "i296"
                                                                  "i294"
                                                                  "i292"
                                                                  "i290"
                                                                  "i288"
                                                                  "i286"
                                                                  "i284"
                                                                  "i282"
                                                                  "i280"
                                                                  "i278"
                                                                  "i276"
                                                                  "i274"
                                                                  "i272"
                                                                  "i270"
                                                                  "i268"
                                                                  "i266"
                                                                  "i264"
                                                                  "i262"
                                                                  "i260"
                                                                  "i258"
                                                                  "i256"
                                                                  "i255"
                                                                  "i253"
                                                                  "i251"
                                                                  "i250"
                                                                  "i249"
                                                                  "i248"
                                                                  "i247"
                                                                  "i245"
                                                                  "i243"
                                                                  "i241"
                                                                  "i238"
                                                                  "i236"
                                                                  "i234"
                                                                  "i232"
                                                                  "i230"
                                                                  "i228"
                                                                  "i226"
                                                                  "i224"
                                                                  "i222"
                                                                  "i220"
                                                                  "i218"
                                                                  "i216"
                                                                  "i214"
                                                                  "i212"
                                                                  "i210"
                                                                  "i208"
                                                                  "i206"
                                                                  "i204"
                                                                  "i202"))
                                                               #(ribcage
                                                                 (define-structure
                                                                   define-expansion-accessors
                                                                   define-expansion-constructors)
                                                                 ((top)
                                                                  (top)
                                                                  (top))
                                                                 ("i40"
                                                                  "i39"
                                                                  "i38")))
                                                              (hygiene guile))
                                                           (#{wrap 409}#
                                                             (cons #{args 1873}#
                                                                   (cons #{e1 1874}#
                                                                         #{e2 1875}#))
                                                             #{w 1780}#
                                                             #{mod 1783}#))
                                                     #{s 1781}#)
                                                   '(())
                                                   #{s 1781}#
                                                   #{mod 1783}#))
                                               #{tmp 1857}#)
                                             (let ((#{tmp 1878}#
                                                     ($sc-dispatch
                                                       #{tmp 1847}#
                                                       '(_ any))))
                                               (if (if #{tmp 1878}#
                                                     (@apply
                                                       (lambda (#{name 1880}#)
                                                         (#{id? 343}#
                                                           #{name 1880}#))
                                                       #{tmp 1878}#)
                                                     #f)
                                                 (@apply
                                                   (lambda (#{name 1882}#)
                                                     (values
                                                       'define-form
                                                       (#{wrap 409}#
                                                         #{name 1882}#
                                                         #{w 1780}#
                                                         #{mod 1783}#)
                                                       '(#(syntax-object
                                                           if
                                                           ((top)
                                                            #(ribcage
                                                              #(name)
                                                              #((top))
                                                              #("i1881"))
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(ftype
                                                                fval
                                                                fe
                                                                fw
                                                                fs
                                                                fmod)
                                                              #((top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                              #("i1821"
                                                                "i1822"
                                                                "i1823"
                                                                "i1824"
                                                                "i1825"
                                                                "i1826"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(first)
                                                              #((top))
                                                              #("i1813"))
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(e
                                                                r
                                                                w
                                                                s
                                                                rib
                                                                mod
                                                                for-car?)
                                                              #((top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                              #("i1785"
                                                                "i1786"
                                                                "i1787"
                                                                "i1788"
                                                                "i1789"
                                                                "i1790"
                                                                "i1791"))
                                                            #(ribcage
                                                              (lambda-var-list
                                                                gen-var
                                                                strip
                                                                chi-lambda-case
                                                                lambda*-formals
                                                                chi-simple-lambda
                                                                lambda-formals
                                                                ellipsis?
                                                                chi-void
                                                                eval-local-transformer
                                                                chi-local-syntax
                                                                chi-body
                                                                chi-macro
                                                                chi-call
                                                                chi-expr
                                                                chi
                                                                syntax-type
                                                                chi-when-list
                                                                chi-install-global
                                                                chi-top-sequence
                                                                chi-sequence
                                                                source-wrap
                                                                wrap
                                                                bound-id-member?
                                                                distinct-bound-ids?
                                                                valid-bound-ids?
                                                                bound-id=?
                                                                free-id=?
                                                                id-var-name
                                                                same-marks?
                                                                join-marks
                                                                join-wraps
                                                                smart-append
                                                                make-binding-wrap
                                                                extend-ribcage!
                                                                make-empty-ribcage
                                                                new-mark
                                                                anti-mark
                                                                the-anti-mark
                                                                top-marked?
                                                                top-wrap
                                                                empty-wrap
                                                                set-ribcage-labels!
                                                                set-ribcage-marks!
                                                                set-ribcage-symnames!
                                                                ribcage-labels
                                                                ribcage-marks
                                                                ribcage-symnames
                                                                ribcage?
                                                                make-ribcage
                                                                gen-labels
                                                                gen-label
                                                                make-rename
                                                                rename-marks
                                                                rename-new
                                                                rename-old
                                                                subst-rename?
                                                                wrap-subst
                                                                wrap-marks
                                                                make-wrap
                                                                id-sym-name&marks
                                                                id-sym-name
                                                                id?
                                                                nonsymbol-id?
                                                                global-extend
                                                                lookup
                                                                macros-only-env
                                                                extend-var-env
                                                                extend-env
                                                                null-env
                                                                binding-value
                                                                binding-type
                                                                make-binding
                                                                arg-check
                                                                source-annotation
                                                                no-source
                                                                set-syntax-object-module!
                                                                set-syntax-object-wrap!
                                                                set-syntax-object-expression!
                                                                syntax-object-module
                                                                syntax-object-wrap
                                                                syntax-object-expression
                                                                syntax-object?
                                                                make-syntax-object
                                                                build-lexical-var
                                                                build-letrec
                                                                build-named-let
                                                                build-let
                                                                build-sequence
                                                                build-data
                                                                build-primref
                                                                build-primcall
                                                                build-lambda-case
                                                                build-case-lambda
                                                                build-simple-lambda
                                                                build-global-definition
                                                                build-global-assignment
                                                                build-global-reference
                                                                analyze-variable
                                                                build-lexical-assignment
                                                                build-lexical-reference
                                                                build-dynlet
                                                                build-conditional
                                                                build-call
                                                                build-void
                                                                maybe-name-value!
                                                                decorate-source
                                                                get-global-definition-hook
                                                                put-global-definition-hook
                                                                gensym-hook
                                                                local-eval-hook
                                                                top-level-eval-hook
                                                                fx<
                                                                fx=
                                                                fx-
                                                                fx+
                                                                set-lambda-meta!
                                                                lambda-meta
                                                                lambda?
                                                                make-dynlet
                                                                make-letrec
                                                                make-let
                                                                make-lambda-case
                                                                make-lambda
                                                                make-seq
                                                                make-primcall
                                                                make-call
                                                                make-conditional
                                                                make-toplevel-define
                                                                make-toplevel-set
                                                                make-toplevel-ref
                                                                make-module-set
                                                                make-module-ref
                                                                make-lexical-set
                                                                make-lexical-ref
                                                                make-primitive-ref
                                                                make-const
                                                                make-void)
                                                              ((top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top))
                                                              ("i452"
                                                               "i450"
                                                               "i448"
                                                               "i446"
                                                               "i444"
                                                               "i442"
                                                               "i440"
                                                               "i438"
                                                               "i436"
                                                               "i434"
                                                               "i432"
                                                               "i430"
                                                               "i428"
                                                               "i426"
                                                               "i424"
                                                               "i422"
                                                               "i420"
                                                               "i418"
                                                               "i416"
                                                               "i414"
                                                               "i412"
                                                               "i410"
                                                               "i408"
                                                               "i406"
                                                               "i404"
                                                               "i402"
                                                               "i400"
                                                               "i398"
                                                               "i396"
                                                               "i394"
                                                               "i392"
                                                               "i390"
                                                               "i388"
                                                               "i386"
                                                               "i384"
                                                               "i383"
                                                               "i382"
                                                               "i380"
                                                               "i379"
                                                               "i378"
                                                               "i377"
                                                               "i376"
                                                               "i374"
                                                               "i372"
                                                               "i370"
                                                               "i368"
                                                               "i366"
                                                               "i364"
                                                               "i362"
                                                               "i360"
                                                               "i357"
                                                               "i355"
                                                               "i354"
                                                               "i353"
                                                               "i352"
                                                               "i351"
                                                               "i350"
                                                               "i349"
                                                               "i348"
                                                               "i347"
                                                               "i345"
                                                               "i344"
                                                               "i342"
                                                               "i340"
                                                               "i338"
                                                               "i336"
                                                               "i334"
                                                               "i332"
                                                               "i330"
                                                               "i329"
                                                               "i328"
                                                               "i327"
                                                               "i326"
                                                               "i325"
                                                               "i323"
                                                               "i322"
                                                               "i320"
                                                               "i318"
                                                               "i316"
                                                               "i314"
                                                               "i312"
                                                               "i310"
                                                               "i308"
                                                               "i306"
                                                               "i304"
                                                               "i302"
                                                               "i300"
                                                               "i298"
                                                               "i296"
                                                               "i294"
                                                               "i292"
                                                               "i290"
                                                               "i288"
                                                               "i286"
                                                               "i284"
                                                               "i282"
                                                               "i280"
                                                               "i278"
                                                               "i276"
                                                               "i274"
                                                               "i272"
                                                               "i270"
                                                               "i268"
                                                               "i266"
                                                               "i264"
                                                               "i262"
                                                               "i260"
                                                               "i258"
                                                               "i256"
                                                               "i255"
                                                               "i253"
                                                               "i251"
                                                               "i250"
                                                               "i249"
                                                               "i248"
                                                               "i247"
                                                               "i245"
                                                               "i243"
                                                               "i241"
                                                               "i238"
                                                               "i236"
                                                               "i234"
                                                               "i232"
                                                               "i230"
                                                               "i228"
                                                               "i226"
                                                               "i224"
                                                               "i222"
                                                               "i220"
                                                               "i218"
                                                               "i216"
                                                               "i214"
                                                               "i212"
                                                               "i210"
                                                               "i208"
                                                               "i206"
                                                               "i204"
                                                               "i202"))
                                                            #(ribcage
                                                              (define-structure
                                                                define-expansion-accessors
                                                                define-expansion-constructors)
                                                              ((top)
                                                               (top)
                                                               (top))
                                                              ("i40"
                                                               "i39"
                                                               "i38")))
                                                           (hygiene guile))
                                                         #(syntax-object
                                                           #f
                                                           ((top)
                                                            #(ribcage
                                                              #(name)
                                                              #((top))
                                                              #("i1881"))
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(ftype
                                                                fval
                                                                fe
                                                                fw
                                                                fs
                                                                fmod)
                                                              #((top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                              #("i1821"
                                                                "i1822"
                                                                "i1823"
                                                                "i1824"
                                                                "i1825"
                                                                "i1826"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(first)
                                                              #((top))
                                                              #("i1813"))
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(e
                                                                r
                                                                w
                                                                s
                                                                rib
                                                                mod
                                                                for-car?)
                                                              #((top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                              #("i1785"
                                                                "i1786"
                                                                "i1787"
                                                                "i1788"
                                                                "i1789"
                                                                "i1790"
                                                                "i1791"))
                                                            #(ribcage
                                                              (lambda-var-list
                                                                gen-var
                                                                strip
                                                                chi-lambda-case
                                                                lambda*-formals
                                                                chi-simple-lambda
                                                                lambda-formals
                                                                ellipsis?
                                                                chi-void
                                                                eval-local-transformer
                                                                chi-local-syntax
                                                                chi-body
                                                                chi-macro
                                                                chi-call
                                                                chi-expr
                                                                chi
                                                                syntax-type
                                                                chi-when-list
                                                                chi-install-global
                                                                chi-top-sequence
                                                                chi-sequence
                                                                source-wrap
                                                                wrap
                                                                bound-id-member?
                                                                distinct-bound-ids?
                                                                valid-bound-ids?
                                                                bound-id=?
                                                                free-id=?
                                                                id-var-name
                                                                same-marks?
                                                                join-marks
                                                                join-wraps
                                                                smart-append
                                                                make-binding-wrap
                                                                extend-ribcage!
                                                                make-empty-ribcage
                                                                new-mark
                                                                anti-mark
                                                                the-anti-mark
                                                                top-marked?
                                                                top-wrap
                                                                empty-wrap
                                                                set-ribcage-labels!
                                                                set-ribcage-marks!
                                                                set-ribcage-symnames!
                                                                ribcage-labels
                                                                ribcage-marks
                                                                ribcage-symnames
                                                                ribcage?
                                                                make-ribcage
                                                                gen-labels
                                                                gen-label
                                                                make-rename
                                                                rename-marks
                                                                rename-new
                                                                rename-old
                                                                subst-rename?
                                                                wrap-subst
                                                                wrap-marks
                                                                make-wrap
                                                                id-sym-name&marks
                                                                id-sym-name
                                                                id?
                                                                nonsymbol-id?
                                                                global-extend
                                                                lookup
                                                                macros-only-env
                                                                extend-var-env
                                                                extend-env
                                                                null-env
                                                                binding-value
                                                                binding-type
                                                                make-binding
                                                                arg-check
                                                                source-annotation
                                                                no-source
                                                                set-syntax-object-module!
                                                                set-syntax-object-wrap!
                                                                set-syntax-object-expression!
                                                                syntax-object-module
                                                                syntax-object-wrap
                                                                syntax-object-expression
                                                                syntax-object?
                                                                make-syntax-object
                                                                build-lexical-var
                                                                build-letrec
                                                                build-named-let
                                                                build-let
                                                                build-sequence
                                                                build-data
                                                                build-primref
                                                                build-primcall
                                                                build-lambda-case
                                                                build-case-lambda
                                                                build-simple-lambda
                                                                build-global-definition
                                                                build-global-assignment
                                                                build-global-reference
                                                                analyze-variable
                                                                build-lexical-assignment
                                                                build-lexical-reference
                                                                build-dynlet
                                                                build-conditional
                                                                build-call
                                                                build-void
                                                                maybe-name-value!
                                                                decorate-source
                                                                get-global-definition-hook
                                                                put-global-definition-hook
                                                                gensym-hook
                                                                local-eval-hook
                                                                top-level-eval-hook
                                                                fx<
                                                                fx=
                                                                fx-
                                                                fx+
                                                                set-lambda-meta!
                                                                lambda-meta
                                                                lambda?
                                                                make-dynlet
                                                                make-letrec
                                                                make-let
                                                                make-lambda-case
                                                                make-lambda
                                                                make-seq
                                                                make-primcall
                                                                make-call
                                                                make-conditional
                                                                make-toplevel-define
                                                                make-toplevel-set
                                                                make-toplevel-ref
                                                                make-module-set
                                                                make-module-ref
                                                                make-lexical-set
                                                                make-lexical-ref
                                                                make-primitive-ref
                                                                make-const
                                                                make-void)
                                                              ((top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top))
                                                              ("i452"
                                                               "i450"
                                                               "i448"
                                                               "i446"
                                                               "i444"
                                                               "i442"
                                                               "i440"
                                                               "i438"
                                                               "i436"
                                                               "i434"
                                                               "i432"
                                                               "i430"
                                                               "i428"
                                                               "i426"
                                                               "i424"
                                                               "i422"
                                                               "i420"
                                                               "i418"
                                                               "i416"
                                                               "i414"
                                                               "i412"
                                                               "i410"
                                                               "i408"
                                                               "i406"
                                                               "i404"
                                                               "i402"
                                                               "i400"
                                                               "i398"
                                                               "i396"
                                                               "i394"
                                                               "i392"
                                                               "i390"
                                                               "i388"
                                                               "i386"
                                                               "i384"
                                                               "i383"
                                                               "i382"
                                                               "i380"
                                                               "i379"
                                                               "i378"
                                                               "i377"
                                                               "i376"
                                                               "i374"
                                                               "i372"
                                                               "i370"
                                                               "i368"
                                                               "i366"
                                                               "i364"
                                                               "i362"
                                                               "i360"
                                                               "i357"
                                                               "i355"
                                                               "i354"
                                                               "i353"
                                                               "i352"
                                                               "i351"
                                                               "i350"
                                                               "i349"
                                                               "i348"
                                                               "i347"
                                                               "i345"
                                                               "i344"
                                                               "i342"
                                                               "i340"
                                                               "i338"
                                                               "i336"
                                                               "i334"
                                                               "i332"
                                                               "i330"
                                                               "i329"
                                                               "i328"
                                                               "i327"
                                                               "i326"
                                                               "i325"
                                                               "i323"
                                                               "i322"
                                                               "i320"
                                                               "i318"
                                                               "i316"
                                                               "i314"
                                                               "i312"
                                                               "i310"
                                                               "i308"
                                                               "i306"
                                                               "i304"
                                                               "i302"
                                                               "i300"
                                                               "i298"
                                                               "i296"
                                                               "i294"
                                                               "i292"
                                                               "i290"
                                                               "i288"
                                                               "i286"
                                                               "i284"
                                                               "i282"
                                                               "i280"
                                                               "i278"
                                                               "i276"
                                                               "i274"
                                                               "i272"
                                                               "i270"
                                                               "i268"
                                                               "i266"
                                                               "i264"
                                                               "i262"
                                                               "i260"
                                                               "i258"
                                                               "i256"
                                                               "i255"
                                                               "i253"
                                                               "i251"
                                                               "i250"
                                                               "i249"
                                                               "i248"
                                                               "i247"
                                                               "i245"
                                                               "i243"
                                                               "i241"
                                                               "i238"
                                                               "i236"
                                                               "i234"
                                                               "i232"
                                                               "i230"
                                                               "i228"
                                                               "i226"
                                                               "i224"
                                                               "i222"
                                                               "i220"
                                                               "i218"
                                                               "i216"
                                                               "i214"
                                                               "i212"
                                                               "i210"
                                                               "i208"
                                                               "i206"
                                                               "i204"
                                                               "i202"))
                                                            #(ribcage
                                                              (define-structure
                                                                define-expansion-accessors
                                                                define-expansion-constructors)
                                                              ((top)
                                                               (top)
                                                               (top))
                                                              ("i40"
                                                               "i39"
                                                               "i38")))
                                                           (hygiene guile))
                                                         #(syntax-object
                                                           #f
                                                           ((top)
                                                            #(ribcage
                                                              #(name)
                                                              #((top))
                                                              #("i1881"))
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(ftype
                                                                fval
                                                                fe
                                                                fw
                                                                fs
                                                                fmod)
                                                              #((top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                              #("i1821"
                                                                "i1822"
                                                                "i1823"
                                                                "i1824"
                                                                "i1825"
                                                                "i1826"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(first)
                                                              #((top))
                                                              #("i1813"))
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(e
                                                                r
                                                                w
                                                                s
                                                                rib
                                                                mod
                                                                for-car?)
                                                              #((top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                              #("i1785"
                                                                "i1786"
                                                                "i1787"
                                                                "i1788"
                                                                "i1789"
                                                                "i1790"
                                                                "i1791"))
                                                            #(ribcage
                                                              (lambda-var-list
                                                                gen-var
                                                                strip
                                                                chi-lambda-case
                                                                lambda*-formals
                                                                chi-simple-lambda
                                                                lambda-formals
                                                                ellipsis?
                                                                chi-void
                                                                eval-local-transformer
                                                                chi-local-syntax
                                                                chi-body
                                                                chi-macro
                                                                chi-call
                                                                chi-expr
                                                                chi
                                                                syntax-type
                                                                chi-when-list
                                                                chi-install-global
                                                                chi-top-sequence
                                                                chi-sequence
                                                                source-wrap
                                                                wrap
                                                                bound-id-member?
                                                                distinct-bound-ids?
                                                                valid-bound-ids?
                                                                bound-id=?
                                                                free-id=?
                                                                id-var-name
                                                                same-marks?
                                                                join-marks
                                                                join-wraps
                                                                smart-append
                                                                make-binding-wrap
                                                                extend-ribcage!
                                                                make-empty-ribcage
                                                                new-mark
                                                                anti-mark
                                                                the-anti-mark
                                                                top-marked?
                                                                top-wrap
                                                                empty-wrap
                                                                set-ribcage-labels!
                                                                set-ribcage-marks!
                                                                set-ribcage-symnames!
                                                                ribcage-labels
                                                                ribcage-marks
                                                                ribcage-symnames
                                                                ribcage?
                                                                make-ribcage
                                                                gen-labels
                                                                gen-label
                                                                make-rename
                                                                rename-marks
                                                                rename-new
                                                                rename-old
                                                                subst-rename?
                                                                wrap-subst
                                                                wrap-marks
                                                                make-wrap
                                                                id-sym-name&marks
                                                                id-sym-name
                                                                id?
                                                                nonsymbol-id?
                                                                global-extend
                                                                lookup
                                                                macros-only-env
                                                                extend-var-env
                                                                extend-env
                                                                null-env
                                                                binding-value
                                                                binding-type
                                                                make-binding
                                                                arg-check
                                                                source-annotation
                                                                no-source
                                                                set-syntax-object-module!
                                                                set-syntax-object-wrap!
                                                                set-syntax-object-expression!
                                                                syntax-object-module
                                                                syntax-object-wrap
                                                                syntax-object-expression
                                                                syntax-object?
                                                                make-syntax-object
                                                                build-lexical-var
                                                                build-letrec
                                                                build-named-let
                                                                build-let
                                                                build-sequence
                                                                build-data
                                                                build-primref
                                                                build-primcall
                                                                build-lambda-case
                                                                build-case-lambda
                                                                build-simple-lambda
                                                                build-global-definition
                                                                build-global-assignment
                                                                build-global-reference
                                                                analyze-variable
                                                                build-lexical-assignment
                                                                build-lexical-reference
                                                                build-dynlet
                                                                build-conditional
                                                                build-call
                                                                build-void
                                                                maybe-name-value!
                                                                decorate-source
                                                                get-global-definition-hook
                                                                put-global-definition-hook
                                                                gensym-hook
                                                                local-eval-hook
                                                                top-level-eval-hook
                                                                fx<
                                                                fx=
                                                                fx-
                                                                fx+
                                                                set-lambda-meta!
                                                                lambda-meta
                                                                lambda?
                                                                make-dynlet
                                                                make-letrec
                                                                make-let
                                                                make-lambda-case
                                                                make-lambda
                                                                make-seq
                                                                make-primcall
                                                                make-call
                                                                make-conditional
                                                                make-toplevel-define
                                                                make-toplevel-set
                                                                make-toplevel-ref
                                                                make-module-set
                                                                make-module-ref
                                                                make-lexical-set
                                                                make-lexical-ref
                                                                make-primitive-ref
                                                                make-const
                                                                make-void)
                                                              ((top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top))
                                                              ("i452"
                                                               "i450"
                                                               "i448"
                                                               "i446"
                                                               "i444"
                                                               "i442"
                                                               "i440"
                                                               "i438"
                                                               "i436"
                                                               "i434"
                                                               "i432"
                                                               "i430"
                                                               "i428"
                                                               "i426"
                                                               "i424"
                                                               "i422"
                                                               "i420"
                                                               "i418"
                                                               "i416"
                                                               "i414"
                                                               "i412"
                                                               "i410"
                                                               "i408"
                                                               "i406"
                                                               "i404"
                                                               "i402"
                                                               "i400"
                                                               "i398"
                                                               "i396"
                                                               "i394"
                                                               "i392"
                                                               "i390"
                                                               "i388"
                                                               "i386"
                                                               "i384"
                                                               "i383"
                                                               "i382"
                                                               "i380"
                                                               "i379"
                                                               "i378"
                                                               "i377"
                                                               "i376"
                                                               "i374"
                                                               "i372"
                                                               "i370"
                                                               "i368"
                                                               "i366"
                                                               "i364"
                                                               "i362"
                                                               "i360"
                                                               "i357"
                                                               "i355"
                                                               "i354"
                                                               "i353"
                                                               "i352"
                                                               "i351"
                                                               "i350"
                                                               "i349"
                                                               "i348"
                                                               "i347"
                                                               "i345"
                                                               "i344"
                                                               "i342"
                                                               "i340"
                                                               "i338"
                                                               "i336"
                                                               "i334"
                                                               "i332"
                                                               "i330"
                                                               "i329"
                                                               "i328"
                                                               "i327"
                                                               "i326"
                                                               "i325"
                                                               "i323"
                                                               "i322"
                                                               "i320"
                                                               "i318"
                                                               "i316"
                                                               "i314"
                                                               "i312"
                                                               "i310"
                                                               "i308"
                                                               "i306"
                                                               "i304"
                                                               "i302"
                                                               "i300"
                                                               "i298"
                                                               "i296"
                                                               "i294"
                                                               "i292"
                                                               "i290"
                                                               "i288"
                                                               "i286"
                                                               "i284"
                                                               "i282"
                                                               "i280"
                                                               "i278"
                                                               "i276"
                                                               "i274"
                                                               "i272"
                                                               "i270"
                                                               "i268"
                                                               "i266"
                                                               "i264"
                                                               "i262"
                                                               "i260"
                                                               "i258"
                                                               "i256"
                                                               "i255"
                                                               "i253"
                                                               "i251"
                                                               "i250"
                                                               "i249"
                                                               "i248"
                                                               "i247"
                                                               "i245"
                                                               "i243"
                                                               "i241"
                                                               "i238"
                                                               "i236"
                                                               "i234"
                                                               "i232"
                                                               "i230"
                                                               "i228"
                                                               "i226"
                                                               "i224"
                                                               "i222"
                                                               "i220"
                                                               "i218"
                                                               "i216"
                                                               "i214"
                                                               "i212"
                                                               "i210"
                                                               "i208"
                                                               "i206"
                                                               "i204"
                                                               "i202"))
                                                            #(ribcage
                                                              (define-structure
                                                                define-expansion-accessors
                                                                define-expansion-constructors)
                                                              ((top)
                                                               (top)
                                                               (top))
                                                              ("i40"
                                                               "i39"
                                                               "i38")))
                                                           (hygiene guile)))
                                                       '(())
                                                       #{s 1781}#
                                                       #{mod 1783}#))
                                                   #{tmp 1878}#)
                                                 (syntax-violation
                                                   #f
                                                   "source expression failed to match any pattern"
                                                   #{tmp 1847}#))))))))
                                   (if (memv #{ftype 1815}# '(define-syntax))
                                     (let ((#{tmp 1885}# #{e 1778}#))
                                       (let ((#{tmp 1886}#
                                               ($sc-dispatch
                                                 #{tmp 1885}#
                                                 '(_ any any))))
                                         (if (if #{tmp 1886}#
                                               (@apply
                                                 (lambda (#{name 1889}#
                                                          #{val 1890}#)
                                                   (#{id? 343}# #{name 1889}#))
                                                 #{tmp 1886}#)
                                               #f)
                                           (@apply
                                             (lambda (#{name 1893}#
                                                      #{val 1894}#)
                                               (values
                                                 'define-syntax-form
                                                 #{name 1893}#
                                                 #{val 1894}#
                                                 #{w 1780}#
                                                 #{s 1781}#
                                                 #{mod 1783}#))
                                             #{tmp 1886}#)
                                           (syntax-violation
                                             #f
                                             "source expression failed to match any pattern"
                                             #{tmp 1885}#))))
                                     (values
                                       'call
                                       #f
                                       #{e 1778}#
                                       #{w 1780}#
                                       #{s 1781}#
                                       #{mod 1783}#))))))))))))))
           (if (#{syntax-object? 309}# #{e 1778}#)
             (#{syntax-type 421}#
               (#{syntax-object-expression 311}# #{e 1778}#)
               #{r 1779}#
               (#{join-wraps 391}#
                 #{w 1780}#
                 (#{syntax-object-wrap 313}# #{e 1778}#))
               (let ((#{t 1900}#
                       (#{source-annotation 324}# #{e 1778}#)))
                 (if #{t 1900}# #{t 1900}# #{s 1781}#))
               #{rib 1782}#
               (let ((#{t 1904}#
                       (#{syntax-object-module 315}# #{e 1778}#)))
                 (if #{t 1904}# #{t 1904}# #{mod 1783}#))
               #{for-car? 1784}#)
             (if (self-evaluating? #{e 1778}#)
               (values
                 'constant
                 #f
                 #{e 1778}#
                 #{w 1780}#
                 #{s 1781}#
                 #{mod 1783}#)
               (values
                 'other
                 #f
                 #{e 1778}#
                 #{w 1780}#
                 #{s 1781}#
                 #{mod 1783}#)))))))
   (#{chi 423}#
     (lambda (#{e 1909}# #{r 1910}# #{w 1911}# #{mod 1912}#)
       (call-with-values
         (lambda ()
           (#{syntax-type 421}#
             #{e 1909}#
             #{r 1910}#
             #{w 1911}#
             (#{source-annotation 324}# #{e 1909}#)
             #f
             #{mod 1912}#
             #f))
         (lambda (#{type 1917}#
                  #{value 1918}#
                  #{e 1919}#
                  #{w 1920}#
                  #{s 1921}#
                  #{mod 1922}#)
           (#{chi-expr 425}#
             #{type 1917}#
             #{value 1918}#
             #{e 1919}#
             #{r 1910}#
             #{w 1920}#
             #{s 1921}#
             #{mod 1922}#)))))
   (#{chi-expr 425}#
     (lambda (#{type 1929}#
              #{value 1930}#
              #{e 1931}#
              #{r 1932}#
              #{w 1933}#
              #{s 1934}#
              #{mod 1935}#)
       (if (memv #{type 1929}# '(lexical))
         (#{build-lexical-reference 273}#
           'value
           #{s 1934}#
           #{e 1931}#
           #{value 1930}#)
         (if (memv #{type 1929}# '(core core-form))
           (#{value 1930}#
             #{e 1931}#
             #{r 1932}#
             #{w 1933}#
             #{s 1934}#
             #{mod 1935}#)
           (if (memv #{type 1929}# '(module-ref))
             (call-with-values
               (lambda ()
                 (#{value 1930}# #{e 1931}# #{r 1932}# #{w 1933}#))
               (lambda (#{e 1946}#
                        #{r 1947}#
                        #{w 1948}#
                        #{s 1949}#
                        #{mod 1950}#)
                 (#{chi 423}#
                   #{e 1946}#
                   #{r 1947}#
                   #{w 1948}#
                   #{mod 1950}#)))
             (if (memv #{type 1929}# '(lexical-call))
               (#{chi-call 427}#
                 (let ((#{id 1958}# (car #{e 1931}#)))
                   (#{build-lexical-reference 273}#
                     'fun
                     (#{source-annotation 324}# #{id 1958}#)
                     (if (#{syntax-object? 309}# #{id 1958}#)
                       (syntax->datum #{id 1958}#)
                       #{id 1958}#)
                     #{value 1930}#))
                 #{e 1931}#
                 #{r 1932}#
                 #{w 1933}#
                 #{s 1934}#
                 #{mod 1935}#)
               (if (memv #{type 1929}# '(global-call))
                 (#{chi-call 427}#
                   (#{build-global-reference 279}#
                     (#{source-annotation 324}# (car #{e 1931}#))
                     (if (#{syntax-object? 309}# #{value 1930}#)
                       (#{syntax-object-expression 311}# #{value 1930}#)
                       #{value 1930}#)
                     (if (#{syntax-object? 309}# #{value 1930}#)
                       (#{syntax-object-module 315}# #{value 1930}#)
                       #{mod 1935}#))
                   #{e 1931}#
                   #{r 1932}#
                   #{w 1933}#
                   #{s 1934}#
                   #{mod 1935}#)
                 (if (memv #{type 1929}# '(constant))
                   (#{build-data 295}#
                     #{s 1934}#
                     (#{strip 449}#
                       (#{source-wrap 411}#
                         #{e 1931}#
                         #{w 1933}#
                         #{s 1934}#
                         #{mod 1935}#)
                       '(())))
                   (if (memv #{type 1929}# '(global))
                     (#{build-global-reference 279}#
                       #{s 1934}#
                       #{value 1930}#
                       #{mod 1935}#)
                     (if (memv #{type 1929}# '(call))
                       (#{chi-call 427}#
                         (#{chi 423}#
                           (car #{e 1931}#)
                           #{r 1932}#
                           #{w 1933}#
                           #{mod 1935}#)
                         #{e 1931}#
                         #{r 1932}#
                         #{w 1933}#
                         #{s 1934}#
                         #{mod 1935}#)
                       (if (memv #{type 1929}# '(begin-form))
                         (let ((#{tmp 1965}# #{e 1931}#))
                           (let ((#{tmp 1966}#
                                   ($sc-dispatch
                                     #{tmp 1965}#
                                     '(_ any . each-any))))
                             (if #{tmp 1966}#
                               (@apply
                                 (lambda (#{e1 1969}# #{e2 1970}#)
                                   (#{chi-sequence 413}#
                                     (cons #{e1 1969}# #{e2 1970}#)
                                     #{r 1932}#
                                     #{w 1933}#
                                     #{s 1934}#
                                     #{mod 1935}#))
                                 #{tmp 1966}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 1965}#))))
                         (if (memv #{type 1929}# '(local-syntax-form))
                           (#{chi-local-syntax 433}#
                             #{value 1930}#
                             #{e 1931}#
                             #{r 1932}#
                             #{w 1933}#
                             #{s 1934}#
                             #{mod 1935}#
                             #{chi-sequence 413}#)
                           (if (memv #{type 1929}# '(eval-when-form))
                             (let ((#{tmp 1974}# #{e 1931}#))
                               (let ((#{tmp 1975}#
                                       ($sc-dispatch
                                         #{tmp 1974}#
                                         '(_ each-any any . each-any))))
                                 (if #{tmp 1975}#
                                   (@apply
                                     (lambda (#{x 1979}#
                                              #{e1 1980}#
                                              #{e2 1981}#)
                                       (let ((#{when-list 1983}#
                                               (#{chi-when-list 419}#
                                                 #{e 1931}#
                                                 #{x 1979}#
                                                 #{w 1933}#)))
                                         (if (memq 'eval #{when-list 1983}#)
                                           (#{chi-sequence 413}#
                                             (cons #{e1 1980}# #{e2 1981}#)
                                             #{r 1932}#
                                             #{w 1933}#
                                             #{s 1934}#
                                             #{mod 1935}#)
                                           (#{chi-void 437}#))))
                                     #{tmp 1975}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp 1974}#))))
                             (if (memv #{type 1929}#
                                       '(define-form define-syntax-form))
                               (syntax-violation
                                 #f
                                 "definition in expression context"
                                 #{e 1931}#
                                 (#{wrap 409}#
                                   #{value 1930}#
                                   #{w 1933}#
                                   #{mod 1935}#))
                               (if (memv #{type 1929}# '(syntax))
                                 (syntax-violation
                                   #f
                                   "reference to pattern variable outside syntax form"
                                   (#{source-wrap 411}#
                                     #{e 1931}#
                                     #{w 1933}#
                                     #{s 1934}#
                                     #{mod 1935}#))
                                 (if (memv #{type 1929}# '(displaced-lexical))
                                   (syntax-violation
                                     #f
                                     "reference to identifier outside its scope"
                                     (#{source-wrap 411}#
                                       #{e 1931}#
                                       #{w 1933}#
                                       #{s 1934}#
                                       #{mod 1935}#))
                                   (syntax-violation
                                     #f
                                     "unexpected syntax"
                                     (#{source-wrap 411}#
                                       #{e 1931}#
                                       #{w 1933}#
                                       #{s 1934}#
                                       #{mod 1935}#))))))))))))))))))
   (#{chi-call 427}#
     (lambda (#{x 1990}#
              #{e 1991}#
              #{r 1992}#
              #{w 1993}#
              #{s 1994}#
              #{mod 1995}#)
       (let ((#{tmp 2002}# #{e 1991}#))
         (let ((#{tmp 2003}#
                 ($sc-dispatch #{tmp 2002}# '(any . each-any))))
           (if #{tmp 2003}#
             (@apply
               (lambda (#{e0 2006}# #{e1 2007}#)
                 (#{build-call 267}#
                   #{s 1994}#
                   #{x 1990}#
                   (map (lambda (#{e 2008}#)
                          (#{chi 423}#
                            #{e 2008}#
                            #{r 1992}#
                            #{w 1993}#
                            #{mod 1995}#))
                        #{e1 2007}#)))
               #{tmp 2003}#)
             (syntax-violation
               #f
               "source expression failed to match any pattern"
               #{tmp 2002}#))))))
   (#{chi-macro 429}#
     (lambda (#{p 2011}#
              #{e 2012}#
              #{r 2013}#
              #{w 2014}#
              #{s 2015}#
              #{rib 2016}#
              #{mod 2017}#)
       (letrec*
         ((#{rebuild-macro-output 2026}#
            (lambda (#{x 2027}# #{m 2028}#)
              (if (pair? #{x 2027}#)
                (#{decorate-source 261}#
                  (cons (#{rebuild-macro-output 2026}#
                          (car #{x 2027}#)
                          #{m 2028}#)
                        (#{rebuild-macro-output 2026}#
                          (cdr #{x 2027}#)
                          #{m 2028}#))
                  #{s 2015}#)
                (if (#{syntax-object? 309}# #{x 2027}#)
                  (let ((#{w 2036}#
                          (#{syntax-object-wrap 313}# #{x 2027}#)))
                    (let ((#{ms 2039}# (car #{w 2036}#))
                          (#{s 2040}# (cdr #{w 2036}#)))
                      (if (if (pair? #{ms 2039}#)
                            (eq? (car #{ms 2039}#) #f)
                            #f)
                        (#{make-syntax-object 307}#
                          (#{syntax-object-expression 311}# #{x 2027}#)
                          (cons (cdr #{ms 2039}#)
                                (if #{rib 2016}#
                                  (cons #{rib 2016}# (cdr #{s 2040}#))
                                  (cdr #{s 2040}#)))
                          (#{syntax-object-module 315}# #{x 2027}#))
                        (#{make-syntax-object 307}#
                          (#{decorate-source 261}#
                            (#{syntax-object-expression 311}# #{x 2027}#)
                            #{s 2040}#)
                          (cons (cons #{m 2028}# #{ms 2039}#)
                                (if #{rib 2016}#
                                  (cons #{rib 2016}# (cons 'shift #{s 2040}#))
                                  (cons 'shift #{s 2040}#)))
                          (#{syntax-object-module 315}# #{x 2027}#)))))
                  (if (vector? #{x 2027}#)
                    (let ((#{n 2052}# (vector-length #{x 2027}#)))
                      (let ((#{v 2054}#
                              (#{decorate-source 261}#
                                (make-vector #{n 2052}#)
                                #{x 2027}#)))
                        (letrec*
                          ((#{loop 2057}#
                             (lambda (#{i 2058}#)
                               (if (= #{i 2058}# #{n 2052}#)
                                 (begin (if #f #f) #{v 2054}#)
                                 (begin
                                   (vector-set!
                                     #{v 2054}#
                                     #{i 2058}#
                                     (#{rebuild-macro-output 2026}#
                                       (vector-ref #{x 2027}# #{i 2058}#)
                                       #{m 2028}#))
                                   (#{loop 2057}# (#{1+}# #{i 2058}#)))))))
                          (#{loop 2057}# 0))))
                    (if (symbol? #{x 2027}#)
                      (syntax-violation
                        #f
                        "encountered raw symbol in macro output"
                        (#{source-wrap 411}#
                          #{e 2012}#
                          #{w 2014}#
                          (cdr #{w 2014}#)
                          #{mod 2017}#)
                        #{x 2027}#)
                      (#{decorate-source 261}# #{x 2027}# #{s 2015}#))))))))
         (#{rebuild-macro-output 2026}#
           (#{p 2011}#
             (#{source-wrap 411}#
               #{e 2012}#
               (#{anti-mark 381}# #{w 2014}#)
               #{s 2015}#
               #{mod 2017}#))
           (gensym "m")))))
   (#{chi-body 431}#
     (lambda (#{body 2068}#
              #{outer-form 2069}#
              #{r 2070}#
              #{w 2071}#
              #{mod 2072}#)
       (let ((#{r 2080}#
               (cons '("placeholder" placeholder) #{r 2070}#)))
         (let ((#{ribcage 2082}#
                 (#{make-ribcage 361}# '() '() '())))
           (let ((#{w 2085}#
                   (cons (car #{w 2071}#)
                         (cons #{ribcage 2082}# (cdr #{w 2071}#)))))
             (letrec*
               ((#{parse 2097}#
                  (lambda (#{body 2098}#
                           #{ids 2099}#
                           #{labels 2100}#
                           #{var-ids 2101}#
                           #{vars 2102}#
                           #{vals 2103}#
                           #{bindings 2104}#)
                    (if (null? #{body 2098}#)
                      (syntax-violation
                        #f
                        "no expressions in body"
                        #{outer-form 2069}#)
                      (let ((#{e 2109}# (cdr (car #{body 2098}#)))
                            (#{er 2110}# (car (car #{body 2098}#))))
                        (call-with-values
                          (lambda ()
                            (#{syntax-type 421}#
                              #{e 2109}#
                              #{er 2110}#
                              '(())
                              (#{source-annotation 324}# #{er 2110}#)
                              #{ribcage 2082}#
                              #{mod 2072}#
                              #f))
                          (lambda (#{type 2112}#
                                   #{value 2113}#
                                   #{e 2114}#
                                   #{w 2115}#
                                   #{s 2116}#
                                   #{mod 2117}#)
                            (if (memv #{type 2112}# '(define-form))
                              (let ((#{id 2127}#
                                      (#{wrap 409}#
                                        #{value 2113}#
                                        #{w 2115}#
                                        #{mod 2117}#))
                                    (#{label 2128}# (#{gen-label 356}#)))
                                (let ((#{var 2130}#
                                        (#{gen-var 451}# #{id 2127}#)))
                                  (begin
                                    (#{extend-ribcage! 385}#
                                      #{ribcage 2082}#
                                      #{id 2127}#
                                      #{label 2128}#)
                                    (#{parse 2097}#
                                      (cdr #{body 2098}#)
                                      (cons #{id 2127}# #{ids 2099}#)
                                      (cons #{label 2128}# #{labels 2100}#)
                                      (cons #{id 2127}# #{var-ids 2101}#)
                                      (cons #{var 2130}# #{vars 2102}#)
                                      (cons (cons #{er 2110}#
                                                  (#{wrap 409}#
                                                    #{e 2114}#
                                                    #{w 2115}#
                                                    #{mod 2117}#))
                                            #{vals 2103}#)
                                      (cons (cons 'lexical #{var 2130}#)
                                            #{bindings 2104}#)))))
                              (if (memv #{type 2112}# '(define-syntax-form))
                                (let ((#{id 2135}#
                                        (#{wrap 409}#
                                          #{value 2113}#
                                          #{w 2115}#
                                          #{mod 2117}#))
                                      (#{label 2136}# (#{gen-label 356}#)))
                                  (begin
                                    (#{extend-ribcage! 385}#
                                      #{ribcage 2082}#
                                      #{id 2135}#
                                      #{label 2136}#)
                                    (#{parse 2097}#
                                      (cdr #{body 2098}#)
                                      (cons #{id 2135}# #{ids 2099}#)
                                      (cons #{label 2136}# #{labels 2100}#)
                                      #{var-ids 2101}#
                                      #{vars 2102}#
                                      #{vals 2103}#
                                      (cons (cons 'macro
                                                  (cons #{er 2110}#
                                                        (#{wrap 409}#
                                                          #{e 2114}#
                                                          #{w 2115}#
                                                          #{mod 2117}#)))
                                            #{bindings 2104}#))))
                                (if (memv #{type 2112}# '(begin-form))
                                  (let ((#{tmp 2139}# #{e 2114}#))
                                    (let ((#{tmp 2140}#
                                            ($sc-dispatch
                                              #{tmp 2139}#
                                              '(_ . each-any))))
                                      (if #{tmp 2140}#
                                        (@apply
                                          (lambda (#{e1 2142}#)
                                            (#{parse 2097}#
                                              (letrec*
                                                ((#{f 2145}#
                                                   (lambda (#{forms 2146}#)
                                                     (if (null? #{forms 2146}#)
                                                       (cdr #{body 2098}#)
                                                       (cons (cons #{er 2110}#
                                                                   (#{wrap 409}#
                                                                     (car #{forms 2146}#)
                                                                     #{w 2115}#
                                                                     #{mod 2117}#))
                                                             (#{f 2145}#
                                                               (cdr #{forms 2146}#)))))))
                                                (#{f 2145}# #{e1 2142}#))
                                              #{ids 2099}#
                                              #{labels 2100}#
                                              #{var-ids 2101}#
                                              #{vars 2102}#
                                              #{vals 2103}#
                                              #{bindings 2104}#))
                                          #{tmp 2140}#)
                                        (syntax-violation
                                          #f
                                          "source expression failed to match any pattern"
                                          #{tmp 2139}#))))
                                  (if (memv #{type 2112}# '(local-syntax-form))
                                    (#{chi-local-syntax 433}#
                                      #{value 2113}#
                                      #{e 2114}#
                                      #{er 2110}#
                                      #{w 2115}#
                                      #{s 2116}#
                                      #{mod 2117}#
                                      (lambda (#{forms 2149}#
                                               #{er 2150}#
                                               #{w 2151}#
                                               #{s 2152}#
                                               #{mod 2153}#)
                                        (#{parse 2097}#
                                          (letrec*
                                            ((#{f 2161}#
                                               (lambda (#{forms 2162}#)
                                                 (if (null? #{forms 2162}#)
                                                   (cdr #{body 2098}#)
                                                   (cons (cons #{er 2150}#
                                                               (#{wrap 409}#
                                                                 (car #{forms 2162}#)
                                                                 #{w 2151}#
                                                                 #{mod 2153}#))
                                                         (#{f 2161}#
                                                           (cdr #{forms 2162}#)))))))
                                            (#{f 2161}# #{forms 2149}#))
                                          #{ids 2099}#
                                          #{labels 2100}#
                                          #{var-ids 2101}#
                                          #{vars 2102}#
                                          #{vals 2103}#
                                          #{bindings 2104}#)))
                                    (if (null? #{ids 2099}#)
                                      (#{build-sequence 297}#
                                        #f
                                        (map (lambda (#{x 2165}#)
                                               (#{chi 423}#
                                                 (cdr #{x 2165}#)
                                                 (car #{x 2165}#)
                                                 '(())
                                                 #{mod 2117}#))
                                             (cons (cons #{er 2110}#
                                                         (#{source-wrap 411}#
                                                           #{e 2114}#
                                                           #{w 2115}#
                                                           #{s 2116}#
                                                           #{mod 2117}#))
                                                   (cdr #{body 2098}#))))
                                      (begin
                                        (if (not (#{valid-bound-ids? 403}#
                                                   #{ids 2099}#))
                                          (syntax-violation
                                            #f
                                            "invalid or duplicate identifier in definition"
                                            #{outer-form 2069}#))
                                        (letrec*
                                          ((#{loop 2172}#
                                             (lambda (#{bs 2173}#
                                                      #{er-cache 2174}#
                                                      #{r-cache 2175}#)
                                               (if (not (null? #{bs 2173}#))
                                                 (let ((#{b 2178}#
                                                         (car #{bs 2173}#)))
                                                   (if (eq? (car #{b 2178}#)
                                                            'macro)
                                                     (let ((#{er 2181}#
                                                             (car (cdr #{b 2178}#))))
                                                       (let ((#{r-cache 2183}#
                                                               (if (eq? #{er 2181}#
                                                                        #{er-cache 2174}#)
                                                                 #{r-cache 2175}#
                                                                 (#{macros-only-env 335}#
                                                                   #{er 2181}#))))
                                                         (begin
                                                           (set-cdr!
                                                             #{b 2178}#
                                                             (#{eval-local-transformer 435}#
                                                               (#{chi 423}#
                                                                 (cdr (cdr #{b 2178}#))
                                                                 #{r-cache 2183}#
                                                                 '(())
                                                                 #{mod 2117}#)
                                                               #{mod 2117}#))
                                                           (#{loop 2172}#
                                                             (cdr #{bs 2173}#)
                                                             #{er 2181}#
                                                             #{r-cache 2183}#))))
                                                     (#{loop 2172}#
                                                       (cdr #{bs 2173}#)
                                                       #{er-cache 2174}#
                                                       #{r-cache 2175}#)))))))
                                          (#{loop 2172}#
                                            #{bindings 2104}#
                                            #f
                                            #f))
                                        (set-cdr!
                                          #{r 2080}#
                                          (#{extend-env 331}#
                                            #{labels 2100}#
                                            #{bindings 2104}#
                                            (cdr #{r 2080}#)))
                                        (#{build-letrec 303}#
                                          #f
                                          #t
                                          (reverse
                                            (map syntax->datum
                                                 #{var-ids 2101}#))
                                          (reverse #{vars 2102}#)
                                          (map (lambda (#{x 2186}#)
                                                 (#{chi 423}#
                                                   (cdr #{x 2186}#)
                                                   (car #{x 2186}#)
                                                   '(())
                                                   #{mod 2117}#))
                                               (reverse #{vals 2103}#))
                                          (#{build-sequence 297}#
                                            #f
                                            (map (lambda (#{x 2190}#)
                                                   (#{chi 423}#
                                                     (cdr #{x 2190}#)
                                                     (car #{x 2190}#)
                                                     '(())
                                                     #{mod 2117}#))
                                                 (cons (cons #{er 2110}#
                                                             (#{source-wrap 411}#
                                                               #{e 2114}#
                                                               #{w 2115}#
                                                               #{s 2116}#
                                                               #{mod 2117}#))
                                                       (cdr #{body 2098}#))))))))))))))))))
               (#{parse 2097}#
                 (map (lambda (#{x 2105}#)
                        (cons #{r 2080}#
                              (#{wrap 409}#
                                #{x 2105}#
                                #{w 2085}#
                                #{mod 2072}#)))
                      #{body 2068}#)
                 '()
                 '()
                 '()
                 '()
                 '()
                 '())))))))
   (#{chi-local-syntax 433}#
     (lambda (#{rec? 2193}#
              #{e 2194}#
              #{r 2195}#
              #{w 2196}#
              #{s 2197}#
              #{mod 2198}#
              #{k 2199}#)
       (let ((#{tmp 2207}# #{e 2194}#))
         (let ((#{tmp 2208}#
                 ($sc-dispatch
                   #{tmp 2207}#
                   '(_ #(each (any any)) any . each-any))))
           (if #{tmp 2208}#
             (@apply
               (lambda (#{id 2213}#
                        #{val 2214}#
                        #{e1 2215}#
                        #{e2 2216}#)
                 (let ((#{ids 2218}# #{id 2213}#))
                   (if (not (#{valid-bound-ids? 403}# #{ids 2218}#))
                     (syntax-violation
                       #f
                       "duplicate bound keyword"
                       #{e 2194}#)
                     (let ((#{labels 2221}#
                             (#{gen-labels 358}# #{ids 2218}#)))
                       (let ((#{new-w 2223}#
                               (#{make-binding-wrap 387}#
                                 #{ids 2218}#
                                 #{labels 2221}#
                                 #{w 2196}#)))
                         (#{k 2199}#
                           (cons #{e1 2215}# #{e2 2216}#)
                           (#{extend-env 331}#
                             #{labels 2221}#
                             (let ((#{w 2227}#
                                     (if #{rec? 2193}#
                                       #{new-w 2223}#
                                       #{w 2196}#))
                                   (#{trans-r 2228}#
                                     (#{macros-only-env 335}# #{r 2195}#)))
                               (map (lambda (#{x 2229}#)
                                      (cons 'macro
                                            (#{eval-local-transformer 435}#
                                              (#{chi 423}#
                                                #{x 2229}#
                                                #{trans-r 2228}#
                                                #{w 2227}#
                                                #{mod 2198}#)
                                              #{mod 2198}#)))
                                    #{val 2214}#))
                             #{r 2195}#)
                           #{new-w 2223}#
                           #{s 2197}#
                           #{mod 2198}#))))))
               #{tmp 2208}#)
             (let ((#{_ 2234}# #{tmp 2207}#))
               (syntax-violation
                 #f
                 "bad local syntax definition"
                 (#{source-wrap 411}#
                   #{e 2194}#
                   #{w 2196}#
                   #{s 2197}#
                   #{mod 2198}#))))))))
   (#{eval-local-transformer 435}#
     (lambda (#{expanded 2235}# #{mod 2236}#)
       (let ((#{p 2240}#
               (#{local-eval-hook 254}#
                 #{expanded 2235}#
                 #{mod 2236}#)))
         (if (procedure? #{p 2240}#)
           #{p 2240}#
           (syntax-violation
             #f
             "nonprocedure transformer"
             #{p 2240}#)))))
   (#{chi-void 437}#
     (lambda () (#{build-void 265}# #f)))
   (#{ellipsis? 439}#
     (lambda (#{x 2242}#)
       (if (#{nonsymbol-id? 341}# #{x 2242}#)
         (#{free-id=? 399}#
           #{x 2242}#
           '#(syntax-object
              ...
              ((top)
               #(ribcage () () ())
               #(ribcage () () ())
               #(ribcage #(x) #((top)) #("i2243"))
               #(ribcage
                 (lambda-var-list
                   gen-var
                   strip
                   chi-lambda-case
                   lambda*-formals
                   chi-simple-lambda
                   lambda-formals
                   ellipsis?
                   chi-void
                   eval-local-transformer
                   chi-local-syntax
                   chi-body
                   chi-macro
                   chi-call
                   chi-expr
                   chi
                   syntax-type
                   chi-when-list
                   chi-install-global
                   chi-top-sequence
                   chi-sequence
                   source-wrap
                   wrap
                   bound-id-member?
                   distinct-bound-ids?
                   valid-bound-ids?
                   bound-id=?
                   free-id=?
                   id-var-name
                   same-marks?
                   join-marks
                   join-wraps
                   smart-append
                   make-binding-wrap
                   extend-ribcage!
                   make-empty-ribcage
                   new-mark
                   anti-mark
                   the-anti-mark
                   top-marked?
                   top-wrap
                   empty-wrap
                   set-ribcage-labels!
                   set-ribcage-marks!
                   set-ribcage-symnames!
                   ribcage-labels
                   ribcage-marks
                   ribcage-symnames
                   ribcage?
                   make-ribcage
                   gen-labels
                   gen-label
                   make-rename
                   rename-marks
                   rename-new
                   rename-old
                   subst-rename?
                   wrap-subst
                   wrap-marks
                   make-wrap
                   id-sym-name&marks
                   id-sym-name
                   id?
                   nonsymbol-id?
                   global-extend
                   lookup
                   macros-only-env
                   extend-var-env
                   extend-env
                   null-env
                   binding-value
                   binding-type
                   make-binding
                   arg-check
                   source-annotation
                   no-source
                   set-syntax-object-module!
                   set-syntax-object-wrap!
                   set-syntax-object-expression!
                   syntax-object-module
                   syntax-object-wrap
                   syntax-object-expression
                   syntax-object?
                   make-syntax-object
                   build-lexical-var
                   build-letrec
                   build-named-let
                   build-let
                   build-sequence
                   build-data
                   build-primref
                   build-primcall
                   build-lambda-case
                   build-case-lambda
                   build-simple-lambda
                   build-global-definition
                   build-global-assignment
                   build-global-reference
                   analyze-variable
                   build-lexical-assignment
                   build-lexical-reference
                   build-dynlet
                   build-conditional
                   build-call
                   build-void
                   maybe-name-value!
                   decorate-source
                   get-global-definition-hook
                   put-global-definition-hook
                   gensym-hook
                   local-eval-hook
                   top-level-eval-hook
                   fx<
                   fx=
                   fx-
                   fx+
                   set-lambda-meta!
                   lambda-meta
                   lambda?
                   make-dynlet
                   make-letrec
                   make-let
                   make-lambda-case
                   make-lambda
                   make-seq
                   make-primcall
                   make-call
                   make-conditional
                   make-toplevel-define
                   make-toplevel-set
                   make-toplevel-ref
                   make-module-set
                   make-module-ref
                   make-lexical-set
                   make-lexical-ref
                   make-primitive-ref
                   make-const
                   make-void)
                 ((top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top)
                  (top))
                 ("i452"
                  "i450"
                  "i448"
                  "i446"
                  "i444"
                  "i442"
                  "i440"
                  "i438"
                  "i436"
                  "i434"
                  "i432"
                  "i430"
                  "i428"
                  "i426"
                  "i424"
                  "i422"
                  "i420"
                  "i418"
                  "i416"
                  "i414"
                  "i412"
                  "i410"
                  "i408"
                  "i406"
                  "i404"
                  "i402"
                  "i400"
                  "i398"
                  "i396"
                  "i394"
                  "i392"
                  "i390"
                  "i388"
                  "i386"
                  "i384"
                  "i383"
                  "i382"
                  "i380"
                  "i379"
                  "i378"
                  "i377"
                  "i376"
                  "i374"
                  "i372"
                  "i370"
                  "i368"
                  "i366"
                  "i364"
                  "i362"
                  "i360"
                  "i357"
                  "i355"
                  "i354"
                  "i353"
                  "i352"
                  "i351"
                  "i350"
                  "i349"
                  "i348"
                  "i347"
                  "i345"
                  "i344"
                  "i342"
                  "i340"
                  "i338"
                  "i336"
                  "i334"
                  "i332"
                  "i330"
                  "i329"
                  "i328"
                  "i327"
                  "i326"
                  "i325"
                  "i323"
                  "i322"
                  "i320"
                  "i318"
                  "i316"
                  "i314"
                  "i312"
                  "i310"
                  "i308"
                  "i306"
                  "i304"
                  "i302"
                  "i300"
                  "i298"
                  "i296"
                  "i294"
                  "i292"
                  "i290"
                  "i288"
                  "i286"
                  "i284"
                  "i282"
                  "i280"
                  "i278"
                  "i276"
                  "i274"
                  "i272"
                  "i270"
                  "i268"
                  "i266"
                  "i264"
                  "i262"
                  "i260"
                  "i258"
                  "i256"
                  "i255"
                  "i253"
                  "i251"
                  "i250"
                  "i249"
                  "i248"
                  "i247"
                  "i245"
                  "i243"
                  "i241"
                  "i238"
                  "i236"
                  "i234"
                  "i232"
                  "i230"
                  "i228"
                  "i226"
                  "i224"
                  "i222"
                  "i220"
                  "i218"
                  "i216"
                  "i214"
                  "i212"
                  "i210"
                  "i208"
                  "i206"
                  "i204"
                  "i202"))
               #(ribcage
                 (define-structure
                   define-expansion-accessors
                   define-expansion-constructors)
                 ((top) (top) (top))
                 ("i40" "i39" "i38")))
              (hygiene guile)))
         #f)))
   (#{lambda-formals 441}#
     (lambda (#{orig-args 2246}#)
       (letrec*
         ((#{req 2249}#
            (lambda (#{args 2252}# #{rreq 2253}#)
              (let ((#{tmp 2256}# #{args 2252}#))
                (let ((#{tmp 2257}# ($sc-dispatch #{tmp 2256}# '())))
                  (if #{tmp 2257}#
                    (@apply
                      (lambda ()
                        (#{check 2251}# (reverse #{rreq 2253}#) #f))
                      #{tmp 2257}#)
                    (let ((#{tmp 2258}#
                            ($sc-dispatch #{tmp 2256}# '(any . any))))
                      (if (if #{tmp 2258}#
                            (@apply
                              (lambda (#{a 2261}# #{b 2262}#)
                                (#{id? 343}# #{a 2261}#))
                              #{tmp 2258}#)
                            #f)
                        (@apply
                          (lambda (#{a 2265}# #{b 2266}#)
                            (#{req 2249}#
                              #{b 2266}#
                              (cons #{a 2265}# #{rreq 2253}#)))
                          #{tmp 2258}#)
                        (let ((#{tmp 2267}# (list #{tmp 2256}#)))
                          (if (if #{tmp 2267}#
                                (@apply
                                  (lambda (#{r 2269}#)
                                    (#{id? 343}# #{r 2269}#))
                                  #{tmp 2267}#)
                                #f)
                            (@apply
                              (lambda (#{r 2271}#)
                                (#{check 2251}#
                                  (reverse #{rreq 2253}#)
                                  #{r 2271}#))
                              #{tmp 2267}#)
                            (let ((#{else 2273}# #{tmp 2256}#))
                              (syntax-violation
                                'lambda
                                "invalid argument list"
                                #{orig-args 2246}#
                                #{args 2252}#)))))))))))
          (#{check 2251}#
            (lambda (#{req 2274}# #{rest 2275}#)
              (if (#{distinct-bound-ids? 405}#
                    (if #{rest 2275}#
                      (cons #{rest 2275}# #{req 2274}#)
                      #{req 2274}#))
                (values #{req 2274}# #f #{rest 2275}# #f)
                (syntax-violation
                  'lambda
                  "duplicate identifier in argument list"
                  #{orig-args 2246}#)))))
         (#{req 2249}# #{orig-args 2246}# '()))))
   (#{chi-simple-lambda 443}#
     (lambda (#{e 2281}#
              #{r 2282}#
              #{w 2283}#
              #{s 2284}#
              #{mod 2285}#
              #{req 2286}#
              #{rest 2287}#
              #{meta 2288}#
              #{body 2289}#)
       (let ((#{ids 2301}#
               (if #{rest 2287}#
                 (append #{req 2286}# (list #{rest 2287}#))
                 #{req 2286}#)))
         (let ((#{vars 2303}#
                 (map #{gen-var 451}# #{ids 2301}#)))
           (let ((#{labels 2305}#
                   (#{gen-labels 358}# #{ids 2301}#)))
             (#{build-simple-lambda 285}#
               #{s 2284}#
               (map syntax->datum #{req 2286}#)
               (if #{rest 2287}#
                 (syntax->datum #{rest 2287}#)
                 #f)
               #{vars 2303}#
               #{meta 2288}#
               (#{chi-body 431}#
                 #{body 2289}#
                 (#{source-wrap 411}#
                   #{e 2281}#
                   #{w 2283}#
                   #{s 2284}#
                   #{mod 2285}#)
                 (#{extend-var-env 333}#
                   #{labels 2305}#
                   #{vars 2303}#
                   #{r 2282}#)
                 (#{make-binding-wrap 387}#
                   #{ids 2301}#
                   #{labels 2305}#
                   #{w 2283}#)
                 #{mod 2285}#)))))))
   (#{lambda*-formals 445}#
     (lambda (#{orig-args 2308}#)
       (letrec*
         ((#{req 2311}#
            (lambda (#{args 2320}# #{rreq 2321}#)
              (let ((#{tmp 2324}# #{args 2320}#))
                (let ((#{tmp 2325}# ($sc-dispatch #{tmp 2324}# '())))
                  (if #{tmp 2325}#
                    (@apply
                      (lambda ()
                        (#{check 2319}#
                          (reverse #{rreq 2321}#)
                          '()
                          #f
                          '()))
                      #{tmp 2325}#)
                    (let ((#{tmp 2326}#
                            ($sc-dispatch #{tmp 2324}# '(any . any))))
                      (if (if #{tmp 2326}#
                            (@apply
                              (lambda (#{a 2329}# #{b 2330}#)
                                (#{id? 343}# #{a 2329}#))
                              #{tmp 2326}#)
                            #f)
                        (@apply
                          (lambda (#{a 2333}# #{b 2334}#)
                            (#{req 2311}#
                              #{b 2334}#
                              (cons #{a 2333}# #{rreq 2321}#)))
                          #{tmp 2326}#)
                        (let ((#{tmp 2335}#
                                ($sc-dispatch #{tmp 2324}# '(any . any))))
                          (if (if #{tmp 2335}#
                                (@apply
                                  (lambda (#{a 2338}# #{b 2339}#)
                                    (eq? (syntax->datum #{a 2338}#)
                                         #:optional))
                                  #{tmp 2335}#)
                                #f)
                            (@apply
                              (lambda (#{a 2342}# #{b 2343}#)
                                (#{opt 2313}#
                                  #{b 2343}#
                                  (reverse #{rreq 2321}#)
                                  '()))
                              #{tmp 2335}#)
                            (let ((#{tmp 2344}#
                                    ($sc-dispatch #{tmp 2324}# '(any . any))))
                              (if (if #{tmp 2344}#
                                    (@apply
                                      (lambda (#{a 2347}# #{b 2348}#)
                                        (eq? (syntax->datum #{a 2347}#) #:key))
                                      #{tmp 2344}#)
                                    #f)
                                (@apply
                                  (lambda (#{a 2351}# #{b 2352}#)
                                    (#{key 2315}#
                                      #{b 2352}#
                                      (reverse #{rreq 2321}#)
                                      '()
                                      '()))
                                  #{tmp 2344}#)
                                (let ((#{tmp 2353}#
                                        ($sc-dispatch
                                          #{tmp 2324}#
                                          '(any any))))
                                  (if (if #{tmp 2353}#
                                        (@apply
                                          (lambda (#{a 2356}# #{b 2357}#)
                                            (eq? (syntax->datum #{a 2356}#)
                                                 #:rest))
                                          #{tmp 2353}#)
                                        #f)
                                    (@apply
                                      (lambda (#{a 2360}# #{b 2361}#)
                                        (#{rest 2317}#
                                          #{b 2361}#
                                          (reverse #{rreq 2321}#)
                                          '()
                                          '()))
                                      #{tmp 2353}#)
                                    (let ((#{tmp 2362}# (list #{tmp 2324}#)))
                                      (if (if #{tmp 2362}#
                                            (@apply
                                              (lambda (#{r 2364}#)
                                                (#{id? 343}# #{r 2364}#))
                                              #{tmp 2362}#)
                                            #f)
                                        (@apply
                                          (lambda (#{r 2366}#)
                                            (#{rest 2317}#
                                              #{r 2366}#
                                              (reverse #{rreq 2321}#)
                                              '()
                                              '()))
                                          #{tmp 2362}#)
                                        (let ((#{else 2368}# #{tmp 2324}#))
                                          (syntax-violation
                                            'lambda*
                                            "invalid argument list"
                                            #{orig-args 2308}#
                                            #{args 2320}#)))))))))))))))))
          (#{opt 2313}#
            (lambda (#{args 2369}# #{req 2370}# #{ropt 2371}#)
              (let ((#{tmp 2375}# #{args 2369}#))
                (let ((#{tmp 2376}# ($sc-dispatch #{tmp 2375}# '())))
                  (if #{tmp 2376}#
                    (@apply
                      (lambda ()
                        (#{check 2319}#
                          #{req 2370}#
                          (reverse #{ropt 2371}#)
                          #f
                          '()))
                      #{tmp 2376}#)
                    (let ((#{tmp 2377}#
                            ($sc-dispatch #{tmp 2375}# '(any . any))))
                      (if (if #{tmp 2377}#
                            (@apply
                              (lambda (#{a 2380}# #{b 2381}#)
                                (#{id? 343}# #{a 2380}#))
                              #{tmp 2377}#)
                            #f)
                        (@apply
                          (lambda (#{a 2384}# #{b 2385}#)
                            (#{opt 2313}#
                              #{b 2385}#
                              #{req 2370}#
                              (cons (cons #{a 2384}#
                                          '(#(syntax-object
                                              #f
                                              ((top)
                                               #(ribcage
                                                 #(a b)
                                                 #((top) (top))
                                                 #("i2382" "i2383"))
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(args req ropt)
                                                 #((top) (top) (top))
                                                 #("i2372" "i2373" "i2374"))
                                               #(ribcage
                                                 (check rest key opt req)
                                                 ((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                 ("i2318"
                                                  "i2316"
                                                  "i2314"
                                                  "i2312"
                                                  "i2310"))
                                               #(ribcage
                                                 #(orig-args)
                                                 #((top))
                                                 #("i2309"))
                                               #(ribcage
                                                 (lambda-var-list
                                                   gen-var
                                                   strip
                                                   chi-lambda-case
                                                   lambda*-formals
                                                   chi-simple-lambda
                                                   lambda-formals
                                                   ellipsis?
                                                   chi-void
                                                   eval-local-transformer
                                                   chi-local-syntax
                                                   chi-body
                                                   chi-macro
                                                   chi-call
                                                   chi-expr
                                                   chi
                                                   syntax-type
                                                   chi-when-list
                                                   chi-install-global
                                                   chi-top-sequence
                                                   chi-sequence
                                                   source-wrap
                                                   wrap
                                                   bound-id-member?
                                                   distinct-bound-ids?
                                                   valid-bound-ids?
                                                   bound-id=?
                                                   free-id=?
                                                   id-var-name
                                                   same-marks?
                                                   join-marks
                                                   join-wraps
                                                   smart-append
                                                   make-binding-wrap
                                                   extend-ribcage!
                                                   make-empty-ribcage
                                                   new-mark
                                                   anti-mark
                                                   the-anti-mark
                                                   top-marked?
                                                   top-wrap
                                                   empty-wrap
                                                   set-ribcage-labels!
                                                   set-ribcage-marks!
                                                   set-ribcage-symnames!
                                                   ribcage-labels
                                                   ribcage-marks
                                                   ribcage-symnames
                                                   ribcage?
                                                   make-ribcage
                                                   gen-labels
                                                   gen-label
                                                   make-rename
                                                   rename-marks
                                                   rename-new
                                                   rename-old
                                                   subst-rename?
                                                   wrap-subst
                                                   wrap-marks
                                                   make-wrap
                                                   id-sym-name&marks
                                                   id-sym-name
                                                   id?
                                                   nonsymbol-id?
                                                   global-extend
                                                   lookup
                                                   macros-only-env
                                                   extend-var-env
                                                   extend-env
                                                   null-env
                                                   binding-value
                                                   binding-type
                                                   make-binding
                                                   arg-check
                                                   source-annotation
                                                   no-source
                                                   set-syntax-object-module!
                                                   set-syntax-object-wrap!
                                                   set-syntax-object-expression!
                                                   syntax-object-module
                                                   syntax-object-wrap
                                                   syntax-object-expression
                                                   syntax-object?
                                                   make-syntax-object
                                                   build-lexical-var
                                                   build-letrec
                                                   build-named-let
                                                   build-let
                                                   build-sequence
                                                   build-data
                                                   build-primref
                                                   build-primcall
                                                   build-lambda-case
                                                   build-case-lambda
                                                   build-simple-lambda
                                                   build-global-definition
                                                   build-global-assignment
                                                   build-global-reference
                                                   analyze-variable
                                                   build-lexical-assignment
                                                   build-lexical-reference
                                                   build-dynlet
                                                   build-conditional
                                                   build-call
                                                   build-void
                                                   maybe-name-value!
                                                   decorate-source
                                                   get-global-definition-hook
                                                   put-global-definition-hook
                                                   gensym-hook
                                                   local-eval-hook
                                                   top-level-eval-hook
                                                   fx<
                                                   fx=
                                                   fx-
                                                   fx+
                                                   set-lambda-meta!
                                                   lambda-meta
                                                   lambda?
                                                   make-dynlet
                                                   make-letrec
                                                   make-let
                                                   make-lambda-case
                                                   make-lambda
                                                   make-seq
                                                   make-primcall
                                                   make-call
                                                   make-conditional
                                                   make-toplevel-define
                                                   make-toplevel-set
                                                   make-toplevel-ref
                                                   make-module-set
                                                   make-module-ref
                                                   make-lexical-set
                                                   make-lexical-ref
                                                   make-primitive-ref
                                                   make-const
                                                   make-void)
                                                 ((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                 ("i452"
                                                  "i450"
                                                  "i448"
                                                  "i446"
                                                  "i444"
                                                  "i442"
                                                  "i440"
                                                  "i438"
                                                  "i436"
                                                  "i434"
                                                  "i432"
                                                  "i430"
                                                  "i428"
                                                  "i426"
                                                  "i424"
                                                  "i422"
                                                  "i420"
                                                  "i418"
                                                  "i416"
                                                  "i414"
                                                  "i412"
                                                  "i410"
                                                  "i408"
                                                  "i406"
                                                  "i404"
                                                  "i402"
                                                  "i400"
                                                  "i398"
                                                  "i396"
                                                  "i394"
                                                  "i392"
                                                  "i390"
                                                  "i388"
                                                  "i386"
                                                  "i384"
                                                  "i383"
                                                  "i382"
                                                  "i380"
                                                  "i379"
                                                  "i378"
                                                  "i377"
                                                  "i376"
                                                  "i374"
                                                  "i372"
                                                  "i370"
                                                  "i368"
                                                  "i366"
                                                  "i364"
                                                  "i362"
                                                  "i360"
                                                  "i357"
                                                  "i355"
                                                  "i354"
                                                  "i353"
                                                  "i352"
                                                  "i351"
                                                  "i350"
                                                  "i349"
                                                  "i348"
                                                  "i347"
                                                  "i345"
                                                  "i344"
                                                  "i342"
                                                  "i340"
                                                  "i338"
                                                  "i336"
                                                  "i334"
                                                  "i332"
                                                  "i330"
                                                  "i329"
                                                  "i328"
                                                  "i327"
                                                  "i326"
                                                  "i325"
                                                  "i323"
                                                  "i322"
                                                  "i320"
                                                  "i318"
                                                  "i316"
                                                  "i314"
                                                  "i312"
                                                  "i310"
                                                  "i308"
                                                  "i306"
                                                  "i304"
                                                  "i302"
                                                  "i300"
                                                  "i298"
                                                  "i296"
                                                  "i294"
                                                  "i292"
                                                  "i290"
                                                  "i288"
                                                  "i286"
                                                  "i284"
                                                  "i282"
                                                  "i280"
                                                  "i278"
                                                  "i276"
                                                  "i274"
                                                  "i272"
                                                  "i270"
                                                  "i268"
                                                  "i266"
                                                  "i264"
                                                  "i262"
                                                  "i260"
                                                  "i258"
                                                  "i256"
                                                  "i255"
                                                  "i253"
                                                  "i251"
                                                  "i250"
                                                  "i249"
                                                  "i248"
                                                  "i247"
                                                  "i245"
                                                  "i243"
                                                  "i241"
                                                  "i238"
                                                  "i236"
                                                  "i234"
                                                  "i232"
                                                  "i230"
                                                  "i228"
                                                  "i226"
                                                  "i224"
                                                  "i222"
                                                  "i220"
                                                  "i218"
                                                  "i216"
                                                  "i214"
                                                  "i212"
                                                  "i210"
                                                  "i208"
                                                  "i206"
                                                  "i204"
                                                  "i202"))
                                               #(ribcage
                                                 (define-structure
                                                   define-expansion-accessors
                                                   define-expansion-constructors)
                                                 ((top) (top) (top))
                                                 ("i40" "i39" "i38")))
                                              (hygiene guile))))
                                    #{ropt 2371}#)))
                          #{tmp 2377}#)
                        (let ((#{tmp 2386}#
                                ($sc-dispatch
                                  #{tmp 2375}#
                                  '((any any) . any))))
                          (if (if #{tmp 2386}#
                                (@apply
                                  (lambda (#{a 2390}# #{init 2391}# #{b 2392}#)
                                    (#{id? 343}# #{a 2390}#))
                                  #{tmp 2386}#)
                                #f)
                            (@apply
                              (lambda (#{a 2396}# #{init 2397}# #{b 2398}#)
                                (#{opt 2313}#
                                  #{b 2398}#
                                  #{req 2370}#
                                  (cons (list #{a 2396}# #{init 2397}#)
                                        #{ropt 2371}#)))
                              #{tmp 2386}#)
                            (let ((#{tmp 2399}#
                                    ($sc-dispatch #{tmp 2375}# '(any . any))))
                              (if (if #{tmp 2399}#
                                    (@apply
                                      (lambda (#{a 2402}# #{b 2403}#)
                                        (eq? (syntax->datum #{a 2402}#) #:key))
                                      #{tmp 2399}#)
                                    #f)
                                (@apply
                                  (lambda (#{a 2406}# #{b 2407}#)
                                    (#{key 2315}#
                                      #{b 2407}#
                                      #{req 2370}#
                                      (reverse #{ropt 2371}#)
                                      '()))
                                  #{tmp 2399}#)
                                (let ((#{tmp 2408}#
                                        ($sc-dispatch
                                          #{tmp 2375}#
                                          '(any any))))
                                  (if (if #{tmp 2408}#
                                        (@apply
                                          (lambda (#{a 2411}# #{b 2412}#)
                                            (eq? (syntax->datum #{a 2411}#)
                                                 #:rest))
                                          #{tmp 2408}#)
                                        #f)
                                    (@apply
                                      (lambda (#{a 2415}# #{b 2416}#)
                                        (#{rest 2317}#
                                          #{b 2416}#
                                          #{req 2370}#
                                          (reverse #{ropt 2371}#)
                                          '()))
                                      #{tmp 2408}#)
                                    (let ((#{tmp 2417}# (list #{tmp 2375}#)))
                                      (if (if #{tmp 2417}#
                                            (@apply
                                              (lambda (#{r 2419}#)
                                                (#{id? 343}# #{r 2419}#))
                                              #{tmp 2417}#)
                                            #f)
                                        (@apply
                                          (lambda (#{r 2421}#)
                                            (#{rest 2317}#
                                              #{r 2421}#
                                              #{req 2370}#
                                              (reverse #{ropt 2371}#)
                                              '()))
                                          #{tmp 2417}#)
                                        (let ((#{else 2423}# #{tmp 2375}#))
                                          (syntax-violation
                                            'lambda*
                                            "invalid optional argument list"
                                            #{orig-args 2308}#
                                            #{args 2369}#)))))))))))))))))
          (#{key 2315}#
            (lambda (#{args 2424}#
                     #{req 2425}#
                     #{opt 2426}#
                     #{rkey 2427}#)
              (let ((#{tmp 2432}# #{args 2424}#))
                (let ((#{tmp 2433}# ($sc-dispatch #{tmp 2432}# '())))
                  (if #{tmp 2433}#
                    (@apply
                      (lambda ()
                        (#{check 2319}#
                          #{req 2425}#
                          #{opt 2426}#
                          #f
                          (cons #f (reverse #{rkey 2427}#))))
                      #{tmp 2433}#)
                    (let ((#{tmp 2434}#
                            ($sc-dispatch #{tmp 2432}# '(any . any))))
                      (if (if #{tmp 2434}#
                            (@apply
                              (lambda (#{a 2437}# #{b 2438}#)
                                (#{id? 343}# #{a 2437}#))
                              #{tmp 2434}#)
                            #f)
                        (@apply
                          (lambda (#{a 2441}# #{b 2442}#)
                            (let ((#{tmp 2444}#
                                    (symbol->keyword
                                      (syntax->datum #{a 2441}#))))
                              (let ((#{k 2446}# #{tmp 2444}#))
                                (#{key 2315}#
                                  #{b 2442}#
                                  #{req 2425}#
                                  #{opt 2426}#
                                  (cons (cons #{k 2446}#
                                              (cons #{a 2441}#
                                                    '(#(syntax-object
                                                        #f
                                                        ((top)
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(k)
                                                           #((top))
                                                           #("i2445"))
                                                         #(ribcage
                                                           #(a b)
                                                           #((top) (top))
                                                           #("i2439" "i2440"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(args req opt rkey)
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i2428"
                                                             "i2429"
                                                             "i2430"
                                                             "i2431"))
                                                         #(ribcage
                                                           (check rest
                                                                  key
                                                                  opt
                                                                  req)
                                                           ((top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                           ("i2318"
                                                            "i2316"
                                                            "i2314"
                                                            "i2312"
                                                            "i2310"))
                                                         #(ribcage
                                                           #(orig-args)
                                                           #((top))
                                                           #("i2309"))
                                                         #(ribcage
                                                           (lambda-var-list
                                                             gen-var
                                                             strip
                                                             chi-lambda-case
                                                             lambda*-formals
                                                             chi-simple-lambda
                                                             lambda-formals
                                                             ellipsis?
                                                             chi-void
                                                             eval-local-transformer
                                                             chi-local-syntax
                                                             chi-body
                                                             chi-macro
                                                             chi-call
                                                             chi-expr
                                                             chi
                                                             syntax-type
                                                             chi-when-list
                                                             chi-install-global
                                                             chi-top-sequence
                                                             chi-sequence
                                                             source-wrap
                                                             wrap
                                                             bound-id-member?
                                                             distinct-bound-ids?
                                                             valid-bound-ids?
                                                             bound-id=?
                                                             free-id=?
                                                             id-var-name
                                                             same-marks?
                                                             join-marks
                                                             join-wraps
                                                             smart-append
                                                             make-binding-wrap
                                                             extend-ribcage!
                                                             make-empty-ribcage
                                                             new-mark
                                                             anti-mark
                                                             the-anti-mark
                                                             top-marked?
                                                             top-wrap
                                                             empty-wrap
                                                             set-ribcage-labels!
                                                             set-ribcage-marks!
                                                             set-ribcage-symnames!
                                                             ribcage-labels
                                                             ribcage-marks
                                                             ribcage-symnames
                                                             ribcage?
                                                             make-ribcage
                                                             gen-labels
                                                             gen-label
                                                             make-rename
                                                             rename-marks
                                                             rename-new
                                                             rename-old
                                                             subst-rename?
                                                             wrap-subst
                                                             wrap-marks
                                                             make-wrap
                                                             id-sym-name&marks
                                                             id-sym-name
                                                             id?
                                                             nonsymbol-id?
                                                             global-extend
                                                             lookup
                                                             macros-only-env
                                                             extend-var-env
                                                             extend-env
                                                             null-env
                                                             binding-value
                                                             binding-type
                                                             make-binding
                                                             arg-check
                                                             source-annotation
                                                             no-source
                                                             set-syntax-object-module!
                                                             set-syntax-object-wrap!
                                                             set-syntax-object-expression!
                                                             syntax-object-module
                                                             syntax-object-wrap
                                                             syntax-object-expression
                                                             syntax-object?
                                                             make-syntax-object
                                                             build-lexical-var
                                                             build-letrec
                                                             build-named-let
                                                             build-let
                                                             build-sequence
                                                             build-data
                                                             build-primref
                                                             build-primcall
                                                             build-lambda-case
                                                             build-case-lambda
                                                             build-simple-lambda
                                                             build-global-definition
                                                             build-global-assignment
                                                             build-global-reference
                                                             analyze-variable
                                                             build-lexical-assignment
                                                             build-lexical-reference
                                                             build-dynlet
                                                             build-conditional
                                                             build-call
                                                             build-void
                                                             maybe-name-value!
                                                             decorate-source
                                                             get-global-definition-hook
                                                             put-global-definition-hook
                                                             gensym-hook
                                                             local-eval-hook
                                                             top-level-eval-hook
                                                             fx<
                                                             fx=
                                                             fx-
                                                             fx+
                                                             set-lambda-meta!
                                                             lambda-meta
                                                             lambda?
                                                             make-dynlet
                                                             make-letrec
                                                             make-let
                                                             make-lambda-case
                                                             make-lambda
                                                             make-seq
                                                             make-primcall
                                                             make-call
                                                             make-conditional
                                                             make-toplevel-define
                                                             make-toplevel-set
                                                             make-toplevel-ref
                                                             make-module-set
                                                             make-module-ref
                                                             make-lexical-set
                                                             make-lexical-ref
                                                             make-primitive-ref
                                                             make-const
                                                             make-void)
                                                           ((top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                           ("i452"
                                                            "i450"
                                                            "i448"
                                                            "i446"
                                                            "i444"
                                                            "i442"
                                                            "i440"
                                                            "i438"
                                                            "i436"
                                                            "i434"
                                                            "i432"
                                                            "i430"
                                                            "i428"
                                                            "i426"
                                                            "i424"
                                                            "i422"
                                                            "i420"
                                                            "i418"
                                                            "i416"
                                                            "i414"
                                                            "i412"
                                                            "i410"
                                                            "i408"
                                                            "i406"
                                                            "i404"
                                                            "i402"
                                                            "i400"
                                                            "i398"
                                                            "i396"
                                                            "i394"
                                                            "i392"
                                                            "i390"
                                                            "i388"
                                                            "i386"
                                                            "i384"
                                                            "i383"
                                                            "i382"
                                                            "i380"
                                                            "i379"
                                                            "i378"
                                                            "i377"
                                                            "i376"
                                                            "i374"
                                                            "i372"
                                                            "i370"
                                                            "i368"
                                                            "i366"
                                                            "i364"
                                                            "i362"
                                                            "i360"
                                                            "i357"
                                                            "i355"
                                                            "i354"
                                                            "i353"
                                                            "i352"
                                                            "i351"
                                                            "i350"
                                                            "i349"
                                                            "i348"
                                                            "i347"
                                                            "i345"
                                                            "i344"
                                                            "i342"
                                                            "i340"
                                                            "i338"
                                                            "i336"
                                                            "i334"
                                                            "i332"
                                                            "i330"
                                                            "i329"
                                                            "i328"
                                                            "i327"
                                                            "i326"
                                                            "i325"
                                                            "i323"
                                                            "i322"
                                                            "i320"
                                                            "i318"
                                                            "i316"
                                                            "i314"
                                                            "i312"
                                                            "i310"
                                                            "i308"
                                                            "i306"
                                                            "i304"
                                                            "i302"
                                                            "i300"
                                                            "i298"
                                                            "i296"
                                                            "i294"
                                                            "i292"
                                                            "i290"
                                                            "i288"
                                                            "i286"
                                                            "i284"
                                                            "i282"
                                                            "i280"
                                                            "i278"
                                                            "i276"
                                                            "i274"
                                                            "i272"
                                                            "i270"
                                                            "i268"
                                                            "i266"
                                                            "i264"
                                                            "i262"
                                                            "i260"
                                                            "i258"
                                                            "i256"
                                                            "i255"
                                                            "i253"
                                                            "i251"
                                                            "i250"
                                                            "i249"
                                                            "i248"
                                                            "i247"
                                                            "i245"
                                                            "i243"
                                                            "i241"
                                                            "i238"
                                                            "i236"
                                                            "i234"
                                                            "i232"
                                                            "i230"
                                                            "i228"
                                                            "i226"
                                                            "i224"
                                                            "i222"
                                                            "i220"
                                                            "i218"
                                                            "i216"
                                                            "i214"
                                                            "i212"
                                                            "i210"
                                                            "i208"
                                                            "i206"
                                                            "i204"
                                                            "i202"))
                                                         #(ribcage
                                                           (define-structure
                                                             define-expansion-accessors
                                                             define-expansion-constructors)
                                                           ((top) (top) (top))
                                                           ("i40"
                                                            "i39"
                                                            "i38")))
                                                        (hygiene guile)))))
                                        #{rkey 2427}#)))))
                          #{tmp 2434}#)
                        (let ((#{tmp 2447}#
                                ($sc-dispatch
                                  #{tmp 2432}#
                                  '((any any) . any))))
                          (if (if #{tmp 2447}#
                                (@apply
                                  (lambda (#{a 2451}# #{init 2452}# #{b 2453}#)
                                    (#{id? 343}# #{a 2451}#))
                                  #{tmp 2447}#)
                                #f)
                            (@apply
                              (lambda (#{a 2457}# #{init 2458}# #{b 2459}#)
                                (let ((#{tmp 2461}#
                                        (symbol->keyword
                                          (syntax->datum #{a 2457}#))))
                                  (let ((#{k 2463}# #{tmp 2461}#))
                                    (#{key 2315}#
                                      #{b 2459}#
                                      #{req 2425}#
                                      #{opt 2426}#
                                      (cons (list #{k 2463}#
                                                  #{a 2457}#
                                                  #{init 2458}#)
                                            #{rkey 2427}#)))))
                              #{tmp 2447}#)
                            (let ((#{tmp 2464}#
                                    ($sc-dispatch
                                      #{tmp 2432}#
                                      '((any any any) . any))))
                              (if (if #{tmp 2464}#
                                    (@apply
                                      (lambda (#{a 2469}#
                                               #{init 2470}#
                                               #{k 2471}#
                                               #{b 2472}#)
                                        (if (#{id? 343}# #{a 2469}#)
                                          (keyword? (syntax->datum #{k 2471}#))
                                          #f))
                                      #{tmp 2464}#)
                                    #f)
                                (@apply
                                  (lambda (#{a 2479}#
                                           #{init 2480}#
                                           #{k 2481}#
                                           #{b 2482}#)
                                    (#{key 2315}#
                                      #{b 2482}#
                                      #{req 2425}#
                                      #{opt 2426}#
                                      (cons (list #{k 2481}#
                                                  #{a 2479}#
                                                  #{init 2480}#)
                                            #{rkey 2427}#)))
                                  #{tmp 2464}#)
                                (let ((#{tmp 2483}#
                                        ($sc-dispatch #{tmp 2432}# '(any))))
                                  (if (if #{tmp 2483}#
                                        (@apply
                                          (lambda (#{aok 2485}#)
                                            (eq? (syntax->datum #{aok 2485}#)
                                                 #:allow-other-keys))
                                          #{tmp 2483}#)
                                        #f)
                                    (@apply
                                      (lambda (#{aok 2487}#)
                                        (#{check 2319}#
                                          #{req 2425}#
                                          #{opt 2426}#
                                          #f
                                          (cons #t (reverse #{rkey 2427}#))))
                                      #{tmp 2483}#)
                                    (let ((#{tmp 2488}#
                                            ($sc-dispatch
                                              #{tmp 2432}#
                                              '(any any any))))
                                      (if (if #{tmp 2488}#
                                            (@apply
                                              (lambda (#{aok 2492}#
                                                       #{a 2493}#
                                                       #{b 2494}#)
                                                (if (eq? (syntax->datum
                                                           #{aok 2492}#)
                                                         #:allow-other-keys)
                                                  (eq? (syntax->datum
                                                         #{a 2493}#)
                                                       #:rest)
                                                  #f))
                                              #{tmp 2488}#)
                                            #f)
                                        (@apply
                                          (lambda (#{aok 2500}#
                                                   #{a 2501}#
                                                   #{b 2502}#)
                                            (#{rest 2317}#
                                              #{b 2502}#
                                              #{req 2425}#
                                              #{opt 2426}#
                                              (cons #t
                                                    (reverse #{rkey 2427}#))))
                                          #{tmp 2488}#)
                                        (let ((#{tmp 2503}#
                                                ($sc-dispatch
                                                  #{tmp 2432}#
                                                  '(any . any))))
                                          (if (if #{tmp 2503}#
                                                (@apply
                                                  (lambda (#{aok 2506}#
                                                           #{r 2507}#)
                                                    (if (eq? (syntax->datum
                                                               #{aok 2506}#)
                                                             #:allow-other-keys)
                                                      (#{id? 343}# #{r 2507}#)
                                                      #f))
                                                  #{tmp 2503}#)
                                                #f)
                                            (@apply
                                              (lambda (#{aok 2512}# #{r 2513}#)
                                                (#{rest 2317}#
                                                  #{r 2513}#
                                                  #{req 2425}#
                                                  #{opt 2426}#
                                                  (cons #t
                                                        (reverse
                                                          #{rkey 2427}#))))
                                              #{tmp 2503}#)
                                            (let ((#{tmp 2514}#
                                                    ($sc-dispatch
                                                      #{tmp 2432}#
                                                      '(any any))))
                                              (if (if #{tmp 2514}#
                                                    (@apply
                                                      (lambda (#{a 2517}#
                                                               #{b 2518}#)
                                                        (eq? (syntax->datum
                                                               #{a 2517}#)
                                                             #:rest))
                                                      #{tmp 2514}#)
                                                    #f)
                                                (@apply
                                                  (lambda (#{a 2521}#
                                                           #{b 2522}#)
                                                    (#{rest 2317}#
                                                      #{b 2522}#
                                                      #{req 2425}#
                                                      #{opt 2426}#
                                                      (cons #f
                                                            (reverse
                                                              #{rkey 2427}#))))
                                                  #{tmp 2514}#)
                                                (let ((#{tmp 2523}#
                                                        (list #{tmp 2432}#)))
                                                  (if (if #{tmp 2523}#
                                                        (@apply
                                                          (lambda (#{r 2525}#)
                                                            (#{id? 343}#
                                                              #{r 2525}#))
                                                          #{tmp 2523}#)
                                                        #f)
                                                    (@apply
                                                      (lambda (#{r 2527}#)
                                                        (#{rest 2317}#
                                                          #{r 2527}#
                                                          #{req 2425}#
                                                          #{opt 2426}#
                                                          (cons #f
                                                                (reverse
                                                                  #{rkey 2427}#))))
                                                      #{tmp 2523}#)
                                                    (let ((#{else 2529}#
                                                            #{tmp 2432}#))
                                                      (syntax-violation
                                                        'lambda*
                                                        "invalid keyword argument list"
                                                        #{orig-args 2308}#
                                                        #{args 2424}#)))))))))))))))))))))))
          (#{rest 2317}#
            (lambda (#{args 2530}#
                     #{req 2531}#
                     #{opt 2532}#
                     #{kw 2533}#)
              (let ((#{tmp 2538}# #{args 2530}#))
                (let ((#{tmp 2539}# (list #{tmp 2538}#)))
                  (if (if #{tmp 2539}#
                        (@apply
                          (lambda (#{r 2541}#) (#{id? 343}# #{r 2541}#))
                          #{tmp 2539}#)
                        #f)
                    (@apply
                      (lambda (#{r 2543}#)
                        (#{check 2319}#
                          #{req 2531}#
                          #{opt 2532}#
                          #{r 2543}#
                          #{kw 2533}#))
                      #{tmp 2539}#)
                    (let ((#{else 2545}# #{tmp 2538}#))
                      (syntax-violation
                        'lambda*
                        "invalid rest argument"
                        #{orig-args 2308}#
                        #{args 2530}#)))))))
          (#{check 2319}#
            (lambda (#{req 2546}#
                     #{opt 2547}#
                     #{rest 2548}#
                     #{kw 2549}#)
              (if (#{distinct-bound-ids? 405}#
                    (append
                      #{req 2546}#
                      (map car #{opt 2547}#)
                      (if #{rest 2548}# (list #{rest 2548}#) '())
                      (if (pair? #{kw 2549}#)
                        (map cadr (cdr #{kw 2549}#))
                        '())))
                (values
                  #{req 2546}#
                  #{opt 2547}#
                  #{rest 2548}#
                  #{kw 2549}#)
                (syntax-violation
                  'lambda*
                  "duplicate identifier in argument list"
                  #{orig-args 2308}#)))))
         (#{req 2311}# #{orig-args 2308}# '()))))
   (#{chi-lambda-case 447}#
     (lambda (#{e 2557}#
              #{r 2558}#
              #{w 2559}#
              #{s 2560}#
              #{mod 2561}#
              #{get-formals 2562}#
              #{clauses 2563}#)
       (letrec*
         ((#{expand-req 2572}#
            (lambda (#{req 2579}#
                     #{opt 2580}#
                     #{rest 2581}#
                     #{kw 2582}#
                     #{body 2583}#)
              (let ((#{vars 2591}#
                      (map #{gen-var 451}# #{req 2579}#))
                    (#{labels 2592}#
                      (#{gen-labels 358}# #{req 2579}#)))
                (let ((#{r* 2595}#
                        (#{extend-var-env 333}#
                          #{labels 2592}#
                          #{vars 2591}#
                          #{r 2558}#))
                      (#{w* 2596}#
                        (#{make-binding-wrap 387}#
                          #{req 2579}#
                          #{labels 2592}#
                          #{w 2559}#)))
                  (#{expand-opt 2574}#
                    (map syntax->datum #{req 2579}#)
                    #{opt 2580}#
                    #{rest 2581}#
                    #{kw 2582}#
                    #{body 2583}#
                    (reverse #{vars 2591}#)
                    #{r* 2595}#
                    #{w* 2596}#
                    '()
                    '())))))
          (#{expand-opt 2574}#
            (lambda (#{req 2597}#
                     #{opt 2598}#
                     #{rest 2599}#
                     #{kw 2600}#
                     #{body 2601}#
                     #{vars 2602}#
                     #{r* 2603}#
                     #{w* 2604}#
                     #{out 2605}#
                     #{inits 2606}#)
              (if (pair? #{opt 2598}#)
                (let ((#{tmp 2619}# (car #{opt 2598}#)))
                  (let ((#{tmp 2620}#
                          ($sc-dispatch #{tmp 2619}# '(any any))))
                    (if #{tmp 2620}#
                      (@apply
                        (lambda (#{id 2623}# #{i 2624}#)
                          (let ((#{v 2627}# (#{gen-var 451}# #{id 2623}#)))
                            (let ((#{l 2629}#
                                    (#{gen-labels 358}# (list #{v 2627}#))))
                              (let ((#{r** 2631}#
                                      (#{extend-var-env 333}#
                                        #{l 2629}#
                                        (list #{v 2627}#)
                                        #{r* 2603}#)))
                                (let ((#{w** 2633}#
                                        (#{make-binding-wrap 387}#
                                          (list #{id 2623}#)
                                          #{l 2629}#
                                          #{w* 2604}#)))
                                  (#{expand-opt 2574}#
                                    #{req 2597}#
                                    (cdr #{opt 2598}#)
                                    #{rest 2599}#
                                    #{kw 2600}#
                                    #{body 2601}#
                                    (cons #{v 2627}# #{vars 2602}#)
                                    #{r** 2631}#
                                    #{w** 2633}#
                                    (cons (syntax->datum #{id 2623}#)
                                          #{out 2605}#)
                                    (cons (#{chi 423}#
                                            #{i 2624}#
                                            #{r* 2603}#
                                            #{w* 2604}#
                                            #{mod 2561}#)
                                          #{inits 2606}#)))))))
                        #{tmp 2620}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp 2619}#))))
                (if #{rest 2599}#
                  (let ((#{v 2638}# (#{gen-var 451}# #{rest 2599}#)))
                    (let ((#{l 2640}#
                            (#{gen-labels 358}# (list #{v 2638}#))))
                      (let ((#{r* 2642}#
                              (#{extend-var-env 333}#
                                #{l 2640}#
                                (list #{v 2638}#)
                                #{r* 2603}#)))
                        (let ((#{w* 2644}#
                                (#{make-binding-wrap 387}#
                                  (list #{rest 2599}#)
                                  #{l 2640}#
                                  #{w* 2604}#)))
                          (#{expand-kw 2576}#
                            #{req 2597}#
                            (if (pair? #{out 2605}#)
                              (reverse #{out 2605}#)
                              #f)
                            (syntax->datum #{rest 2599}#)
                            (if (pair? #{kw 2600}#)
                              (cdr #{kw 2600}#)
                              #{kw 2600}#)
                            #{body 2601}#
                            (cons #{v 2638}# #{vars 2602}#)
                            #{r* 2642}#
                            #{w* 2644}#
                            (if (pair? #{kw 2600}#) (car #{kw 2600}#) #f)
                            '()
                            #{inits 2606}#)))))
                  (#{expand-kw 2576}#
                    #{req 2597}#
                    (if (pair? #{out 2605}#)
                      (reverse #{out 2605}#)
                      #f)
                    #f
                    (if (pair? #{kw 2600}#)
                      (cdr #{kw 2600}#)
                      #{kw 2600}#)
                    #{body 2601}#
                    #{vars 2602}#
                    #{r* 2603}#
                    #{w* 2604}#
                    (if (pair? #{kw 2600}#) (car #{kw 2600}#) #f)
                    '()
                    #{inits 2606}#)))))
          (#{expand-kw 2576}#
            (lambda (#{req 2646}#
                     #{opt 2647}#
                     #{rest 2648}#
                     #{kw 2649}#
                     #{body 2650}#
                     #{vars 2651}#
                     #{r* 2652}#
                     #{w* 2653}#
                     #{aok 2654}#
                     #{out 2655}#
                     #{inits 2656}#)
              (if (pair? #{kw 2649}#)
                (let ((#{tmp 2670}# (car #{kw 2649}#)))
                  (let ((#{tmp 2671}#
                          ($sc-dispatch #{tmp 2670}# '(any any any))))
                    (if #{tmp 2671}#
                      (@apply
                        (lambda (#{k 2675}# #{id 2676}# #{i 2677}#)
                          (let ((#{v 2680}# (#{gen-var 451}# #{id 2676}#)))
                            (let ((#{l 2682}#
                                    (#{gen-labels 358}# (list #{v 2680}#))))
                              (let ((#{r** 2684}#
                                      (#{extend-var-env 333}#
                                        #{l 2682}#
                                        (list #{v 2680}#)
                                        #{r* 2652}#)))
                                (let ((#{w** 2686}#
                                        (#{make-binding-wrap 387}#
                                          (list #{id 2676}#)
                                          #{l 2682}#
                                          #{w* 2653}#)))
                                  (#{expand-kw 2576}#
                                    #{req 2646}#
                                    #{opt 2647}#
                                    #{rest 2648}#
                                    (cdr #{kw 2649}#)
                                    #{body 2650}#
                                    (cons #{v 2680}# #{vars 2651}#)
                                    #{r** 2684}#
                                    #{w** 2686}#
                                    #{aok 2654}#
                                    (cons (list (syntax->datum #{k 2675}#)
                                                (syntax->datum #{id 2676}#)
                                                #{v 2680}#)
                                          #{out 2655}#)
                                    (cons (#{chi 423}#
                                            #{i 2677}#
                                            #{r* 2652}#
                                            #{w* 2653}#
                                            #{mod 2561}#)
                                          #{inits 2656}#)))))))
                        #{tmp 2671}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp 2670}#))))
                (#{expand-body 2578}#
                  #{req 2646}#
                  #{opt 2647}#
                  #{rest 2648}#
                  (if (let ((#{t 2690}# #{aok 2654}#))
                        (if #{t 2690}# #{t 2690}# (pair? #{out 2655}#)))
                    (cons #{aok 2654}# (reverse #{out 2655}#))
                    #f)
                  #{body 2650}#
                  (reverse #{vars 2651}#)
                  #{r* 2652}#
                  #{w* 2653}#
                  (reverse #{inits 2656}#)
                  '()))))
          (#{expand-body 2578}#
            (lambda (#{req 2692}#
                     #{opt 2693}#
                     #{rest 2694}#
                     #{kw 2695}#
                     #{body 2696}#
                     #{vars 2697}#
                     #{r* 2698}#
                     #{w* 2699}#
                     #{inits 2700}#
                     #{meta 2701}#)
              (let ((#{tmp 2712}# #{body 2696}#))
                (let ((#{tmp 2713}#
                        ($sc-dispatch #{tmp 2712}# '(any any . each-any))))
                  (if (if #{tmp 2713}#
                        (@apply
                          (lambda (#{docstring 2717}# #{e1 2718}# #{e2 2719}#)
                            (string? (syntax->datum #{docstring 2717}#)))
                          #{tmp 2713}#)
                        #f)
                    (@apply
                      (lambda (#{docstring 2723}# #{e1 2724}# #{e2 2725}#)
                        (#{expand-body 2578}#
                          #{req 2692}#
                          #{opt 2693}#
                          #{rest 2694}#
                          #{kw 2695}#
                          (cons #{e1 2724}# #{e2 2725}#)
                          #{vars 2697}#
                          #{r* 2698}#
                          #{w* 2699}#
                          #{inits 2700}#
                          (append
                            #{meta 2701}#
                            (list (cons 'documentation
                                        (syntax->datum #{docstring 2723}#))))))
                      #{tmp 2713}#)
                    (let ((#{tmp 2728}#
                            ($sc-dispatch
                              #{tmp 2712}#
                              '(#(vector #(each (any . any)))
                                any
                                .
                                each-any))))
                      (if #{tmp 2728}#
                        (@apply
                          (lambda (#{k 2733}#
                                   #{v 2734}#
                                   #{e1 2735}#
                                   #{e2 2736}#)
                            (#{expand-body 2578}#
                              #{req 2692}#
                              #{opt 2693}#
                              #{rest 2694}#
                              #{kw 2695}#
                              (cons #{e1 2735}# #{e2 2736}#)
                              #{vars 2697}#
                              #{r* 2698}#
                              #{w* 2699}#
                              #{inits 2700}#
                              (append
                                #{meta 2701}#
                                (syntax->datum
                                  (map cons #{k 2733}# #{v 2734}#)))))
                          #{tmp 2728}#)
                        (let ((#{tmp 2740}#
                                ($sc-dispatch #{tmp 2712}# '(any . each-any))))
                          (if #{tmp 2740}#
                            (@apply
                              (lambda (#{e1 2743}# #{e2 2744}#)
                                (values
                                  #{meta 2701}#
                                  #{req 2692}#
                                  #{opt 2693}#
                                  #{rest 2694}#
                                  #{kw 2695}#
                                  #{inits 2700}#
                                  #{vars 2697}#
                                  (#{chi-body 431}#
                                    (cons #{e1 2743}# #{e2 2744}#)
                                    (#{source-wrap 411}#
                                      #{e 2557}#
                                      #{w 2559}#
                                      #{s 2560}#
                                      #{mod 2561}#)
                                    #{r* 2698}#
                                    #{w* 2699}#
                                    #{mod 2561}#)))
                              #{tmp 2740}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp 2712}#)))))))))))
         (let ((#{tmp 2746}# #{clauses 2563}#))
           (let ((#{tmp 2747}# ($sc-dispatch #{tmp 2746}# '())))
             (if #{tmp 2747}#
               (@apply (lambda () (values '() #f)) #{tmp 2747}#)
               (let ((#{tmp 2748}#
                       ($sc-dispatch
                         #{tmp 2746}#
                         '((any any . each-any)
                           .
                           #(each (any any . each-any))))))
                 (if #{tmp 2748}#
                   (@apply
                     (lambda (#{args 2755}#
                              #{e1 2756}#
                              #{e2 2757}#
                              #{args* 2758}#
                              #{e1* 2759}#
                              #{e2* 2760}#)
                       (call-with-values
                         (lambda () (#{get-formals 2562}# #{args 2755}#))
                         (lambda (#{req 2761}#
                                  #{opt 2762}#
                                  #{rest 2763}#
                                  #{kw 2764}#)
                           (call-with-values
                             (lambda ()
                               (#{expand-req 2572}#
                                 #{req 2761}#
                                 #{opt 2762}#
                                 #{rest 2763}#
                                 #{kw 2764}#
                                 (cons #{e1 2756}# #{e2 2757}#)))
                             (lambda (#{meta 2770}#
                                      #{req 2771}#
                                      #{opt 2772}#
                                      #{rest 2773}#
                                      #{kw 2774}#
                                      #{inits 2775}#
                                      #{vars 2776}#
                                      #{body 2777}#)
                               (call-with-values
                                 (lambda ()
                                   (#{chi-lambda-case 447}#
                                     #{e 2557}#
                                     #{r 2558}#
                                     #{w 2559}#
                                     #{s 2560}#
                                     #{mod 2561}#
                                     #{get-formals 2562}#
                                     (map (lambda (#{tmp 2788}#
                                                   #{tmp 2787}#
                                                   #{tmp 2786}#)
                                            (cons #{tmp 2786}#
                                                  (cons #{tmp 2787}#
                                                        #{tmp 2788}#)))
                                          #{e2* 2760}#
                                          #{e1* 2759}#
                                          #{args* 2758}#)))
                                 (lambda (#{meta* 2790}# #{else* 2791}#)
                                   (values
                                     (append #{meta 2770}# #{meta* 2790}#)
                                     (#{build-lambda-case 289}#
                                       #{s 2560}#
                                       #{req 2771}#
                                       #{opt 2772}#
                                       #{rest 2773}#
                                       #{kw 2774}#
                                       #{inits 2775}#
                                       #{vars 2776}#
                                       #{body 2777}#
                                       #{else* 2791}#)))))))))
                     #{tmp 2748}#)
                   (syntax-violation
                     #f
                     "source expression failed to match any pattern"
                     #{tmp 2746}#)))))))))
   (#{strip 449}#
     (lambda (#{x 2794}# #{w 2795}#)
       (if (memq 'top (car #{w 2795}#))
         #{x 2794}#
         (letrec*
           ((#{f 2802}#
              (lambda (#{x 2803}#)
                (if (#{syntax-object? 309}# #{x 2803}#)
                  (#{strip 449}#
                    (#{syntax-object-expression 311}# #{x 2803}#)
                    (#{syntax-object-wrap 313}# #{x 2803}#))
                  (if (pair? #{x 2803}#)
                    (let ((#{a 2810}# (#{f 2802}# (car #{x 2803}#)))
                          (#{d 2811}# (#{f 2802}# (cdr #{x 2803}#))))
                      (if (if (eq? #{a 2810}# (car #{x 2803}#))
                            (eq? #{d 2811}# (cdr #{x 2803}#))
                            #f)
                        #{x 2803}#
                        (cons #{a 2810}# #{d 2811}#)))
                    (if (vector? #{x 2803}#)
                      (let ((#{old 2817}# (vector->list #{x 2803}#)))
                        (let ((#{new 2819}# (map #{f 2802}# #{old 2817}#)))
                          (letrec*
                            ((#{lp 2823}#
                               (lambda (#{l1 2824}# #{l2 2825}#)
                                 (if (null? #{l1 2824}#)
                                   #{x 2803}#
                                   (if (eq? (car #{l1 2824}#)
                                            (car #{l2 2825}#))
                                     (#{lp 2823}#
                                       (cdr #{l1 2824}#)
                                       (cdr #{l2 2825}#))
                                     (list->vector #{new 2819}#))))))
                            (#{lp 2823}# #{old 2817}# #{new 2819}#))))
                      #{x 2803}#))))))
           (#{f 2802}# #{x 2794}#)))))
   (#{gen-var 451}#
     (lambda (#{id 2827}#)
       (let ((#{id 2830}#
               (if (#{syntax-object? 309}# #{id 2827}#)
                 (#{syntax-object-expression 311}# #{id 2827}#)
                 #{id 2827}#)))
         (gensym
           (string-append (symbol->string #{id 2830}#) " ")))))
   (#{lambda-var-list 453}#
     (lambda (#{vars 2832}#)
       (letrec*
         ((#{lvl 2838}#
            (lambda (#{vars 2839}# #{ls 2840}# #{w 2841}#)
              (if (pair? #{vars 2839}#)
                (#{lvl 2838}#
                  (cdr #{vars 2839}#)
                  (cons (#{wrap 409}# (car #{vars 2839}#) #{w 2841}# #f)
                        #{ls 2840}#)
                  #{w 2841}#)
                (if (#{id? 343}# #{vars 2839}#)
                  (cons (#{wrap 409}# #{vars 2839}# #{w 2841}# #f)
                        #{ls 2840}#)
                  (if (null? #{vars 2839}#)
                    #{ls 2840}#
                    (if (#{syntax-object? 309}# #{vars 2839}#)
                      (#{lvl 2838}#
                        (#{syntax-object-expression 311}# #{vars 2839}#)
                        #{ls 2840}#
                        (#{join-wraps 391}#
                          #{w 2841}#
                          (#{syntax-object-wrap 313}# #{vars 2839}#)))
                      (cons #{vars 2839}# #{ls 2840}#))))))))
         (#{lvl 2838}# #{vars 2832}# '() '(()))))))
  (begin
    (lambda (#{x 1163}# #{update 1164}#)
      (vector-set! #{x 1163}# 1 #{update 1164}#))
    (lambda (#{x 1167}# #{update 1168}#)
      (vector-set! #{x 1167}# 2 #{update 1168}#))
    (lambda (#{x 1171}# #{update 1172}#)
      (vector-set! #{x 1171}# 3 #{update 1172}#))
    (lambda (#{x 1252}#)
      (if (vector? #{x 1252}#)
        (if (= (vector-length #{x 1252}#) 4)
          (eq? (vector-ref #{x 1252}# 0) 'ribcage)
          #f)
        #f))
    (#{global-extend 339}#
      'local-syntax
      'letrec-syntax
      #t)
    (#{global-extend 339}#
      'local-syntax
      'let-syntax
      #f)
    (#{global-extend 339}#
      'core
      'fluid-let-syntax
      (lambda (#{e 2852}#
               #{r 2853}#
               #{w 2854}#
               #{s 2855}#
               #{mod 2856}#)
        (let ((#{tmp 2862}# #{e 2852}#))
          (let ((#{tmp 2863}#
                  ($sc-dispatch
                    #{tmp 2862}#
                    '(_ #(each (any any)) any . each-any))))
            (if (if #{tmp 2863}#
                  (@apply
                    (lambda (#{var 2868}#
                             #{val 2869}#
                             #{e1 2870}#
                             #{e2 2871}#)
                      (#{valid-bound-ids? 403}# #{var 2868}#))
                    #{tmp 2863}#)
                  #f)
              (@apply
                (lambda (#{var 2877}#
                         #{val 2878}#
                         #{e1 2879}#
                         #{e2 2880}#)
                  (let ((#{names 2882}#
                          (map (lambda (#{x 2883}#)
                                 (#{id-var-name 397}# #{x 2883}# #{w 2854}#))
                               #{var 2877}#)))
                    (begin
                      (for-each
                        (lambda (#{id 2886}# #{n 2887}#)
                          (let ((#{atom-key 2892}#
                                  (car (#{lookup 337}#
                                         #{n 2887}#
                                         #{r 2853}#
                                         #{mod 2856}#))))
                            (if (memv #{atom-key 2892}# '(displaced-lexical))
                              (syntax-violation
                                'fluid-let-syntax
                                "identifier out of context"
                                #{e 2852}#
                                (#{source-wrap 411}#
                                  #{id 2886}#
                                  #{w 2854}#
                                  #{s 2855}#
                                  #{mod 2856}#)))))
                        #{var 2877}#
                        #{names 2882}#)
                      (#{chi-body 431}#
                        (cons #{e1 2879}# #{e2 2880}#)
                        (#{source-wrap 411}#
                          #{e 2852}#
                          #{w 2854}#
                          #{s 2855}#
                          #{mod 2856}#)
                        (#{extend-env 331}#
                          #{names 2882}#
                          (let ((#{trans-r 2898}#
                                  (#{macros-only-env 335}# #{r 2853}#)))
                            (map (lambda (#{x 2899}#)
                                   (cons 'macro
                                         (#{eval-local-transformer 435}#
                                           (#{chi 423}#
                                             #{x 2899}#
                                             #{trans-r 2898}#
                                             #{w 2854}#
                                             #{mod 2856}#)
                                           #{mod 2856}#)))
                                 #{val 2878}#))
                          #{r 2853}#)
                        #{w 2854}#
                        #{mod 2856}#))))
                #{tmp 2863}#)
              (let ((#{_ 2904}# #{tmp 2862}#))
                (syntax-violation
                  'fluid-let-syntax
                  "bad syntax"
                  (#{source-wrap 411}#
                    #{e 2852}#
                    #{w 2854}#
                    #{s 2855}#
                    #{mod 2856}#))))))))
    (#{global-extend 339}#
      'core
      'quote
      (lambda (#{e 2905}#
               #{r 2906}#
               #{w 2907}#
               #{s 2908}#
               #{mod 2909}#)
        (let ((#{tmp 2915}# #{e 2905}#))
          (let ((#{tmp 2916}#
                  ($sc-dispatch #{tmp 2915}# '(_ any))))
            (if #{tmp 2916}#
              (@apply
                (lambda (#{e 2918}#)
                  (#{build-data 295}#
                    #{s 2908}#
                    (#{strip 449}# #{e 2918}# #{w 2907}#)))
                #{tmp 2916}#)
              (let ((#{_ 2920}# #{tmp 2915}#))
                (syntax-violation
                  'quote
                  "bad syntax"
                  (#{source-wrap 411}#
                    #{e 2905}#
                    #{w 2907}#
                    #{s 2908}#
                    #{mod 2909}#))))))))
    (#{global-extend 339}#
      'core
      'syntax
      (letrec*
        ((#{gen-syntax 2922}#
           (lambda (#{src 2937}#
                    #{e 2938}#
                    #{r 2939}#
                    #{maps 2940}#
                    #{ellipsis? 2941}#
                    #{mod 2942}#)
             (if (#{id? 343}# #{e 2938}#)
               (let ((#{label 2950}#
                       (#{id-var-name 397}# #{e 2938}# '(()))))
                 (let ((#{b 2953}#
                         (#{lookup 337}#
                           #{label 2950}#
                           #{r 2939}#
                           #{mod 2942}#)))
                   (if (eq? (car #{b 2953}#) 'syntax)
                     (call-with-values
                       (lambda ()
                         (let ((#{var.lev 2956}# (cdr #{b 2953}#)))
                           (#{gen-ref 2924}#
                             #{src 2937}#
                             (car #{var.lev 2956}#)
                             (cdr #{var.lev 2956}#)
                             #{maps 2940}#)))
                       (lambda (#{var 2958}# #{maps 2959}#)
                         (values (list 'ref #{var 2958}#) #{maps 2959}#)))
                     (if (#{ellipsis? 2941}# #{e 2938}#)
                       (syntax-violation
                         'syntax
                         "misplaced ellipsis"
                         #{src 2937}#)
                       (values (list 'quote #{e 2938}#) #{maps 2940}#)))))
               (let ((#{tmp 2964}# #{e 2938}#))
                 (let ((#{tmp 2965}#
                         ($sc-dispatch #{tmp 2964}# '(any any))))
                   (if (if #{tmp 2965}#
                         (@apply
                           (lambda (#{dots 2968}# #{e 2969}#)
                             (#{ellipsis? 2941}# #{dots 2968}#))
                           #{tmp 2965}#)
                         #f)
                     (@apply
                       (lambda (#{dots 2972}# #{e 2973}#)
                         (#{gen-syntax 2922}#
                           #{src 2937}#
                           #{e 2973}#
                           #{r 2939}#
                           #{maps 2940}#
                           (lambda (#{x 2974}#) #f)
                           #{mod 2942}#))
                       #{tmp 2965}#)
                     (let ((#{tmp 2976}#
                             ($sc-dispatch #{tmp 2964}# '(any any . any))))
                       (if (if #{tmp 2976}#
                             (@apply
                               (lambda (#{x 2980}# #{dots 2981}# #{y 2982}#)
                                 (#{ellipsis? 2941}# #{dots 2981}#))
                               #{tmp 2976}#)
                             #f)
                         (@apply
                           (lambda (#{x 2986}# #{dots 2987}# #{y 2988}#)
                             (letrec*
                               ((#{f 2992}#
                                  (lambda (#{y 2993}# #{k 2994}#)
                                    (let ((#{tmp 3001}# #{y 2993}#))
                                      (let ((#{tmp 3002}#
                                              ($sc-dispatch
                                                #{tmp 3001}#
                                                '(any . any))))
                                        (if (if #{tmp 3002}#
                                              (@apply
                                                (lambda (#{dots 3005}#
                                                         #{y 3006}#)
                                                  (#{ellipsis? 2941}#
                                                    #{dots 3005}#))
                                                #{tmp 3002}#)
                                              #f)
                                          (@apply
                                            (lambda (#{dots 3009}# #{y 3010}#)
                                              (#{f 2992}#
                                                #{y 3010}#
                                                (lambda (#{maps 3011}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{k 2994}#
                                                        (cons '()
                                                              #{maps 3011}#)))
                                                    (lambda (#{x 3013}#
                                                             #{maps 3014}#)
                                                      (if (null? (car #{maps 3014}#))
                                                        (syntax-violation
                                                          'syntax
                                                          "extra ellipsis"
                                                          #{src 2937}#)
                                                        (values
                                                          (#{gen-mappend 2926}#
                                                            #{x 3013}#
                                                            (car #{maps 3014}#))
                                                          (cdr #{maps 3014}#))))))))
                                            #{tmp 3002}#)
                                          (let ((#{_ 3018}# #{tmp 3001}#))
                                            (call-with-values
                                              (lambda ()
                                                (#{gen-syntax 2922}#
                                                  #{src 2937}#
                                                  #{y 2993}#
                                                  #{r 2939}#
                                                  #{maps 2940}#
                                                  #{ellipsis? 2941}#
                                                  #{mod 2942}#))
                                              (lambda (#{y 3019}#
                                                       #{maps 3020}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{k 2994}# #{maps 3020}#))
                                                  (lambda (#{x 3023}#
                                                           #{maps 3024}#)
                                                    (values
                                                      (#{gen-append 2932}#
                                                        #{x 3023}#
                                                        #{y 3019}#)
                                                      #{maps 3024}#))))))))))))
                               (#{f 2992}#
                                 #{y 2988}#
                                 (lambda (#{maps 2995}#)
                                   (call-with-values
                                     (lambda ()
                                       (#{gen-syntax 2922}#
                                         #{src 2937}#
                                         #{x 2986}#
                                         #{r 2939}#
                                         (cons '() #{maps 2995}#)
                                         #{ellipsis? 2941}#
                                         #{mod 2942}#))
                                     (lambda (#{x 2997}# #{maps 2998}#)
                                       (if (null? (car #{maps 2998}#))
                                         (syntax-violation
                                           'syntax
                                           "extra ellipsis"
                                           #{src 2937}#)
                                         (values
                                           (#{gen-map 2928}#
                                             #{x 2997}#
                                             (car #{maps 2998}#))
                                           (cdr #{maps 2998}#)))))))))
                           #{tmp 2976}#)
                         (let ((#{tmp 3027}#
                                 ($sc-dispatch #{tmp 2964}# '(any . any))))
                           (if #{tmp 3027}#
                             (@apply
                               (lambda (#{x 3030}# #{y 3031}#)
                                 (call-with-values
                                   (lambda ()
                                     (#{gen-syntax 2922}#
                                       #{src 2937}#
                                       #{x 3030}#
                                       #{r 2939}#
                                       #{maps 2940}#
                                       #{ellipsis? 2941}#
                                       #{mod 2942}#))
                                   (lambda (#{x 3032}# #{maps 3033}#)
                                     (call-with-values
                                       (lambda ()
                                         (#{gen-syntax 2922}#
                                           #{src 2937}#
                                           #{y 3031}#
                                           #{r 2939}#
                                           #{maps 3033}#
                                           #{ellipsis? 2941}#
                                           #{mod 2942}#))
                                       (lambda (#{y 3036}# #{maps 3037}#)
                                         (values
                                           (#{gen-cons 2930}#
                                             #{x 3032}#
                                             #{y 3036}#)
                                           #{maps 3037}#))))))
                               #{tmp 3027}#)
                             (let ((#{tmp 3040}#
                                     ($sc-dispatch
                                       #{tmp 2964}#
                                       '#(vector (any . each-any)))))
                               (if #{tmp 3040}#
                                 (@apply
                                   (lambda (#{e1 3043}# #{e2 3044}#)
                                     (call-with-values
                                       (lambda ()
                                         (#{gen-syntax 2922}#
                                           #{src 2937}#
                                           (cons #{e1 3043}# #{e2 3044}#)
                                           #{r 2939}#
                                           #{maps 2940}#
                                           #{ellipsis? 2941}#
                                           #{mod 2942}#))
                                       (lambda (#{e 3046}# #{maps 3047}#)
                                         (values
                                           (#{gen-vector 2934}# #{e 3046}#)
                                           #{maps 3047}#))))
                                   #{tmp 3040}#)
                                 (let ((#{_ 3051}# #{tmp 2964}#))
                                   (values
                                     (list 'quote #{e 2938}#)
                                     #{maps 2940}#))))))))))))))
         (#{gen-ref 2924}#
           (lambda (#{src 3053}#
                    #{var 3054}#
                    #{level 3055}#
                    #{maps 3056}#)
             (if (= #{level 3055}# 0)
               (values #{var 3054}# #{maps 3056}#)
               (if (null? #{maps 3056}#)
                 (syntax-violation
                   'syntax
                   "missing ellipsis"
                   #{src 3053}#)
                 (call-with-values
                   (lambda ()
                     (#{gen-ref 2924}#
                       #{src 3053}#
                       #{var 3054}#
                       (#{1-}# #{level 3055}#)
                       (cdr #{maps 3056}#)))
                   (lambda (#{outer-var 3063}# #{outer-maps 3064}#)
                     (let ((#{b 3068}#
                             (assq #{outer-var 3063}# (car #{maps 3056}#))))
                       (if #{b 3068}#
                         (values (cdr #{b 3068}#) #{maps 3056}#)
                         (let ((#{inner-var 3070}# (#{gen-var 451}# 'tmp)))
                           (values
                             #{inner-var 3070}#
                             (cons (cons (cons #{outer-var 3063}#
                                               #{inner-var 3070}#)
                                         (car #{maps 3056}#))
                                   #{outer-maps 3064}#)))))))))))
         (#{gen-mappend 2926}#
           (lambda (#{e 3071}# #{map-env 3072}#)
             (list 'apply
                   '(primitive append)
                   (#{gen-map 2928}# #{e 3071}# #{map-env 3072}#))))
         (#{gen-map 2928}#
           (lambda (#{e 3076}# #{map-env 3077}#)
             (let ((#{formals 3082}# (map cdr #{map-env 3077}#))
                   (#{actuals 3083}#
                     (map (lambda (#{x 3084}#)
                            (list 'ref (car #{x 3084}#)))
                          #{map-env 3077}#)))
               (if (eq? (car #{e 3076}#) 'ref)
                 (car #{actuals 3083}#)
                 (if (and-map
                       (lambda (#{x 3091}#)
                         (if (eq? (car #{x 3091}#) 'ref)
                           (memq (car (cdr #{x 3091}#)) #{formals 3082}#)
                           #f))
                       (cdr #{e 3076}#))
                   (cons 'map
                         (cons (list 'primitive (car #{e 3076}#))
                               (map (let ((#{r 3097}#
                                            (map cons
                                                 #{formals 3082}#
                                                 #{actuals 3083}#)))
                                      (lambda (#{x 3098}#)
                                        (cdr (assq (car (cdr #{x 3098}#))
                                                   #{r 3097}#))))
                                    (cdr #{e 3076}#))))
                   (cons 'map
                         (cons (list 'lambda #{formals 3082}# #{e 3076}#)
                               #{actuals 3083}#)))))))
         (#{gen-cons 2930}#
           (lambda (#{x 3102}# #{y 3103}#)
             (let ((#{atom-key 3108}# (car #{y 3103}#)))
               (if (memv #{atom-key 3108}# '(quote))
                 (if (eq? (car #{x 3102}#) 'quote)
                   (list 'quote
                         (cons (car (cdr #{x 3102}#))
                               (car (cdr #{y 3103}#))))
                   (if (eq? (car (cdr #{y 3103}#)) '())
                     (list 'list #{x 3102}#)
                     (list 'cons #{x 3102}# #{y 3103}#)))
                 (if (memv #{atom-key 3108}# '(list))
                   (cons 'list (cons #{x 3102}# (cdr #{y 3103}#)))
                   (list 'cons #{x 3102}# #{y 3103}#))))))
         (#{gen-append 2932}#
           (lambda (#{x 3117}# #{y 3118}#)
             (if (equal? #{y 3118}# ''())
               #{x 3117}#
               (list 'append #{x 3117}# #{y 3118}#))))
         (#{gen-vector 2934}#
           (lambda (#{x 3122}#)
             (if (eq? (car #{x 3122}#) 'list)
               (cons 'vector (cdr #{x 3122}#))
               (if (eq? (car #{x 3122}#) 'quote)
                 (list 'quote
                       (list->vector (car (cdr #{x 3122}#))))
                 (list 'list->vector #{x 3122}#)))))
         (#{regen 2936}#
           (lambda (#{x 3132}#)
             (let ((#{atom-key 3136}# (car #{x 3132}#)))
               (if (memv #{atom-key 3136}# '(ref))
                 (#{build-lexical-reference 273}#
                   'value
                   #f
                   (car (cdr #{x 3132}#))
                   (car (cdr #{x 3132}#)))
                 (if (memv #{atom-key 3136}# '(primitive))
                   (#{build-primref 293}# #f (car (cdr #{x 3132}#)))
                   (if (memv #{atom-key 3136}# '(quote))
                     (#{build-data 295}# #f (car (cdr #{x 3132}#)))
                     (if (memv #{atom-key 3136}# '(lambda))
                       (if (list? (car (cdr #{x 3132}#)))
                         (#{build-simple-lambda 285}#
                           #f
                           (car (cdr #{x 3132}#))
                           #f
                           (car (cdr #{x 3132}#))
                           '()
                           (#{regen 2936}# (car (cdr (cdr #{x 3132}#)))))
                         (error "how did we get here" #{x 3132}#))
                       (#{build-primcall 291}#
                         #f
                         (car #{x 3132}#)
                         (map #{regen 2936}# (cdr #{x 3132}#)))))))))))
        (lambda (#{e 3147}#
                 #{r 3148}#
                 #{w 3149}#
                 #{s 3150}#
                 #{mod 3151}#)
          (let ((#{e 3158}#
                  (#{source-wrap 411}#
                    #{e 3147}#
                    #{w 3149}#
                    #{s 3150}#
                    #{mod 3151}#)))
            (let ((#{tmp 3159}# #{e 3158}#))
              (let ((#{tmp 3160}#
                      ($sc-dispatch #{tmp 3159}# '(_ any))))
                (if #{tmp 3160}#
                  (@apply
                    (lambda (#{x 3162}#)
                      (call-with-values
                        (lambda ()
                          (#{gen-syntax 2922}#
                            #{e 3158}#
                            #{x 3162}#
                            #{r 3148}#
                            '()
                            #{ellipsis? 439}#
                            #{mod 3151}#))
                        (lambda (#{e 3163}# #{maps 3164}#)
                          (#{regen 2936}# #{e 3163}#))))
                    #{tmp 3160}#)
                  (let ((#{_ 3168}# #{tmp 3159}#))
                    (syntax-violation
                      'syntax
                      "bad `syntax' form"
                      #{e 3158}#)))))))))
    (#{global-extend 339}#
      'core
      'lambda
      (lambda (#{e 3169}#
               #{r 3170}#
               #{w 3171}#
               #{s 3172}#
               #{mod 3173}#)
        (let ((#{tmp 3179}# #{e 3169}#))
          (let ((#{tmp 3180}#
                  ($sc-dispatch
                    #{tmp 3179}#
                    '(_ any any . each-any))))
            (if #{tmp 3180}#
              (@apply
                (lambda (#{args 3184}# #{e1 3185}# #{e2 3186}#)
                  (call-with-values
                    (lambda ()
                      (#{lambda-formals 441}# #{args 3184}#))
                    (lambda (#{req 3187}#
                             #{opt 3188}#
                             #{rest 3189}#
                             #{kw 3190}#)
                      (letrec*
                        ((#{lp 3198}#
                           (lambda (#{body 3199}# #{meta 3200}#)
                             (let ((#{tmp 3202}# #{body 3199}#))
                               (let ((#{tmp 3203}#
                                       ($sc-dispatch
                                         #{tmp 3202}#
                                         '(any any . each-any))))
                                 (if (if #{tmp 3203}#
                                       (@apply
                                         (lambda (#{docstring 3207}#
                                                  #{e1 3208}#
                                                  #{e2 3209}#)
                                           (string?
                                             (syntax->datum
                                               #{docstring 3207}#)))
                                         #{tmp 3203}#)
                                       #f)
                                   (@apply
                                     (lambda (#{docstring 3213}#
                                              #{e1 3214}#
                                              #{e2 3215}#)
                                       (#{lp 3198}#
                                         (cons #{e1 3214}# #{e2 3215}#)
                                         (append
                                           #{meta 3200}#
                                           (list (cons 'documentation
                                                       (syntax->datum
                                                         #{docstring 3213}#))))))
                                     #{tmp 3203}#)
                                   (let ((#{tmp 3218}#
                                           ($sc-dispatch
                                             #{tmp 3202}#
                                             '(#(vector #(each (any . any)))
                                               any
                                               .
                                               each-any))))
                                     (if #{tmp 3218}#
                                       (@apply
                                         (lambda (#{k 3223}#
                                                  #{v 3224}#
                                                  #{e1 3225}#
                                                  #{e2 3226}#)
                                           (#{lp 3198}#
                                             (cons #{e1 3225}# #{e2 3226}#)
                                             (append
                                               #{meta 3200}#
                                               (syntax->datum
                                                 (map cons
                                                      #{k 3223}#
                                                      #{v 3224}#)))))
                                         #{tmp 3218}#)
                                       (let ((#{_ 3231}# #{tmp 3202}#))
                                         (#{chi-simple-lambda 443}#
                                           #{e 3169}#
                                           #{r 3170}#
                                           #{w 3171}#
                                           #{s 3172}#
                                           #{mod 3173}#
                                           #{req 3187}#
                                           #{rest 3189}#
                                           #{meta 3200}#
                                           #{body 3199}#))))))))))
                        (#{lp 3198}# (cons #{e1 3185}# #{e2 3186}#) '())))))
                #{tmp 3180}#)
              (let ((#{_ 3233}# #{tmp 3179}#))
                (syntax-violation
                  'lambda
                  "bad lambda"
                  #{e 3169}#)))))))
    (#{global-extend 339}#
      'core
      'lambda*
      (lambda (#{e 3234}#
               #{r 3235}#
               #{w 3236}#
               #{s 3237}#
               #{mod 3238}#)
        (let ((#{tmp 3244}# #{e 3234}#))
          (let ((#{tmp 3245}#
                  ($sc-dispatch
                    #{tmp 3244}#
                    '(_ any any . each-any))))
            (if #{tmp 3245}#
              (@apply
                (lambda (#{args 3249}# #{e1 3250}# #{e2 3251}#)
                  (call-with-values
                    (lambda ()
                      (#{chi-lambda-case 447}#
                        #{e 3234}#
                        #{r 3235}#
                        #{w 3236}#
                        #{s 3237}#
                        #{mod 3238}#
                        #{lambda*-formals 445}#
                        (list (cons #{args 3249}#
                                    (cons #{e1 3250}# #{e2 3251}#)))))
                    (lambda (#{meta 3253}# #{lcase 3254}#)
                      (#{build-case-lambda 287}#
                        #{s 3237}#
                        #{meta 3253}#
                        #{lcase 3254}#))))
                #{tmp 3245}#)
              (let ((#{_ 3258}# #{tmp 3244}#))
                (syntax-violation
                  'lambda
                  "bad lambda*"
                  #{e 3234}#)))))))
    (#{global-extend 339}#
      'core
      'case-lambda
      (lambda (#{e 3259}#
               #{r 3260}#
               #{w 3261}#
               #{s 3262}#
               #{mod 3263}#)
        (let ((#{tmp 3269}# #{e 3259}#))
          (let ((#{tmp 3270}#
                  ($sc-dispatch
                    #{tmp 3269}#
                    '(_ (any any . each-any)
                        .
                        #(each (any any . each-any))))))
            (if #{tmp 3270}#
              (@apply
                (lambda (#{args 3277}#
                         #{e1 3278}#
                         #{e2 3279}#
                         #{args* 3280}#
                         #{e1* 3281}#
                         #{e2* 3282}#)
                  (call-with-values
                    (lambda ()
                      (#{chi-lambda-case 447}#
                        #{e 3259}#
                        #{r 3260}#
                        #{w 3261}#
                        #{s 3262}#
                        #{mod 3263}#
                        #{lambda-formals 441}#
                        (cons (cons #{args 3277}#
                                    (cons #{e1 3278}# #{e2 3279}#))
                              (map (lambda (#{tmp 3286}#
                                            #{tmp 3285}#
                                            #{tmp 3284}#)
                                     (cons #{tmp 3284}#
                                           (cons #{tmp 3285}# #{tmp 3286}#)))
                                   #{e2* 3282}#
                                   #{e1* 3281}#
                                   #{args* 3280}#))))
                    (lambda (#{meta 3288}# #{lcase 3289}#)
                      (#{build-case-lambda 287}#
                        #{s 3262}#
                        #{meta 3288}#
                        #{lcase 3289}#))))
                #{tmp 3270}#)
              (let ((#{_ 3293}# #{tmp 3269}#))
                (syntax-violation
                  'case-lambda
                  "bad case-lambda"
                  #{e 3259}#)))))))
    (#{global-extend 339}#
      'core
      'case-lambda*
      (lambda (#{e 3294}#
               #{r 3295}#
               #{w 3296}#
               #{s 3297}#
               #{mod 3298}#)
        (let ((#{tmp 3304}# #{e 3294}#))
          (let ((#{tmp 3305}#
                  ($sc-dispatch
                    #{tmp 3304}#
                    '(_ (any any . each-any)
                        .
                        #(each (any any . each-any))))))
            (if #{tmp 3305}#
              (@apply
                (lambda (#{args 3312}#
                         #{e1 3313}#
                         #{e2 3314}#
                         #{args* 3315}#
                         #{e1* 3316}#
                         #{e2* 3317}#)
                  (call-with-values
                    (lambda ()
                      (#{chi-lambda-case 447}#
                        #{e 3294}#
                        #{r 3295}#
                        #{w 3296}#
                        #{s 3297}#
                        #{mod 3298}#
                        #{lambda*-formals 445}#
                        (cons (cons #{args 3312}#
                                    (cons #{e1 3313}# #{e2 3314}#))
                              (map (lambda (#{tmp 3321}#
                                            #{tmp 3320}#
                                            #{tmp 3319}#)
                                     (cons #{tmp 3319}#
                                           (cons #{tmp 3320}# #{tmp 3321}#)))
                                   #{e2* 3317}#
                                   #{e1* 3316}#
                                   #{args* 3315}#))))
                    (lambda (#{meta 3323}# #{lcase 3324}#)
                      (#{build-case-lambda 287}#
                        #{s 3297}#
                        #{meta 3323}#
                        #{lcase 3324}#))))
                #{tmp 3305}#)
              (let ((#{_ 3328}# #{tmp 3304}#))
                (syntax-violation
                  'case-lambda
                  "bad case-lambda*"
                  #{e 3294}#)))))))
    (#{global-extend 339}#
      'core
      'let
      (letrec*
        ((#{chi-let 3330}#
           (lambda (#{e 3331}#
                    #{r 3332}#
                    #{w 3333}#
                    #{s 3334}#
                    #{mod 3335}#
                    #{constructor 3336}#
                    #{ids 3337}#
                    #{vals 3338}#
                    #{exps 3339}#)
             (if (not (#{valid-bound-ids? 403}# #{ids 3337}#))
               (syntax-violation
                 'let
                 "duplicate bound variable"
                 #{e 3331}#)
               (let ((#{labels 3351}#
                       (#{gen-labels 358}# #{ids 3337}#))
                     (#{new-vars 3352}#
                       (map #{gen-var 451}# #{ids 3337}#)))
                 (let ((#{nw 3355}#
                         (#{make-binding-wrap 387}#
                           #{ids 3337}#
                           #{labels 3351}#
                           #{w 3333}#))
                       (#{nr 3356}#
                         (#{extend-var-env 333}#
                           #{labels 3351}#
                           #{new-vars 3352}#
                           #{r 3332}#)))
                   (#{constructor 3336}#
                     #{s 3334}#
                     (map syntax->datum #{ids 3337}#)
                     #{new-vars 3352}#
                     (map (lambda (#{x 3357}#)
                            (#{chi 423}#
                              #{x 3357}#
                              #{r 3332}#
                              #{w 3333}#
                              #{mod 3335}#))
                          #{vals 3338}#)
                     (#{chi-body 431}#
                       #{exps 3339}#
                       (#{source-wrap 411}#
                         #{e 3331}#
                         #{nw 3355}#
                         #{s 3334}#
                         #{mod 3335}#)
                       #{nr 3356}#
                       #{nw 3355}#
                       #{mod 3335}#))))))))
        (lambda (#{e 3359}#
                 #{r 3360}#
                 #{w 3361}#
                 #{s 3362}#
                 #{mod 3363}#)
          (let ((#{tmp 3369}# #{e 3359}#))
            (let ((#{tmp 3370}#
                    ($sc-dispatch
                      #{tmp 3369}#
                      '(_ #(each (any any)) any . each-any))))
              (if (if #{tmp 3370}#
                    (@apply
                      (lambda (#{id 3375}#
                               #{val 3376}#
                               #{e1 3377}#
                               #{e2 3378}#)
                        (and-map #{id? 343}# #{id 3375}#))
                      #{tmp 3370}#)
                    #f)
                (@apply
                  (lambda (#{id 3384}#
                           #{val 3385}#
                           #{e1 3386}#
                           #{e2 3387}#)
                    (#{chi-let 3330}#
                      #{e 3359}#
                      #{r 3360}#
                      #{w 3361}#
                      #{s 3362}#
                      #{mod 3363}#
                      #{build-let 299}#
                      #{id 3384}#
                      #{val 3385}#
                      (cons #{e1 3386}# #{e2 3387}#)))
                  #{tmp 3370}#)
                (let ((#{tmp 3391}#
                        ($sc-dispatch
                          #{tmp 3369}#
                          '(_ any #(each (any any)) any . each-any))))
                  (if (if #{tmp 3391}#
                        (@apply
                          (lambda (#{f 3397}#
                                   #{id 3398}#
                                   #{val 3399}#
                                   #{e1 3400}#
                                   #{e2 3401}#)
                            (if (#{id? 343}# #{f 3397}#)
                              (and-map #{id? 343}# #{id 3398}#)
                              #f))
                          #{tmp 3391}#)
                        #f)
                    (@apply
                      (lambda (#{f 3410}#
                               #{id 3411}#
                               #{val 3412}#
                               #{e1 3413}#
                               #{e2 3414}#)
                        (#{chi-let 3330}#
                          #{e 3359}#
                          #{r 3360}#
                          #{w 3361}#
                          #{s 3362}#
                          #{mod 3363}#
                          #{build-named-let 301}#
                          (cons #{f 3410}# #{id 3411}#)
                          #{val 3412}#
                          (cons #{e1 3413}# #{e2 3414}#)))
                      #{tmp 3391}#)
                    (let ((#{_ 3419}# #{tmp 3369}#))
                      (syntax-violation
                        'let
                        "bad let"
                        (#{source-wrap 411}#
                          #{e 3359}#
                          #{w 3361}#
                          #{s 3362}#
                          #{mod 3363}#)))))))))))
    (#{global-extend 339}#
      'core
      'letrec
      (lambda (#{e 3420}#
               #{r 3421}#
               #{w 3422}#
               #{s 3423}#
               #{mod 3424}#)
        (let ((#{tmp 3430}# #{e 3420}#))
          (let ((#{tmp 3431}#
                  ($sc-dispatch
                    #{tmp 3430}#
                    '(_ #(each (any any)) any . each-any))))
            (if (if #{tmp 3431}#
                  (@apply
                    (lambda (#{id 3436}#
                             #{val 3437}#
                             #{e1 3438}#
                             #{e2 3439}#)
                      (and-map #{id? 343}# #{id 3436}#))
                    #{tmp 3431}#)
                  #f)
              (@apply
                (lambda (#{id 3445}#
                         #{val 3446}#
                         #{e1 3447}#
                         #{e2 3448}#)
                  (let ((#{ids 3450}# #{id 3445}#))
                    (if (not (#{valid-bound-ids? 403}# #{ids 3450}#))
                      (syntax-violation
                        'letrec
                        "duplicate bound variable"
                        #{e 3420}#)
                      (let ((#{labels 3454}#
                              (#{gen-labels 358}# #{ids 3450}#))
                            (#{new-vars 3455}#
                              (map #{gen-var 451}# #{ids 3450}#)))
                        (let ((#{w 3458}#
                                (#{make-binding-wrap 387}#
                                  #{ids 3450}#
                                  #{labels 3454}#
                                  #{w 3422}#))
                              (#{r 3459}#
                                (#{extend-var-env 333}#
                                  #{labels 3454}#
                                  #{new-vars 3455}#
                                  #{r 3421}#)))
                          (#{build-letrec 303}#
                            #{s 3423}#
                            #f
                            (map syntax->datum #{ids 3450}#)
                            #{new-vars 3455}#
                            (map (lambda (#{x 3460}#)
                                   (#{chi 423}#
                                     #{x 3460}#
                                     #{r 3459}#
                                     #{w 3458}#
                                     #{mod 3424}#))
                                 #{val 3446}#)
                            (#{chi-body 431}#
                              (cons #{e1 3447}# #{e2 3448}#)
                              (#{source-wrap 411}#
                                #{e 3420}#
                                #{w 3458}#
                                #{s 3423}#
                                #{mod 3424}#)
                              #{r 3459}#
                              #{w 3458}#
                              #{mod 3424}#)))))))
                #{tmp 3431}#)
              (let ((#{_ 3465}# #{tmp 3430}#))
                (syntax-violation
                  'letrec
                  "bad letrec"
                  (#{source-wrap 411}#
                    #{e 3420}#
                    #{w 3422}#
                    #{s 3423}#
                    #{mod 3424}#))))))))
    (#{global-extend 339}#
      'core
      'letrec*
      (lambda (#{e 3466}#
               #{r 3467}#
               #{w 3468}#
               #{s 3469}#
               #{mod 3470}#)
        (let ((#{tmp 3476}# #{e 3466}#))
          (let ((#{tmp 3477}#
                  ($sc-dispatch
                    #{tmp 3476}#
                    '(_ #(each (any any)) any . each-any))))
            (if (if #{tmp 3477}#
                  (@apply
                    (lambda (#{id 3482}#
                             #{val 3483}#
                             #{e1 3484}#
                             #{e2 3485}#)
                      (and-map #{id? 343}# #{id 3482}#))
                    #{tmp 3477}#)
                  #f)
              (@apply
                (lambda (#{id 3491}#
                         #{val 3492}#
                         #{e1 3493}#
                         #{e2 3494}#)
                  (let ((#{ids 3496}# #{id 3491}#))
                    (if (not (#{valid-bound-ids? 403}# #{ids 3496}#))
                      (syntax-violation
                        'letrec*
                        "duplicate bound variable"
                        #{e 3466}#)
                      (let ((#{labels 3500}#
                              (#{gen-labels 358}# #{ids 3496}#))
                            (#{new-vars 3501}#
                              (map #{gen-var 451}# #{ids 3496}#)))
                        (let ((#{w 3504}#
                                (#{make-binding-wrap 387}#
                                  #{ids 3496}#
                                  #{labels 3500}#
                                  #{w 3468}#))
                              (#{r 3505}#
                                (#{extend-var-env 333}#
                                  #{labels 3500}#
                                  #{new-vars 3501}#
                                  #{r 3467}#)))
                          (#{build-letrec 303}#
                            #{s 3469}#
                            #t
                            (map syntax->datum #{ids 3496}#)
                            #{new-vars 3501}#
                            (map (lambda (#{x 3506}#)
                                   (#{chi 423}#
                                     #{x 3506}#
                                     #{r 3505}#
                                     #{w 3504}#
                                     #{mod 3470}#))
                                 #{val 3492}#)
                            (#{chi-body 431}#
                              (cons #{e1 3493}# #{e2 3494}#)
                              (#{source-wrap 411}#
                                #{e 3466}#
                                #{w 3504}#
                                #{s 3469}#
                                #{mod 3470}#)
                              #{r 3505}#
                              #{w 3504}#
                              #{mod 3470}#)))))))
                #{tmp 3477}#)
              (let ((#{_ 3511}# #{tmp 3476}#))
                (syntax-violation
                  'letrec*
                  "bad letrec*"
                  (#{source-wrap 411}#
                    #{e 3466}#
                    #{w 3468}#
                    #{s 3469}#
                    #{mod 3470}#))))))))
    (#{global-extend 339}#
      'core
      'set!
      (lambda (#{e 3512}#
               #{r 3513}#
               #{w 3514}#
               #{s 3515}#
               #{mod 3516}#)
        (let ((#{tmp 3522}# #{e 3512}#))
          (let ((#{tmp 3523}#
                  ($sc-dispatch #{tmp 3522}# '(_ any any))))
            (if (if #{tmp 3523}#
                  (@apply
                    (lambda (#{id 3526}# #{val 3527}#)
                      (#{id? 343}# #{id 3526}#))
                    #{tmp 3523}#)
                  #f)
              (@apply
                (lambda (#{id 3530}# #{val 3531}#)
                  (let ((#{n 3534}#
                          (#{id-var-name 397}# #{id 3530}# #{w 3514}#))
                        (#{id-mod 3535}#
                          (if (#{syntax-object? 309}# #{id 3530}#)
                            (#{syntax-object-module 315}# #{id 3530}#)
                            #{mod 3516}#)))
                    (let ((#{b 3537}#
                            (#{lookup 337}#
                              #{n 3534}#
                              #{r 3513}#
                              #{id-mod 3535}#)))
                      (let ((#{atom-key 3540}# (car #{b 3537}#)))
                        (if (memv #{atom-key 3540}# '(lexical))
                          (#{build-lexical-assignment 275}#
                            #{s 3515}#
                            (syntax->datum #{id 3530}#)
                            (cdr #{b 3537}#)
                            (#{chi 423}#
                              #{val 3531}#
                              #{r 3513}#
                              #{w 3514}#
                              #{mod 3516}#))
                          (if (memv #{atom-key 3540}# '(global))
                            (#{build-global-assignment 281}#
                              #{s 3515}#
                              #{n 3534}#
                              (#{chi 423}#
                                #{val 3531}#
                                #{r 3513}#
                                #{w 3514}#
                                #{mod 3516}#)
                              #{id-mod 3535}#)
                            (if (memv #{atom-key 3540}# '(macro))
                              (let ((#{p 3547}# (cdr #{b 3537}#)))
                                (if (procedure-property
                                      #{p 3547}#
                                      'variable-transformer)
                                  (#{chi 423}#
                                    (#{chi-macro 429}#
                                      #{p 3547}#
                                      #{e 3512}#
                                      #{r 3513}#
                                      #{w 3514}#
                                      #{s 3515}#
                                      #f
                                      #{mod 3516}#)
                                    #{r 3513}#
                                    '(())
                                    #{mod 3516}#)
                                  (syntax-violation
                                    'set!
                                    "not a variable transformer"
                                    (#{wrap 409}#
                                      #{e 3512}#
                                      #{w 3514}#
                                      #{mod 3516}#)
                                    (#{wrap 409}#
                                      #{id 3530}#
                                      #{w 3514}#
                                      #{id-mod 3535}#))))
                              (if (memv #{atom-key 3540}# '(displaced-lexical))
                                (syntax-violation
                                  'set!
                                  "identifier out of context"
                                  (#{wrap 409}#
                                    #{id 3530}#
                                    #{w 3514}#
                                    #{mod 3516}#))
                                (syntax-violation
                                  'set!
                                  "bad set!"
                                  (#{source-wrap 411}#
                                    #{e 3512}#
                                    #{w 3514}#
                                    #{s 3515}#
                                    #{mod 3516}#))))))))))
                #{tmp 3523}#)
              (let ((#{tmp 3552}#
                      ($sc-dispatch
                        #{tmp 3522}#
                        '(_ (any . each-any) any))))
                (if #{tmp 3552}#
                  (@apply
                    (lambda (#{head 3556}# #{tail 3557}# #{val 3558}#)
                      (call-with-values
                        (lambda ()
                          (#{syntax-type 421}#
                            #{head 3556}#
                            #{r 3513}#
                            '(())
                            #f
                            #f
                            #{mod 3516}#
                            #t))
                        (lambda (#{type 3561}#
                                 #{value 3562}#
                                 #{ee 3563}#
                                 #{ww 3564}#
                                 #{ss 3565}#
                                 #{modmod 3566}#)
                          (if (memv #{type 3561}# '(module-ref))
                            (let ((#{val 3575}#
                                    (#{chi 423}#
                                      #{val 3558}#
                                      #{r 3513}#
                                      #{w 3514}#
                                      #{mod 3516}#)))
                              (call-with-values
                                (lambda ()
                                  (#{value 3562}#
                                    (cons #{head 3556}# #{tail 3557}#)
                                    #{r 3513}#
                                    #{w 3514}#))
                                (lambda (#{e 3577}#
                                         #{r 3578}#
                                         #{w 3579}#
                                         #{s* 3580}#
                                         #{mod 3581}#)
                                  (let ((#{tmp 3587}# #{e 3577}#))
                                    (let ((#{tmp 3588}# (list #{tmp 3587}#)))
                                      (if (if #{tmp 3588}#
                                            (@apply
                                              (lambda (#{e 3590}#)
                                                (#{id? 343}# #{e 3590}#))
                                              #{tmp 3588}#)
                                            #f)
                                        (@apply
                                          (lambda (#{e 3592}#)
                                            (#{build-global-assignment 281}#
                                              #{s 3515}#
                                              (syntax->datum #{e 3592}#)
                                              #{val 3575}#
                                              #{mod 3581}#))
                                          #{tmp 3588}#)
                                        (syntax-violation
                                          #f
                                          "source expression failed to match any pattern"
                                          #{tmp 3587}#)))))))
                            (#{build-call 267}#
                              #{s 3515}#
                              (#{chi 423}#
                                (list '#(syntax-object
                                         setter
                                         ((top)
                                          #(ribcage () () ())
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(type value ee ww ss modmod)
                                            #((top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top))
                                            #("i3567"
                                              "i3568"
                                              "i3569"
                                              "i3570"
                                              "i3571"
                                              "i3572"))
                                          #(ribcage
                                            #(head tail val)
                                            #((top) (top) (top))
                                            #("i3553" "i3554" "i3555"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(e r w s mod)
                                            #((top) (top) (top) (top) (top))
                                            #("i3517"
                                              "i3518"
                                              "i3519"
                                              "i3520"
                                              "i3521"))
                                          #(ribcage
                                            (lambda-var-list
                                              gen-var
                                              strip
                                              chi-lambda-case
                                              lambda*-formals
                                              chi-simple-lambda
                                              lambda-formals
                                              ellipsis?
                                              chi-void
                                              eval-local-transformer
                                              chi-local-syntax
                                              chi-body
                                              chi-macro
                                              chi-call
                                              chi-expr
                                              chi
                                              syntax-type
                                              chi-when-list
                                              chi-install-global
                                              chi-top-sequence
                                              chi-sequence
                                              source-wrap
                                              wrap
                                              bound-id-member?
                                              distinct-bound-ids?
                                              valid-bound-ids?
                                              bound-id=?
                                              free-id=?
                                              id-var-name
                                              same-marks?
                                              join-marks
                                              join-wraps
                                              smart-append
                                              make-binding-wrap
                                              extend-ribcage!
                                              make-empty-ribcage
                                              new-mark
                                              anti-mark
                                              the-anti-mark
                                              top-marked?
                                              top-wrap
                                              empty-wrap
                                              set-ribcage-labels!
                                              set-ribcage-marks!
                                              set-ribcage-symnames!
                                              ribcage-labels
                                              ribcage-marks
                                              ribcage-symnames
                                              ribcage?
                                              make-ribcage
                                              gen-labels
                                              gen-label
                                              make-rename
                                              rename-marks
                                              rename-new
                                              rename-old
                                              subst-rename?
                                              wrap-subst
                                              wrap-marks
                                              make-wrap
                                              id-sym-name&marks
                                              id-sym-name
                                              id?
                                              nonsymbol-id?
                                              global-extend
                                              lookup
                                              macros-only-env
                                              extend-var-env
                                              extend-env
                                              null-env
                                              binding-value
                                              binding-type
                                              make-binding
                                              arg-check
                                              source-annotation
                                              no-source
                                              set-syntax-object-module!
                                              set-syntax-object-wrap!
                                              set-syntax-object-expression!
                                              syntax-object-module
                                              syntax-object-wrap
                                              syntax-object-expression
                                              syntax-object?
                                              make-syntax-object
                                              build-lexical-var
                                              build-letrec
                                              build-named-let
                                              build-let
                                              build-sequence
                                              build-data
                                              build-primref
                                              build-primcall
                                              build-lambda-case
                                              build-case-lambda
                                              build-simple-lambda
                                              build-global-definition
                                              build-global-assignment
                                              build-global-reference
                                              analyze-variable
                                              build-lexical-assignment
                                              build-lexical-reference
                                              build-dynlet
                                              build-conditional
                                              build-call
                                              build-void
                                              maybe-name-value!
                                              decorate-source
                                              get-global-definition-hook
                                              put-global-definition-hook
                                              gensym-hook
                                              local-eval-hook
                                              top-level-eval-hook
                                              fx<
                                              fx=
                                              fx-
                                              fx+
                                              set-lambda-meta!
                                              lambda-meta
                                              lambda?
                                              make-dynlet
                                              make-letrec
                                              make-let
                                              make-lambda-case
                                              make-lambda
                                              make-seq
                                              make-primcall
                                              make-call
                                              make-conditional
                                              make-toplevel-define
                                              make-toplevel-set
                                              make-toplevel-ref
                                              make-module-set
                                              make-module-ref
                                              make-lexical-set
                                              make-lexical-ref
                                              make-primitive-ref
                                              make-const
                                              make-void)
                                            ((top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top))
                                            ("i452"
                                             "i450"
                                             "i448"
                                             "i446"
                                             "i444"
                                             "i442"
                                             "i440"
                                             "i438"
                                             "i436"
                                             "i434"
                                             "i432"
                                             "i430"
                                             "i428"
                                             "i426"
                                             "i424"
                                             "i422"
                                             "i420"
                                             "i418"
                                             "i416"
                                             "i414"
                                             "i412"
                                             "i410"
                                             "i408"
                                             "i406"
                                             "i404"
                                             "i402"
                                             "i400"
                                             "i398"
                                             "i396"
                                             "i394"
                                             "i392"
                                             "i390"
                                             "i388"
                                             "i386"
                                             "i384"
                                             "i383"
                                             "i382"
                                             "i380"
                                             "i379"
                                             "i378"
                                             "i377"
                                             "i376"
                                             "i374"
                                             "i372"
                                             "i370"
                                             "i368"
                                             "i366"
                                             "i364"
                                             "i362"
                                             "i360"
                                             "i357"
                                             "i355"
                                             "i354"
                                             "i353"
                                             "i352"
                                             "i351"
                                             "i350"
                                             "i349"
                                             "i348"
                                             "i347"
                                             "i345"
                                             "i344"
                                             "i342"
                                             "i340"
                                             "i338"
                                             "i336"
                                             "i334"
                                             "i332"
                                             "i330"
                                             "i329"
                                             "i328"
                                             "i327"
                                             "i326"
                                             "i325"
                                             "i323"
                                             "i322"
                                             "i320"
                                             "i318"
                                             "i316"
                                             "i314"
                                             "i312"
                                             "i310"
                                             "i308"
                                             "i306"
                                             "i304"
                                             "i302"
                                             "i300"
                                             "i298"
                                             "i296"
                                             "i294"
                                             "i292"
                                             "i290"
                                             "i288"
                                             "i286"
                                             "i284"
                                             "i282"
                                             "i280"
                                             "i278"
                                             "i276"
                                             "i274"
                                             "i272"
                                             "i270"
                                             "i268"
                                             "i266"
                                             "i264"
                                             "i262"
                                             "i260"
                                             "i258"
                                             "i256"
                                             "i255"
                                             "i253"
                                             "i251"
                                             "i250"
                                             "i249"
                                             "i248"
                                             "i247"
                                             "i245"
                                             "i243"
                                             "i241"
                                             "i238"
                                             "i236"
                                             "i234"
                                             "i232"
                                             "i230"
                                             "i228"
                                             "i226"
                                             "i224"
                                             "i222"
                                             "i220"
                                             "i218"
                                             "i216"
                                             "i214"
                                             "i212"
                                             "i210"
                                             "i208"
                                             "i206"
                                             "i204"
                                             "i202"))
                                          #(ribcage
                                            (define-structure
                                              define-expansion-accessors
                                              define-expansion-constructors)
                                            ((top) (top) (top))
                                            ("i40" "i39" "i38")))
                                         (hygiene guile))
                                      #{head 3556}#)
                                #{r 3513}#
                                #{w 3514}#
                                #{mod 3516}#)
                              (map (lambda (#{e 3594}#)
                                     (#{chi 423}#
                                       #{e 3594}#
                                       #{r 3513}#
                                       #{w 3514}#
                                       #{mod 3516}#))
                                   (append
                                     #{tail 3557}#
                                     (list #{val 3558}#))))))))
                    #{tmp 3552}#)
                  (let ((#{_ 3598}# #{tmp 3522}#))
                    (syntax-violation
                      'set!
                      "bad set!"
                      (#{source-wrap 411}#
                        #{e 3512}#
                        #{w 3514}#
                        #{s 3515}#
                        #{mod 3516}#))))))))))
    (#{global-extend 339}#
      'module-ref
      '@
      (lambda (#{e 3599}# #{r 3600}# #{w 3601}#)
        (let ((#{tmp 3605}# #{e 3599}#))
          (let ((#{tmp 3606}#
                  ($sc-dispatch #{tmp 3605}# '(_ each-any any))))
            (if (if #{tmp 3606}#
                  (@apply
                    (lambda (#{mod 3609}# #{id 3610}#)
                      (if (and-map #{id? 343}# #{mod 3609}#)
                        (#{id? 343}# #{id 3610}#)
                        #f))
                    #{tmp 3606}#)
                  #f)
              (@apply
                (lambda (#{mod 3616}# #{id 3617}#)
                  (values
                    (syntax->datum #{id 3617}#)
                    #{r 3600}#
                    #{w 3601}#
                    #f
                    (syntax->datum
                      (cons '#(syntax-object
                               public
                               ((top)
                                #(ribcage
                                  #(mod id)
                                  #((top) (top))
                                  #("i3614" "i3615"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(e r w)
                                  #((top) (top) (top))
                                  #("i3602" "i3603" "i3604"))
                                #(ribcage
                                  (lambda-var-list
                                    gen-var
                                    strip
                                    chi-lambda-case
                                    lambda*-formals
                                    chi-simple-lambda
                                    lambda-formals
                                    ellipsis?
                                    chi-void
                                    eval-local-transformer
                                    chi-local-syntax
                                    chi-body
                                    chi-macro
                                    chi-call
                                    chi-expr
                                    chi
                                    syntax-type
                                    chi-when-list
                                    chi-install-global
                                    chi-top-sequence
                                    chi-sequence
                                    source-wrap
                                    wrap
                                    bound-id-member?
                                    distinct-bound-ids?
                                    valid-bound-ids?
                                    bound-id=?
                                    free-id=?
                                    id-var-name
                                    same-marks?
                                    join-marks
                                    join-wraps
                                    smart-append
                                    make-binding-wrap
                                    extend-ribcage!
                                    make-empty-ribcage
                                    new-mark
                                    anti-mark
                                    the-anti-mark
                                    top-marked?
                                    top-wrap
                                    empty-wrap
                                    set-ribcage-labels!
                                    set-ribcage-marks!
                                    set-ribcage-symnames!
                                    ribcage-labels
                                    ribcage-marks
                                    ribcage-symnames
                                    ribcage?
                                    make-ribcage
                                    gen-labels
                                    gen-label
                                    make-rename
                                    rename-marks
                                    rename-new
                                    rename-old
                                    subst-rename?
                                    wrap-subst
                                    wrap-marks
                                    make-wrap
                                    id-sym-name&marks
                                    id-sym-name
                                    id?
                                    nonsymbol-id?
                                    global-extend
                                    lookup
                                    macros-only-env
                                    extend-var-env
                                    extend-env
                                    null-env
                                    binding-value
                                    binding-type
                                    make-binding
                                    arg-check
                                    source-annotation
                                    no-source
                                    set-syntax-object-module!
                                    set-syntax-object-wrap!
                                    set-syntax-object-expression!
                                    syntax-object-module
                                    syntax-object-wrap
                                    syntax-object-expression
                                    syntax-object?
                                    make-syntax-object
                                    build-lexical-var
                                    build-letrec
                                    build-named-let
                                    build-let
                                    build-sequence
                                    build-data
                                    build-primref
                                    build-primcall
                                    build-lambda-case
                                    build-case-lambda
                                    build-simple-lambda
                                    build-global-definition
                                    build-global-assignment
                                    build-global-reference
                                    analyze-variable
                                    build-lexical-assignment
                                    build-lexical-reference
                                    build-dynlet
                                    build-conditional
                                    build-call
                                    build-void
                                    maybe-name-value!
                                    decorate-source
                                    get-global-definition-hook
                                    put-global-definition-hook
                                    gensym-hook
                                    local-eval-hook
                                    top-level-eval-hook
                                    fx<
                                    fx=
                                    fx-
                                    fx+
                                    set-lambda-meta!
                                    lambda-meta
                                    lambda?
                                    make-dynlet
                                    make-letrec
                                    make-let
                                    make-lambda-case
                                    make-lambda
                                    make-seq
                                    make-primcall
                                    make-call
                                    make-conditional
                                    make-toplevel-define
                                    make-toplevel-set
                                    make-toplevel-ref
                                    make-module-set
                                    make-module-ref
                                    make-lexical-set
                                    make-lexical-ref
                                    make-primitive-ref
                                    make-const
                                    make-void)
                                  ((top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top)
                                   (top))
                                  ("i452"
                                   "i450"
                                   "i448"
                                   "i446"
                                   "i444"
                                   "i442"
                                   "i440"
                                   "i438"
                                   "i436"
                                   "i434"
                                   "i432"
                                   "i430"
                                   "i428"
                                   "i426"
                                   "i424"
                                   "i422"
                                   "i420"
                                   "i418"
                                   "i416"
                                   "i414"
                                   "i412"
                                   "i410"
                                   "i408"
                                   "i406"
                                   "i404"
                                   "i402"
                                   "i400"
                                   "i398"
                                   "i396"
                                   "i394"
                                   "i392"
                                   "i390"
                                   "i388"
                                   "i386"
                                   "i384"
                                   "i383"
                                   "i382"
                                   "i380"
                                   "i379"
                                   "i378"
                                   "i377"
                                   "i376"
                                   "i374"
                                   "i372"
                                   "i370"
                                   "i368"
                                   "i366"
                                   "i364"
                                   "i362"
                                   "i360"
                                   "i357"
                                   "i355"
                                   "i354"
                                   "i353"
                                   "i352"
                                   "i351"
                                   "i350"
                                   "i349"
                                   "i348"
                                   "i347"
                                   "i345"
                                   "i344"
                                   "i342"
                                   "i340"
                                   "i338"
                                   "i336"
                                   "i334"
                                   "i332"
                                   "i330"
                                   "i329"
                                   "i328"
                                   "i327"
                                   "i326"
                                   "i325"
                                   "i323"
                                   "i322"
                                   "i320"
                                   "i318"
                                   "i316"
                                   "i314"
                                   "i312"
                                   "i310"
                                   "i308"
                                   "i306"
                                   "i304"
                                   "i302"
                                   "i300"
                                   "i298"
                                   "i296"
                                   "i294"
                                   "i292"
                                   "i290"
                                   "i288"
                                   "i286"
                                   "i284"
                                   "i282"
                                   "i280"
                                   "i278"
                                   "i276"
                                   "i274"
                                   "i272"
                                   "i270"
                                   "i268"
                                   "i266"
                                   "i264"
                                   "i262"
                                   "i260"
                                   "i258"
                                   "i256"
                                   "i255"
                                   "i253"
                                   "i251"
                                   "i250"
                                   "i249"
                                   "i248"
                                   "i247"
                                   "i245"
                                   "i243"
                                   "i241"
                                   "i238"
                                   "i236"
                                   "i234"
                                   "i232"
                                   "i230"
                                   "i228"
                                   "i226"
                                   "i224"
                                   "i222"
                                   "i220"
                                   "i218"
                                   "i216"
                                   "i214"
                                   "i212"
                                   "i210"
                                   "i208"
                                   "i206"
                                   "i204"
                                   "i202"))
                                #(ribcage
                                  (define-structure
                                    define-expansion-accessors
                                    define-expansion-constructors)
                                  ((top) (top) (top))
                                  ("i40" "i39" "i38")))
                               (hygiene guile))
                            #{mod 3616}#))))
                #{tmp 3606}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 3605}#))))))
    (#{global-extend 339}#
      'module-ref
      '@@
      (lambda (#{e 3619}# #{r 3620}# #{w 3621}#)
        (letrec*
          ((#{remodulate 3626}#
             (lambda (#{x 3627}# #{mod 3628}#)
               (if (pair? #{x 3627}#)
                 (cons (#{remodulate 3626}#
                         (car #{x 3627}#)
                         #{mod 3628}#)
                       (#{remodulate 3626}#
                         (cdr #{x 3627}#)
                         #{mod 3628}#))
                 (if (#{syntax-object? 309}# #{x 3627}#)
                   (#{make-syntax-object 307}#
                     (#{remodulate 3626}#
                       (#{syntax-object-expression 311}# #{x 3627}#)
                       #{mod 3628}#)
                     (#{syntax-object-wrap 313}# #{x 3627}#)
                     #{mod 3628}#)
                   (if (vector? #{x 3627}#)
                     (let ((#{n 3639}# (vector-length #{x 3627}#)))
                       (let ((#{v 3641}# (make-vector #{n 3639}#)))
                         (letrec*
                           ((#{loop 3644}#
                              (lambda (#{i 3645}#)
                                (if (= #{i 3645}# #{n 3639}#)
                                  (begin (if #f #f) #{v 3641}#)
                                  (begin
                                    (vector-set!
                                      #{v 3641}#
                                      #{i 3645}#
                                      (#{remodulate 3626}#
                                        (vector-ref #{x 3627}# #{i 3645}#)
                                        #{mod 3628}#))
                                    (#{loop 3644}# (#{1+}# #{i 3645}#)))))))
                           (#{loop 3644}# 0))))
                     #{x 3627}#))))))
          (let ((#{tmp 3651}# #{e 3619}#))
            (let ((#{tmp 3652}#
                    ($sc-dispatch #{tmp 3651}# '(_ each-any any))))
              (if (if #{tmp 3652}#
                    (@apply
                      (lambda (#{mod 3655}# #{exp 3656}#)
                        (and-map #{id? 343}# #{mod 3655}#))
                      #{tmp 3652}#)
                    #f)
                (@apply
                  (lambda (#{mod 3660}# #{exp 3661}#)
                    (let ((#{mod 3663}#
                            (syntax->datum
                              (cons '#(syntax-object
                                       private
                                       ((top)
                                        #(ribcage
                                          #(mod exp)
                                          #((top) (top))
                                          #("i3658" "i3659"))
                                        #(ribcage
                                          (remodulate)
                                          ((top))
                                          ("i3625"))
                                        #(ribcage
                                          #(e r w)
                                          #((top) (top) (top))
                                          #("i3622" "i3623" "i3624"))
                                        #(ribcage
                                          (lambda-var-list
                                            gen-var
                                            strip
                                            chi-lambda-case
                                            lambda*-formals
                                            chi-simple-lambda
                                            lambda-formals
                                            ellipsis?
                                            chi-void
                                            eval-local-transformer
                                            chi-local-syntax
                                            chi-body
                                            chi-macro
                                            chi-call
                                            chi-expr
                                            chi
                                            syntax-type
                                            chi-when-list
                                            chi-install-global
                                            chi-top-sequence
                                            chi-sequence
                                            source-wrap
                                            wrap
                                            bound-id-member?
                                            distinct-bound-ids?
                                            valid-bound-ids?
                                            bound-id=?
                                            free-id=?
                                            id-var-name
                                            same-marks?
                                            join-marks
                                            join-wraps
                                            smart-append
                                            make-binding-wrap
                                            extend-ribcage!
                                            make-empty-ribcage
                                            new-mark
                                            anti-mark
                                            the-anti-mark
                                            top-marked?
                                            top-wrap
                                            empty-wrap
                                            set-ribcage-labels!
                                            set-ribcage-marks!
                                            set-ribcage-symnames!
                                            ribcage-labels
                                            ribcage-marks
                                            ribcage-symnames
                                            ribcage?
                                            make-ribcage
                                            gen-labels
                                            gen-label
                                            make-rename
                                            rename-marks
                                            rename-new
                                            rename-old
                                            subst-rename?
                                            wrap-subst
                                            wrap-marks
                                            make-wrap
                                            id-sym-name&marks
                                            id-sym-name
                                            id?
                                            nonsymbol-id?
                                            global-extend
                                            lookup
                                            macros-only-env
                                            extend-var-env
                                            extend-env
                                            null-env
                                            binding-value
                                            binding-type
                                            make-binding
                                            arg-check
                                            source-annotation
                                            no-source
                                            set-syntax-object-module!
                                            set-syntax-object-wrap!
                                            set-syntax-object-expression!
                                            syntax-object-module
                                            syntax-object-wrap
                                            syntax-object-expression
                                            syntax-object?
                                            make-syntax-object
                                            build-lexical-var
                                            build-letrec
                                            build-named-let
                                            build-let
                                            build-sequence
                                            build-data
                                            build-primref
                                            build-primcall
                                            build-lambda-case
                                            build-case-lambda
                                            build-simple-lambda
                                            build-global-definition
                                            build-global-assignment
                                            build-global-reference
                                            analyze-variable
                                            build-lexical-assignment
                                            build-lexical-reference
                                            build-dynlet
                                            build-conditional
                                            build-call
                                            build-void
                                            maybe-name-value!
                                            decorate-source
                                            get-global-definition-hook
                                            put-global-definition-hook
                                            gensym-hook
                                            local-eval-hook
                                            top-level-eval-hook
                                            fx<
                                            fx=
                                            fx-
                                            fx+
                                            set-lambda-meta!
                                            lambda-meta
                                            lambda?
                                            make-dynlet
                                            make-letrec
                                            make-let
                                            make-lambda-case
                                            make-lambda
                                            make-seq
                                            make-primcall
                                            make-call
                                            make-conditional
                                            make-toplevel-define
                                            make-toplevel-set
                                            make-toplevel-ref
                                            make-module-set
                                            make-module-ref
                                            make-lexical-set
                                            make-lexical-ref
                                            make-primitive-ref
                                            make-const
                                            make-void)
                                          ((top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top))
                                          ("i452"
                                           "i450"
                                           "i448"
                                           "i446"
                                           "i444"
                                           "i442"
                                           "i440"
                                           "i438"
                                           "i436"
                                           "i434"
                                           "i432"
                                           "i430"
                                           "i428"
                                           "i426"
                                           "i424"
                                           "i422"
                                           "i420"
                                           "i418"
                                           "i416"
                                           "i414"
                                           "i412"
                                           "i410"
                                           "i408"
                                           "i406"
                                           "i404"
                                           "i402"
                                           "i400"
                                           "i398"
                                           "i396"
                                           "i394"
                                           "i392"
                                           "i390"
                                           "i388"
                                           "i386"
                                           "i384"
                                           "i383"
                                           "i382"
                                           "i380"
                                           "i379"
                                           "i378"
                                           "i377"
                                           "i376"
                                           "i374"
                                           "i372"
                                           "i370"
                                           "i368"
                                           "i366"
                                           "i364"
                                           "i362"
                                           "i360"
                                           "i357"
                                           "i355"
                                           "i354"
                                           "i353"
                                           "i352"
                                           "i351"
                                           "i350"
                                           "i349"
                                           "i348"
                                           "i347"
                                           "i345"
                                           "i344"
                                           "i342"
                                           "i340"
                                           "i338"
                                           "i336"
                                           "i334"
                                           "i332"
                                           "i330"
                                           "i329"
                                           "i328"
                                           "i327"
                                           "i326"
                                           "i325"
                                           "i323"
                                           "i322"
                                           "i320"
                                           "i318"
                                           "i316"
                                           "i314"
                                           "i312"
                                           "i310"
                                           "i308"
                                           "i306"
                                           "i304"
                                           "i302"
                                           "i300"
                                           "i298"
                                           "i296"
                                           "i294"
                                           "i292"
                                           "i290"
                                           "i288"
                                           "i286"
                                           "i284"
                                           "i282"
                                           "i280"
                                           "i278"
                                           "i276"
                                           "i274"
                                           "i272"
                                           "i270"
                                           "i268"
                                           "i266"
                                           "i264"
                                           "i262"
                                           "i260"
                                           "i258"
                                           "i256"
                                           "i255"
                                           "i253"
                                           "i251"
                                           "i250"
                                           "i249"
                                           "i248"
                                           "i247"
                                           "i245"
                                           "i243"
                                           "i241"
                                           "i238"
                                           "i236"
                                           "i234"
                                           "i232"
                                           "i230"
                                           "i228"
                                           "i226"
                                           "i224"
                                           "i222"
                                           "i220"
                                           "i218"
                                           "i216"
                                           "i214"
                                           "i212"
                                           "i210"
                                           "i208"
                                           "i206"
                                           "i204"
                                           "i202"))
                                        #(ribcage
                                          (define-structure
                                            define-expansion-accessors
                                            define-expansion-constructors)
                                          ((top) (top) (top))
                                          ("i40" "i39" "i38")))
                                       (hygiene guile))
                                    #{mod 3660}#))))
                      (values
                        (#{remodulate 3626}# #{exp 3661}# #{mod 3663}#)
                        #{r 3620}#
                        #{w 3621}#
                        (#{source-annotation 324}# #{exp 3661}#)
                        #{mod 3663}#)))
                  #{tmp 3652}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 3651}#)))))))
    (#{global-extend 339}#
      'core
      'if
      (lambda (#{e 3665}#
               #{r 3666}#
               #{w 3667}#
               #{s 3668}#
               #{mod 3669}#)
        (let ((#{tmp 3675}# #{e 3665}#))
          (let ((#{tmp 3676}#
                  ($sc-dispatch #{tmp 3675}# '(_ any any))))
            (if #{tmp 3676}#
              (@apply
                (lambda (#{test 3679}# #{then 3680}#)
                  (#{build-conditional 269}#
                    #{s 3668}#
                    (#{chi 423}#
                      #{test 3679}#
                      #{r 3666}#
                      #{w 3667}#
                      #{mod 3669}#)
                    (#{chi 423}#
                      #{then 3680}#
                      #{r 3666}#
                      #{w 3667}#
                      #{mod 3669}#)
                    (#{build-void 265}# #f)))
                #{tmp 3676}#)
              (let ((#{tmp 3682}#
                      ($sc-dispatch #{tmp 3675}# '(_ any any any))))
                (if #{tmp 3682}#
                  (@apply
                    (lambda (#{test 3686}# #{then 3687}# #{else 3688}#)
                      (#{build-conditional 269}#
                        #{s 3668}#
                        (#{chi 423}#
                          #{test 3686}#
                          #{r 3666}#
                          #{w 3667}#
                          #{mod 3669}#)
                        (#{chi 423}#
                          #{then 3687}#
                          #{r 3666}#
                          #{w 3667}#
                          #{mod 3669}#)
                        (#{chi 423}#
                          #{else 3688}#
                          #{r 3666}#
                          #{w 3667}#
                          #{mod 3669}#)))
                    #{tmp 3682}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp 3675}#))))))))
    (#{global-extend 339}#
      'core
      'with-fluids
      (lambda (#{e 3689}#
               #{r 3690}#
               #{w 3691}#
               #{s 3692}#
               #{mod 3693}#)
        (let ((#{tmp 3699}# #{e 3689}#))
          (let ((#{tmp 3700}#
                  ($sc-dispatch
                    #{tmp 3699}#
                    '(_ #(each (any any)) any . each-any))))
            (if #{tmp 3700}#
              (@apply
                (lambda (#{fluid 3705}#
                         #{val 3706}#
                         #{b 3707}#
                         #{b* 3708}#)
                  (#{build-dynlet 271}#
                    #{s 3692}#
                    (map (lambda (#{x 3709}#)
                           (#{chi 423}#
                             #{x 3709}#
                             #{r 3690}#
                             #{w 3691}#
                             #{mod 3693}#))
                         #{fluid 3705}#)
                    (map (lambda (#{x 3712}#)
                           (#{chi 423}#
                             #{x 3712}#
                             #{r 3690}#
                             #{w 3691}#
                             #{mod 3693}#))
                         #{val 3706}#)
                    (#{chi-body 431}#
                      (cons #{b 3707}# #{b* 3708}#)
                      (#{source-wrap 411}#
                        #{e 3689}#
                        #{w 3691}#
                        #{s 3692}#
                        #{mod 3693}#)
                      #{r 3690}#
                      #{w 3691}#
                      #{mod 3693}#)))
                #{tmp 3700}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 3699}#))))))
    (#{global-extend 339}# 'begin 'begin '())
    (#{global-extend 339}# 'define 'define '())
    (#{global-extend 339}#
      'define-syntax
      'define-syntax
      '())
    (#{global-extend 339}# 'eval-when 'eval-when '())
    (#{global-extend 339}#
      'core
      'syntax-case
      (letrec*
        ((#{convert-pattern 3717}#
           (lambda (#{pattern 3724}# #{keys 3725}#)
             (letrec*
               ((#{cvt* 3729}#
                  (lambda (#{p* 3734}# #{n 3735}# #{ids 3736}#)
                    (if (not (pair? #{p* 3734}#))
                      (#{cvt 3733}#
                        #{p* 3734}#
                        #{n 3735}#
                        #{ids 3736}#)
                      (call-with-values
                        (lambda ()
                          (#{cvt* 3729}#
                            (cdr #{p* 3734}#)
                            #{n 3735}#
                            #{ids 3736}#))
                        (lambda (#{y 3740}# #{ids 3741}#)
                          (call-with-values
                            (lambda ()
                              (#{cvt 3733}#
                                (car #{p* 3734}#)
                                #{n 3735}#
                                #{ids 3741}#))
                            (lambda (#{x 3744}# #{ids 3745}#)
                              (values
                                (cons #{x 3744}# #{y 3740}#)
                                #{ids 3745}#))))))))
                (#{v-reverse 3731}#
                  (lambda (#{x 3748}#)
                    (letrec*
                      ((#{loop 3753}#
                         (lambda (#{r 3754}# #{x 3755}#)
                           (if (not (pair? #{x 3755}#))
                             (values #{r 3754}# #{x 3755}#)
                             (#{loop 3753}#
                               (cons (car #{x 3755}#) #{r 3754}#)
                               (cdr #{x 3755}#))))))
                      (#{loop 3753}# '() #{x 3748}#))))
                (#{cvt 3733}#
                  (lambda (#{p 3756}# #{n 3757}# #{ids 3758}#)
                    (if (#{id? 343}# #{p 3756}#)
                      (if (#{bound-id-member? 407}#
                            #{p 3756}#
                            #{keys 3725}#)
                        (values
                          (vector 'free-id #{p 3756}#)
                          #{ids 3758}#)
                        (if (#{free-id=? 399}#
                              #{p 3756}#
                              '#(syntax-object
                                 _
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage
                                    #(p n ids)
                                    #((top) (top) (top))
                                    #("i3759" "i3760" "i3761"))
                                  #(ribcage
                                    (cvt v-reverse cvt*)
                                    ((top) (top) (top))
                                    ("i3732" "i3730" "i3728"))
                                  #(ribcage
                                    #(pattern keys)
                                    #((top) (top))
                                    #("i3726" "i3727"))
                                  #(ribcage
                                    (gen-syntax-case
                                      gen-clause
                                      build-dispatch-call
                                      convert-pattern)
                                    ((top) (top) (top) (top))
                                    ("i3722" "i3720" "i3718" "i3716"))
                                  #(ribcage
                                    (lambda-var-list
                                      gen-var
                                      strip
                                      chi-lambda-case
                                      lambda*-formals
                                      chi-simple-lambda
                                      lambda-formals
                                      ellipsis?
                                      chi-void
                                      eval-local-transformer
                                      chi-local-syntax
                                      chi-body
                                      chi-macro
                                      chi-call
                                      chi-expr
                                      chi
                                      syntax-type
                                      chi-when-list
                                      chi-install-global
                                      chi-top-sequence
                                      chi-sequence
                                      source-wrap
                                      wrap
                                      bound-id-member?
                                      distinct-bound-ids?
                                      valid-bound-ids?
                                      bound-id=?
                                      free-id=?
                                      id-var-name
                                      same-marks?
                                      join-marks
                                      join-wraps
                                      smart-append
                                      make-binding-wrap
                                      extend-ribcage!
                                      make-empty-ribcage
                                      new-mark
                                      anti-mark
                                      the-anti-mark
                                      top-marked?
                                      top-wrap
                                      empty-wrap
                                      set-ribcage-labels!
                                      set-ribcage-marks!
                                      set-ribcage-symnames!
                                      ribcage-labels
                                      ribcage-marks
                                      ribcage-symnames
                                      ribcage?
                                      make-ribcage
                                      gen-labels
                                      gen-label
                                      make-rename
                                      rename-marks
                                      rename-new
                                      rename-old
                                      subst-rename?
                                      wrap-subst
                                      wrap-marks
                                      make-wrap
                                      id-sym-name&marks
                                      id-sym-name
                                      id?
                                      nonsymbol-id?
                                      global-extend
                                      lookup
                                      macros-only-env
                                      extend-var-env
                                      extend-env
                                      null-env
                                      binding-value
                                      binding-type
                                      make-binding
                                      arg-check
                                      source-annotation
                                      no-source
                                      set-syntax-object-module!
                                      set-syntax-object-wrap!
                                      set-syntax-object-expression!
                                      syntax-object-module
                                      syntax-object-wrap
                                      syntax-object-expression
                                      syntax-object?
                                      make-syntax-object
                                      build-lexical-var
                                      build-letrec
                                      build-named-let
                                      build-let
                                      build-sequence
                                      build-data
                                      build-primref
                                      build-primcall
                                      build-lambda-case
                                      build-case-lambda
                                      build-simple-lambda
                                      build-global-definition
                                      build-global-assignment
                                      build-global-reference
                                      analyze-variable
                                      build-lexical-assignment
                                      build-lexical-reference
                                      build-dynlet
                                      build-conditional
                                      build-call
                                      build-void
                                      maybe-name-value!
                                      decorate-source
                                      get-global-definition-hook
                                      put-global-definition-hook
                                      gensym-hook
                                      local-eval-hook
                                      top-level-eval-hook
                                      fx<
                                      fx=
                                      fx-
                                      fx+
                                      set-lambda-meta!
                                      lambda-meta
                                      lambda?
                                      make-dynlet
                                      make-letrec
                                      make-let
                                      make-lambda-case
                                      make-lambda
                                      make-seq
                                      make-primcall
                                      make-call
                                      make-conditional
                                      make-toplevel-define
                                      make-toplevel-set
                                      make-toplevel-ref
                                      make-module-set
                                      make-module-ref
                                      make-lexical-set
                                      make-lexical-ref
                                      make-primitive-ref
                                      make-const
                                      make-void)
                                    ((top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top)
                                     (top))
                                    ("i452"
                                     "i450"
                                     "i448"
                                     "i446"
                                     "i444"
                                     "i442"
                                     "i440"
                                     "i438"
                                     "i436"
                                     "i434"
                                     "i432"
                                     "i430"
                                     "i428"
                                     "i426"
                                     "i424"
                                     "i422"
                                     "i420"
                                     "i418"
                                     "i416"
                                     "i414"
                                     "i412"
                                     "i410"
                                     "i408"
                                     "i406"
                                     "i404"
                                     "i402"
                                     "i400"
                                     "i398"
                                     "i396"
                                     "i394"
                                     "i392"
                                     "i390"
                                     "i388"
                                     "i386"
                                     "i384"
                                     "i383"
                                     "i382"
                                     "i380"
                                     "i379"
                                     "i378"
                                     "i377"
                                     "i376"
                                     "i374"
                                     "i372"
                                     "i370"
                                     "i368"
                                     "i366"
                                     "i364"
                                     "i362"
                                     "i360"
                                     "i357"
                                     "i355"
                                     "i354"
                                     "i353"
                                     "i352"
                                     "i351"
                                     "i350"
                                     "i349"
                                     "i348"
                                     "i347"
                                     "i345"
                                     "i344"
                                     "i342"
                                     "i340"
                                     "i338"
                                     "i336"
                                     "i334"
                                     "i332"
                                     "i330"
                                     "i329"
                                     "i328"
                                     "i327"
                                     "i326"
                                     "i325"
                                     "i323"
                                     "i322"
                                     "i320"
                                     "i318"
                                     "i316"
                                     "i314"
                                     "i312"
                                     "i310"
                                     "i308"
                                     "i306"
                                     "i304"
                                     "i302"
                                     "i300"
                                     "i298"
                                     "i296"
                                     "i294"
                                     "i292"
                                     "i290"
                                     "i288"
                                     "i286"
                                     "i284"
                                     "i282"
                                     "i280"
                                     "i278"
                                     "i276"
                                     "i274"
                                     "i272"
                                     "i270"
                                     "i268"
                                     "i266"
                                     "i264"
                                     "i262"
                                     "i260"
                                     "i258"
                                     "i256"
                                     "i255"
                                     "i253"
                                     "i251"
                                     "i250"
                                     "i249"
                                     "i248"
                                     "i247"
                                     "i245"
                                     "i243"
                                     "i241"
                                     "i238"
                                     "i236"
                                     "i234"
                                     "i232"
                                     "i230"
                                     "i228"
                                     "i226"
                                     "i224"
                                     "i222"
                                     "i220"
                                     "i218"
                                     "i216"
                                     "i214"
                                     "i212"
                                     "i210"
                                     "i208"
                                     "i206"
                                     "i204"
                                     "i202"))
                                  #(ribcage
                                    (define-structure
                                      define-expansion-accessors
                                      define-expansion-constructors)
                                    ((top) (top) (top))
                                    ("i40" "i39" "i38")))
                                 (hygiene guile)))
                          (values '_ #{ids 3758}#)
                          (values
                            'any
                            (cons (cons #{p 3756}# #{n 3757}#) #{ids 3758}#))))
                      (let ((#{tmp 3767}# #{p 3756}#))
                        (let ((#{tmp 3768}#
                                ($sc-dispatch #{tmp 3767}# '(any any))))
                          (if (if #{tmp 3768}#
                                (@apply
                                  (lambda (#{x 3771}# #{dots 3772}#)
                                    (#{ellipsis? 439}# #{dots 3772}#))
                                  #{tmp 3768}#)
                                #f)
                            (@apply
                              (lambda (#{x 3775}# #{dots 3776}#)
                                (call-with-values
                                  (lambda ()
                                    (#{cvt 3733}#
                                      #{x 3775}#
                                      (#{1+}# #{n 3757}#)
                                      #{ids 3758}#))
                                  (lambda (#{p 3778}# #{ids 3779}#)
                                    (values
                                      (if (eq? #{p 3778}# 'any)
                                        'each-any
                                        (vector 'each #{p 3778}#))
                                      #{ids 3779}#))))
                              #{tmp 3768}#)
                            (let ((#{tmp 3782}#
                                    ($sc-dispatch
                                      #{tmp 3767}#
                                      '(any any . any))))
                              (if (if #{tmp 3782}#
                                    (@apply
                                      (lambda (#{x 3786}#
                                               #{dots 3787}#
                                               #{ys 3788}#)
                                        (#{ellipsis? 439}# #{dots 3787}#))
                                      #{tmp 3782}#)
                                    #f)
                                (@apply
                                  (lambda (#{x 3792}#
                                           #{dots 3793}#
                                           #{ys 3794}#)
                                    (call-with-values
                                      (lambda ()
                                        (#{cvt* 3729}#
                                          #{ys 3794}#
                                          #{n 3757}#
                                          #{ids 3758}#))
                                      (lambda (#{ys 3795}# #{ids 3796}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt 3733}#
                                              #{x 3792}#
                                              (#{1+}# #{n 3757}#)
                                              #{ids 3796}#))
                                          (lambda (#{x 3799}# #{ids 3800}#)
                                            (call-with-values
                                              (lambda ()
                                                (#{v-reverse 3731}#
                                                  #{ys 3795}#))
                                              (lambda (#{ys 3803}# #{e 3804}#)
                                                (values
                                                  (vector
                                                    'each+
                                                    #{x 3799}#
                                                    #{ys 3803}#
                                                    #{e 3804}#)
                                                  #{ids 3800}#))))))))
                                  #{tmp 3782}#)
                                (let ((#{tmp 3808}#
                                        ($sc-dispatch
                                          #{tmp 3767}#
                                          '(any . any))))
                                  (if #{tmp 3808}#
                                    (@apply
                                      (lambda (#{x 3811}# #{y 3812}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt 3733}#
                                              #{y 3812}#
                                              #{n 3757}#
                                              #{ids 3758}#))
                                          (lambda (#{y 3813}# #{ids 3814}#)
                                            (call-with-values
                                              (lambda ()
                                                (#{cvt 3733}#
                                                  #{x 3811}#
                                                  #{n 3757}#
                                                  #{ids 3814}#))
                                              (lambda (#{x 3817}# #{ids 3818}#)
                                                (values
                                                  (cons #{x 3817}# #{y 3813}#)
                                                  #{ids 3818}#))))))
                                      #{tmp 3808}#)
                                    (let ((#{tmp 3821}#
                                            ($sc-dispatch #{tmp 3767}# '())))
                                      (if #{tmp 3821}#
                                        (@apply
                                          (lambda () (values '() #{ids 3758}#))
                                          #{tmp 3821}#)
                                        (let ((#{tmp 3822}#
                                                ($sc-dispatch
                                                  #{tmp 3767}#
                                                  '#(vector each-any))))
                                          (if #{tmp 3822}#
                                            (@apply
                                              (lambda (#{x 3824}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt 3733}#
                                                      #{x 3824}#
                                                      #{n 3757}#
                                                      #{ids 3758}#))
                                                  (lambda (#{p 3826}#
                                                           #{ids 3827}#)
                                                    (values
                                                      (vector
                                                        'vector
                                                        #{p 3826}#)
                                                      #{ids 3827}#))))
                                              #{tmp 3822}#)
                                            (let ((#{x 3831}# #{tmp 3767}#))
                                              (values
                                                (vector
                                                  'atom
                                                  (#{strip 449}#
                                                    #{p 3756}#
                                                    '(())))
                                                #{ids 3758}#)))))))))))))))))
               (#{cvt 3733}# #{pattern 3724}# 0 '()))))
         (#{build-dispatch-call 3719}#
           (lambda (#{pvars 3833}#
                    #{exp 3834}#
                    #{y 3835}#
                    #{r 3836}#
                    #{mod 3837}#)
             (begin
               (map cdr #{pvars 3833}#)
               (let ((#{ids 3845}# (map car #{pvars 3833}#)))
                 (let ((#{labels 3849}#
                         (#{gen-labels 358}# #{ids 3845}#))
                       (#{new-vars 3850}#
                         (map #{gen-var 451}# #{ids 3845}#)))
                   (#{build-primcall 291}#
                     #f
                     'apply
                     (list (#{build-simple-lambda 285}#
                             #f
                             (map syntax->datum #{ids 3845}#)
                             #f
                             #{new-vars 3850}#
                             '()
                             (#{chi 423}#
                               #{exp 3834}#
                               (#{extend-env 331}#
                                 #{labels 3849}#
                                 (map (lambda (#{var 3853}# #{level 3854}#)
                                        (cons 'syntax
                                              (cons #{var 3853}#
                                                    #{level 3854}#)))
                                      #{new-vars 3850}#
                                      (map cdr #{pvars 3833}#))
                                 #{r 3836}#)
                               (#{make-binding-wrap 387}#
                                 #{ids 3845}#
                                 #{labels 3849}#
                                 '(()))
                               #{mod 3837}#))
                           #{y 3835}#)))))))
         (#{gen-clause 3721}#
           (lambda (#{x 3860}#
                    #{keys 3861}#
                    #{clauses 3862}#
                    #{r 3863}#
                    #{pat 3864}#
                    #{fender 3865}#
                    #{exp 3866}#
                    #{mod 3867}#)
             (call-with-values
               (lambda ()
                 (#{convert-pattern 3717}#
                   #{pat 3864}#
                   #{keys 3861}#))
               (lambda (#{p 3876}# #{pvars 3877}#)
                 (if (not (#{distinct-bound-ids? 405}#
                            (map car #{pvars 3877}#)))
                   (syntax-violation
                     'syntax-case
                     "duplicate pattern variable"
                     #{pat 3864}#)
                   (if (not (and-map
                              (lambda (#{x 3884}#)
                                (not (#{ellipsis? 439}# (car #{x 3884}#))))
                              #{pvars 3877}#))
                     (syntax-violation
                       'syntax-case
                       "misplaced ellipsis"
                       #{pat 3864}#)
                     (let ((#{y 3888}# (#{gen-var 451}# 'tmp)))
                       (#{build-call 267}#
                         #f
                         (#{build-simple-lambda 285}#
                           #f
                           (list 'tmp)
                           #f
                           (list #{y 3888}#)
                           '()
                           (let ((#{y 3892}#
                                   (#{build-lexical-reference 273}#
                                     'value
                                     #f
                                     'tmp
                                     #{y 3888}#)))
                             (#{build-conditional 269}#
                               #f
                               (let ((#{tmp 3895}# #{fender 3865}#))
                                 (let ((#{tmp 3896}#
                                         ($sc-dispatch
                                           #{tmp 3895}#
                                           '#(atom #t))))
                                   (if #{tmp 3896}#
                                     (@apply
                                       (lambda () #{y 3892}#)
                                       #{tmp 3896}#)
                                     (let ((#{_ 3898}# #{tmp 3895}#))
                                       (#{build-conditional 269}#
                                         #f
                                         #{y 3892}#
                                         (#{build-dispatch-call 3719}#
                                           #{pvars 3877}#
                                           #{fender 3865}#
                                           #{y 3892}#
                                           #{r 3863}#
                                           #{mod 3867}#)
                                         (#{build-data 295}# #f #f))))))
                               (#{build-dispatch-call 3719}#
                                 #{pvars 3877}#
                                 #{exp 3866}#
                                 #{y 3892}#
                                 #{r 3863}#
                                 #{mod 3867}#)
                               (#{gen-syntax-case 3723}#
                                 #{x 3860}#
                                 #{keys 3861}#
                                 #{clauses 3862}#
                                 #{r 3863}#
                                 #{mod 3867}#))))
                         (list (if (eq? #{p 3876}# 'any)
                                 (#{build-primcall 291}#
                                   #f
                                   'list
                                   (list #{x 3860}#))
                                 (#{build-primcall 291}#
                                   #f
                                   '$sc-dispatch
                                   (list #{x 3860}#
                                         (#{build-data 295}#
                                           #f
                                           #{p 3876}#)))))))))))))
         (#{gen-syntax-case 3723}#
           (lambda (#{x 3904}#
                    #{keys 3905}#
                    #{clauses 3906}#
                    #{r 3907}#
                    #{mod 3908}#)
             (if (null? #{clauses 3906}#)
               (#{build-primcall 291}#
                 #f
                 'syntax-violation
                 (list (#{build-data 295}# #f #f)
                       (#{build-data 295}#
                         #f
                         "source expression failed to match any pattern")
                       #{x 3904}#))
               (let ((#{tmp 3917}# (car #{clauses 3906}#)))
                 (let ((#{tmp 3918}#
                         ($sc-dispatch #{tmp 3917}# '(any any))))
                   (if #{tmp 3918}#
                     (@apply
                       (lambda (#{pat 3921}# #{exp 3922}#)
                         (if (if (#{id? 343}# #{pat 3921}#)
                               (and-map
                                 (lambda (#{x 3925}#)
                                   (not (#{free-id=? 399}#
                                          #{pat 3921}#
                                          #{x 3925}#)))
                                 (cons '#(syntax-object
                                          ...
                                          ((top)
                                           #(ribcage
                                             #(pat exp)
                                             #((top) (top))
                                             #("i3919" "i3920"))
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(x keys clauses r mod)
                                             #((top) (top) (top) (top) (top))
                                             #("i3909"
                                               "i3910"
                                               "i3911"
                                               "i3912"
                                               "i3913"))
                                           #(ribcage
                                             (gen-syntax-case
                                               gen-clause
                                               build-dispatch-call
                                               convert-pattern)
                                             ((top) (top) (top) (top))
                                             ("i3722" "i3720" "i3718" "i3716"))
                                           #(ribcage
                                             (lambda-var-list
                                               gen-var
                                               strip
                                               chi-lambda-case
                                               lambda*-formals
                                               chi-simple-lambda
                                               lambda-formals
                                               ellipsis?
                                               chi-void
                                               eval-local-transformer
                                               chi-local-syntax
                                               chi-body
                                               chi-macro
                                               chi-call
                                               chi-expr
                                               chi
                                               syntax-type
                                               chi-when-list
                                               chi-install-global
                                               chi-top-sequence
                                               chi-sequence
                                               source-wrap
                                               wrap
                                               bound-id-member?
                                               distinct-bound-ids?
                                               valid-bound-ids?
                                               bound-id=?
                                               free-id=?
                                               id-var-name
                                               same-marks?
                                               join-marks
                                               join-wraps
                                               smart-append
                                               make-binding-wrap
                                               extend-ribcage!
                                               make-empty-ribcage
                                               new-mark
                                               anti-mark
                                               the-anti-mark
                                               top-marked?
                                               top-wrap
                                               empty-wrap
                                               set-ribcage-labels!
                                               set-ribcage-marks!
                                               set-ribcage-symnames!
                                               ribcage-labels
                                               ribcage-marks
                                               ribcage-symnames
                                               ribcage?
                                               make-ribcage
                                               gen-labels
                                               gen-label
                                               make-rename
                                               rename-marks
                                               rename-new
                                               rename-old
                                               subst-rename?
                                               wrap-subst
                                               wrap-marks
                                               make-wrap
                                               id-sym-name&marks
                                               id-sym-name
                                               id?
                                               nonsymbol-id?
                                               global-extend
                                               lookup
                                               macros-only-env
                                               extend-var-env
                                               extend-env
                                               null-env
                                               binding-value
                                               binding-type
                                               make-binding
                                               arg-check
                                               source-annotation
                                               no-source
                                               set-syntax-object-module!
                                               set-syntax-object-wrap!
                                               set-syntax-object-expression!
                                               syntax-object-module
                                               syntax-object-wrap
                                               syntax-object-expression
                                               syntax-object?
                                               make-syntax-object
                                               build-lexical-var
                                               build-letrec
                                               build-named-let
                                               build-let
                                               build-sequence
                                               build-data
                                               build-primref
                                               build-primcall
                                               build-lambda-case
                                               build-case-lambda
                                               build-simple-lambda
                                               build-global-definition
                                               build-global-assignment
                                               build-global-reference
                                               analyze-variable
                                               build-lexical-assignment
                                               build-lexical-reference
                                               build-dynlet
                                               build-conditional
                                               build-call
                                               build-void
                                               maybe-name-value!
                                               decorate-source
                                               get-global-definition-hook
                                               put-global-definition-hook
                                               gensym-hook
                                               local-eval-hook
                                               top-level-eval-hook
                                               fx<
                                               fx=
                                               fx-
                                               fx+
                                               set-lambda-meta!
                                               lambda-meta
                                               lambda?
                                               make-dynlet
                                               make-letrec
                                               make-let
                                               make-lambda-case
                                               make-lambda
                                               make-seq
                                               make-primcall
                                               make-call
                                               make-conditional
                                               make-toplevel-define
                                               make-toplevel-set
                                               make-toplevel-ref
                                               make-module-set
                                               make-module-ref
                                               make-lexical-set
                                               make-lexical-ref
                                               make-primitive-ref
                                               make-const
                                               make-void)
                                             ((top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top)
                                              (top))
                                             ("i452"
                                              "i450"
                                              "i448"
                                              "i446"
                                              "i444"
                                              "i442"
                                              "i440"
                                              "i438"
                                              "i436"
                                              "i434"
                                              "i432"
                                              "i430"
                                              "i428"
                                              "i426"
                                              "i424"
                                              "i422"
                                              "i420"
                                              "i418"
                                              "i416"
                                              "i414"
                                              "i412"
                                              "i410"
                                              "i408"
                                              "i406"
                                              "i404"
                                              "i402"
                                              "i400"
                                              "i398"
                                              "i396"
                                              "i394"
                                              "i392"
                                              "i390"
                                              "i388"
                                              "i386"
                                              "i384"
                                              "i383"
                                              "i382"
                                              "i380"
                                              "i379"
                                              "i378"
                                              "i377"
                                              "i376"
                                              "i374"
                                              "i372"
                                              "i370"
                                              "i368"
                                              "i366"
                                              "i364"
                                              "i362"
                                              "i360"
                                              "i357"
                                              "i355"
                                              "i354"
                                              "i353"
                                              "i352"
                                              "i351"
                                              "i350"
                                              "i349"
                                              "i348"
                                              "i347"
                                              "i345"
                                              "i344"
                                              "i342"
                                              "i340"
                                              "i338"
                                              "i336"
                                              "i334"
                                              "i332"
                                              "i330"
                                              "i329"
                                              "i328"
                                              "i327"
                                              "i326"
                                              "i325"
                                              "i323"
                                              "i322"
                                              "i320"
                                              "i318"
                                              "i316"
                                              "i314"
                                              "i312"
                                              "i310"
                                              "i308"
                                              "i306"
                                              "i304"
                                              "i302"
                                              "i300"
                                              "i298"
                                              "i296"
                                              "i294"
                                              "i292"
                                              "i290"
                                              "i288"
                                              "i286"
                                              "i284"
                                              "i282"
                                              "i280"
                                              "i278"
                                              "i276"
                                              "i274"
                                              "i272"
                                              "i270"
                                              "i268"
                                              "i266"
                                              "i264"
                                              "i262"
                                              "i260"
                                              "i258"
                                              "i256"
                                              "i255"
                                              "i253"
                                              "i251"
                                              "i250"
                                              "i249"
                                              "i248"
                                              "i247"
                                              "i245"
                                              "i243"
                                              "i241"
                                              "i238"
                                              "i236"
                                              "i234"
                                              "i232"
                                              "i230"
                                              "i228"
                                              "i226"
                                              "i224"
                                              "i222"
                                              "i220"
                                              "i218"
                                              "i216"
                                              "i214"
                                              "i212"
                                              "i210"
                                              "i208"
                                              "i206"
                                              "i204"
                                              "i202"))
                                           #(ribcage
                                             (define-structure
                                               define-expansion-accessors
                                               define-expansion-constructors)
                                             ((top) (top) (top))
                                             ("i40" "i39" "i38")))
                                          (hygiene guile))
                                       #{keys 3905}#))
                               #f)
                           (if (#{free-id=? 399}#
                                 '#(syntax-object
                                    pad
                                    ((top)
                                     #(ribcage
                                       #(pat exp)
                                       #((top) (top))
                                       #("i3919" "i3920"))
                                     #(ribcage () () ())
                                     #(ribcage
                                       #(x keys clauses r mod)
                                       #((top) (top) (top) (top) (top))
                                       #("i3909"
                                         "i3910"
                                         "i3911"
                                         "i3912"
                                         "i3913"))
                                     #(ribcage
                                       (gen-syntax-case
                                         gen-clause
                                         build-dispatch-call
                                         convert-pattern)
                                       ((top) (top) (top) (top))
                                       ("i3722" "i3720" "i3718" "i3716"))
                                     #(ribcage
                                       (lambda-var-list
                                         gen-var
                                         strip
                                         chi-lambda-case
                                         lambda*-formals
                                         chi-simple-lambda
                                         lambda-formals
                                         ellipsis?
                                         chi-void
                                         eval-local-transformer
                                         chi-local-syntax
                                         chi-body
                                         chi-macro
                                         chi-call
                                         chi-expr
                                         chi
                                         syntax-type
                                         chi-when-list
                                         chi-install-global
                                         chi-top-sequence
                                         chi-sequence
                                         source-wrap
                                         wrap
                                         bound-id-member?
                                         distinct-bound-ids?
                                         valid-bound-ids?
                                         bound-id=?
                                         free-id=?
                                         id-var-name
                                         same-marks?
                                         join-marks
                                         join-wraps
                                         smart-append
                                         make-binding-wrap
                                         extend-ribcage!
                                         make-empty-ribcage
                                         new-mark
                                         anti-mark
                                         the-anti-mark
                                         top-marked?
                                         top-wrap
                                         empty-wrap
                                         set-ribcage-labels!
                                         set-ribcage-marks!
                                         set-ribcage-symnames!
                                         ribcage-labels
                                         ribcage-marks
                                         ribcage-symnames
                                         ribcage?
                                         make-ribcage
                                         gen-labels
                                         gen-label
                                         make-rename
                                         rename-marks
                                         rename-new
                                         rename-old
                                         subst-rename?
                                         wrap-subst
                                         wrap-marks
                                         make-wrap
                                         id-sym-name&marks
                                         id-sym-name
                                         id?
                                         nonsymbol-id?
                                         global-extend
                                         lookup
                                         macros-only-env
                                         extend-var-env
                                         extend-env
                                         null-env
                                         binding-value
                                         binding-type
                                         make-binding
                                         arg-check
                                         source-annotation
                                         no-source
                                         set-syntax-object-module!
                                         set-syntax-object-wrap!
                                         set-syntax-object-expression!
                                         syntax-object-module
                                         syntax-object-wrap
                                         syntax-object-expression
                                         syntax-object?
                                         make-syntax-object
                                         build-lexical-var
                                         build-letrec
                                         build-named-let
                                         build-let
                                         build-sequence
                                         build-data
                                         build-primref
                                         build-primcall
                                         build-lambda-case
                                         build-case-lambda
                                         build-simple-lambda
                                         build-global-definition
                                         build-global-assignment
                                         build-global-reference
                                         analyze-variable
                                         build-lexical-assignment
                                         build-lexical-reference
                                         build-dynlet
                                         build-conditional
                                         build-call
                                         build-void
                                         maybe-name-value!
                                         decorate-source
                                         get-global-definition-hook
                                         put-global-definition-hook
                                         gensym-hook
                                         local-eval-hook
                                         top-level-eval-hook
                                         fx<
                                         fx=
                                         fx-
                                         fx+
                                         set-lambda-meta!
                                         lambda-meta
                                         lambda?
                                         make-dynlet
                                         make-letrec
                                         make-let
                                         make-lambda-case
                                         make-lambda
                                         make-seq
                                         make-primcall
                                         make-call
                                         make-conditional
                                         make-toplevel-define
                                         make-toplevel-set
                                         make-toplevel-ref
                                         make-module-set
                                         make-module-ref
                                         make-lexical-set
                                         make-lexical-ref
                                         make-primitive-ref
                                         make-const
                                         make-void)
                                       ((top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top))
                                       ("i452"
                                        "i450"
                                        "i448"
                                        "i446"
                                        "i444"
                                        "i442"
                                        "i440"
                                        "i438"
                                        "i436"
                                        "i434"
                                        "i432"
                                        "i430"
                                        "i428"
                                        "i426"
                                        "i424"
                                        "i422"
                                        "i420"
                                        "i418"
                                        "i416"
                                        "i414"
                                        "i412"
                                        "i410"
                                        "i408"
                                        "i406"
                                        "i404"
                                        "i402"
                                        "i400"
                                        "i398"
                                        "i396"
                                        "i394"
                                        "i392"
                                        "i390"
                                        "i388"
                                        "i386"
                                        "i384"
                                        "i383"
                                        "i382"
                                        "i380"
                                        "i379"
                                        "i378"
                                        "i377"
                                        "i376"
                                        "i374"
                                        "i372"
                                        "i370"
                                        "i368"
                                        "i366"
                                        "i364"
                                        "i362"
                                        "i360"
                                        "i357"
                                        "i355"
                                        "i354"
                                        "i353"
                                        "i352"
                                        "i351"
                                        "i350"
                                        "i349"
                                        "i348"
                                        "i347"
                                        "i345"
                                        "i344"
                                        "i342"
                                        "i340"
                                        "i338"
                                        "i336"
                                        "i334"
                                        "i332"
                                        "i330"
                                        "i329"
                                        "i328"
                                        "i327"
                                        "i326"
                                        "i325"
                                        "i323"
                                        "i322"
                                        "i320"
                                        "i318"
                                        "i316"
                                        "i314"
                                        "i312"
                                        "i310"
                                        "i308"
                                        "i306"
                                        "i304"
                                        "i302"
                                        "i300"
                                        "i298"
                                        "i296"
                                        "i294"
                                        "i292"
                                        "i290"
                                        "i288"
                                        "i286"
                                        "i284"
                                        "i282"
                                        "i280"
                                        "i278"
                                        "i276"
                                        "i274"
                                        "i272"
                                        "i270"
                                        "i268"
                                        "i266"
                                        "i264"
                                        "i262"
                                        "i260"
                                        "i258"
                                        "i256"
                                        "i255"
                                        "i253"
                                        "i251"
                                        "i250"
                                        "i249"
                                        "i248"
                                        "i247"
                                        "i245"
                                        "i243"
                                        "i241"
                                        "i238"
                                        "i236"
                                        "i234"
                                        "i232"
                                        "i230"
                                        "i228"
                                        "i226"
                                        "i224"
                                        "i222"
                                        "i220"
                                        "i218"
                                        "i216"
                                        "i214"
                                        "i212"
                                        "i210"
                                        "i208"
                                        "i206"
                                        "i204"
                                        "i202"))
                                     #(ribcage
                                       (define-structure
                                         define-expansion-accessors
                                         define-expansion-constructors)
                                       ((top) (top) (top))
                                       ("i40" "i39" "i38")))
                                    (hygiene guile))
                                 '#(syntax-object
                                    _
                                    ((top)
                                     #(ribcage
                                       #(pat exp)
                                       #((top) (top))
                                       #("i3919" "i3920"))
                                     #(ribcage () () ())
                                     #(ribcage
                                       #(x keys clauses r mod)
                                       #((top) (top) (top) (top) (top))
                                       #("i3909"
                                         "i3910"
                                         "i3911"
                                         "i3912"
                                         "i3913"))
                                     #(ribcage
                                       (gen-syntax-case
                                         gen-clause
                                         build-dispatch-call
                                         convert-pattern)
                                       ((top) (top) (top) (top))
                                       ("i3722" "i3720" "i3718" "i3716"))
                                     #(ribcage
                                       (lambda-var-list
                                         gen-var
                                         strip
                                         chi-lambda-case
                                         lambda*-formals
                                         chi-simple-lambda
                                         lambda-formals
                                         ellipsis?
                                         chi-void
                                         eval-local-transformer
                                         chi-local-syntax
                                         chi-body
                                         chi-macro
                                         chi-call
                                         chi-expr
                                         chi
                                         syntax-type
                                         chi-when-list
                                         chi-install-global
                                         chi-top-sequence
                                         chi-sequence
                                         source-wrap
                                         wrap
                                         bound-id-member?
                                         distinct-bound-ids?
                                         valid-bound-ids?
                                         bound-id=?
                                         free-id=?
                                         id-var-name
                                         same-marks?
                                         join-marks
                                         join-wraps
                                         smart-append
                                         make-binding-wrap
                                         extend-ribcage!
                                         make-empty-ribcage
                                         new-mark
                                         anti-mark
                                         the-anti-mark
                                         top-marked?
                                         top-wrap
                                         empty-wrap
                                         set-ribcage-labels!
                                         set-ribcage-marks!
                                         set-ribcage-symnames!
                                         ribcage-labels
                                         ribcage-marks
                                         ribcage-symnames
                                         ribcage?
                                         make-ribcage
                                         gen-labels
                                         gen-label
                                         make-rename
                                         rename-marks
                                         rename-new
                                         rename-old
                                         subst-rename?
                                         wrap-subst
                                         wrap-marks
                                         make-wrap
                                         id-sym-name&marks
                                         id-sym-name
                                         id?
                                         nonsymbol-id?
                                         global-extend
                                         lookup
                                         macros-only-env
                                         extend-var-env
                                         extend-env
                                         null-env
                                         binding-value
                                         binding-type
                                         make-binding
                                         arg-check
                                         source-annotation
                                         no-source
                                         set-syntax-object-module!
                                         set-syntax-object-wrap!
                                         set-syntax-object-expression!
                                         syntax-object-module
                                         syntax-object-wrap
                                         syntax-object-expression
                                         syntax-object?
                                         make-syntax-object
                                         build-lexical-var
                                         build-letrec
                                         build-named-let
                                         build-let
                                         build-sequence
                                         build-data
                                         build-primref
                                         build-primcall
                                         build-lambda-case
                                         build-case-lambda
                                         build-simple-lambda
                                         build-global-definition
                                         build-global-assignment
                                         build-global-reference
                                         analyze-variable
                                         build-lexical-assignment
                                         build-lexical-reference
                                         build-dynlet
                                         build-conditional
                                         build-call
                                         build-void
                                         maybe-name-value!
                                         decorate-source
                                         get-global-definition-hook
                                         put-global-definition-hook
                                         gensym-hook
                                         local-eval-hook
                                         top-level-eval-hook
                                         fx<
                                         fx=
                                         fx-
                                         fx+
                                         set-lambda-meta!
                                         lambda-meta
                                         lambda?
                                         make-dynlet
                                         make-letrec
                                         make-let
                                         make-lambda-case
                                         make-lambda
                                         make-seq
                                         make-primcall
                                         make-call
                                         make-conditional
                                         make-toplevel-define
                                         make-toplevel-set
                                         make-toplevel-ref
                                         make-module-set
                                         make-module-ref
                                         make-lexical-set
                                         make-lexical-ref
                                         make-primitive-ref
                                         make-const
                                         make-void)
                                       ((top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top)
                                        (top))
                                       ("i452"
                                        "i450"
                                        "i448"
                                        "i446"
                                        "i444"
                                        "i442"
                                        "i440"
                                        "i438"
                                        "i436"
                                        "i434"
                                        "i432"
                                        "i430"
                                        "i428"
                                        "i426"
                                        "i424"
                                        "i422"
                                        "i420"
                                        "i418"
                                        "i416"
                                        "i414"
                                        "i412"
                                        "i410"
                                        "i408"
                                        "i406"
                                        "i404"
                                        "i402"
                                        "i400"
                                        "i398"
                                        "i396"
                                        "i394"
                                        "i392"
                                        "i390"
                                        "i388"
                                        "i386"
                                        "i384"
                                        "i383"
                                        "i382"
                                        "i380"
                                        "i379"
                                        "i378"
                                        "i377"
                                        "i376"
                                        "i374"
                                        "i372"
                                        "i370"
                                        "i368"
                                        "i366"
                                        "i364"
                                        "i362"
                                        "i360"
                                        "i357"
                                        "i355"
                                        "i354"
                                        "i353"
                                        "i352"
                                        "i351"
                                        "i350"
                                        "i349"
                                        "i348"
                                        "i347"
                                        "i345"
                                        "i344"
                                        "i342"
                                        "i340"
                                        "i338"
                                        "i336"
                                        "i334"
                                        "i332"
                                        "i330"
                                        "i329"
                                        "i328"
                                        "i327"
                                        "i326"
                                        "i325"
                                        "i323"
                                        "i322"
                                        "i320"
                                        "i318"
                                        "i316"
                                        "i314"
                                        "i312"
                                        "i310"
                                        "i308"
                                        "i306"
                                        "i304"
                                        "i302"
                                        "i300"
                                        "i298"
                                        "i296"
                                        "i294"
                                        "i292"
                                        "i290"
                                        "i288"
                                        "i286"
                                        "i284"
                                        "i282"
                                        "i280"
                                        "i278"
                                        "i276"
                                        "i274"
                                        "i272"
                                        "i270"
                                        "i268"
                                        "i266"
                                        "i264"
                                        "i262"
                                        "i260"
                                        "i258"
                                        "i256"
                                        "i255"
                                        "i253"
                                        "i251"
                                        "i250"
                                        "i249"
                                        "i248"
                                        "i247"
                                        "i245"
                                        "i243"
                                        "i241"
                                        "i238"
                                        "i236"
                                        "i234"
                                        "i232"
                                        "i230"
                                        "i228"
                                        "i226"
                                        "i224"
                                        "i222"
                                        "i220"
                                        "i218"
                                        "i216"
                                        "i214"
                                        "i212"
                                        "i210"
                                        "i208"
                                        "i206"
                                        "i204"
                                        "i202"))
                                     #(ribcage
                                       (define-structure
                                         define-expansion-accessors
                                         define-expansion-constructors)
                                       ((top) (top) (top))
                                       ("i40" "i39" "i38")))
                                    (hygiene guile)))
                             (#{chi 423}#
                               #{exp 3922}#
                               #{r 3907}#
                               '(())
                               #{mod 3908}#)
                             (let ((#{labels 3930}# (list (#{gen-label 356}#)))
                                   (#{var 3931}#
                                     (#{gen-var 451}# #{pat 3921}#)))
                               (#{build-call 267}#
                                 #f
                                 (#{build-simple-lambda 285}#
                                   #f
                                   (list (syntax->datum #{pat 3921}#))
                                   #f
                                   (list #{var 3931}#)
                                   '()
                                   (#{chi 423}#
                                     #{exp 3922}#
                                     (#{extend-env 331}#
                                       #{labels 3930}#
                                       (list (cons 'syntax
                                                   (cons #{var 3931}# 0)))
                                       #{r 3907}#)
                                     (#{make-binding-wrap 387}#
                                       (list #{pat 3921}#)
                                       #{labels 3930}#
                                       '(()))
                                     #{mod 3908}#))
                                 (list #{x 3904}#))))
                           (#{gen-clause 3721}#
                             #{x 3904}#
                             #{keys 3905}#
                             (cdr #{clauses 3906}#)
                             #{r 3907}#
                             #{pat 3921}#
                             #t
                             #{exp 3922}#
                             #{mod 3908}#)))
                       #{tmp 3918}#)
                     (let ((#{tmp 3937}#
                             ($sc-dispatch #{tmp 3917}# '(any any any))))
                       (if #{tmp 3937}#
                         (@apply
                           (lambda (#{pat 3941}# #{fender 3942}# #{exp 3943}#)
                             (#{gen-clause 3721}#
                               #{x 3904}#
                               #{keys 3905}#
                               (cdr #{clauses 3906}#)
                               #{r 3907}#
                               #{pat 3941}#
                               #{fender 3942}#
                               #{exp 3943}#
                               #{mod 3908}#))
                           #{tmp 3937}#)
                         (let ((#{_ 3945}# #{tmp 3917}#))
                           (syntax-violation
                             'syntax-case
                             "invalid clause"
                             (car #{clauses 3906}#))))))))))))
        (lambda (#{e 3946}#
                 #{r 3947}#
                 #{w 3948}#
                 #{s 3949}#
                 #{mod 3950}#)
          (let ((#{e 3957}#
                  (#{source-wrap 411}#
                    #{e 3946}#
                    #{w 3948}#
                    #{s 3949}#
                    #{mod 3950}#)))
            (let ((#{tmp 3958}# #{e 3957}#))
              (let ((#{tmp 3959}#
                      ($sc-dispatch
                        #{tmp 3958}#
                        '(_ any each-any . each-any))))
                (if #{tmp 3959}#
                  (@apply
                    (lambda (#{val 3963}# #{key 3964}# #{m 3965}#)
                      (if (and-map
                            (lambda (#{x 3966}#)
                              (if (#{id? 343}# #{x 3966}#)
                                (not (#{ellipsis? 439}# #{x 3966}#))
                                #f))
                            #{key 3964}#)
                        (let ((#{x 3972}# (#{gen-var 451}# 'tmp)))
                          (#{build-call 267}#
                            #{s 3949}#
                            (#{build-simple-lambda 285}#
                              #f
                              (list 'tmp)
                              #f
                              (list #{x 3972}#)
                              '()
                              (#{gen-syntax-case 3723}#
                                (#{build-lexical-reference 273}#
                                  'value
                                  #f
                                  'tmp
                                  #{x 3972}#)
                                #{key 3964}#
                                #{m 3965}#
                                #{r 3947}#
                                #{mod 3950}#))
                            (list (#{chi 423}#
                                    #{val 3963}#
                                    #{r 3947}#
                                    '(())
                                    #{mod 3950}#))))
                        (syntax-violation
                          'syntax-case
                          "invalid literals list"
                          #{e 3957}#)))
                    #{tmp 3959}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp 3958}#))))))))
    (set! macroexpand
      (lambda*
        (#{x 3978}#
          #:optional
          (#{m 3980}# 'e)
          (#{esew 3982}# '(eval)))
        (#{chi-top-sequence 415}#
          (list #{x 3978}#)
          '()
          '((top))
          #f
          #{m 3980}#
          #{esew 3982}#
          (cons 'hygiene (module-name (current-module))))))
    (set! identifier?
      (lambda (#{x 3986}#)
        (#{nonsymbol-id? 341}# #{x 3986}#)))
    (set! datum->syntax
      (lambda (#{id 3988}# #{datum 3989}#)
        (#{make-syntax-object 307}#
          #{datum 3989}#
          (#{syntax-object-wrap 313}# #{id 3988}#)
          (#{syntax-object-module 315}# #{id 3988}#))))
    (set! syntax->datum
      (lambda (#{x 3992}#)
        (#{strip 449}# #{x 3992}# '(()))))
    (set! syntax-source
      (lambda (#{x 3995}#)
        (#{source-annotation 324}# #{x 3995}#)))
    (set! generate-temporaries
      (lambda (#{ls 3997}#)
        (begin
          (let ((#{x 4001}# #{ls 3997}#))
            (if (not (list? #{x 4001}#))
              (syntax-violation
                'generate-temporaries
                "invalid argument"
                #{x 4001}#)))
          (let ((#{mod 4003}#
                  (cons 'hygiene (module-name (current-module)))))
            (map (lambda (#{x 4004}#)
                   (#{wrap 409}# (gensym) '((top)) #{mod 4003}#))
                 #{ls 3997}#)))))
    (set! free-identifier=?
      (lambda (#{x 4008}# #{y 4009}#)
        (begin
          (let ((#{x 4014}# #{x 4008}#))
            (if (not (#{nonsymbol-id? 341}# #{x 4014}#))
              (syntax-violation
                'free-identifier=?
                "invalid argument"
                #{x 4014}#)))
          (let ((#{x 4017}# #{y 4009}#))
            (if (not (#{nonsymbol-id? 341}# #{x 4017}#))
              (syntax-violation
                'free-identifier=?
                "invalid argument"
                #{x 4017}#)))
          (#{free-id=? 399}# #{x 4008}# #{y 4009}#))))
    (set! bound-identifier=?
      (lambda (#{x 4018}# #{y 4019}#)
        (begin
          (let ((#{x 4024}# #{x 4018}#))
            (if (not (#{nonsymbol-id? 341}# #{x 4024}#))
              (syntax-violation
                'bound-identifier=?
                "invalid argument"
                #{x 4024}#)))
          (let ((#{x 4027}# #{y 4019}#))
            (if (not (#{nonsymbol-id? 341}# #{x 4027}#))
              (syntax-violation
                'bound-identifier=?
                "invalid argument"
                #{x 4027}#)))
          (#{bound-id=? 401}# #{x 4018}# #{y 4019}#))))
    (set! syntax-violation
      (lambda*
        (#{who 4028}#
          #{message 4029}#
          #{form 4030}#
          #:optional
          (#{subform 4034}# #f))
        (begin
          (let ((#{x 4038}# #{who 4028}#))
            (if (not (let ((#{x 4039}# #{x 4038}#))
                       (let ((#{t 4043}# (not #{x 4039}#)))
                         (if #{t 4043}#
                           #{t 4043}#
                           (let ((#{t 4046}# (string? #{x 4039}#)))
                             (if #{t 4046}#
                               #{t 4046}#
                               (symbol? #{x 4039}#)))))))
              (syntax-violation
                'syntax-violation
                "invalid argument"
                #{x 4038}#)))
          (let ((#{x 4050}# #{message 4029}#))
            (if (not (string? #{x 4050}#))
              (syntax-violation
                'syntax-violation
                "invalid argument"
                #{x 4050}#)))
          (throw 'syntax-error
                 #{who 4028}#
                 #{message 4029}#
                 (#{source-annotation 324}#
                   (let ((#{t 4053}# #{form 4030}#))
                     (if #{t 4053}# #{t 4053}# #{subform 4034}#)))
                 (#{strip 449}# #{form 4030}# '(()))
                 (if #{subform 4034}#
                   (#{strip 449}# #{subform 4034}# '(()))
                   #f)))))
    (letrec*
      ((#{match-each 4060}#
         (lambda (#{e 4073}# #{p 4074}# #{w 4075}# #{mod 4076}#)
           (if (pair? #{e 4073}#)
             (let ((#{first 4084}#
                     (#{match 4072}#
                       (car #{e 4073}#)
                       #{p 4074}#
                       #{w 4075}#
                       '()
                       #{mod 4076}#)))
               (if #{first 4084}#
                 (let ((#{rest 4088}#
                         (#{match-each 4060}#
                           (cdr #{e 4073}#)
                           #{p 4074}#
                           #{w 4075}#
                           #{mod 4076}#)))
                   (if #{rest 4088}#
                     (cons #{first 4084}# #{rest 4088}#)
                     #f))
                 #f))
             (if (null? #{e 4073}#)
               '()
               (if (#{syntax-object? 309}# #{e 4073}#)
                 (#{match-each 4060}#
                   (#{syntax-object-expression 311}# #{e 4073}#)
                   #{p 4074}#
                   (#{join-wraps 391}#
                     #{w 4075}#
                     (#{syntax-object-wrap 313}# #{e 4073}#))
                   (#{syntax-object-module 315}# #{e 4073}#))
                 #f)))))
       (#{match-each+ 4062}#
         (lambda (#{e 4096}#
                  #{x-pat 4097}#
                  #{y-pat 4098}#
                  #{z-pat 4099}#
                  #{w 4100}#
                  #{r 4101}#
                  #{mod 4102}#)
           (letrec*
             ((#{f 4113}#
                (lambda (#{e 4114}# #{w 4115}#)
                  (if (pair? #{e 4114}#)
                    (call-with-values
                      (lambda ()
                        (#{f 4113}# (cdr #{e 4114}#) #{w 4115}#))
                      (lambda (#{xr* 4118}# #{y-pat 4119}# #{r 4120}#)
                        (if #{r 4120}#
                          (if (null? #{y-pat 4119}#)
                            (let ((#{xr 4125}#
                                    (#{match 4072}#
                                      (car #{e 4114}#)
                                      #{x-pat 4097}#
                                      #{w 4115}#
                                      '()
                                      #{mod 4102}#)))
                              (if #{xr 4125}#
                                (values
                                  (cons #{xr 4125}# #{xr* 4118}#)
                                  #{y-pat 4119}#
                                  #{r 4120}#)
                                (values #f #f #f)))
                            (values
                              '()
                              (cdr #{y-pat 4119}#)
                              (#{match 4072}#
                                (car #{e 4114}#)
                                (car #{y-pat 4119}#)
                                #{w 4115}#
                                #{r 4120}#
                                #{mod 4102}#)))
                          (values #f #f #f))))
                    (if (#{syntax-object? 309}# #{e 4114}#)
                      (#{f 4113}#
                        (#{syntax-object-expression 311}# #{e 4114}#)
                        (#{join-wraps 391}# #{w 4115}# #{e 4114}#))
                      (values
                        '()
                        #{y-pat 4098}#
                        (#{match 4072}#
                          #{e 4114}#
                          #{z-pat 4099}#
                          #{w 4115}#
                          #{r 4101}#
                          #{mod 4102}#)))))))
             (#{f 4113}# #{e 4096}# #{w 4100}#))))
       (#{match-each-any 4064}#
         (lambda (#{e 4129}# #{w 4130}# #{mod 4131}#)
           (if (pair? #{e 4129}#)
             (let ((#{l 4138}#
                     (#{match-each-any 4064}#
                       (cdr #{e 4129}#)
                       #{w 4130}#
                       #{mod 4131}#)))
               (if #{l 4138}#
                 (cons (#{wrap 409}#
                         (car #{e 4129}#)
                         #{w 4130}#
                         #{mod 4131}#)
                       #{l 4138}#)
                 #f))
             (if (null? #{e 4129}#)
               '()
               (if (#{syntax-object? 309}# #{e 4129}#)
                 (#{match-each-any 4064}#
                   (#{syntax-object-expression 311}# #{e 4129}#)
                   (#{join-wraps 391}#
                     #{w 4130}#
                     (#{syntax-object-wrap 313}# #{e 4129}#))
                   #{mod 4131}#)
                 #f)))))
       (#{match-empty 4066}#
         (lambda (#{p 4146}# #{r 4147}#)
           (if (null? #{p 4146}#)
             #{r 4147}#
             (if (eq? #{p 4146}# '_)
               #{r 4147}#
               (if (eq? #{p 4146}# 'any)
                 (cons '() #{r 4147}#)
                 (if (pair? #{p 4146}#)
                   (#{match-empty 4066}#
                     (car #{p 4146}#)
                     (#{match-empty 4066}#
                       (cdr #{p 4146}#)
                       #{r 4147}#))
                   (if (eq? #{p 4146}# 'each-any)
                     (cons '() #{r 4147}#)
                     (let ((#{atom-key 4163}# (vector-ref #{p 4146}# 0)))
                       (if (memv #{atom-key 4163}# '(each))
                         (#{match-empty 4066}#
                           (vector-ref #{p 4146}# 1)
                           #{r 4147}#)
                         (if (memv #{atom-key 4163}# '(each+))
                           (#{match-empty 4066}#
                             (vector-ref #{p 4146}# 1)
                             (#{match-empty 4066}#
                               (reverse (vector-ref #{p 4146}# 2))
                               (#{match-empty 4066}#
                                 (vector-ref #{p 4146}# 3)
                                 #{r 4147}#)))
                           (if (memv #{atom-key 4163}# '(free-id atom))
                             #{r 4147}#
                             (if (memv #{atom-key 4163}# '(vector))
                               (#{match-empty 4066}#
                                 (vector-ref #{p 4146}# 1)
                                 #{r 4147}#)))))))))))))
       (#{combine 4068}#
         (lambda (#{r* 4168}# #{r 4169}#)
           (if (null? (car #{r* 4168}#))
             #{r 4169}#
             (cons (map car #{r* 4168}#)
                   (#{combine 4068}#
                     (map cdr #{r* 4168}#)
                     #{r 4169}#)))))
       (#{match* 4070}#
         (lambda (#{e 4172}#
                  #{p 4173}#
                  #{w 4174}#
                  #{r 4175}#
                  #{mod 4176}#)
           (if (null? #{p 4173}#)
             (if (null? #{e 4172}#) #{r 4175}# #f)
             (if (pair? #{p 4173}#)
               (if (pair? #{e 4172}#)
                 (#{match 4072}#
                   (car #{e 4172}#)
                   (car #{p 4173}#)
                   #{w 4174}#
                   (#{match 4072}#
                     (cdr #{e 4172}#)
                     (cdr #{p 4173}#)
                     #{w 4174}#
                     #{r 4175}#
                     #{mod 4176}#)
                   #{mod 4176}#)
                 #f)
               (if (eq? #{p 4173}# 'each-any)
                 (let ((#{l 4193}#
                         (#{match-each-any 4064}#
                           #{e 4172}#
                           #{w 4174}#
                           #{mod 4176}#)))
                   (if #{l 4193}# (cons #{l 4193}# #{r 4175}#) #f))
                 (let ((#{atom-key 4199}# (vector-ref #{p 4173}# 0)))
                   (if (memv #{atom-key 4199}# '(each))
                     (if (null? #{e 4172}#)
                       (#{match-empty 4066}#
                         (vector-ref #{p 4173}# 1)
                         #{r 4175}#)
                       (let ((#{l 4202}#
                               (#{match-each 4060}#
                                 #{e 4172}#
                                 (vector-ref #{p 4173}# 1)
                                 #{w 4174}#
                                 #{mod 4176}#)))
                         (if #{l 4202}#
                           (letrec*
                             ((#{collect 4207}#
                                (lambda (#{l 4208}#)
                                  (if (null? (car #{l 4208}#))
                                    #{r 4175}#
                                    (cons (map car #{l 4208}#)
                                          (#{collect 4207}#
                                            (map cdr #{l 4208}#)))))))
                             (#{collect 4207}# #{l 4202}#))
                           #f)))
                     (if (memv #{atom-key 4199}# '(each+))
                       (call-with-values
                         (lambda ()
                           (#{match-each+ 4062}#
                             #{e 4172}#
                             (vector-ref #{p 4173}# 1)
                             (vector-ref #{p 4173}# 2)
                             (vector-ref #{p 4173}# 3)
                             #{w 4174}#
                             #{r 4175}#
                             #{mod 4176}#))
                         (lambda (#{xr* 4210}# #{y-pat 4211}# #{r 4212}#)
                           (if #{r 4212}#
                             (if (null? #{y-pat 4211}#)
                               (if (null? #{xr* 4210}#)
                                 (#{match-empty 4066}#
                                   (vector-ref #{p 4173}# 1)
                                   #{r 4212}#)
                                 (#{combine 4068}# #{xr* 4210}# #{r 4212}#))
                               #f)
                             #f)))
                       (if (memv #{atom-key 4199}# '(free-id))
                         (if (#{id? 343}# #{e 4172}#)
                           (if (#{free-id=? 399}#
                                 (#{wrap 409}#
                                   #{e 4172}#
                                   #{w 4174}#
                                   #{mod 4176}#)
                                 (vector-ref #{p 4173}# 1))
                             #{r 4175}#
                             #f)
                           #f)
                         (if (memv #{atom-key 4199}# '(atom))
                           (if (equal?
                                 (vector-ref #{p 4173}# 1)
                                 (#{strip 449}# #{e 4172}# #{w 4174}#))
                             #{r 4175}#
                             #f)
                           (if (memv #{atom-key 4199}# '(vector))
                             (if (vector? #{e 4172}#)
                               (#{match 4072}#
                                 (vector->list #{e 4172}#)
                                 (vector-ref #{p 4173}# 1)
                                 #{w 4174}#
                                 #{r 4175}#
                                 #{mod 4176}#)
                               #f))))))))))))
       (#{match 4072}#
         (lambda (#{e 4229}#
                  #{p 4230}#
                  #{w 4231}#
                  #{r 4232}#
                  #{mod 4233}#)
           (if (not #{r 4232}#)
             #f
             (if (eq? #{p 4230}# '_)
               #{r 4232}#
               (if (eq? #{p 4230}# 'any)
                 (cons (#{wrap 409}# #{e 4229}# #{w 4231}# #{mod 4233}#)
                       #{r 4232}#)
                 (if (#{syntax-object? 309}# #{e 4229}#)
                   (#{match* 4070}#
                     (#{syntax-object-expression 311}# #{e 4229}#)
                     #{p 4230}#
                     (#{join-wraps 391}#
                       #{w 4231}#
                       (#{syntax-object-wrap 313}# #{e 4229}#))
                     #{r 4232}#
                     (#{syntax-object-module 315}# #{e 4229}#))
                   (#{match* 4070}#
                     #{e 4229}#
                     #{p 4230}#
                     #{w 4231}#
                     #{r 4232}#
                     #{mod 4233}#))))))))
      (set! $sc-dispatch
        (lambda (#{e 4248}# #{p 4249}#)
          (if (eq? #{p 4249}# 'any)
            (list #{e 4248}#)
            (if (eq? #{p 4249}# '_)
              '()
              (if (#{syntax-object? 309}# #{e 4248}#)
                (#{match* 4070}#
                  (#{syntax-object-expression 311}# #{e 4248}#)
                  #{p 4249}#
                  (#{syntax-object-wrap 313}# #{e 4248}#)
                  '()
                  (#{syntax-object-module 315}# #{e 4248}#))
                (#{match* 4070}#
                  #{e 4248}#
                  #{p 4249}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syntax-transformer
    'with-syntax
    'macro
    (lambda (#{x 4260}#)
      (let ((#{tmp 4262}# #{x 4260}#))
        (let ((#{tmp 4263}#
                ($sc-dispatch
                  #{tmp 4262}#
                  '(_ () any . each-any))))
          (if #{tmp 4263}#
            (@apply
              (lambda (#{e1 4266}# #{e2 4267}#)
                (cons '#(syntax-object
                         let
                         ((top)
                          #(ribcage
                            #(e1 e2)
                            #((top) (top))
                            #("i4264" "i4265"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4261")))
                         (hygiene guile))
                      (cons '() (cons #{e1 4266}# #{e2 4267}#))))
              #{tmp 4263}#)
            (let ((#{tmp 4269}#
                    ($sc-dispatch
                      #{tmp 4262}#
                      '(_ ((any any)) any . each-any))))
              (if #{tmp 4269}#
                (@apply
                  (lambda (#{out 4274}#
                           #{in 4275}#
                           #{e1 4276}#
                           #{e2 4277}#)
                    (list '#(syntax-object
                             syntax-case
                             ((top)
                              #(ribcage
                                #(out in e1 e2)
                                #((top) (top) (top) (top))
                                #("i4270" "i4271" "i4272" "i4273"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4261")))
                             (hygiene guile))
                          #{in 4275}#
                          '()
                          (list #{out 4274}#
                                (cons '#(syntax-object
                                         let
                                         ((top)
                                          #(ribcage
                                            #(out in e1 e2)
                                            #((top) (top) (top) (top))
                                            #("i4270" "i4271" "i4272" "i4273"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4261")))
                                         (hygiene guile))
                                      (cons '()
                                            (cons #{e1 4276}# #{e2 4277}#))))))
                  #{tmp 4269}#)
                (let ((#{tmp 4279}#
                        ($sc-dispatch
                          #{tmp 4262}#
                          '(_ #(each (any any)) any . each-any))))
                  (if #{tmp 4279}#
                    (@apply
                      (lambda (#{out 4284}#
                               #{in 4285}#
                               #{e1 4286}#
                               #{e2 4287}#)
                        (list '#(syntax-object
                                 syntax-case
                                 ((top)
                                  #(ribcage
                                    #(out in e1 e2)
                                    #((top) (top) (top) (top))
                                    #("i4280" "i4281" "i4282" "i4283"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4261")))
                                 (hygiene guile))
                              (cons '#(syntax-object
                                       list
                                       ((top)
                                        #(ribcage
                                          #(out in e1 e2)
                                          #((top) (top) (top) (top))
                                          #("i4280" "i4281" "i4282" "i4283"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4261")))
                                       (hygiene guile))
                                    #{in 4285}#)
                              '()
                              (list #{out 4284}#
                                    (cons '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage
                                                #(out in e1 e2)
                                                #((top) (top) (top) (top))
                                                #("i4280"
                                                  "i4281"
                                                  "i4282"
                                                  "i4283"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i4261")))
                                             (hygiene guile))
                                          (cons '()
                                                (cons #{e1 4286}#
                                                      #{e2 4287}#))))))
                      #{tmp 4279}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp 4262}#)))))))))))

(define syntax-rules
  (make-syntax-transformer
    'syntax-rules
    'macro
    (lambda (#{x 4291}#)
      (let ((#{tmp 4293}# #{x 4291}#))
        (let ((#{tmp 4294}#
                ($sc-dispatch
                  #{tmp 4293}#
                  '(_ each-any . #(each ((any . any) any))))))
          (if #{tmp 4294}#
            (@apply
              (lambda (#{k 4299}#
                       #{keyword 4300}#
                       #{pattern 4301}#
                       #{template 4302}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(k keyword pattern template)
                            #((top) (top) (top) (top))
                            #("i4295" "i4296" "i4297" "i4298"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4292")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(k keyword pattern template)
                             #((top) (top) (top) (top))
                             #("i4295" "i4296" "i4297" "i4298"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4292")))
                          (hygiene guile)))
                      (vector
                        '(#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(k keyword pattern template)
                               #((top) (top) (top) (top))
                               #("i4295" "i4296" "i4297" "i4298"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4292")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            syntax-rules
                            ((top)
                             #(ribcage
                               #(k keyword pattern template)
                               #((top) (top) (top) (top))
                               #("i4295" "i4296" "i4297" "i4298"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4292")))
                            (hygiene guile)))
                        (cons '#(syntax-object
                                 patterns
                                 ((top)
                                  #(ribcage
                                    #(k keyword pattern template)
                                    #((top) (top) (top) (top))
                                    #("i4295" "i4296" "i4297" "i4298"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4292")))
                                 (hygiene guile))
                              #{pattern 4301}#))
                      (cons '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(k keyword pattern template)
                                  #((top) (top) (top) (top))
                                  #("i4295" "i4296" "i4297" "i4298"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4292")))
                               (hygiene guile))
                            (cons '#(syntax-object
                                     x
                                     ((top)
                                      #(ribcage
                                        #(k keyword pattern template)
                                        #((top) (top) (top) (top))
                                        #("i4295" "i4296" "i4297" "i4298"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4292")))
                                     (hygiene guile))
                                  (cons #{k 4299}#
                                        (map (lambda (#{tmp 4306}#
                                                      #{tmp 4305}#)
                                               (list (cons '#(syntax-object
                                                              dummy
                                                              ((top)
                                                               #(ribcage
                                                                 #(k
                                                                   keyword
                                                                   pattern
                                                                   template)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4295"
                                                                   "i4296"
                                                                   "i4297"
                                                                   "i4298"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4292")))
                                                              (hygiene guile))
                                                           #{tmp 4305}#)
                                                     (list '#(syntax-object
                                                              syntax
                                                              ((top)
                                                               #(ribcage
                                                                 #(k
                                                                   keyword
                                                                   pattern
                                                                   template)
                                                                 #((top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4295"
                                                                   "i4296"
                                                                   "i4297"
                                                                   "i4298"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4292")))
                                                              (hygiene guile))
                                                           #{tmp 4306}#)))
                                             #{template 4302}#
                                             #{pattern 4301}#))))))
              #{tmp 4294}#)
            (let ((#{tmp 4307}#
                    ($sc-dispatch
                      #{tmp 4293}#
                      '(_ each-any any . #(each ((any . any) any))))))
              (if (if #{tmp 4307}#
                    (@apply
                      (lambda (#{k 4313}#
                               #{docstring 4314}#
                               #{keyword 4315}#
                               #{pattern 4316}#
                               #{template 4317}#)
                        (string? (syntax->datum #{docstring 4314}#)))
                      #{tmp 4307}#)
                    #f)
                (@apply
                  (lambda (#{k 4323}#
                           #{docstring 4324}#
                           #{keyword 4325}#
                           #{pattern 4326}#
                           #{template 4327}#)
                    (list '#(syntax-object
                             lambda
                             ((top)
                              #(ribcage
                                #(k docstring keyword pattern template)
                                #((top) (top) (top) (top) (top))
                                #("i4318" "i4319" "i4320" "i4321" "i4322"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4292")))
                             (hygiene guile))
                          '(#(syntax-object
                              x
                              ((top)
                               #(ribcage
                                 #(k docstring keyword pattern template)
                                 #((top) (top) (top) (top) (top))
                                 #("i4318" "i4319" "i4320" "i4321" "i4322"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4292")))
                              (hygiene guile)))
                          #{docstring 4324}#
                          (vector
                            '(#(syntax-object
                                macro-type
                                ((top)
                                 #(ribcage
                                   #(k docstring keyword pattern template)
                                   #((top) (top) (top) (top) (top))
                                   #("i4318" "i4319" "i4320" "i4321" "i4322"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4292")))
                                (hygiene guile))
                              .
                              #(syntax-object
                                syntax-rules
                                ((top)
                                 #(ribcage
                                   #(k docstring keyword pattern template)
                                   #((top) (top) (top) (top) (top))
                                   #("i4318" "i4319" "i4320" "i4321" "i4322"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4292")))
                                (hygiene guile)))
                            (cons '#(syntax-object
                                     patterns
                                     ((top)
                                      #(ribcage
                                        #(k docstring keyword pattern template)
                                        #((top) (top) (top) (top) (top))
                                        #("i4318"
                                          "i4319"
                                          "i4320"
                                          "i4321"
                                          "i4322"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4292")))
                                     (hygiene guile))
                                  #{pattern 4326}#))
                          (cons '#(syntax-object
                                   syntax-case
                                   ((top)
                                    #(ribcage
                                      #(k docstring keyword pattern template)
                                      #((top) (top) (top) (top) (top))
                                      #("i4318"
                                        "i4319"
                                        "i4320"
                                        "i4321"
                                        "i4322"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i4292")))
                                   (hygiene guile))
                                (cons '#(syntax-object
                                         x
                                         ((top)
                                          #(ribcage
                                            #(k
                                              docstring
                                              keyword
                                              pattern
                                              template)
                                            #((top) (top) (top) (top) (top))
                                            #("i4318"
                                              "i4319"
                                              "i4320"
                                              "i4321"
                                              "i4322"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4292")))
                                         (hygiene guile))
                                      (cons #{k 4323}#
                                            (map (lambda (#{tmp 4331}#
                                                          #{tmp 4330}#)
                                                   (list (cons '#(syntax-object
                                                                  dummy
                                                                  ((top)
                                                                   #(ribcage
                                                                     #(k
                                                                       docstring
                                                                       keyword
                                                                       pattern
                                                                       template)
                                                                     #((top)
                                                                       (top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                     #("i4318"
                                                                       "i4319"
                                                                       "i4320"
                                                                       "i4321"
                                                                       "i4322"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(x)
                                                                     #((top))
                                                                     #("i4292")))
                                                                  (hygiene
                                                                    guile))
                                                               #{tmp 4330}#)
                                                         (list '#(syntax-object
                                                                  syntax
                                                                  ((top)
                                                                   #(ribcage
                                                                     #(k
                                                                       docstring
                                                                       keyword
                                                                       pattern
                                                                       template)
                                                                     #((top)
                                                                       (top)
                                                                       (top)
                                                                       (top)
                                                                       (top))
                                                                     #("i4318"
                                                                       "i4319"
                                                                       "i4320"
                                                                       "i4321"
                                                                       "i4322"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(x)
                                                                     #((top))
                                                                     #("i4292")))
                                                                  (hygiene
                                                                    guile))
                                                               #{tmp 4331}#)))
                                                 #{template 4327}#
                                                 #{pattern 4326}#))))))
                  #{tmp 4307}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4293}#)))))))))

(define let*
  (make-syntax-transformer
    'let*
    'macro
    (lambda (#{x 4332}#)
      (let ((#{tmp 4334}# #{x 4332}#))
        (let ((#{tmp 4335}#
                ($sc-dispatch
                  #{tmp 4334}#
                  '(any #(each (any any)) any . each-any))))
          (if (if #{tmp 4335}#
                (@apply
                  (lambda (#{let* 4341}#
                           #{x 4342}#
                           #{v 4343}#
                           #{e1 4344}#
                           #{e2 4345}#)
                    (and-map identifier? #{x 4342}#))
                  #{tmp 4335}#)
                #f)
            (@apply
              (lambda (#{let* 4352}#
                       #{x 4353}#
                       #{v 4354}#
                       #{e1 4355}#
                       #{e2 4356}#)
                (letrec*
                  ((#{f 4359}#
                     (lambda (#{bindings 4360}#)
                       (if (null? #{bindings 4360}#)
                         (cons '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(f bindings)
                                     #((top) (top))
                                     #("i4357" "i4358"))
                                   #(ribcage
                                     #(let* x v e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("i4347"
                                       "i4348"
                                       "i4349"
                                       "i4350"
                                       "i4351"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i4333")))
                                  (hygiene guile))
                               (cons '() (cons #{e1 4355}# #{e2 4356}#)))
                         (let ((#{tmp 4365}#
                                 (list (#{f 4359}# (cdr #{bindings 4360}#))
                                       (car #{bindings 4360}#))))
                           (let ((#{tmp 4366}#
                                   ($sc-dispatch #{tmp 4365}# '(any any))))
                             (if #{tmp 4366}#
                               (@apply
                                 (lambda (#{body 4369}# #{binding 4370}#)
                                   (list '#(syntax-object
                                            let
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(body binding)
                                               #((top) (top))
                                               #("i4367" "i4368"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(f bindings)
                                               #((top) (top))
                                               #("i4357" "i4358"))
                                             #(ribcage
                                               #(let* x v e1 e2)
                                               #((top) (top) (top) (top) (top))
                                               #("i4347"
                                                 "i4348"
                                                 "i4349"
                                                 "i4350"
                                                 "i4351"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4333")))
                                            (hygiene guile))
                                         (list #{binding 4370}#)
                                         #{body 4369}#))
                                 #{tmp 4366}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4365}#))))))))
                  (#{f 4359}# (map list #{x 4353}# #{v 4354}#))))
              #{tmp 4335}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4334}#)))))))

(define do
  (make-syntax-transformer
    'do
    'macro
    (lambda (#{orig-x 4371}#)
      (let ((#{tmp 4373}# #{orig-x 4371}#))
        (let ((#{tmp 4374}#
                ($sc-dispatch
                  #{tmp 4373}#
                  '(_ #(each (any any . any))
                      (any . each-any)
                      .
                      each-any))))
          (if #{tmp 4374}#
            (@apply
              (lambda (#{var 4381}#
                       #{init 4382}#
                       #{step 4383}#
                       #{e0 4384}#
                       #{e1 4385}#
                       #{c 4386}#)
                (let ((#{tmp 4388}#
                        (map (lambda (#{v 4409}# #{s 4410}#)
                               (let ((#{tmp 4413}# #{s 4410}#))
                                 (let ((#{tmp 4414}#
                                         ($sc-dispatch #{tmp 4413}# '())))
                                   (if #{tmp 4414}#
                                     (@apply
                                       (lambda () #{v 4409}#)
                                       #{tmp 4414}#)
                                     (let ((#{tmp 4415}#
                                             ($sc-dispatch
                                               #{tmp 4413}#
                                               '(any))))
                                       (if #{tmp 4415}#
                                         (@apply
                                           (lambda (#{e 4417}#) #{e 4417}#)
                                           #{tmp 4415}#)
                                         (let ((#{_ 4419}# #{tmp 4413}#))
                                           (syntax-violation
                                             'do
                                             "bad step expression"
                                             #{orig-x 4371}#
                                             #{s 4410}#))))))))
                             #{var 4381}#
                             #{step 4383}#)))
                  (let ((#{tmp 4389}#
                          ($sc-dispatch #{tmp 4388}# 'each-any)))
                    (if #{tmp 4389}#
                      (@apply
                        (lambda (#{step 4391}#)
                          (let ((#{tmp 4392}# #{e1 4385}#))
                            (let ((#{tmp 4393}#
                                    ($sc-dispatch #{tmp 4392}# '())))
                              (if #{tmp 4393}#
                                (@apply
                                  (lambda ()
                                    (list '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4390"))
                                              #(ribcage
                                                #(var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4375"
                                                  "i4376"
                                                  "i4377"
                                                  "i4378"
                                                  "i4379"
                                                  "i4380"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4372")))
                                             (hygiene guile))
                                          '#(syntax-object
                                             doloop
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4390"))
                                              #(ribcage
                                                #(var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4375"
                                                  "i4376"
                                                  "i4377"
                                                  "i4378"
                                                  "i4379"
                                                  "i4380"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4372")))
                                             (hygiene guile))
                                          (map list #{var 4381}# #{init 4382}#)
                                          (list '#(syntax-object
                                                   if
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(step)
                                                      #((top))
                                                      #("i4390"))
                                                    #(ribcage
                                                      #(var init step e0 e1 c)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i4375"
                                                        "i4376"
                                                        "i4377"
                                                        "i4378"
                                                        "i4379"
                                                        "i4380"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(orig-x)
                                                      #((top))
                                                      #("i4372")))
                                                   (hygiene guile))
                                                (list '#(syntax-object
                                                         not
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4390"))
                                                          #(ribcage
                                                            #(var
                                                              init
                                                              step
                                                              e0
                                                              e1
                                                              c)
                                                            #((top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top))
                                                            #("i4375"
                                                              "i4376"
                                                              "i4377"
                                                              "i4378"
                                                              "i4379"
                                                              "i4380"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4372")))
                                                         (hygiene guile))
                                                      #{e0 4384}#)
                                                (cons '#(syntax-object
                                                         begin
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4390"))
                                                          #(ribcage
                                                            #(var
                                                              init
                                                              step
                                                              e0
                                                              e1
                                                              c)
                                                            #((top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top)
                                                              (top))
                                                            #("i4375"
                                                              "i4376"
                                                              "i4377"
                                                              "i4378"
                                                              "i4379"
                                                              "i4380"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4372")))
                                                         (hygiene guile))
                                                      (append
                                                        #{c 4386}#
                                                        (list (cons '#(syntax-object
                                                                       doloop
                                                                       ((top)
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(step)
                                                                          #((top))
                                                                          #("i4390"))
                                                                        #(ribcage
                                                                          #(var
                                                                            init
                                                                            step
                                                                            e0
                                                                            e1
                                                                            c)
                                                                          #((top)
                                                                            (top)
                                                                            (top)
                                                                            (top)
                                                                            (top)
                                                                            (top))
                                                                          #("i4375"
                                                                            "i4376"
                                                                            "i4377"
                                                                            "i4378"
                                                                            "i4379"
                                                                            "i4380"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(orig-x)
                                                                          #((top))
                                                                          #("i4372")))
                                                                       (hygiene
                                                                         guile))
                                                                    #{step 4391}#)))))))
                                  #{tmp 4393}#)
                                (let ((#{tmp 4398}#
                                        ($sc-dispatch
                                          #{tmp 4392}#
                                          '(any . each-any))))
                                  (if #{tmp 4398}#
                                    (@apply
                                      (lambda (#{e1 4401}# #{e2 4402}#)
                                        (list '#(syntax-object
                                                 let
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4399" "i4400"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4390"))
                                                  #(ribcage
                                                    #(var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4375"
                                                      "i4376"
                                                      "i4377"
                                                      "i4378"
                                                      "i4379"
                                                      "i4380"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4372")))
                                                 (hygiene guile))
                                              '#(syntax-object
                                                 doloop
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4399" "i4400"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4390"))
                                                  #(ribcage
                                                    #(var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4375"
                                                      "i4376"
                                                      "i4377"
                                                      "i4378"
                                                      "i4379"
                                                      "i4380"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4372")))
                                                 (hygiene guile))
                                              (map list
                                                   #{var 4381}#
                                                   #{init 4382}#)
                                              (list '#(syntax-object
                                                       if
                                                       ((top)
                                                        #(ribcage
                                                          #(e1 e2)
                                                          #((top) (top))
                                                          #("i4399" "i4400"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(step)
                                                          #((top))
                                                          #("i4390"))
                                                        #(ribcage
                                                          #(var
                                                            init
                                                            step
                                                            e0
                                                            e1
                                                            c)
                                                          #((top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                          #("i4375"
                                                            "i4376"
                                                            "i4377"
                                                            "i4378"
                                                            "i4379"
                                                            "i4380"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(orig-x)
                                                          #((top))
                                                          #("i4372")))
                                                       (hygiene guile))
                                                    #{e0 4384}#
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4399"
                                                                  "i4400"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4390"))
                                                              #(ribcage
                                                                #(var
                                                                  init
                                                                  step
                                                                  e0
                                                                  e1
                                                                  c)
                                                                #((top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top))
                                                                #("i4375"
                                                                  "i4376"
                                                                  "i4377"
                                                                  "i4378"
                                                                  "i4379"
                                                                  "i4380"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4372")))
                                                             (hygiene guile))
                                                          (cons #{e1 4401}#
                                                                #{e2 4402}#))
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4399"
                                                                  "i4400"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4390"))
                                                              #(ribcage
                                                                #(var
                                                                  init
                                                                  step
                                                                  e0
                                                                  e1
                                                                  c)
                                                                #((top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top)
                                                                  (top))
                                                                #("i4375"
                                                                  "i4376"
                                                                  "i4377"
                                                                  "i4378"
                                                                  "i4379"
                                                                  "i4380"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4372")))
                                                             (hygiene guile))
                                                          (append
                                                            #{c 4386}#
                                                            (list (cons '#(syntax-object
                                                                           doloop
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(e1
                                                                                e2)
                                                                              #((top)
                                                                                (top))
                                                                              #("i4399"
                                                                                "i4400"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(step)
                                                                              #((top))
                                                                              #("i4390"))
                                                                            #(ribcage
                                                                              #(var
                                                                                init
                                                                                step
                                                                                e0
                                                                                e1
                                                                                c)
                                                                              #((top)
                                                                                (top)
                                                                                (top)
                                                                                (top)
                                                                                (top)
                                                                                (top))
                                                                              #("i4375"
                                                                                "i4376"
                                                                                "i4377"
                                                                                "i4378"
                                                                                "i4379"
                                                                                "i4380"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(orig-x)
                                                                              #((top))
                                                                              #("i4372")))
                                                                           (hygiene
                                                                             guile))
                                                                        #{step 4391}#)))))))
                                      #{tmp 4398}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp 4392}#)))))))
                        #{tmp 4389}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp 4388}#)))))
              #{tmp 4374}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4373}#)))))))

(define quasiquote
  (make-syntax-transformer
    'quasiquote
    'macro
    (letrec*
      ((#{quasi 4423}#
         (lambda (#{p 4436}# #{lev 4437}#)
           (let ((#{tmp 4440}# #{p 4436}#))
             (let ((#{tmp 4441}#
                     ($sc-dispatch
                       #{tmp 4440}#
                       '(#(free-id
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4438" "i4439"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4434"
                                 "i4432"
                                 "i4430"
                                 "i4428"
                                 "i4426"
                                 "i4424"
                                 "i4422")))
                             (hygiene guile)))
                         any))))
               (if #{tmp 4441}#
                 (@apply
                   (lambda (#{p 4443}#)
                     (if (= #{lev 4437}# 0)
                       (list '#(syntax-object
                                "value"
                                ((top)
                                 #(ribcage #(p) #((top)) #("i4442"))
                                 #(ribcage () () ())
                                 #(ribcage
                                   #(p lev)
                                   #((top) (top))
                                   #("i4438" "i4439"))
                                 #(ribcage
                                   (emit quasivector
                                         quasilist*
                                         quasiappend
                                         quasicons
                                         vquasi
                                         quasi)
                                   ((top) (top) (top) (top) (top) (top) (top))
                                   ("i4434"
                                    "i4432"
                                    "i4430"
                                    "i4428"
                                    "i4426"
                                    "i4424"
                                    "i4422")))
                                (hygiene guile))
                             #{p 4443}#)
                       (#{quasicons 4427}#
                         '(#(syntax-object
                             "quote"
                             ((top)
                              #(ribcage #(p) #((top)) #("i4442"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4438" "i4439"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4434"
                                 "i4432"
                                 "i4430"
                                 "i4428"
                                 "i4426"
                                 "i4424"
                                 "i4422")))
                             (hygiene guile))
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage #(p) #((top)) #("i4442"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4438" "i4439"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4434"
                                 "i4432"
                                 "i4430"
                                 "i4428"
                                 "i4426"
                                 "i4424"
                                 "i4422")))
                             (hygiene guile)))
                         (#{quasi 4423}#
                           (list #{p 4443}#)
                           (#{1-}# #{lev 4437}#)))))
                   #{tmp 4441}#)
                 (let ((#{tmp 4444}#
                         ($sc-dispatch
                           #{tmp 4440}#
                           '(#(free-id
                               #(syntax-object
                                 quasiquote
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage
                                    #(p lev)
                                    #((top) (top))
                                    #("i4438" "i4439"))
                                  #(ribcage
                                    (emit quasivector
                                          quasilist*
                                          quasiappend
                                          quasicons
                                          vquasi
                                          quasi)
                                    ((top) (top) (top) (top) (top) (top) (top))
                                    ("i4434"
                                     "i4432"
                                     "i4430"
                                     "i4428"
                                     "i4426"
                                     "i4424"
                                     "i4422")))
                                 (hygiene guile)))
                             any))))
                   (if #{tmp 4444}#
                     (@apply
                       (lambda (#{p 4446}#)
                         (#{quasicons 4427}#
                           '(#(syntax-object
                               "quote"
                               ((top)
                                #(ribcage #(p) #((top)) #("i4445"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(p lev)
                                  #((top) (top))
                                  #("i4438" "i4439"))
                                #(ribcage
                                  (emit quasivector
                                        quasilist*
                                        quasiappend
                                        quasicons
                                        vquasi
                                        quasi)
                                  ((top) (top) (top) (top) (top) (top) (top))
                                  ("i4434"
                                   "i4432"
                                   "i4430"
                                   "i4428"
                                   "i4426"
                                   "i4424"
                                   "i4422")))
                               (hygiene guile))
                             #(syntax-object
                               quasiquote
                               ((top)
                                #(ribcage #(p) #((top)) #("i4445"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(p lev)
                                  #((top) (top))
                                  #("i4438" "i4439"))
                                #(ribcage
                                  (emit quasivector
                                        quasilist*
                                        quasiappend
                                        quasicons
                                        vquasi
                                        quasi)
                                  ((top) (top) (top) (top) (top) (top) (top))
                                  ("i4434"
                                   "i4432"
                                   "i4430"
                                   "i4428"
                                   "i4426"
                                   "i4424"
                                   "i4422")))
                               (hygiene guile)))
                           (#{quasi 4423}#
                             (list #{p 4446}#)
                             (#{1+}# #{lev 4437}#))))
                       #{tmp 4444}#)
                     (let ((#{tmp 4447}#
                             ($sc-dispatch #{tmp 4440}# '(any . any))))
                       (if #{tmp 4447}#
                         (@apply
                           (lambda (#{p 4450}# #{q 4451}#)
                             (let ((#{tmp 4452}# #{p 4450}#))
                               (let ((#{tmp 4453}#
                                       ($sc-dispatch
                                         #{tmp 4452}#
                                         '(#(free-id
                                             #(syntax-object
                                               unquote
                                               ((top)
                                                #(ribcage
                                                  #(p q)
                                                  #((top) (top))
                                                  #("i4448" "i4449"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(p lev)
                                                  #((top) (top))
                                                  #("i4438" "i4439"))
                                                #(ribcage
                                                  (emit quasivector
                                                        quasilist*
                                                        quasiappend
                                                        quasicons
                                                        vquasi
                                                        quasi)
                                                  ((top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top)
                                                   (top))
                                                  ("i4434"
                                                   "i4432"
                                                   "i4430"
                                                   "i4428"
                                                   "i4426"
                                                   "i4424"
                                                   "i4422")))
                                               (hygiene guile)))
                                           .
                                           each-any))))
                                 (if #{tmp 4453}#
                                   (@apply
                                     (lambda (#{p 4455}#)
                                       (if (= #{lev 4437}# 0)
                                         (#{quasilist* 4431}#
                                           (map (lambda (#{tmp 4456}#)
                                                  (list '#(syntax-object
                                                           "value"
                                                           ((top)
                                                            #(ribcage
                                                              #(p)
                                                              #((top))
                                                              #("i4454"))
                                                            #(ribcage
                                                              #(p q)
                                                              #((top) (top))
                                                              #("i4448"
                                                                "i4449"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(p lev)
                                                              #((top) (top))
                                                              #("i4438"
                                                                "i4439"))
                                                            #(ribcage
                                                              (emit quasivector
                                                                    quasilist*
                                                                    quasiappend
                                                                    quasicons
                                                                    vquasi
                                                                    quasi)
                                                              ((top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top)
                                                               (top))
                                                              ("i4434"
                                                               "i4432"
                                                               "i4430"
                                                               "i4428"
                                                               "i4426"
                                                               "i4424"
                                                               "i4422")))
                                                           (hygiene guile))
                                                        #{tmp 4456}#))
                                                #{p 4455}#)
                                           (#{quasi 4423}#
                                             #{q 4451}#
                                             #{lev 4437}#))
                                         (#{quasicons 4427}#
                                           (#{quasicons 4427}#
                                             '(#(syntax-object
                                                 "quote"
                                                 ((top)
                                                  #(ribcage
                                                    #(p)
                                                    #((top))
                                                    #("i4454"))
                                                  #(ribcage
                                                    #(p q)
                                                    #((top) (top))
                                                    #("i4448" "i4449"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(p lev)
                                                    #((top) (top))
                                                    #("i4438" "i4439"))
                                                  #(ribcage
                                                    (emit quasivector
                                                          quasilist*
                                                          quasiappend
                                                          quasicons
                                                          vquasi
                                                          quasi)
                                                    ((top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top))
                                                    ("i4434"
                                                     "i4432"
                                                     "i4430"
                                                     "i4428"
                                                     "i4426"
                                                     "i4424"
                                                     "i4422")))
                                                 (hygiene guile))
                                               #(syntax-object
                                                 unquote
                                                 ((top)
                                                  #(ribcage
                                                    #(p)
                                                    #((top))
                                                    #("i4454"))
                                                  #(ribcage
                                                    #(p q)
                                                    #((top) (top))
                                                    #("i4448" "i4449"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(p lev)
                                                    #((top) (top))
                                                    #("i4438" "i4439"))
                                                  #(ribcage
                                                    (emit quasivector
                                                          quasilist*
                                                          quasiappend
                                                          quasicons
                                                          vquasi
                                                          quasi)
                                                    ((top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top))
                                                    ("i4434"
                                                     "i4432"
                                                     "i4430"
                                                     "i4428"
                                                     "i4426"
                                                     "i4424"
                                                     "i4422")))
                                                 (hygiene guile)))
                                             (#{quasi 4423}#
                                               #{p 4455}#
                                               (#{1-}# #{lev 4437}#)))
                                           (#{quasi 4423}#
                                             #{q 4451}#
                                             #{lev 4437}#))))
                                     #{tmp 4453}#)
                                   (let ((#{tmp 4458}#
                                           ($sc-dispatch
                                             #{tmp 4452}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   unquote-splicing
                                                   ((top)
                                                    #(ribcage
                                                      #(p q)
                                                      #((top) (top))
                                                      #("i4448" "i4449"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(p lev)
                                                      #((top) (top))
                                                      #("i4438" "i4439"))
                                                    #(ribcage
                                                      (emit quasivector
                                                            quasilist*
                                                            quasiappend
                                                            quasicons
                                                            vquasi
                                                            quasi)
                                                      ((top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top))
                                                      ("i4434"
                                                       "i4432"
                                                       "i4430"
                                                       "i4428"
                                                       "i4426"
                                                       "i4424"
                                                       "i4422")))
                                                   (hygiene guile)))
                                               .
                                               each-any))))
                                     (if #{tmp 4458}#
                                       (@apply
                                         (lambda (#{p 4460}#)
                                           (if (= #{lev 4437}# 0)
                                             (#{quasiappend 4429}#
                                               (map (lambda (#{tmp 4461}#)
                                                      (list '#(syntax-object
                                                               "value"
                                                               ((top)
                                                                #(ribcage
                                                                  #(p)
                                                                  #((top))
                                                                  #("i4459"))
                                                                #(ribcage
                                                                  #(p q)
                                                                  #((top)
                                                                    (top))
                                                                  #("i4448"
                                                                    "i4449"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(p lev)
                                                                  #((top)
                                                                    (top))
                                                                  #("i4438"
                                                                    "i4439"))
                                                                #(ribcage
                                                                  (emit quasivector
                                                                        quasilist*
                                                                        quasiappend
                                                                        quasicons
                                                                        vquasi
                                                                        quasi)
                                                                  ((top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top)
                                                                   (top))
                                                                  ("i4434"
                                                                   "i4432"
                                                                   "i4430"
                                                                   "i4428"
                                                                   "i4426"
                                                                   "i4424"
                                                                   "i4422")))
                                                               (hygiene guile))
                                                            #{tmp 4461}#))
                                                    #{p 4460}#)
                                               (#{quasi 4423}#
                                                 #{q 4451}#
                                                 #{lev 4437}#))
                                             (#{quasicons 4427}#
                                               (#{quasicons 4427}#
                                                 '(#(syntax-object
                                                     "quote"
                                                     ((top)
                                                      #(ribcage
                                                        #(p)
                                                        #((top))
                                                        #("i4459"))
                                                      #(ribcage
                                                        #(p q)
                                                        #((top) (top))
                                                        #("i4448" "i4449"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(p lev)
                                                        #((top) (top))
                                                        #("i4438" "i4439"))
                                                      #(ribcage
                                                        (emit quasivector
                                                              quasilist*
                                                              quasiappend
                                                              quasicons
                                                              vquasi
                                                              quasi)
                                                        ((top)
                                                         (top)
                                                         (top)
                                                         (top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                        ("i4434"
                                                         "i4432"
                                                         "i4430"
                                                         "i4428"
                                                         "i4426"
                                                         "i4424"
                                                         "i4422")))
                                                     (hygiene guile))
                                                   #(syntax-object
                                                     unquote-splicing
                                                     ((top)
                                                      #(ribcage
                                                        #(p)
                                                        #((top))
                                                        #("i4459"))
                                                      #(ribcage
                                                        #(p q)
                                                        #((top) (top))
                                                        #("i4448" "i4449"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(p lev)
                                                        #((top) (top))
                                                        #("i4438" "i4439"))
                                                      #(ribcage
                                                        (emit quasivector
                                                              quasilist*
                                                              quasiappend
                                                              quasicons
                                                              vquasi
                                                              quasi)
                                                        ((top)
                                                         (top)
                                                         (top)
                                                         (top)
                                                         (top)
                                                         (top)
                                                         (top))
                                                        ("i4434"
                                                         "i4432"
                                                         "i4430"
                                                         "i4428"
                                                         "i4426"
                                                         "i4424"
                                                         "i4422")))
                                                     (hygiene guile)))
                                                 (#{quasi 4423}#
                                                   #{p 4460}#
                                                   (#{1-}# #{lev 4437}#)))
                                               (#{quasi 4423}#
                                                 #{q 4451}#
                                                 #{lev 4437}#))))
                                         #{tmp 4458}#)
                                       (let ((#{_ 4464}# #{tmp 4452}#))
                                         (#{quasicons 4427}#
                                           (#{quasi 4423}#
                                             #{p 4450}#
                                             #{lev 4437}#)
                                           (#{quasi 4423}#
                                             #{q 4451}#
                                             #{lev 4437}#)))))))))
                           #{tmp 4447}#)
                         (let ((#{tmp 4465}#
                                 ($sc-dispatch
                                   #{tmp 4440}#
                                   '#(vector each-any))))
                           (if #{tmp 4465}#
                             (@apply
                               (lambda (#{x 4467}#)
                                 (#{quasivector 4433}#
                                   (#{vquasi 4425}# #{x 4467}# #{lev 4437}#)))
                               #{tmp 4465}#)
                             (let ((#{p 4470}# #{tmp 4440}#))
                               (list '#(syntax-object
                                        "quote"
                                        ((top)
                                         #(ribcage #(p) #((top)) #("i4469"))
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p lev)
                                           #((top) (top))
                                           #("i4438" "i4439"))
                                         #(ribcage
                                           (emit quasivector
                                                 quasilist*
                                                 quasiappend
                                                 quasicons
                                                 vquasi
                                                 quasi)
                                           ((top)
                                            (top)
                                            (top)
                                            (top)
                                            (top)
                                            (top)
                                            (top))
                                           ("i4434"
                                            "i4432"
                                            "i4430"
                                            "i4428"
                                            "i4426"
                                            "i4424"
                                            "i4422")))
                                        (hygiene guile))
                                     #{p 4470}#)))))))))))))
       (#{vquasi 4425}#
         (lambda (#{p 4471}# #{lev 4472}#)
           (let ((#{tmp 4475}# #{p 4471}#))
             (let ((#{tmp 4476}#
                     ($sc-dispatch #{tmp 4475}# '(any . any))))
               (if #{tmp 4476}#
                 (@apply
                   (lambda (#{p 4479}# #{q 4480}#)
                     (let ((#{tmp 4481}# #{p 4479}#))
                       (let ((#{tmp 4482}#
                               ($sc-dispatch
                                 #{tmp 4481}#
                                 '(#(free-id
                                     #(syntax-object
                                       unquote
                                       ((top)
                                        #(ribcage
                                          #(p q)
                                          #((top) (top))
                                          #("i4477" "i4478"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p lev)
                                          #((top) (top))
                                          #("i4473" "i4474"))
                                        #(ribcage
                                          (emit quasivector
                                                quasilist*
                                                quasiappend
                                                quasicons
                                                vquasi
                                                quasi)
                                          ((top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top)
                                           (top))
                                          ("i4434"
                                           "i4432"
                                           "i4430"
                                           "i4428"
                                           "i4426"
                                           "i4424"
                                           "i4422")))
                                       (hygiene guile)))
                                   .
                                   each-any))))
                         (if #{tmp 4482}#
                           (@apply
                             (lambda (#{p 4484}#)
                               (if (= #{lev 4472}# 0)
                                 (#{quasilist* 4431}#
                                   (map (lambda (#{tmp 4485}#)
                                          (list '#(syntax-object
                                                   "value"
                                                   ((top)
                                                    #(ribcage
                                                      #(p)
                                                      #((top))
                                                      #("i4483"))
                                                    #(ribcage
                                                      #(p q)
                                                      #((top) (top))
                                                      #("i4477" "i4478"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(p lev)
                                                      #((top) (top))
                                                      #("i4473" "i4474"))
                                                    #(ribcage
                                                      (emit quasivector
                                                            quasilist*
                                                            quasiappend
                                                            quasicons
                                                            vquasi
                                                            quasi)
                                                      ((top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top)
                                                       (top))
                                                      ("i4434"
                                                       "i4432"
                                                       "i4430"
                                                       "i4428"
                                                       "i4426"
                                                       "i4424"
                                                       "i4422")))
                                                   (hygiene guile))
                                                #{tmp 4485}#))
                                        #{p 4484}#)
                                   (#{vquasi 4425}# #{q 4480}# #{lev 4472}#))
                                 (#{quasicons 4427}#
                                   (#{quasicons 4427}#
                                     '(#(syntax-object
                                         "quote"
                                         ((top)
                                          #(ribcage #(p) #((top)) #("i4483"))
                                          #(ribcage
                                            #(p q)
                                            #((top) (top))
                                            #("i4477" "i4478"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(p lev)
                                            #((top) (top))
                                            #("i4473" "i4474"))
                                          #(ribcage
                                            (emit quasivector
                                                  quasilist*
                                                  quasiappend
                                                  quasicons
                                                  vquasi
                                                  quasi)
                                            ((top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top))
                                            ("i4434"
                                             "i4432"
                                             "i4430"
                                             "i4428"
                                             "i4426"
                                             "i4424"
                                             "i4422")))
                                         (hygiene guile))
                                       #(syntax-object
                                         unquote
                                         ((top)
                                          #(ribcage #(p) #((top)) #("i4483"))
                                          #(ribcage
                                            #(p q)
                                            #((top) (top))
                                            #("i4477" "i4478"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(p lev)
                                            #((top) (top))
                                            #("i4473" "i4474"))
                                          #(ribcage
                                            (emit quasivector
                                                  quasilist*
                                                  quasiappend
                                                  quasicons
                                                  vquasi
                                                  quasi)
                                            ((top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top)
                                             (top))
                                            ("i4434"
                                             "i4432"
                                             "i4430"
                                             "i4428"
                                             "i4426"
                                             "i4424"
                                             "i4422")))
                                         (hygiene guile)))
                                     (#{quasi 4423}#
                                       #{p 4484}#
                                       (#{1-}# #{lev 4472}#)))
                                   (#{vquasi 4425}# #{q 4480}# #{lev 4472}#))))
                             #{tmp 4482}#)
                           (let ((#{tmp 4487}#
                                   ($sc-dispatch
                                     #{tmp 4481}#
                                     '(#(free-id
                                         #(syntax-object
                                           unquote-splicing
                                           ((top)
                                            #(ribcage
                                              #(p q)
                                              #((top) (top))
                                              #("i4477" "i4478"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(p lev)
                                              #((top) (top))
                                              #("i4473" "i4474"))
                                            #(ribcage
                                              (emit quasivector
                                                    quasilist*
                                                    quasiappend
                                                    quasicons
                                                    vquasi
                                                    quasi)
                                              ((top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top)
                                               (top))
                                              ("i4434"
                                               "i4432"
                                               "i4430"
                                               "i4428"
                                               "i4426"
                                               "i4424"
                                               "i4422")))
                                           (hygiene guile)))
                                       .
                                       each-any))))
                             (if #{tmp 4487}#
                               (@apply
                                 (lambda (#{p 4489}#)
                                   (if (= #{lev 4472}# 0)
                                     (#{quasiappend 4429}#
                                       (map (lambda (#{tmp 4490}#)
                                              (list '#(syntax-object
                                                       "value"
                                                       ((top)
                                                        #(ribcage
                                                          #(p)
                                                          #((top))
                                                          #("i4488"))
                                                        #(ribcage
                                                          #(p q)
                                                          #((top) (top))
                                                          #("i4477" "i4478"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(p lev)
                                                          #((top) (top))
                                                          #("i4473" "i4474"))
                                                        #(ribcage
                                                          (emit quasivector
                                                                quasilist*
                                                                quasiappend
                                                                quasicons
                                                                vquasi
                                                                quasi)
                                                          ((top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                          ("i4434"
                                                           "i4432"
                                                           "i4430"
                                                           "i4428"
                                                           "i4426"
                                                           "i4424"
                                                           "i4422")))
                                                       (hygiene guile))
                                                    #{tmp 4490}#))
                                            #{p 4489}#)
                                       (#{vquasi 4425}#
                                         #{q 4480}#
                                         #{lev 4472}#))
                                     (#{quasicons 4427}#
                                       (#{quasicons 4427}#
                                         '(#(syntax-object
                                             "quote"
                                             ((top)
                                              #(ribcage
                                                #(p)
                                                #((top))
                                                #("i4488"))
                                              #(ribcage
                                                #(p q)
                                                #((top) (top))
                                                #("i4477" "i4478"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(p lev)
                                                #((top) (top))
                                                #("i4473" "i4474"))
                                              #(ribcage
                                                (emit quasivector
                                                      quasilist*
                                                      quasiappend
                                                      quasicons
                                                      vquasi
                                                      quasi)
                                                ((top)
                                                 (top)
                                                 (top)
                                                 (top)
                                                 (top)
                                                 (top)
                                                 (top))
                                                ("i4434"
                                                 "i4432"
                                                 "i4430"
                                                 "i4428"
                                                 "i4426"
                                                 "i4424"
                                                 "i4422")))
                                             (hygiene guile))
                                           #(syntax-object
                                             unquote-splicing
                                             ((top)
                                              #(ribcage
                                                #(p)
                                                #((top))
                                                #("i4488"))
                                              #(ribcage
                                                #(p q)
                                                #((top) (top))
                                                #("i4477" "i4478"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(p lev)
                                                #((top) (top))
                                                #("i4473" "i4474"))
                                              #(ribcage
                                                (emit quasivector
                                                      quasilist*
                                                      quasiappend
                                                      quasicons
                                                      vquasi
                                                      quasi)
                                                ((top)
                                                 (top)
                                                 (top)
                                                 (top)
                                                 (top)
                                                 (top)
                                                 (top))
                                                ("i4434"
                                                 "i4432"
                                                 "i4430"
                                                 "i4428"
                                                 "i4426"
                                                 "i4424"
                                                 "i4422")))
                                             (hygiene guile)))
                                         (#{quasi 4423}#
                                           #{p 4489}#
                                           (#{1-}# #{lev 4472}#)))
                                       (#{vquasi 4425}#
                                         #{q 4480}#
                                         #{lev 4472}#))))
                                 #{tmp 4487}#)
                               (let ((#{_ 4493}# #{tmp 4481}#))
                                 (#{quasicons 4427}#
                                   (#{quasi 4423}# #{p 4479}# #{lev 4472}#)
                                   (#{vquasi 4425}#
                                     #{q 4480}#
                                     #{lev 4472}#)))))))))
                   #{tmp 4476}#)
                 (let ((#{tmp 4494}# ($sc-dispatch #{tmp 4475}# '())))
                   (if #{tmp 4494}#
                     (@apply
                       (lambda ()
                         '(#(syntax-object
                             "quote"
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4473" "i4474"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4434"
                                 "i4432"
                                 "i4430"
                                 "i4428"
                                 "i4426"
                                 "i4424"
                                 "i4422")))
                             (hygiene guile))
                           ()))
                       #{tmp 4494}#)
                     (syntax-violation
                       #f
                       "source expression failed to match any pattern"
                       #{tmp 4475}#))))))))
       (#{quasicons 4427}#
         (lambda (#{x 4495}# #{y 4496}#)
           (let ((#{tmp 4500}# (list #{x 4495}# #{y 4496}#)))
             (let ((#{tmp 4501}#
                     ($sc-dispatch #{tmp 4500}# '(any any))))
               (if #{tmp 4501}#
                 (@apply
                   (lambda (#{x 4504}# #{y 4505}#)
                     (let ((#{tmp 4506}# #{y 4505}#))
                       (let ((#{tmp 4507}#
                               ($sc-dispatch
                                 #{tmp 4506}#
                                 '(#(atom "quote") any))))
                         (if #{tmp 4507}#
                           (@apply
                             (lambda (#{dy 4509}#)
                               (let ((#{tmp 4510}# #{x 4504}#))
                                 (let ((#{tmp 4511}#
                                         ($sc-dispatch
                                           #{tmp 4510}#
                                           '(#(atom "quote") any))))
                                   (if #{tmp 4511}#
                                     (@apply
                                       (lambda (#{dx 4513}#)
                                         (list '#(syntax-object
                                                  "quote"
                                                  ((top)
                                                   #(ribcage
                                                     #(dx)
                                                     #((top))
                                                     #("i4512"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4508"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4502" "i4503"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4497" "i4498"))
                                                   #(ribcage
                                                     (emit quasivector
                                                           quasilist*
                                                           quasiappend
                                                           quasicons
                                                           vquasi
                                                           quasi)
                                                     ((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                     ("i4434"
                                                      "i4432"
                                                      "i4430"
                                                      "i4428"
                                                      "i4426"
                                                      "i4424"
                                                      "i4422")))
                                                  (hygiene guile))
                                               (cons #{dx 4513}# #{dy 4509}#)))
                                       #{tmp 4511}#)
                                     (let ((#{_ 4515}# #{tmp 4510}#))
                                       (if (null? #{dy 4509}#)
                                         (list '#(syntax-object
                                                  "list"
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4514"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4508"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4502" "i4503"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4497" "i4498"))
                                                   #(ribcage
                                                     (emit quasivector
                                                           quasilist*
                                                           quasiappend
                                                           quasicons
                                                           vquasi
                                                           quasi)
                                                     ((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                     ("i4434"
                                                      "i4432"
                                                      "i4430"
                                                      "i4428"
                                                      "i4426"
                                                      "i4424"
                                                      "i4422")))
                                                  (hygiene guile))
                                               #{x 4504}#)
                                         (list '#(syntax-object
                                                  "list*"
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4514"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4508"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4502" "i4503"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4497" "i4498"))
                                                   #(ribcage
                                                     (emit quasivector
                                                           quasilist*
                                                           quasiappend
                                                           quasicons
                                                           vquasi
                                                           quasi)
                                                     ((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                     ("i4434"
                                                      "i4432"
                                                      "i4430"
                                                      "i4428"
                                                      "i4426"
                                                      "i4424"
                                                      "i4422")))
                                                  (hygiene guile))
                                               #{x 4504}#
                                               #{y 4505}#)))))))
                             #{tmp 4507}#)
                           (let ((#{tmp 4516}#
                                   ($sc-dispatch
                                     #{tmp 4506}#
                                     '(#(atom "list") . any))))
                             (if #{tmp 4516}#
                               (@apply
                                 (lambda (#{stuff 4518}#)
                                   (cons '#(syntax-object
                                            "list"
                                            ((top)
                                             #(ribcage
                                               #(stuff)
                                               #((top))
                                               #("i4517"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4502" "i4503"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4497" "i4498"))
                                             #(ribcage
                                               (emit quasivector
                                                     quasilist*
                                                     quasiappend
                                                     quasicons
                                                     vquasi
                                                     quasi)
                                               ((top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top))
                                               ("i4434"
                                                "i4432"
                                                "i4430"
                                                "i4428"
                                                "i4426"
                                                "i4424"
                                                "i4422")))
                                            (hygiene guile))
                                         (cons #{x 4504}# #{stuff 4518}#)))
                                 #{tmp 4516}#)
                               (let ((#{tmp 4519}#
                                       ($sc-dispatch
                                         #{tmp 4506}#
                                         '(#(atom "list*") . any))))
                                 (if #{tmp 4519}#
                                   (@apply
                                     (lambda (#{stuff 4521}#)
                                       (cons '#(syntax-object
                                                "list*"
                                                ((top)
                                                 #(ribcage
                                                   #(stuff)
                                                   #((top))
                                                   #("i4520"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x y)
                                                   #((top) (top))
                                                   #("i4502" "i4503"))
                                                 #(ribcage () () ())
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x y)
                                                   #((top) (top))
                                                   #("i4497" "i4498"))
                                                 #(ribcage
                                                   (emit quasivector
                                                         quasilist*
                                                         quasiappend
                                                         quasicons
                                                         vquasi
                                                         quasi)
                                                   ((top)
                                                    (top)
                                                    (top)
                                                    (top)
                                                    (top)
                                                    (top)
                                                    (top))
                                                   ("i4434"
                                                    "i4432"
                                                    "i4430"
                                                    "i4428"
                                                    "i4426"
                                                    "i4424"
                                                    "i4422")))
                                                (hygiene guile))
                                             (cons #{x 4504}# #{stuff 4521}#)))
                                     #{tmp 4519}#)
                                   (let ((#{_ 4523}# #{tmp 4506}#))
                                     (list '#(syntax-object
                                              "list*"
                                              ((top)
                                               #(ribcage
                                                 #(_)
                                                 #((top))
                                                 #("i4522"))
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i4502" "i4503"))
                                               #(ribcage () () ())
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i4497" "i4498"))
                                               #(ribcage
                                                 (emit quasivector
                                                       quasilist*
                                                       quasiappend
                                                       quasicons
                                                       vquasi
                                                       quasi)
                                                 ((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                 ("i4434"
                                                  "i4432"
                                                  "i4430"
                                                  "i4428"
                                                  "i4426"
                                                  "i4424"
                                                  "i4422")))
                                              (hygiene guile))
                                           #{x 4504}#
                                           #{y 4505}#))))))))))
                   #{tmp 4501}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp 4500}#))))))
       (#{quasiappend 4429}#
         (lambda (#{x 4524}# #{y 4525}#)
           (let ((#{tmp 4528}# #{y 4525}#))
             (let ((#{tmp 4529}#
                     ($sc-dispatch #{tmp 4528}# '(#(atom "quote") ()))))
               (if #{tmp 4529}#
                 (@apply
                   (lambda ()
                     (if (null? #{x 4524}#)
                       '(#(syntax-object
                           "quote"
                           ((top)
                            #(ribcage () () ())
                            #(ribcage
                              #(x y)
                              #((top) (top))
                              #("i4526" "i4527"))
                            #(ribcage
                              (emit quasivector
                                    quasilist*
                                    quasiappend
                                    quasicons
                                    vquasi
                                    quasi)
                              ((top) (top) (top) (top) (top) (top) (top))
                              ("i4434"
                               "i4432"
                               "i4430"
                               "i4428"
                               "i4426"
                               "i4424"
                               "i4422")))
                           (hygiene guile))
                         ())
                       (if (null? (cdr #{x 4524}#))
                         (car #{x 4524}#)
                         (let ((#{tmp 4536}# #{x 4524}#))
                           (let ((#{tmp 4537}#
                                   ($sc-dispatch #{tmp 4536}# 'each-any)))
                             (if #{tmp 4537}#
                               (@apply
                                 (lambda (#{p 4539}#)
                                   (cons '#(syntax-object
                                            "append"
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(p)
                                               #((top))
                                               #("i4538"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4526" "i4527"))
                                             #(ribcage
                                               (emit quasivector
                                                     quasilist*
                                                     quasiappend
                                                     quasicons
                                                     vquasi
                                                     quasi)
                                               ((top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top))
                                               ("i4434"
                                                "i4432"
                                                "i4430"
                                                "i4428"
                                                "i4426"
                                                "i4424"
                                                "i4422")))
                                            (hygiene guile))
                                         #{p 4539}#))
                                 #{tmp 4537}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4536}#)))))))
                   #{tmp 4529}#)
                 (let ((#{_ 4542}# #{tmp 4528}#))
                   (if (null? #{x 4524}#)
                     #{y 4525}#
                     (let ((#{tmp 4547}# (list #{x 4524}# #{y 4525}#)))
                       (let ((#{tmp 4548}#
                               ($sc-dispatch #{tmp 4547}# '(each-any any))))
                         (if #{tmp 4548}#
                           (@apply
                             (lambda (#{p 4551}# #{y 4552}#)
                               (cons '#(syntax-object
                                        "append"
                                        ((top)
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p y)
                                           #((top) (top))
                                           #("i4549" "i4550"))
                                         #(ribcage #(_) #((top)) #("i4541"))
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(x y)
                                           #((top) (top))
                                           #("i4526" "i4527"))
                                         #(ribcage
                                           (emit quasivector
                                                 quasilist*
                                                 quasiappend
                                                 quasicons
                                                 vquasi
                                                 quasi)
                                           ((top)
                                            (top)
                                            (top)
                                            (top)
                                            (top)
                                            (top)
                                            (top))
                                           ("i4434"
                                            "i4432"
                                            "i4430"
                                            "i4428"
                                            "i4426"
                                            "i4424"
                                            "i4422")))
                                        (hygiene guile))
                                     (append #{p 4551}# (list #{y 4552}#))))
                             #{tmp 4548}#)
                           (syntax-violation
                             #f
                             "source expression failed to match any pattern"
                             #{tmp 4547}#)))))))))))
       (#{quasilist* 4431}#
         (lambda (#{x 4554}# #{y 4555}#)
           (letrec*
             ((#{f 4560}#
                (lambda (#{x 4561}#)
                  (if (null? #{x 4561}#)
                    #{y 4555}#
                    (#{quasicons 4427}#
                      (car #{x 4561}#)
                      (#{f 4560}# (cdr #{x 4561}#)))))))
             (#{f 4560}# #{x 4554}#))))
       (#{quasivector 4433}#
         (lambda (#{x 4562}#)
           (let ((#{tmp 4564}# #{x 4562}#))
             (let ((#{tmp 4565}#
                     ($sc-dispatch
                       #{tmp 4564}#
                       '(#(atom "quote") each-any))))
               (if #{tmp 4565}#
                 (@apply
                   (lambda (#{x 4567}#)
                     (list '#(syntax-object
                              "quote"
                              ((top)
                               #(ribcage #(x) #((top)) #("i4566"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4563"))
                               #(ribcage
                                 (emit quasivector
                                       quasilist*
                                       quasiappend
                                       quasicons
                                       vquasi
                                       quasi)
                                 ((top) (top) (top) (top) (top) (top) (top))
                                 ("i4434"
                                  "i4432"
                                  "i4430"
                                  "i4428"
                                  "i4426"
                                  "i4424"
                                  "i4422")))
                              (hygiene guile))
                           (list->vector #{x 4567}#)))
                   #{tmp 4565}#)
                 (let ((#{_ 4570}# #{tmp 4564}#))
                   (letrec*
                     ((#{f 4574}#
                        (lambda (#{y 4575}# #{k 4576}#)
                          (let ((#{tmp 4587}# #{y 4575}#))
                            (let ((#{tmp 4588}#
                                    ($sc-dispatch
                                      #{tmp 4587}#
                                      '(#(atom "quote") each-any))))
                              (if #{tmp 4588}#
                                (@apply
                                  (lambda (#{y 4590}#)
                                    (#{k 4576}#
                                      (map (lambda (#{tmp 4591}#)
                                             (list '#(syntax-object
                                                      "quote"
                                                      ((top)
                                                       #(ribcage
                                                         #(y)
                                                         #((top))
                                                         #("i4589"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(f y k)
                                                         #((top) (top) (top))
                                                         #("i4571"
                                                           "i4572"
                                                           "i4573"))
                                                       #(ribcage
                                                         #(_)
                                                         #((top))
                                                         #("i4569"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4563"))
                                                       #(ribcage
                                                         (emit quasivector
                                                               quasilist*
                                                               quasiappend
                                                               quasicons
                                                               vquasi
                                                               quasi)
                                                         ((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                         ("i4434"
                                                          "i4432"
                                                          "i4430"
                                                          "i4428"
                                                          "i4426"
                                                          "i4424"
                                                          "i4422")))
                                                      (hygiene guile))
                                                   #{tmp 4591}#))
                                           #{y 4590}#)))
                                  #{tmp 4588}#)
                                (let ((#{tmp 4592}#
                                        ($sc-dispatch
                                          #{tmp 4587}#
                                          '(#(atom "list") . each-any))))
                                  (if #{tmp 4592}#
                                    (@apply
                                      (lambda (#{y 4594}#)
                                        (#{k 4576}# #{y 4594}#))
                                      #{tmp 4592}#)
                                    (let ((#{tmp 4596}#
                                            ($sc-dispatch
                                              #{tmp 4587}#
                                              '(#(atom "list*")
                                                .
                                                #(each+ any (any) ())))))
                                      (if #{tmp 4596}#
                                        (@apply
                                          (lambda (#{y 4599}# #{z 4600}#)
                                            (#{f 4574}#
                                              #{z 4600}#
                                              (lambda (#{ls 4601}#)
                                                (#{k 4576}#
                                                  (append
                                                    #{y 4599}#
                                                    #{ls 4601}#)))))
                                          #{tmp 4596}#)
                                        (let ((#{else 4605}# #{tmp 4587}#))
                                          (let ((#{tmp 4609}# #{x 4562}#))
                                            (let ((#{ g4606 4611}#
                                                    #{tmp 4609}#))
                                              (list '#(syntax-object
                                                       "list->vector"
                                                       ((top)
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(#{ g4606}#)
                                                          #((m4607 top))
                                                          #("i4610"))
                                                        #(ribcage
                                                          #(else)
                                                          #((top))
                                                          #("i4604"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(f y k)
                                                          #((top) (top) (top))
                                                          #("i4571"
                                                            "i4572"
                                                            "i4573"))
                                                        #(ribcage
                                                          #(_)
                                                          #((top))
                                                          #("i4569"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(x)
                                                          #((top))
                                                          #("i4563"))
                                                        #(ribcage
                                                          (emit quasivector
                                                                quasilist*
                                                                quasiappend
                                                                quasicons
                                                                vquasi
                                                                quasi)
                                                          ((top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                          ("i4434"
                                                           "i4432"
                                                           "i4430"
                                                           "i4428"
                                                           "i4426"
                                                           "i4424"
                                                           "i4422")))
                                                       (hygiene guile))
                                                    #{ g4606 4611}#))))))))))))))
                     (#{f 4574}#
                       #{x 4562}#
                       (lambda (#{ls 4577}#)
                         (let ((#{tmp 4582}# #{ls 4577}#))
                           (let ((#{tmp 4583}#
                                   ($sc-dispatch #{tmp 4582}# 'each-any)))
                             (if #{tmp 4583}#
                               (@apply
                                 (lambda (#{ g4579 4585}#)
                                   (cons '#(syntax-object
                                            "vector"
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(#{ g4579}#)
                                               #((m4580 top))
                                               #("i4584"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(ls)
                                               #((top))
                                               #("i4578"))
                                             #(ribcage
                                               #(_)
                                               #((top))
                                               #("i4569"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4563"))
                                             #(ribcage
                                               (emit quasivector
                                                     quasilist*
                                                     quasiappend
                                                     quasicons
                                                     vquasi
                                                     quasi)
                                               ((top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top))
                                               ("i4434"
                                                "i4432"
                                                "i4430"
                                                "i4428"
                                                "i4426"
                                                "i4424"
                                                "i4422")))
                                            (hygiene guile))
                                         #{ g4579 4585}#))
                                 #{tmp 4583}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4582}#)))))))))))))
       (#{emit 4435}#
         (lambda (#{x 4612}#)
           (let ((#{tmp 4614}# #{x 4612}#))
             (let ((#{tmp 4615}#
                     ($sc-dispatch
                       #{tmp 4614}#
                       '(#(atom "quote") any))))
               (if #{tmp 4615}#
                 (@apply
                   (lambda (#{x 4617}#)
                     (list '#(syntax-object
                              quote
                              ((top)
                               #(ribcage #(x) #((top)) #("i4616"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4613"))
                               #(ribcage
                                 (emit quasivector
                                       quasilist*
                                       quasiappend
                                       quasicons
                                       vquasi
                                       quasi)
                                 ((top) (top) (top) (top) (top) (top) (top))
                                 ("i4434"
                                  "i4432"
                                  "i4430"
                                  "i4428"
                                  "i4426"
                                  "i4424"
                                  "i4422")))
                              (hygiene guile))
                           #{x 4617}#))
                   #{tmp 4615}#)
                 (let ((#{tmp 4618}#
                         ($sc-dispatch
                           #{tmp 4614}#
                           '(#(atom "list") . each-any))))
                   (if #{tmp 4618}#
                     (@apply
                       (lambda (#{x 4620}#)
                         (let ((#{tmp 4624}# (map #{emit 4435}# #{x 4620}#)))
                           (let ((#{tmp 4625}#
                                   ($sc-dispatch #{tmp 4624}# 'each-any)))
                             (if #{tmp 4625}#
                               (@apply
                                 (lambda (#{ g4621 4627}#)
                                   (cons '#(syntax-object
                                            list
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(#{ g4621}#)
                                               #((m4622 top))
                                               #("i4626"))
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4619"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4613"))
                                             #(ribcage
                                               (emit quasivector
                                                     quasilist*
                                                     quasiappend
                                                     quasicons
                                                     vquasi
                                                     quasi)
                                               ((top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top)
                                                (top))
                                               ("i4434"
                                                "i4432"
                                                "i4430"
                                                "i4428"
                                                "i4426"
                                                "i4424"
                                                "i4422")))
                                            (hygiene guile))
                                         #{ g4621 4627}#))
                                 #{tmp 4625}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4624}#)))))
                       #{tmp 4618}#)
                     (let ((#{tmp 4630}#
                             ($sc-dispatch
                               #{tmp 4614}#
                               '(#(atom "list*") . #(each+ any (any) ())))))
                       (if #{tmp 4630}#
                         (@apply
                           (lambda (#{x 4633}# #{y 4634}#)
                             (letrec*
                               ((#{f 4637}#
                                  (lambda (#{x* 4638}#)
                                    (if (null? #{x* 4638}#)
                                      (#{emit 4435}# #{y 4634}#)
                                      (let ((#{tmp 4644}#
                                              (list (#{emit 4435}#
                                                      (car #{x* 4638}#))
                                                    (#{f 4637}#
                                                      (cdr #{x* 4638}#)))))
                                        (let ((#{tmp 4645}#
                                                ($sc-dispatch
                                                  #{tmp 4644}#
                                                  '(any any))))
                                          (if #{tmp 4645}#
                                            (@apply
                                              (lambda (#{ g4641 4648}#
                                                       #{ g4640 4649}#)
                                                (list '#(syntax-object
                                                         cons
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(#{ g4641}#
                                                              #{ g4640}#)
                                                            #((m4642 top)
                                                              (m4642 top))
                                                            #("i4646" "i4647"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(f x*)
                                                            #((top) (top))
                                                            #("i4635" "i4636"))
                                                          #(ribcage
                                                            #(x y)
                                                            #((top) (top))
                                                            #("i4631" "i4632"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x)
                                                            #((top))
                                                            #("i4613"))
                                                          #(ribcage
                                                            (emit quasivector
                                                                  quasilist*
                                                                  quasiappend
                                                                  quasicons
                                                                  vquasi
                                                                  quasi)
                                                            ((top)
                                                             (top)
                                                             (top)
                                                             (top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                            ("i4434"
                                                             "i4432"
                                                             "i4430"
                                                             "i4428"
                                                             "i4426"
                                                             "i4424"
                                                             "i4422")))
                                                         (hygiene guile))
                                                      #{ g4641 4648}#
                                                      #{ g4640 4649}#))
                                              #{tmp 4645}#)
                                            (syntax-violation
                                              #f
                                              "source expression failed to match any pattern"
                                              #{tmp 4644}#))))))))
                               (#{f 4637}# #{x 4633}#)))
                           #{tmp 4630}#)
                         (let ((#{tmp 4650}#
                                 ($sc-dispatch
                                   #{tmp 4614}#
                                   '(#(atom "append") . each-any))))
                           (if #{tmp 4650}#
                             (@apply
                               (lambda (#{x 4652}#)
                                 (let ((#{tmp 4656}#
                                         (map #{emit 4435}# #{x 4652}#)))
                                   (let ((#{tmp 4657}#
                                           ($sc-dispatch
                                             #{tmp 4656}#
                                             'each-any)))
                                     (if #{tmp 4657}#
                                       (@apply
                                         (lambda (#{ g4653 4659}#)
                                           (cons '#(syntax-object
                                                    append
                                                    ((top)
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(#{ g4653}#)
                                                       #((m4654 top))
                                                       #("i4658"))
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4651"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4613"))
                                                     #(ribcage
                                                       (emit quasivector
                                                             quasilist*
                                                             quasiappend
                                                             quasicons
                                                             vquasi
                                                             quasi)
                                                       ((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                       ("i4434"
                                                        "i4432"
                                                        "i4430"
                                                        "i4428"
                                                        "i4426"
                                                        "i4424"
                                                        "i4422")))
                                                    (hygiene guile))
                                                 #{ g4653 4659}#))
                                         #{tmp 4657}#)
                                       (syntax-violation
                                         #f
                                         "source expression failed to match any pattern"
                                         #{tmp 4656}#)))))
                               #{tmp 4650}#)
                             (let ((#{tmp 4662}#
                                     ($sc-dispatch
                                       #{tmp 4614}#
                                       '(#(atom "vector") . each-any))))
                               (if #{tmp 4662}#
                                 (@apply
                                   (lambda (#{x 4664}#)
                                     (let ((#{tmp 4668}#
                                             (map #{emit 4435}# #{x 4664}#)))
                                       (let ((#{tmp 4669}#
                                               ($sc-dispatch
                                                 #{tmp 4668}#
                                                 'each-any)))
                                         (if #{tmp 4669}#
                                           (@apply
                                             (lambda (#{ g4665 4671}#)
                                               (cons '#(syntax-object
                                                        vector
                                                        ((top)
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(#{ g4665}#)
                                                           #((m4666 top))
                                                           #("i4670"))
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4663"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4613"))
                                                         #(ribcage
                                                           (emit quasivector
                                                                 quasilist*
                                                                 quasiappend
                                                                 quasicons
                                                                 vquasi
                                                                 quasi)
                                                           ((top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top)
                                                            (top))
                                                           ("i4434"
                                                            "i4432"
                                                            "i4430"
                                                            "i4428"
                                                            "i4426"
                                                            "i4424"
                                                            "i4422")))
                                                        (hygiene guile))
                                                     #{ g4665 4671}#))
                                             #{tmp 4669}#)
                                           (syntax-violation
                                             #f
                                             "source expression failed to match any pattern"
                                             #{tmp 4668}#)))))
                                   #{tmp 4662}#)
                                 (let ((#{tmp 4674}#
                                         ($sc-dispatch
                                           #{tmp 4614}#
                                           '(#(atom "list->vector") any))))
                                   (if #{tmp 4674}#
                                     (@apply
                                       (lambda (#{x 4676}#)
                                         (let ((#{tmp 4680}#
                                                 (#{emit 4435}# #{x 4676}#)))
                                           (let ((#{ g4677 4682}#
                                                   #{tmp 4680}#))
                                             (list '#(syntax-object
                                                      list->vector
                                                      ((top)
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(#{ g4677}#)
                                                         #((m4678 top))
                                                         #("i4681"))
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4675"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4613"))
                                                       #(ribcage
                                                         (emit quasivector
                                                               quasilist*
                                                               quasiappend
                                                               quasicons
                                                               vquasi
                                                               quasi)
                                                         ((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                         ("i4434"
                                                          "i4432"
                                                          "i4430"
                                                          "i4428"
                                                          "i4426"
                                                          "i4424"
                                                          "i4422")))
                                                      (hygiene guile))
                                                   #{ g4677 4682}#))))
                                       #{tmp 4674}#)
                                     (let ((#{tmp 4683}#
                                             ($sc-dispatch
                                               #{tmp 4614}#
                                               '(#(atom "value") any))))
                                       (if #{tmp 4683}#
                                         (@apply
                                           (lambda (#{x 4685}#) #{x 4685}#)
                                           #{tmp 4683}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp 4614}#)))))))))))))))))))
      (lambda (#{x 4686}#)
        (let ((#{tmp 4688}# #{x 4686}#))
          (let ((#{tmp 4689}#
                  ($sc-dispatch #{tmp 4688}# '(_ any))))
            (if #{tmp 4689}#
              (@apply
                (lambda (#{e 4691}#)
                  (#{emit 4435}# (#{quasi 4423}# #{e 4691}# 0)))
                #{tmp 4689}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 4688}#))))))))

(define include
  (make-syntax-transformer
    'include
    'macro
    (lambda (#{x 4692}#)
      (letrec*
        ((#{read-file 4695}#
           (lambda (#{fn 4696}# #{k 4697}#)
             (let ((#{p 4701}# (open-input-file #{fn 4696}#)))
               (letrec*
                 ((#{f 4705}#
                    (lambda (#{x 4706}# #{result 4707}#)
                      (if (eof-object? #{x 4706}#)
                        (begin
                          (close-input-port #{p 4701}#)
                          (reverse #{result 4707}#))
                        (#{f 4705}#
                          (read #{p 4701}#)
                          (cons (datum->syntax #{k 4697}# #{x 4706}#)
                                #{result 4707}#))))))
                 (#{f 4705}# (read #{p 4701}#) '()))))))
        (let ((#{tmp 4708}# #{x 4692}#))
          (let ((#{tmp 4709}#
                  ($sc-dispatch #{tmp 4708}# '(any any))))
            (if #{tmp 4709}#
              (@apply
                (lambda (#{k 4712}# #{filename 4713}#)
                  (let ((#{fn 4715}# (syntax->datum #{filename 4713}#)))
                    (let ((#{tmp 4717}#
                            (#{read-file 4695}#
                              #{fn 4715}#
                              #{filename 4713}#)))
                      (let ((#{tmp 4718}#
                              ($sc-dispatch #{tmp 4717}# 'each-any)))
                        (if #{tmp 4718}#
                          (@apply
                            (lambda (#{exp 4720}#)
                              (cons '#(syntax-object
                                       begin
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(exp) #((top)) #("i4719"))
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage #(fn) #((top)) #("i4714"))
                                        #(ribcage
                                          #(k filename)
                                          #((top) (top))
                                          #("i4710" "i4711"))
                                        #(ribcage
                                          (read-file)
                                          ((top))
                                          ("i4694"))
                                        #(ribcage #(x) #((top)) #("i4693")))
                                       (hygiene guile))
                                    #{exp 4720}#))
                            #{tmp 4718}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp 4717}#))))))
                #{tmp 4709}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 4708}#))))))))

(define include-from-path
  (make-syntax-transformer
    'include-from-path
    'macro
    (lambda (#{x 4722}#)
      (let ((#{tmp 4724}# #{x 4722}#))
        (let ((#{tmp 4725}#
                ($sc-dispatch #{tmp 4724}# '(any any))))
          (if #{tmp 4725}#
            (@apply
              (lambda (#{k 4728}# #{filename 4729}#)
                (let ((#{fn 4731}# (syntax->datum #{filename 4729}#)))
                  (let ((#{tmp 4733}#
                          (datum->syntax
                            #{filename 4729}#
                            (let ((#{t 4738}# (%search-load-path #{fn 4731}#)))
                              (if #{t 4738}#
                                #{t 4738}#
                                (syntax-violation
                                  'include-from-path
                                  "file not found in path"
                                  #{x 4722}#
                                  #{filename 4729}#))))))
                    (let ((#{fn 4735}# #{tmp 4733}#))
                      (list '#(syntax-object
                               include
                               ((top)
                                #(ribcage () () ())
                                #(ribcage #(fn) #((top)) #("i4734"))
                                #(ribcage () () ())
                                #(ribcage () () ())
                                #(ribcage #(fn) #((top)) #("i4730"))
                                #(ribcage
                                  #(k filename)
                                  #((top) (top))
                                  #("i4726" "i4727"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4723")))
                               (hygiene guile))
                            #{fn 4735}#)))))
              #{tmp 4725}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4724}#)))))))

(define unquote
  (make-syntax-transformer
    'unquote
    'macro
    (lambda (#{x 4740}#)
      (syntax-violation
        'unquote
        "expression not valid outside of quasiquote"
        #{x 4740}#))))

(define unquote-splicing
  (make-syntax-transformer
    'unquote-splicing
    'macro
    (lambda (#{x 4742}#)
      (syntax-violation
        'unquote-splicing
        "expression not valid outside of quasiquote"
        #{x 4742}#))))

(define case
  (make-syntax-transformer
    'case
    'macro
    (lambda (#{x 4744}#)
      (let ((#{tmp 4746}# #{x 4744}#))
        (let ((#{tmp 4747}#
                ($sc-dispatch
                  #{tmp 4746}#
                  '(_ any any . each-any))))
          (if #{tmp 4747}#
            (@apply
              (lambda (#{e 4751}# #{m1 4752}# #{m2 4753}#)
                (let ((#{tmp 4755}#
                        (letrec*
                          ((#{f 4761}#
                             (lambda (#{clause 4762}# #{clauses 4763}#)
                               (if (null? #{clauses 4763}#)
                                 (let ((#{tmp 4765}# #{clause 4762}#))
                                   (let ((#{tmp 4766}#
                                           ($sc-dispatch
                                             #{tmp 4765}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   else
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(f clause clauses)
                                                      #((top) (top) (top))
                                                      #("i4758"
                                                        "i4759"
                                                        "i4760"))
                                                    #(ribcage
                                                      #(e m1 m2)
                                                      #((top) (top) (top))
                                                      #("i4748"
                                                        "i4749"
                                                        "i4750"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i4745")))
                                                   (hygiene guile)))
                                               any
                                               .
                                               each-any))))
                                     (if #{tmp 4766}#
                                       (@apply
                                         (lambda (#{e1 4769}# #{e2 4770}#)
                                           (cons '#(syntax-object
                                                    begin
                                                    ((top)
                                                     #(ribcage
                                                       #(e1 e2)
                                                       #((top) (top))
                                                       #("i4767" "i4768"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(f clause clauses)
                                                       #((top) (top) (top))
                                                       #("i4758"
                                                         "i4759"
                                                         "i4760"))
                                                     #(ribcage
                                                       #(e m1 m2)
                                                       #((top) (top) (top))
                                                       #("i4748"
                                                         "i4749"
                                                         "i4750"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4745")))
                                                    (hygiene guile))
                                                 (cons #{e1 4769}#
                                                       #{e2 4770}#)))
                                         #{tmp 4766}#)
                                       (let ((#{tmp 4772}#
                                               ($sc-dispatch
                                                 #{tmp 4765}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp 4772}#
                                           (@apply
                                             (lambda (#{k 4776}#
                                                      #{e1 4777}#
                                                      #{e2 4778}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4773"
                                                             "i4774"
                                                             "i4775"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4758"
                                                             "i4759"
                                                             "i4760"))
                                                         #(ribcage
                                                           #(e m1 m2)
                                                           #((top) (top) (top))
                                                           #("i4748"
                                                             "i4749"
                                                             "i4750"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4745")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4773"
                                                                   "i4774"
                                                                   "i4775"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(f
                                                                   clause
                                                                   clauses)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4758"
                                                                   "i4759"
                                                                   "i4760"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4748"
                                                                   "i4749"
                                                                   "i4750"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4745")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4773"
                                                                   "i4774"
                                                                   "i4775"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(f
                                                                   clause
                                                                   clauses)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4758"
                                                                   "i4759"
                                                                   "i4760"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4748"
                                                                   "i4749"
                                                                   "i4750"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4745")))
                                                              (hygiene guile))
                                                           (list '#(syntax-object
                                                                    quote
                                                                    ((top)
                                                                     #(ribcage
                                                                       #(k
                                                                         e1
                                                                         e2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4773"
                                                                         "i4774"
                                                                         "i4775"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(f
                                                                         clause
                                                                         clauses)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4758"
                                                                         "i4759"
                                                                         "i4760"))
                                                                     #(ribcage
                                                                       #(e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4748"
                                                                         "i4749"
                                                                         "i4750"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4745")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k 4776}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4773"
                                                                   "i4774"
                                                                   "i4775"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(f
                                                                   clause
                                                                   clauses)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4758"
                                                                   "i4759"
                                                                   "i4760"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4748"
                                                                   "i4749"
                                                                   "i4750"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4745")))
                                                              (hygiene guile))
                                                           (cons #{e1 4777}#
                                                                 #{e2 4778}#))))
                                             #{tmp 4772}#)
                                           (let ((#{_ 4782}# #{tmp 4765}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x 4744}#
                                               #{clause 4762}#)))))))
                                 (let ((#{tmp 4784}#
                                         (#{f 4761}#
                                           (car #{clauses 4763}#)
                                           (cdr #{clauses 4763}#))))
                                   (let ((#{rest 4786}# #{tmp 4784}#))
                                     (let ((#{tmp 4787}# #{clause 4762}#))
                                       (let ((#{tmp 4788}#
                                               ($sc-dispatch
                                                 #{tmp 4787}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp 4788}#
                                           (@apply
                                             (lambda (#{k 4792}#
                                                      #{e1 4793}#
                                                      #{e2 4794}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4789"
                                                             "i4790"
                                                             "i4791"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(rest)
                                                           #((top))
                                                           #("i4785"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4758"
                                                             "i4759"
                                                             "i4760"))
                                                         #(ribcage
                                                           #(e m1 m2)
                                                           #((top) (top) (top))
                                                           #("i4748"
                                                             "i4749"
                                                             "i4750"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4745")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4789"
                                                                   "i4790"
                                                                   "i4791"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4785"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(f
                                                                   clause
                                                                   clauses)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4758"
                                                                   "i4759"
                                                                   "i4760"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4748"
                                                                   "i4749"
                                                                   "i4750"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4745")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4789"
                                                                   "i4790"
                                                                   "i4791"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4785"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(f
                                                                   clause
                                                                   clauses)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4758"
                                                                   "i4759"
                                                                   "i4760"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4748"
                                                                   "i4749"
                                                                   "i4750"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4745")))
                                                              (hygiene guile))
                                                           (list '#(syntax-object
                                                                    quote
                                                                    ((top)
                                                                     #(ribcage
                                                                       #(k
                                                                         e1
                                                                         e2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4789"
                                                                         "i4790"
                                                                         "i4791"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(rest)
                                                                       #((top))
                                                                       #("i4785"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(f
                                                                         clause
                                                                         clauses)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4758"
                                                                         "i4759"
                                                                         "i4760"))
                                                                     #(ribcage
                                                                       #(e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4748"
                                                                         "i4749"
                                                                         "i4750"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4745")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k 4792}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4789"
                                                                   "i4790"
                                                                   "i4791"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4785"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(f
                                                                   clause
                                                                   clauses)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4758"
                                                                   "i4759"
                                                                   "i4760"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4748"
                                                                   "i4749"
                                                                   "i4750"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4745")))
                                                              (hygiene guile))
                                                           (cons #{e1 4793}#
                                                                 #{e2 4794}#))
                                                     #{rest 4786}#))
                                             #{tmp 4788}#)
                                           (let ((#{_ 4798}# #{tmp 4787}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x 4744}#
                                               #{clause 4762}#)))))))))))
                          (#{f 4761}# #{m1 4752}# #{m2 4753}#))))
                  (let ((#{body 4757}# #{tmp 4755}#))
                    (list '#(syntax-object
                             let
                             ((top)
                              #(ribcage () () ())
                              #(ribcage #(body) #((top)) #("i4756"))
                              #(ribcage
                                #(e m1 m2)
                                #((top) (top) (top))
                                #("i4748" "i4749" "i4750"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4745")))
                             (hygiene guile))
                          (list (list '#(syntax-object
                                         t
                                         ((top)
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(body)
                                            #((top))
                                            #("i4756"))
                                          #(ribcage
                                            #(e m1 m2)
                                            #((top) (top) (top))
                                            #("i4748" "i4749" "i4750"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4745")))
                                         (hygiene guile))
                                      #{e 4751}#))
                          #{body 4757}#))))
              #{tmp 4747}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4746}#)))))))

(define make-variable-transformer
  (lambda (#{proc 4799}#)
    (if (procedure? #{proc 4799}#)
      (letrec*
        ((#{trans 4802}#
           (lambda (#{x 4803}#) (#{proc 4799}# #{x 4803}#))))
        (begin
          (set-procedure-property!
            #{trans 4802}#
            'variable-transformer
            #t)
          #{trans 4802}#))
      (error "variable transformer not a procedure"
             #{proc 4799}#))))

(define identifier-syntax
  (make-syntax-transformer
    'identifier-syntax
    'macro
    (lambda (#{x 4805}#)
      (let ((#{tmp 4807}# #{x 4805}#))
        (let ((#{tmp 4808}#
                ($sc-dispatch #{tmp 4807}# '(_ any))))
          (if #{tmp 4808}#
            (@apply
              (lambda (#{e 4810}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage #(e) #((top)) #("i4809"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4806")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage #(e) #((top)) #("i4809"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4806")))
                          (hygiene guile)))
                      '#((#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage #(e) #((top)) #("i4809"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4806")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            identifier-syntax
                            ((top)
                             #(ribcage #(e) #((top)) #("i4809"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4806")))
                            (hygiene guile))))
                      (list '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage #(e) #((top)) #("i4809"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4806")))
                               (hygiene guile))
                            '#(syntax-object
                               x
                               ((top)
                                #(ribcage #(e) #((top)) #("i4809"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4806")))
                               (hygiene guile))
                            '()
                            (list '#(syntax-object
                                     id
                                     ((top)
                                      #(ribcage #(e) #((top)) #("i4809"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4806")))
                                     (hygiene guile))
                                  '(#(syntax-object
                                      identifier?
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4809"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4806")))
                                      (hygiene guile))
                                    (#(syntax-object
                                       syntax
                                       ((top)
                                        #(ribcage #(e) #((top)) #("i4809"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4806")))
                                       (hygiene guile))
                                     #(syntax-object
                                       id
                                       ((top)
                                        #(ribcage #(e) #((top)) #("i4809"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4806")))
                                       (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage #(e) #((top)) #("i4809"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4806")))
                                           (hygiene guile))
                                        #{e 4810}#))
                            (list '(#(syntax-object
                                      _
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4809"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4806")))
                                      (hygiene guile))
                                    #(syntax-object
                                      x
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4809"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4806")))
                                      (hygiene guile))
                                    #(syntax-object
                                      ...
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4809"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4806")))
                                      (hygiene guile)))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage #(e) #((top)) #("i4809"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4806")))
                                           (hygiene guile))
                                        (cons #{e 4810}#
                                              '(#(syntax-object
                                                  x
                                                  ((top)
                                                   #(ribcage
                                                     #(e)
                                                     #((top))
                                                     #("i4809"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4806")))
                                                  (hygiene guile))
                                                #(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(e)
                                                     #((top))
                                                     #("i4809"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4806")))
                                                  (hygiene guile)))))))))
              #{tmp 4808}#)
            (let ((#{tmp 4811}#
                    ($sc-dispatch
                      #{tmp 4807}#
                      '(_ (any any)
                          ((#(free-id
                              #(syntax-object
                                set!
                                ((top)
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4806")))
                                (hygiene guile)))
                            any
                            any)
                           any)))))
              (if (if #{tmp 4811}#
                    (@apply
                      (lambda (#{id 4817}#
                               #{exp1 4818}#
                               #{var 4819}#
                               #{val 4820}#
                               #{exp2 4821}#)
                        (if (identifier? #{id 4817}#)
                          (identifier? #{var 4819}#)
                          #f))
                      #{tmp 4811}#)
                    #f)
                (@apply
                  (lambda (#{id 4829}#
                           #{exp1 4830}#
                           #{var 4831}#
                           #{val 4832}#
                           #{exp2 4833}#)
                    (list '#(syntax-object
                             make-variable-transformer
                             ((top)
                              #(ribcage
                                #(id exp1 var val exp2)
                                #((top) (top) (top) (top) (top))
                                #("i4824" "i4825" "i4826" "i4827" "i4828"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4806")))
                             (hygiene guile))
                          (list '#(syntax-object
                                   lambda
                                   ((top)
                                    #(ribcage
                                      #(id exp1 var val exp2)
                                      #((top) (top) (top) (top) (top))
                                      #("i4824"
                                        "i4825"
                                        "i4826"
                                        "i4827"
                                        "i4828"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i4806")))
                                   (hygiene guile))
                                '(#(syntax-object
                                    x
                                    ((top)
                                     #(ribcage
                                       #(id exp1 var val exp2)
                                       #((top) (top) (top) (top) (top))
                                       #("i4824"
                                         "i4825"
                                         "i4826"
                                         "i4827"
                                         "i4828"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i4806")))
                                    (hygiene guile)))
                                '#((#(syntax-object
                                      macro-type
                                      ((top)
                                       #(ribcage
                                         #(id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top))
                                         #("i4824"
                                           "i4825"
                                           "i4826"
                                           "i4827"
                                           "i4828"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4806")))
                                      (hygiene guile))
                                    .
                                    #(syntax-object
                                      variable-transformer
                                      ((top)
                                       #(ribcage
                                         #(id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top))
                                         #("i4824"
                                           "i4825"
                                           "i4826"
                                           "i4827"
                                           "i4828"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4806")))
                                      (hygiene guile))))
                                (list '#(syntax-object
                                         syntax-case
                                         ((top)
                                          #(ribcage
                                            #(id exp1 var val exp2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4824"
                                              "i4825"
                                              "i4826"
                                              "i4827"
                                              "i4828"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4806")))
                                         (hygiene guile))
                                      '#(syntax-object
                                         x
                                         ((top)
                                          #(ribcage
                                            #(id exp1 var val exp2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4824"
                                              "i4825"
                                              "i4826"
                                              "i4827"
                                              "i4828"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4806")))
                                         (hygiene guile))
                                      '(#(syntax-object
                                          set!
                                          ((top)
                                           #(ribcage
                                             #(id exp1 var val exp2)
                                             #((top) (top) (top) (top) (top))
                                             #("i4824"
                                               "i4825"
                                               "i4826"
                                               "i4827"
                                               "i4828"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i4806")))
                                          (hygiene guile)))
                                      (list (list '#(syntax-object
                                                     set!
                                                     ((top)
                                                      #(ribcage
                                                        #(id exp1 var val exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i4824"
                                                          "i4825"
                                                          "i4826"
                                                          "i4827"
                                                          "i4828"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4806")))
                                                     (hygiene guile))
                                                  #{var 4831}#
                                                  #{val 4832}#)
                                            (list '#(syntax-object
                                                     syntax
                                                     ((top)
                                                      #(ribcage
                                                        #(id exp1 var val exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i4824"
                                                          "i4825"
                                                          "i4826"
                                                          "i4827"
                                                          "i4828"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4806")))
                                                     (hygiene guile))
                                                  #{exp2 4833}#))
                                      (list (cons #{id 4829}#
                                                  '(#(syntax-object
                                                      x
                                                      ((top)
                                                       #(ribcage
                                                         #(id
                                                           exp1
                                                           var
                                                           val
                                                           exp2)
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i4824"
                                                           "i4825"
                                                           "i4826"
                                                           "i4827"
                                                           "i4828"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4806")))
                                                      (hygiene guile))
                                                    #(syntax-object
                                                      ...
                                                      ((top)
                                                       #(ribcage
                                                         #(id
                                                           exp1
                                                           var
                                                           val
                                                           exp2)
                                                         #((top)
                                                           (top)
                                                           (top)
                                                           (top)
                                                           (top))
                                                         #("i4824"
                                                           "i4825"
                                                           "i4826"
                                                           "i4827"
                                                           "i4828"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4806")))
                                                      (hygiene guile))))
                                            (list '#(syntax-object
                                                     syntax
                                                     ((top)
                                                      #(ribcage
                                                        #(id exp1 var val exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i4824"
                                                          "i4825"
                                                          "i4826"
                                                          "i4827"
                                                          "i4828"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4806")))
                                                     (hygiene guile))
                                                  (cons #{exp1 4830}#
                                                        '(#(syntax-object
                                                            x
                                                            ((top)
                                                             #(ribcage
                                                               #(id
                                                                 exp1
                                                                 var
                                                                 val
                                                                 exp2)
                                                               #((top)
                                                                 (top)
                                                                 (top)
                                                                 (top)
                                                                 (top))
                                                               #("i4824"
                                                                 "i4825"
                                                                 "i4826"
                                                                 "i4827"
                                                                 "i4828"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4806")))
                                                            (hygiene guile))
                                                          #(syntax-object
                                                            ...
                                                            ((top)
                                                             #(ribcage
                                                               #(id
                                                                 exp1
                                                                 var
                                                                 val
                                                                 exp2)
                                                               #((top)
                                                                 (top)
                                                                 (top)
                                                                 (top)
                                                                 (top))
                                                               #("i4824"
                                                                 "i4825"
                                                                 "i4826"
                                                                 "i4827"
                                                                 "i4828"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4806")))
                                                            (hygiene
                                                              guile))))))
                                      (list #{id 4829}#
                                            (list '#(syntax-object
                                                     identifier?
                                                     ((top)
                                                      #(ribcage
                                                        #(id exp1 var val exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i4824"
                                                          "i4825"
                                                          "i4826"
                                                          "i4827"
                                                          "i4828"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4806")))
                                                     (hygiene guile))
                                                  (list '#(syntax-object
                                                           syntax
                                                           ((top)
                                                            #(ribcage
                                                              #(id
                                                                exp1
                                                                var
                                                                val
                                                                exp2)
                                                              #((top)
                                                                (top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                              #("i4824"
                                                                "i4825"
                                                                "i4826"
                                                                "i4827"
                                                                "i4828"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(x)
                                                              #((top))
                                                              #("i4806")))
                                                           (hygiene guile))
                                                        #{id 4829}#))
                                            (list '#(syntax-object
                                                     syntax
                                                     ((top)
                                                      #(ribcage
                                                        #(id exp1 var val exp2)
                                                        #((top)
                                                          (top)
                                                          (top)
                                                          (top)
                                                          (top))
                                                        #("i4824"
                                                          "i4825"
                                                          "i4826"
                                                          "i4827"
                                                          "i4828"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4806")))
                                                     (hygiene guile))
                                                  #{exp1 4830}#))))))
                  #{tmp 4811}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4807}#)))))))))

(define define*
  (make-syntax-transformer
    'define*
    'macro
    (lambda (#{x 4834}#)
      (let ((#{tmp 4836}# #{x 4834}#))
        (let ((#{tmp 4837}#
                ($sc-dispatch
                  #{tmp 4836}#
                  '(_ (any . any) any . each-any))))
          (if #{tmp 4837}#
            (@apply
              (lambda (#{id 4842}#
                       #{args 4843}#
                       #{b0 4844}#
                       #{b1 4845}#)
                (list '#(syntax-object
                         define
                         ((top)
                          #(ribcage
                            #(id args b0 b1)
                            #((top) (top) (top) (top))
                            #("i4838" "i4839" "i4840" "i4841"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4835")))
                         (hygiene guile))
                      #{id 4842}#
                      (cons '#(syntax-object
                               lambda*
                               ((top)
                                #(ribcage
                                  #(id args b0 b1)
                                  #((top) (top) (top) (top))
                                  #("i4838" "i4839" "i4840" "i4841"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4835")))
                               (hygiene guile))
                            (cons #{args 4843}#
                                  (cons #{b0 4844}# #{b1 4845}#)))))
              #{tmp 4837}#)
            (let ((#{tmp 4847}#
                    ($sc-dispatch #{tmp 4836}# '(_ any any))))
              (if (if #{tmp 4847}#
                    (@apply
                      (lambda (#{id 4850}# #{val 4851}#)
                        (identifier?
                          '#(syntax-object
                             x
                             ((top)
                              #(ribcage
                                #(id val)
                                #((top) (top))
                                #("i4848" "i4849"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4835")))
                             (hygiene guile))))
                      #{tmp 4847}#)
                    #f)
                (@apply
                  (lambda (#{id 4854}# #{val 4855}#)
                    (list '#(syntax-object
                             define
                             ((top)
                              #(ribcage
                                #(id val)
                                #((top) (top))
                                #("i4852" "i4853"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4835")))
                             (hygiene guile))
                          #{id 4854}#
                          #{val 4855}#))
                  #{tmp 4847}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4836}#)))))))))

