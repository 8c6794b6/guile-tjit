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
       (if (eq? (let ((#{x 1472}# #{i 1465}#))
                  (if (#{syntax-object? 309}# #{x 1472}#)
                    (#{syntax-object-expression 311}# #{x 1472}#)
                    #{x 1472}#))
                (let ((#{x 1475}# #{j 1466}#))
                  (if (#{syntax-object? 309}# #{x 1475}#)
                    (#{syntax-object-expression 311}# #{x 1475}#)
                    #{x 1475}#)))
         (eq? (#{id-var-name 397}# #{i 1465}# '(()))
              (#{id-var-name 397}# #{j 1466}# '(())))
         #f)))
   (#{bound-id=? 401}#
     (lambda (#{i 1479}# #{j 1480}#)
       (if (if (#{syntax-object? 309}# #{i 1479}#)
             (#{syntax-object? 309}# #{j 1480}#)
             #f)
         (if (eq? (#{syntax-object-expression 311}# #{i 1479}#)
                  (#{syntax-object-expression 311}# #{j 1480}#))
           (#{same-marks? 395}#
             (car (#{syntax-object-wrap 313}# #{i 1479}#))
             (car (#{syntax-object-wrap 313}# #{j 1480}#)))
           #f)
         (eq? #{i 1479}# #{j 1480}#))))
   (#{valid-bound-ids? 403}#
     (lambda (#{ids 1489}#)
       (if (letrec*
             ((#{all-ids? 1494}#
                (lambda (#{ids 1495}#)
                  (let ((#{t 1498}# (null? #{ids 1495}#)))
                    (if #{t 1498}#
                      #{t 1498}#
                      (if (#{id? 343}# (car #{ids 1495}#))
                        (#{all-ids? 1494}# (cdr #{ids 1495}#))
                        #f))))))
             (#{all-ids? 1494}# #{ids 1489}#))
         (#{distinct-bound-ids? 405}# #{ids 1489}#)
         #f)))
   (#{distinct-bound-ids? 405}#
     (lambda (#{ids 1503}#)
       (letrec*
         ((#{distinct? 1507}#
            (lambda (#{ids 1508}#)
              (let ((#{t 1511}# (null? #{ids 1508}#)))
                (if #{t 1511}#
                  #{t 1511}#
                  (if (not (#{bound-id-member? 407}#
                             (car #{ids 1508}#)
                             (cdr #{ids 1508}#)))
                    (#{distinct? 1507}# (cdr #{ids 1508}#))
                    #f))))))
         (#{distinct? 1507}# #{ids 1503}#))))
   (#{bound-id-member? 407}#
     (lambda (#{x 1515}# #{list 1516}#)
       (if (not (null? #{list 1516}#))
         (let ((#{t 1523}#
                 (#{bound-id=? 401}#
                   #{x 1515}#
                   (car #{list 1516}#))))
           (if #{t 1523}#
             #{t 1523}#
             (#{bound-id-member? 407}#
               #{x 1515}#
               (cdr #{list 1516}#))))
         #f)))
   (#{wrap 409}#
     (lambda (#{x 1525}# #{w 1526}# #{defmod 1527}#)
       (if (if (null? (car #{w 1526}#))
             (null? (cdr #{w 1526}#))
             #f)
         #{x 1525}#
         (if (#{syntax-object? 309}# #{x 1525}#)
           (#{make-syntax-object 307}#
             (#{syntax-object-expression 311}# #{x 1525}#)
             (#{join-wraps 391}#
               #{w 1526}#
               (#{syntax-object-wrap 313}# #{x 1525}#))
             (#{syntax-object-module 315}# #{x 1525}#))
           (if (null? #{x 1525}#)
             #{x 1525}#
             (#{make-syntax-object 307}#
               #{x 1525}#
               #{w 1526}#
               #{defmod 1527}#))))))
   (#{source-wrap 411}#
     (lambda (#{x 1542}#
              #{w 1543}#
              #{s 1544}#
              #{defmod 1545}#)
       (#{wrap 409}#
         (#{decorate-source 261}# #{x 1542}# #{s 1544}#)
         #{w 1543}#
         #{defmod 1545}#)))
   (#{chi-sequence 413}#
     (lambda (#{body 1550}#
              #{r 1551}#
              #{w 1552}#
              #{s 1553}#
              #{mod 1554}#)
       (#{build-sequence 297}#
         #{s 1553}#
         (letrec*
           ((#{dobody 1565}#
              (lambda (#{body 1566}#
                       #{r 1567}#
                       #{w 1568}#
                       #{mod 1569}#)
                (if (null? #{body 1566}#)
                  '()
                  (let ((#{first 1571}#
                          (#{chi 423}#
                            (car #{body 1566}#)
                            #{r 1567}#
                            #{w 1568}#
                            #{mod 1569}#)))
                    (cons #{first 1571}#
                          (#{dobody 1565}#
                            (cdr #{body 1566}#)
                            #{r 1567}#
                            #{w 1568}#
                            #{mod 1569}#)))))))
           (#{dobody 1565}#
             #{body 1550}#
             #{r 1551}#
             #{w 1552}#
             #{mod 1554}#)))))
   (#{chi-top-sequence 415}#
     (lambda (#{body 1572}#
              #{r 1573}#
              #{w 1574}#
              #{s 1575}#
              #{m 1576}#
              #{esew 1577}#
              #{mod 1578}#)
       (letrec*
         ((#{scan 1587}#
            (lambda (#{body 1588}#
                     #{r 1589}#
                     #{w 1590}#
                     #{s 1591}#
                     #{m 1592}#
                     #{esew 1593}#
                     #{mod 1594}#
                     #{exps 1595}#)
              (if (null? #{body 1588}#)
                #{exps 1595}#
                (call-with-values
                  (lambda ()
                    (call-with-values
                      (lambda ()
                        (let ((#{e 1608}# (car #{body 1588}#)))
                          (#{syntax-type 421}#
                            #{e 1608}#
                            #{r 1589}#
                            #{w 1590}#
                            (let ((#{t 1611}#
                                    (#{source-annotation 324}# #{e 1608}#)))
                              (if #{t 1611}# #{t 1611}# #{s 1591}#))
                            #f
                            #{mod 1594}#
                            #f)))
                      (lambda (#{type 1613}#
                               #{value 1614}#
                               #{e 1615}#
                               #{w 1616}#
                               #{s 1617}#
                               #{mod 1618}#)
                        (if (memv #{type 1613}# '(begin-form))
                          (let ((#{tmp 1626}# #{e 1615}#))
                            (let ((#{tmp 1627}#
                                    ($sc-dispatch #{tmp 1626}# '(_))))
                              (if #{tmp 1627}#
                                (@apply (lambda () #{exps 1595}#) #{tmp 1627}#)
                                (let ((#{tmp 1628}#
                                        ($sc-dispatch
                                          #{tmp 1626}#
                                          '(_ any . each-any))))
                                  (if #{tmp 1628}#
                                    (@apply
                                      (lambda (#{e1 1631}# #{e2 1632}#)
                                        (#{scan 1587}#
                                          (cons #{e1 1631}# #{e2 1632}#)
                                          #{r 1589}#
                                          #{w 1616}#
                                          #{s 1617}#
                                          #{m 1592}#
                                          #{esew 1593}#
                                          #{mod 1618}#
                                          #{exps 1595}#))
                                      #{tmp 1628}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp 1626}#))))))
                          (if (memv #{type 1613}# '(local-syntax-form))
                            (#{chi-local-syntax 433}#
                              #{value 1614}#
                              #{e 1615}#
                              #{r 1589}#
                              #{w 1616}#
                              #{s 1617}#
                              #{mod 1618}#
                              (lambda (#{body 1635}#
                                       #{r 1636}#
                                       #{w 1637}#
                                       #{s 1638}#
                                       #{mod 1639}#)
                                (#{scan 1587}#
                                  #{body 1635}#
                                  #{r 1636}#
                                  #{w 1637}#
                                  #{s 1638}#
                                  #{m 1592}#
                                  #{esew 1593}#
                                  #{mod 1639}#
                                  #{exps 1595}#)))
                            (if (memv #{type 1613}# '(eval-when-form))
                              (let ((#{tmp 1646}# #{e 1615}#))
                                (let ((#{tmp 1647}#
                                        ($sc-dispatch
                                          #{tmp 1646}#
                                          '(_ each-any any . each-any))))
                                  (if #{tmp 1647}#
                                    (@apply
                                      (lambda (#{x 1651}#
                                               #{e1 1652}#
                                               #{e2 1653}#)
                                        (let ((#{when-list 1656}#
                                                (#{chi-when-list 419}#
                                                  #{e 1615}#
                                                  #{x 1651}#
                                                  #{w 1616}#))
                                              (#{body 1657}#
                                                (cons #{e1 1652}#
                                                      #{e2 1653}#)))
                                          (if (eq? #{m 1592}# 'e)
                                            (if (memq 'eval #{when-list 1656}#)
                                              (#{scan 1587}#
                                                #{body 1657}#
                                                #{r 1589}#
                                                #{w 1616}#
                                                #{s 1617}#
                                                (if (memq 'expand
                                                          #{when-list 1656}#)
                                                  'c&e
                                                  'e)
                                                '(eval)
                                                #{mod 1618}#
                                                #{exps 1595}#)
                                              (begin
                                                (if (memq 'expand
                                                          #{when-list 1656}#)
                                                  (#{top-level-eval-hook 252}#
                                                    (#{chi-top-sequence 415}#
                                                      #{body 1657}#
                                                      #{r 1589}#
                                                      #{w 1616}#
                                                      #{s 1617}#
                                                      'e
                                                      '(eval)
                                                      #{mod 1618}#)
                                                    #{mod 1618}#))
                                                (values #{exps 1595}#)))
                                            (if (memq 'load #{when-list 1656}#)
                                              (if (let ((#{t 1666}#
                                                          (memq 'compile
                                                                #{when-list 1656}#)))
                                                    (if #{t 1666}#
                                                      #{t 1666}#
                                                      (let ((#{t 1669}#
                                                              (memq 'expand
                                                                    #{when-list 1656}#)))
                                                        (if #{t 1669}#
                                                          #{t 1669}#
                                                          (if (eq? #{m 1592}#
                                                                   'c&e)
                                                            (memq 'eval
                                                                  #{when-list 1656}#)
                                                            #f)))))
                                                (#{scan 1587}#
                                                  #{body 1657}#
                                                  #{r 1589}#
                                                  #{w 1616}#
                                                  #{s 1617}#
                                                  'c&e
                                                  '(compile load)
                                                  #{mod 1618}#
                                                  #{exps 1595}#)
                                                (if (memq #{m 1592}# '(c c&e))
                                                  (#{scan 1587}#
                                                    #{body 1657}#
                                                    #{r 1589}#
                                                    #{w 1616}#
                                                    #{s 1617}#
                                                    'c
                                                    '(load)
                                                    #{mod 1618}#
                                                    #{exps 1595}#)
                                                  (values #{exps 1595}#)))
                                              (if (let ((#{t 1677}#
                                                          (memq 'compile
                                                                #{when-list 1656}#)))
                                                    (if #{t 1677}#
                                                      #{t 1677}#
                                                      (let ((#{t 1680}#
                                                              (memq 'expand
                                                                    #{when-list 1656}#)))
                                                        (if #{t 1680}#
                                                          #{t 1680}#
                                                          (if (eq? #{m 1592}#
                                                                   'c&e)
                                                            (memq 'eval
                                                                  #{when-list 1656}#)
                                                            #f)))))
                                                (begin
                                                  (#{top-level-eval-hook 252}#
                                                    (#{chi-top-sequence 415}#
                                                      #{body 1657}#
                                                      #{r 1589}#
                                                      #{w 1616}#
                                                      #{s 1617}#
                                                      'e
                                                      '(eval)
                                                      #{mod 1618}#)
                                                    #{mod 1618}#)
                                                  (values #{exps 1595}#))
                                                (values #{exps 1595}#))))))
                                      #{tmp 1647}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp 1646}#))))
                              (if (memv #{type 1613}# '(define-syntax-form))
                                (let ((#{n 1688}#
                                        (#{id-var-name 397}#
                                          #{value 1614}#
                                          #{w 1616}#))
                                      (#{r 1689}#
                                        (#{macros-only-env 335}# #{r 1589}#)))
                                  (if (memv #{m 1592}# '(c))
                                    (if (memq 'compile #{esew 1593}#)
                                      (let ((#{e 1692}#
                                              (#{chi-install-global 417}#
                                                #{n 1688}#
                                                (#{chi 423}#
                                                  #{e 1615}#
                                                  #{r 1689}#
                                                  #{w 1616}#
                                                  #{mod 1618}#))))
                                        (begin
                                          (#{top-level-eval-hook 252}#
                                            #{e 1692}#
                                            #{mod 1618}#)
                                          (if (memq 'load #{esew 1593}#)
                                            (values
                                              (cons #{e 1692}# #{exps 1595}#))
                                            (values #{exps 1595}#))))
                                      (if (memq 'load #{esew 1593}#)
                                        (values
                                          (cons (#{chi-install-global 417}#
                                                  #{n 1688}#
                                                  (#{chi 423}#
                                                    #{e 1615}#
                                                    #{r 1689}#
                                                    #{w 1616}#
                                                    #{mod 1618}#))
                                                #{exps 1595}#))
                                        (values #{exps 1595}#)))
                                    (if (memv #{m 1592}# '(c&e))
                                      (let ((#{e 1695}#
                                              (#{chi-install-global 417}#
                                                #{n 1688}#
                                                (#{chi 423}#
                                                  #{e 1615}#
                                                  #{r 1689}#
                                                  #{w 1616}#
                                                  #{mod 1618}#))))
                                        (begin
                                          (#{top-level-eval-hook 252}#
                                            #{e 1695}#
                                            #{mod 1618}#)
                                          (values
                                            (cons #{e 1695}# #{exps 1595}#))))
                                      (begin
                                        (if (memq 'eval #{esew 1593}#)
                                          (#{top-level-eval-hook 252}#
                                            (#{chi-install-global 417}#
                                              #{n 1688}#
                                              (#{chi 423}#
                                                #{e 1615}#
                                                #{r 1689}#
                                                #{w 1616}#
                                                #{mod 1618}#))
                                            #{mod 1618}#))
                                        (values #{exps 1595}#)))))
                                (if (memv #{type 1613}# '(define-form))
                                  (let ((#{n 1700}#
                                          (#{id-var-name 397}#
                                            #{value 1614}#
                                            #{w 1616}#)))
                                    (let ((#{type 1702}#
                                            (car (#{lookup 337}#
                                                   #{n 1700}#
                                                   #{r 1589}#
                                                   #{mod 1618}#))))
                                      (if (memv #{type 1702}#
                                                '(global
                                                   core
                                                   macro
                                                   module-ref))
                                        (begin
                                          (if (if (memq #{m 1592}# '(c c&e))
                                                (if (not (module-local-variable
                                                           (current-module)
                                                           #{n 1700}#))
                                                  (current-module)
                                                  #f)
                                                #f)
                                            (let ((#{old 1709}#
                                                    (module-variable
                                                      (current-module)
                                                      #{n 1700}#)))
                                              (if (if (variable? #{old 1709}#)
                                                    (variable-bound?
                                                      #{old 1709}#)
                                                    #f)
                                                (module-define!
                                                  (current-module)
                                                  #{n 1700}#
                                                  (variable-ref #{old 1709}#))
                                                (module-add!
                                                  (current-module)
                                                  #{n 1700}#
                                                  (make-undefined-variable)))))
                                          (values
                                            (cons (if (eq? #{m 1592}# 'c&e)
                                                    (let ((#{x 1713}#
                                                            (#{build-global-definition 283}#
                                                              #{s 1617}#
                                                              #{n 1700}#
                                                              (#{chi 423}#
                                                                #{e 1615}#
                                                                #{r 1589}#
                                                                #{w 1616}#
                                                                #{mod 1618}#))))
                                                      (begin
                                                        (#{top-level-eval-hook 252}#
                                                          #{x 1713}#
                                                          #{mod 1618}#)
                                                        #{x 1713}#))
                                                    (lambda ()
                                                      (#{build-global-definition 283}#
                                                        #{s 1617}#
                                                        #{n 1700}#
                                                        (#{chi 423}#
                                                          #{e 1615}#
                                                          #{r 1589}#
                                                          #{w 1616}#
                                                          #{mod 1618}#))))
                                                  #{exps 1595}#)))
                                        (if (memv #{type 1702}#
                                                  '(displaced-lexical))
                                          (syntax-violation
                                            #f
                                            "identifier out of context"
                                            #{e 1615}#
                                            (#{wrap 409}#
                                              #{value 1614}#
                                              #{w 1616}#
                                              #{mod 1618}#))
                                          (syntax-violation
                                            #f
                                            "cannot define keyword at top level"
                                            #{e 1615}#
                                            (#{wrap 409}#
                                              #{value 1614}#
                                              #{w 1616}#
                                              #{mod 1618}#))))))
                                  (values
                                    (cons (if (eq? #{m 1592}# 'c&e)
                                            (let ((#{x 1718}#
                                                    (#{chi-expr 425}#
                                                      #{type 1613}#
                                                      #{value 1614}#
                                                      #{e 1615}#
                                                      #{r 1589}#
                                                      #{w 1616}#
                                                      #{s 1617}#
                                                      #{mod 1618}#)))
                                              (begin
                                                (#{top-level-eval-hook 252}#
                                                  #{x 1718}#
                                                  #{mod 1618}#)
                                                #{x 1718}#))
                                            (lambda ()
                                              (#{chi-expr 425}#
                                                #{type 1613}#
                                                #{value 1614}#
                                                #{e 1615}#
                                                #{r 1589}#
                                                #{w 1616}#
                                                #{s 1617}#
                                                #{mod 1618}#)))
                                          #{exps 1595}#))))))))))
                  (lambda (#{exps 1719}#)
                    (#{scan 1587}#
                      (cdr #{body 1588}#)
                      #{r 1589}#
                      #{w 1590}#
                      #{s 1591}#
                      #{m 1592}#
                      #{esew 1593}#
                      #{mod 1594}#
                      #{exps 1719}#)))))))
         (call-with-values
           (lambda ()
             (#{scan 1587}#
               #{body 1572}#
               #{r 1573}#
               #{w 1574}#
               #{s 1575}#
               #{m 1576}#
               #{esew 1577}#
               #{mod 1578}#
               '()))
           (lambda (#{exps 1721}#)
             (if (null? #{exps 1721}#)
               (#{build-void 265}# #{s 1575}#)
               (#{build-sequence 297}#
                 #{s 1575}#
                 (letrec*
                   ((#{lp 1726}#
                      (lambda (#{in 1727}# #{out 1728}#)
                        (if (null? #{in 1727}#)
                          #{out 1728}#
                          (let ((#{e 1730}# (car #{in 1727}#)))
                            (#{lp 1726}#
                              (cdr #{in 1727}#)
                              (cons (if (procedure? #{e 1730}#)
                                      (#{e 1730}#)
                                      #{e 1730}#)
                                    #{out 1728}#)))))))
                   (#{lp 1726}# #{exps 1721}# '())))))))))
   (#{chi-install-global 417}#
     (lambda (#{name 1731}# #{e 1732}#)
       (#{build-global-definition 283}#
         #f
         #{name 1731}#
         (#{build-primcall 291}#
           #f
           'make-syntax-transformer
           (list (#{build-data 295}# #f #{name 1731}#)
                 (#{build-data 295}# #f 'macro)
                 #{e 1732}#)))))
   (#{chi-when-list 419}#
     (lambda (#{e 1739}# #{when-list 1740}# #{w 1741}#)
       (letrec*
         ((#{f 1748}#
            (lambda (#{when-list 1749}# #{situations 1750}#)
              (if (null? #{when-list 1749}#)
                #{situations 1750}#
                (#{f 1748}#
                  (cdr #{when-list 1749}#)
                  (cons (let ((#{x 1752}# (car #{when-list 1749}#)))
                          (if (#{free-id=? 399}#
                                #{x 1752}#
                                '#(syntax-object
                                   compile
                                   ((top)
                                    #(ribcage () () ())
                                    #(ribcage () () ())
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i1751"))
                                    #(ribcage () () ())
                                    #(ribcage
                                      #(f when-list situations)
                                      #((top) (top) (top))
                                      #("i1745" "i1746" "i1747"))
                                    #(ribcage () () ())
                                    #(ribcage
                                      #(e when-list w)
                                      #((top) (top) (top))
                                      #("i1742" "i1743" "i1744"))
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
                            'compile
                            (if (#{free-id=? 399}#
                                  #{x 1752}#
                                  '#(syntax-object
                                     load
                                     ((top)
                                      #(ribcage () () ())
                                      #(ribcage () () ())
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i1751"))
                                      #(ribcage () () ())
                                      #(ribcage
                                        #(f when-list situations)
                                        #((top) (top) (top))
                                        #("i1745" "i1746" "i1747"))
                                      #(ribcage () () ())
                                      #(ribcage
                                        #(e when-list w)
                                        #((top) (top) (top))
                                        #("i1742" "i1743" "i1744"))
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
                              'load
                              (if (#{free-id=? 399}#
                                    #{x 1752}#
                                    '#(syntax-object
                                       eval
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i1751"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(f when-list situations)
                                          #((top) (top) (top))
                                          #("i1745" "i1746" "i1747"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(e when-list w)
                                          #((top) (top) (top))
                                          #("i1742" "i1743" "i1744"))
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
                                'eval
                                (if (#{free-id=? 399}#
                                      #{x 1752}#
                                      '#(syntax-object
                                         expand
                                         ((top)
                                          #(ribcage () () ())
                                          #(ribcage () () ())
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i1751"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(f when-list situations)
                                            #((top) (top) (top))
                                            #("i1745" "i1746" "i1747"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(e when-list w)
                                            #((top) (top) (top))
                                            #("i1742" "i1743" "i1744"))
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
                                  'expand
                                  (syntax-violation
                                    'eval-when
                                    "invalid situation"
                                    #{e 1739}#
                                    (#{wrap 409}#
                                      #{x 1752}#
                                      #{w 1741}#
                                      #f)))))))
                        #{situations 1750}#))))))
         (#{f 1748}# #{when-list 1740}# '()))))
   (#{syntax-type 421}#
     (lambda (#{e 1762}#
              #{r 1763}#
              #{w 1764}#
              #{s 1765}#
              #{rib 1766}#
              #{mod 1767}#
              #{for-car? 1768}#)
       (if (symbol? #{e 1762}#)
         (let ((#{n 1780}#
                 (#{id-var-name 397}# #{e 1762}# #{w 1764}#)))
           (let ((#{b 1782}#
                   (#{lookup 337}#
                     #{n 1780}#
                     #{r 1763}#
                     #{mod 1767}#)))
             (let ((#{type 1784}# (car #{b 1782}#)))
               (if (memv #{type 1784}# '(lexical))
                 (values
                   #{type 1784}#
                   (cdr #{b 1782}#)
                   #{e 1762}#
                   #{w 1764}#
                   #{s 1765}#
                   #{mod 1767}#)
                 (if (memv #{type 1784}# '(global))
                   (values
                     #{type 1784}#
                     #{n 1780}#
                     #{e 1762}#
                     #{w 1764}#
                     #{s 1765}#
                     #{mod 1767}#)
                   (if (memv #{type 1784}# '(macro))
                     (if #{for-car? 1768}#
                       (values
                         #{type 1784}#
                         (cdr #{b 1782}#)
                         #{e 1762}#
                         #{w 1764}#
                         #{s 1765}#
                         #{mod 1767}#)
                       (#{syntax-type 421}#
                         (#{chi-macro 429}#
                           (cdr #{b 1782}#)
                           #{e 1762}#
                           #{r 1763}#
                           #{w 1764}#
                           #{s 1765}#
                           #{rib 1766}#
                           #{mod 1767}#)
                         #{r 1763}#
                         '(())
                         #{s 1765}#
                         #{rib 1766}#
                         #{mod 1767}#
                         #f))
                     (values
                       #{type 1784}#
                       (cdr #{b 1782}#)
                       #{e 1762}#
                       #{w 1764}#
                       #{s 1765}#
                       #{mod 1767}#)))))))
         (if (pair? #{e 1762}#)
           (let ((#{first 1798}# (car #{e 1762}#)))
             (call-with-values
               (lambda ()
                 (#{syntax-type 421}#
                   #{first 1798}#
                   #{r 1763}#
                   #{w 1764}#
                   #{s 1765}#
                   #{rib 1766}#
                   #{mod 1767}#
                   #t))
               (lambda (#{ftype 1799}#
                        #{fval 1800}#
                        #{fe 1801}#
                        #{fw 1802}#
                        #{fs 1803}#
                        #{fmod 1804}#)
                 (if (memv #{ftype 1799}# '(lexical))
                   (values
                     'lexical-call
                     #{fval 1800}#
                     #{e 1762}#
                     #{w 1764}#
                     #{s 1765}#
                     #{mod 1767}#)
                   (if (memv #{ftype 1799}# '(global))
                     (values
                       'global-call
                       (#{make-syntax-object 307}#
                         #{fval 1800}#
                         #{w 1764}#
                         #{fmod 1804}#)
                       #{e 1762}#
                       #{w 1764}#
                       #{s 1765}#
                       #{mod 1767}#)
                     (if (memv #{ftype 1799}# '(macro))
                       (#{syntax-type 421}#
                         (#{chi-macro 429}#
                           #{fval 1800}#
                           #{e 1762}#
                           #{r 1763}#
                           #{w 1764}#
                           #{s 1765}#
                           #{rib 1766}#
                           #{mod 1767}#)
                         #{r 1763}#
                         '(())
                         #{s 1765}#
                         #{rib 1766}#
                         #{mod 1767}#
                         #{for-car? 1768}#)
                       (if (memv #{ftype 1799}# '(module-ref))
                         (call-with-values
                           (lambda ()
                             (#{fval 1800}# #{e 1762}# #{r 1763}# #{w 1764}#))
                           (lambda (#{e 1816}#
                                    #{r 1817}#
                                    #{w 1818}#
                                    #{s 1819}#
                                    #{mod 1820}#)
                             (#{syntax-type 421}#
                               #{e 1816}#
                               #{r 1817}#
                               #{w 1818}#
                               #{s 1819}#
                               #{rib 1766}#
                               #{mod 1820}#
                               #{for-car? 1768}#)))
                         (if (memv #{ftype 1799}# '(core))
                           (values
                             'core-form
                             #{fval 1800}#
                             #{e 1762}#
                             #{w 1764}#
                             #{s 1765}#
                             #{mod 1767}#)
                           (if (memv #{ftype 1799}# '(local-syntax))
                             (values
                               'local-syntax-form
                               #{fval 1800}#
                               #{e 1762}#
                               #{w 1764}#
                               #{s 1765}#
                               #{mod 1767}#)
                             (if (memv #{ftype 1799}# '(begin))
                               (values
                                 'begin-form
                                 #f
                                 #{e 1762}#
                                 #{w 1764}#
                                 #{s 1765}#
                                 #{mod 1767}#)
                               (if (memv #{ftype 1799}# '(eval-when))
                                 (values
                                   'eval-when-form
                                   #f
                                   #{e 1762}#
                                   #{w 1764}#
                                   #{s 1765}#
                                   #{mod 1767}#)
                                 (if (memv #{ftype 1799}# '(define))
                                   (let ((#{tmp 1831}# #{e 1762}#))
                                     (let ((#{tmp 1832}#
                                             ($sc-dispatch
                                               #{tmp 1831}#
                                               '(_ any any))))
                                       (if (if #{tmp 1832}#
                                             (@apply
                                               (lambda (#{name 1835}#
                                                        #{val 1836}#)
                                                 (#{id? 343}# #{name 1835}#))
                                               #{tmp 1832}#)
                                             #f)
                                         (@apply
                                           (lambda (#{name 1839}# #{val 1840}#)
                                             (values
                                               'define-form
                                               #{name 1839}#
                                               #{val 1840}#
                                               #{w 1764}#
                                               #{s 1765}#
                                               #{mod 1767}#))
                                           #{tmp 1832}#)
                                         (let ((#{tmp 1841}#
                                                 ($sc-dispatch
                                                   #{tmp 1831}#
                                                   '(_ (any . any)
                                                       any
                                                       .
                                                       each-any))))
                                           (if (if #{tmp 1841}#
                                                 (@apply
                                                   (lambda (#{name 1846}#
                                                            #{args 1847}#
                                                            #{e1 1848}#
                                                            #{e2 1849}#)
                                                     (if (#{id? 343}#
                                                           #{name 1846}#)
                                                       (#{valid-bound-ids? 403}#
                                                         (#{lambda-var-list 453}#
                                                           #{args 1847}#))
                                                       #f))
                                                   #{tmp 1841}#)
                                                 #f)
                                             (@apply
                                               (lambda (#{name 1856}#
                                                        #{args 1857}#
                                                        #{e1 1858}#
                                                        #{e2 1859}#)
                                                 (values
                                                   'define-form
                                                   (#{wrap 409}#
                                                     #{name 1856}#
                                                     #{w 1764}#
                                                     #{mod 1767}#)
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
                                                                 #("i1852"
                                                                   "i1853"
                                                                   "i1854"
                                                                   "i1855"))
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
                                                                 #("i1805"
                                                                   "i1806"
                                                                   "i1807"
                                                                   "i1808"
                                                                   "i1809"
                                                                   "i1810"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(first)
                                                                 #((top))
                                                                 #("i1797"))
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
                                                                 #("i1769"
                                                                   "i1770"
                                                                   "i1771"
                                                                   "i1772"
                                                                   "i1773"
                                                                   "i1774"
                                                                   "i1775"))
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
                                                             (cons #{args 1857}#
                                                                   (cons #{e1 1858}#
                                                                         #{e2 1859}#))
                                                             #{w 1764}#
                                                             #{mod 1767}#))
                                                     #{s 1765}#)
                                                   '(())
                                                   #{s 1765}#
                                                   #{mod 1767}#))
                                               #{tmp 1841}#)
                                             (let ((#{tmp 1862}#
                                                     ($sc-dispatch
                                                       #{tmp 1831}#
                                                       '(_ any))))
                                               (if (if #{tmp 1862}#
                                                     (@apply
                                                       (lambda (#{name 1864}#)
                                                         (#{id? 343}#
                                                           #{name 1864}#))
                                                       #{tmp 1862}#)
                                                     #f)
                                                 (@apply
                                                   (lambda (#{name 1866}#)
                                                     (values
                                                       'define-form
                                                       (#{wrap 409}#
                                                         #{name 1866}#
                                                         #{w 1764}#
                                                         #{mod 1767}#)
                                                       '(#(syntax-object
                                                           if
                                                           ((top)
                                                            #(ribcage
                                                              #(name)
                                                              #((top))
                                                              #("i1865"))
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
                                                              #("i1805"
                                                                "i1806"
                                                                "i1807"
                                                                "i1808"
                                                                "i1809"
                                                                "i1810"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(first)
                                                              #((top))
                                                              #("i1797"))
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
                                                              #("i1769"
                                                                "i1770"
                                                                "i1771"
                                                                "i1772"
                                                                "i1773"
                                                                "i1774"
                                                                "i1775"))
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
                                                              #("i1865"))
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
                                                              #("i1805"
                                                                "i1806"
                                                                "i1807"
                                                                "i1808"
                                                                "i1809"
                                                                "i1810"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(first)
                                                              #((top))
                                                              #("i1797"))
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
                                                              #("i1769"
                                                                "i1770"
                                                                "i1771"
                                                                "i1772"
                                                                "i1773"
                                                                "i1774"
                                                                "i1775"))
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
                                                              #("i1865"))
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
                                                              #("i1805"
                                                                "i1806"
                                                                "i1807"
                                                                "i1808"
                                                                "i1809"
                                                                "i1810"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(first)
                                                              #((top))
                                                              #("i1797"))
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
                                                              #("i1769"
                                                                "i1770"
                                                                "i1771"
                                                                "i1772"
                                                                "i1773"
                                                                "i1774"
                                                                "i1775"))
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
                                                       #{s 1765}#
                                                       #{mod 1767}#))
                                                   #{tmp 1862}#)
                                                 (syntax-violation
                                                   #f
                                                   "source expression failed to match any pattern"
                                                   #{tmp 1831}#))))))))
                                   (if (memv #{ftype 1799}# '(define-syntax))
                                     (let ((#{tmp 1869}# #{e 1762}#))
                                       (let ((#{tmp 1870}#
                                               ($sc-dispatch
                                                 #{tmp 1869}#
                                                 '(_ any any))))
                                         (if (if #{tmp 1870}#
                                               (@apply
                                                 (lambda (#{name 1873}#
                                                          #{val 1874}#)
                                                   (#{id? 343}# #{name 1873}#))
                                                 #{tmp 1870}#)
                                               #f)
                                           (@apply
                                             (lambda (#{name 1877}#
                                                      #{val 1878}#)
                                               (values
                                                 'define-syntax-form
                                                 #{name 1877}#
                                                 #{val 1878}#
                                                 #{w 1764}#
                                                 #{s 1765}#
                                                 #{mod 1767}#))
                                             #{tmp 1870}#)
                                           (syntax-violation
                                             #f
                                             "source expression failed to match any pattern"
                                             #{tmp 1869}#))))
                                     (values
                                       'call
                                       #f
                                       #{e 1762}#
                                       #{w 1764}#
                                       #{s 1765}#
                                       #{mod 1767}#))))))))))))))
           (if (#{syntax-object? 309}# #{e 1762}#)
             (#{syntax-type 421}#
               (#{syntax-object-expression 311}# #{e 1762}#)
               #{r 1763}#
               (#{join-wraps 391}#
                 #{w 1764}#
                 (#{syntax-object-wrap 313}# #{e 1762}#))
               (let ((#{t 1884}#
                       (#{source-annotation 324}# #{e 1762}#)))
                 (if #{t 1884}# #{t 1884}# #{s 1765}#))
               #{rib 1766}#
               (let ((#{t 1888}#
                       (#{syntax-object-module 315}# #{e 1762}#)))
                 (if #{t 1888}# #{t 1888}# #{mod 1767}#))
               #{for-car? 1768}#)
             (if (self-evaluating? #{e 1762}#)
               (values
                 'constant
                 #f
                 #{e 1762}#
                 #{w 1764}#
                 #{s 1765}#
                 #{mod 1767}#)
               (values
                 'other
                 #f
                 #{e 1762}#
                 #{w 1764}#
                 #{s 1765}#
                 #{mod 1767}#)))))))
   (#{chi 423}#
     (lambda (#{e 1893}# #{r 1894}# #{w 1895}# #{mod 1896}#)
       (call-with-values
         (lambda ()
           (#{syntax-type 421}#
             #{e 1893}#
             #{r 1894}#
             #{w 1895}#
             (#{source-annotation 324}# #{e 1893}#)
             #f
             #{mod 1896}#
             #f))
         (lambda (#{type 1901}#
                  #{value 1902}#
                  #{e 1903}#
                  #{w 1904}#
                  #{s 1905}#
                  #{mod 1906}#)
           (#{chi-expr 425}#
             #{type 1901}#
             #{value 1902}#
             #{e 1903}#
             #{r 1894}#
             #{w 1904}#
             #{s 1905}#
             #{mod 1906}#)))))
   (#{chi-expr 425}#
     (lambda (#{type 1913}#
              #{value 1914}#
              #{e 1915}#
              #{r 1916}#
              #{w 1917}#
              #{s 1918}#
              #{mod 1919}#)
       (if (memv #{type 1913}# '(lexical))
         (#{build-lexical-reference 273}#
           'value
           #{s 1918}#
           #{e 1915}#
           #{value 1914}#)
         (if (memv #{type 1913}# '(core core-form))
           (#{value 1914}#
             #{e 1915}#
             #{r 1916}#
             #{w 1917}#
             #{s 1918}#
             #{mod 1919}#)
           (if (memv #{type 1913}# '(module-ref))
             (call-with-values
               (lambda ()
                 (#{value 1914}# #{e 1915}# #{r 1916}# #{w 1917}#))
               (lambda (#{e 1930}#
                        #{r 1931}#
                        #{w 1932}#
                        #{s 1933}#
                        #{mod 1934}#)
                 (#{chi 423}#
                   #{e 1930}#
                   #{r 1931}#
                   #{w 1932}#
                   #{mod 1934}#)))
             (if (memv #{type 1913}# '(lexical-call))
               (#{chi-call 427}#
                 (let ((#{id 1942}# (car #{e 1915}#)))
                   (#{build-lexical-reference 273}#
                     'fun
                     (#{source-annotation 324}# #{id 1942}#)
                     (if (#{syntax-object? 309}# #{id 1942}#)
                       (syntax->datum #{id 1942}#)
                       #{id 1942}#)
                     #{value 1914}#))
                 #{e 1915}#
                 #{r 1916}#
                 #{w 1917}#
                 #{s 1918}#
                 #{mod 1919}#)
               (if (memv #{type 1913}# '(global-call))
                 (#{chi-call 427}#
                   (#{build-global-reference 279}#
                     (#{source-annotation 324}# (car #{e 1915}#))
                     (if (#{syntax-object? 309}# #{value 1914}#)
                       (#{syntax-object-expression 311}# #{value 1914}#)
                       #{value 1914}#)
                     (if (#{syntax-object? 309}# #{value 1914}#)
                       (#{syntax-object-module 315}# #{value 1914}#)
                       #{mod 1919}#))
                   #{e 1915}#
                   #{r 1916}#
                   #{w 1917}#
                   #{s 1918}#
                   #{mod 1919}#)
                 (if (memv #{type 1913}# '(constant))
                   (#{build-data 295}#
                     #{s 1918}#
                     (#{strip 449}#
                       (#{source-wrap 411}#
                         #{e 1915}#
                         #{w 1917}#
                         #{s 1918}#
                         #{mod 1919}#)
                       '(())))
                   (if (memv #{type 1913}# '(global))
                     (#{build-global-reference 279}#
                       #{s 1918}#
                       #{value 1914}#
                       #{mod 1919}#)
                     (if (memv #{type 1913}# '(call))
                       (#{chi-call 427}#
                         (#{chi 423}#
                           (car #{e 1915}#)
                           #{r 1916}#
                           #{w 1917}#
                           #{mod 1919}#)
                         #{e 1915}#
                         #{r 1916}#
                         #{w 1917}#
                         #{s 1918}#
                         #{mod 1919}#)
                       (if (memv #{type 1913}# '(begin-form))
                         (let ((#{tmp 1949}# #{e 1915}#))
                           (let ((#{tmp 1950}#
                                   ($sc-dispatch
                                     #{tmp 1949}#
                                     '(_ any . each-any))))
                             (if #{tmp 1950}#
                               (@apply
                                 (lambda (#{e1 1953}# #{e2 1954}#)
                                   (#{chi-sequence 413}#
                                     (cons #{e1 1953}# #{e2 1954}#)
                                     #{r 1916}#
                                     #{w 1917}#
                                     #{s 1918}#
                                     #{mod 1919}#))
                                 #{tmp 1950}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 1949}#))))
                         (if (memv #{type 1913}# '(local-syntax-form))
                           (#{chi-local-syntax 433}#
                             #{value 1914}#
                             #{e 1915}#
                             #{r 1916}#
                             #{w 1917}#
                             #{s 1918}#
                             #{mod 1919}#
                             #{chi-sequence 413}#)
                           (if (memv #{type 1913}# '(eval-when-form))
                             (let ((#{tmp 1958}# #{e 1915}#))
                               (let ((#{tmp 1959}#
                                       ($sc-dispatch
                                         #{tmp 1958}#
                                         '(_ each-any any . each-any))))
                                 (if #{tmp 1959}#
                                   (@apply
                                     (lambda (#{x 1963}#
                                              #{e1 1964}#
                                              #{e2 1965}#)
                                       (let ((#{when-list 1967}#
                                               (#{chi-when-list 419}#
                                                 #{e 1915}#
                                                 #{x 1963}#
                                                 #{w 1917}#)))
                                         (if (memq 'eval #{when-list 1967}#)
                                           (#{chi-sequence 413}#
                                             (cons #{e1 1964}# #{e2 1965}#)
                                             #{r 1916}#
                                             #{w 1917}#
                                             #{s 1918}#
                                             #{mod 1919}#)
                                           (#{chi-void 437}#))))
                                     #{tmp 1959}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp 1958}#))))
                             (if (memv #{type 1913}#
                                       '(define-form define-syntax-form))
                               (syntax-violation
                                 #f
                                 "definition in expression context"
                                 #{e 1915}#
                                 (#{wrap 409}#
                                   #{value 1914}#
                                   #{w 1917}#
                                   #{mod 1919}#))
                               (if (memv #{type 1913}# '(syntax))
                                 (syntax-violation
                                   #f
                                   "reference to pattern variable outside syntax form"
                                   (#{source-wrap 411}#
                                     #{e 1915}#
                                     #{w 1917}#
                                     #{s 1918}#
                                     #{mod 1919}#))
                                 (if (memv #{type 1913}# '(displaced-lexical))
                                   (syntax-violation
                                     #f
                                     "reference to identifier outside its scope"
                                     (#{source-wrap 411}#
                                       #{e 1915}#
                                       #{w 1917}#
                                       #{s 1918}#
                                       #{mod 1919}#))
                                   (syntax-violation
                                     #f
                                     "unexpected syntax"
                                     (#{source-wrap 411}#
                                       #{e 1915}#
                                       #{w 1917}#
                                       #{s 1918}#
                                       #{mod 1919}#))))))))))))))))))
   (#{chi-call 427}#
     (lambda (#{x 1974}#
              #{e 1975}#
              #{r 1976}#
              #{w 1977}#
              #{s 1978}#
              #{mod 1979}#)
       (let ((#{tmp 1986}# #{e 1975}#))
         (let ((#{tmp 1987}#
                 ($sc-dispatch #{tmp 1986}# '(any . each-any))))
           (if #{tmp 1987}#
             (@apply
               (lambda (#{e0 1990}# #{e1 1991}#)
                 (#{build-call 267}#
                   #{s 1978}#
                   #{x 1974}#
                   (map (lambda (#{e 1992}#)
                          (#{chi 423}#
                            #{e 1992}#
                            #{r 1976}#
                            #{w 1977}#
                            #{mod 1979}#))
                        #{e1 1991}#)))
               #{tmp 1987}#)
             (syntax-violation
               #f
               "source expression failed to match any pattern"
               #{tmp 1986}#))))))
   (#{chi-macro 429}#
     (lambda (#{p 1995}#
              #{e 1996}#
              #{r 1997}#
              #{w 1998}#
              #{s 1999}#
              #{rib 2000}#
              #{mod 2001}#)
       (letrec*
         ((#{rebuild-macro-output 2010}#
            (lambda (#{x 2011}# #{m 2012}#)
              (if (pair? #{x 2011}#)
                (#{decorate-source 261}#
                  (cons (#{rebuild-macro-output 2010}#
                          (car #{x 2011}#)
                          #{m 2012}#)
                        (#{rebuild-macro-output 2010}#
                          (cdr #{x 2011}#)
                          #{m 2012}#))
                  #{s 1999}#)
                (if (#{syntax-object? 309}# #{x 2011}#)
                  (let ((#{w 2020}#
                          (#{syntax-object-wrap 313}# #{x 2011}#)))
                    (let ((#{ms 2023}# (car #{w 2020}#))
                          (#{s 2024}# (cdr #{w 2020}#)))
                      (if (if (pair? #{ms 2023}#)
                            (eq? (car #{ms 2023}#) #f)
                            #f)
                        (#{make-syntax-object 307}#
                          (#{syntax-object-expression 311}# #{x 2011}#)
                          (cons (cdr #{ms 2023}#)
                                (if #{rib 2000}#
                                  (cons #{rib 2000}# (cdr #{s 2024}#))
                                  (cdr #{s 2024}#)))
                          (#{syntax-object-module 315}# #{x 2011}#))
                        (#{make-syntax-object 307}#
                          (#{decorate-source 261}#
                            (#{syntax-object-expression 311}# #{x 2011}#)
                            #{s 2024}#)
                          (cons (cons #{m 2012}# #{ms 2023}#)
                                (if #{rib 2000}#
                                  (cons #{rib 2000}# (cons 'shift #{s 2024}#))
                                  (cons 'shift #{s 2024}#)))
                          (#{syntax-object-module 315}# #{x 2011}#)))))
                  (if (vector? #{x 2011}#)
                    (let ((#{n 2036}# (vector-length #{x 2011}#)))
                      (let ((#{v 2038}#
                              (#{decorate-source 261}#
                                (make-vector #{n 2036}#)
                                #{x 2011}#)))
                        (letrec*
                          ((#{loop 2041}#
                             (lambda (#{i 2042}#)
                               (if (= #{i 2042}# #{n 2036}#)
                                 (begin (if #f #f) #{v 2038}#)
                                 (begin
                                   (vector-set!
                                     #{v 2038}#
                                     #{i 2042}#
                                     (#{rebuild-macro-output 2010}#
                                       (vector-ref #{x 2011}# #{i 2042}#)
                                       #{m 2012}#))
                                   (#{loop 2041}# (#{1+}# #{i 2042}#)))))))
                          (#{loop 2041}# 0))))
                    (if (symbol? #{x 2011}#)
                      (syntax-violation
                        #f
                        "encountered raw symbol in macro output"
                        (#{source-wrap 411}#
                          #{e 1996}#
                          #{w 1998}#
                          (cdr #{w 1998}#)
                          #{mod 2001}#)
                        #{x 2011}#)
                      (#{decorate-source 261}# #{x 2011}# #{s 1999}#))))))))
         (#{rebuild-macro-output 2010}#
           (#{p 1995}#
             (#{source-wrap 411}#
               #{e 1996}#
               (#{anti-mark 381}# #{w 1998}#)
               #{s 1999}#
               #{mod 2001}#))
           (gensym "m")))))
   (#{chi-body 431}#
     (lambda (#{body 2052}#
              #{outer-form 2053}#
              #{r 2054}#
              #{w 2055}#
              #{mod 2056}#)
       (let ((#{r 2064}#
               (cons '("placeholder" placeholder) #{r 2054}#)))
         (let ((#{ribcage 2066}#
                 (#{make-ribcage 361}# '() '() '())))
           (let ((#{w 2069}#
                   (cons (car #{w 2055}#)
                         (cons #{ribcage 2066}# (cdr #{w 2055}#)))))
             (letrec*
               ((#{parse 2081}#
                  (lambda (#{body 2082}#
                           #{ids 2083}#
                           #{labels 2084}#
                           #{var-ids 2085}#
                           #{vars 2086}#
                           #{vals 2087}#
                           #{bindings 2088}#)
                    (if (null? #{body 2082}#)
                      (syntax-violation
                        #f
                        "no expressions in body"
                        #{outer-form 2053}#)
                      (let ((#{e 2093}# (cdr (car #{body 2082}#)))
                            (#{er 2094}# (car (car #{body 2082}#))))
                        (call-with-values
                          (lambda ()
                            (#{syntax-type 421}#
                              #{e 2093}#
                              #{er 2094}#
                              '(())
                              (#{source-annotation 324}# #{er 2094}#)
                              #{ribcage 2066}#
                              #{mod 2056}#
                              #f))
                          (lambda (#{type 2096}#
                                   #{value 2097}#
                                   #{e 2098}#
                                   #{w 2099}#
                                   #{s 2100}#
                                   #{mod 2101}#)
                            (if (memv #{type 2096}# '(define-form))
                              (let ((#{id 2111}#
                                      (#{wrap 409}#
                                        #{value 2097}#
                                        #{w 2099}#
                                        #{mod 2101}#))
                                    (#{label 2112}# (#{gen-label 356}#)))
                                (let ((#{var 2114}#
                                        (#{gen-var 451}# #{id 2111}#)))
                                  (begin
                                    (#{extend-ribcage! 385}#
                                      #{ribcage 2066}#
                                      #{id 2111}#
                                      #{label 2112}#)
                                    (#{parse 2081}#
                                      (cdr #{body 2082}#)
                                      (cons #{id 2111}# #{ids 2083}#)
                                      (cons #{label 2112}# #{labels 2084}#)
                                      (cons #{id 2111}# #{var-ids 2085}#)
                                      (cons #{var 2114}# #{vars 2086}#)
                                      (cons (cons #{er 2094}#
                                                  (#{wrap 409}#
                                                    #{e 2098}#
                                                    #{w 2099}#
                                                    #{mod 2101}#))
                                            #{vals 2087}#)
                                      (cons (cons 'lexical #{var 2114}#)
                                            #{bindings 2088}#)))))
                              (if (memv #{type 2096}# '(define-syntax-form))
                                (let ((#{id 2119}#
                                        (#{wrap 409}#
                                          #{value 2097}#
                                          #{w 2099}#
                                          #{mod 2101}#))
                                      (#{label 2120}# (#{gen-label 356}#)))
                                  (begin
                                    (#{extend-ribcage! 385}#
                                      #{ribcage 2066}#
                                      #{id 2119}#
                                      #{label 2120}#)
                                    (#{parse 2081}#
                                      (cdr #{body 2082}#)
                                      (cons #{id 2119}# #{ids 2083}#)
                                      (cons #{label 2120}# #{labels 2084}#)
                                      #{var-ids 2085}#
                                      #{vars 2086}#
                                      #{vals 2087}#
                                      (cons (cons 'macro
                                                  (cons #{er 2094}#
                                                        (#{wrap 409}#
                                                          #{e 2098}#
                                                          #{w 2099}#
                                                          #{mod 2101}#)))
                                            #{bindings 2088}#))))
                                (if (memv #{type 2096}# '(begin-form))
                                  (let ((#{tmp 2123}# #{e 2098}#))
                                    (let ((#{tmp 2124}#
                                            ($sc-dispatch
                                              #{tmp 2123}#
                                              '(_ . each-any))))
                                      (if #{tmp 2124}#
                                        (@apply
                                          (lambda (#{e1 2126}#)
                                            (#{parse 2081}#
                                              (letrec*
                                                ((#{f 2129}#
                                                   (lambda (#{forms 2130}#)
                                                     (if (null? #{forms 2130}#)
                                                       (cdr #{body 2082}#)
                                                       (cons (cons #{er 2094}#
                                                                   (#{wrap 409}#
                                                                     (car #{forms 2130}#)
                                                                     #{w 2099}#
                                                                     #{mod 2101}#))
                                                             (#{f 2129}#
                                                               (cdr #{forms 2130}#)))))))
                                                (#{f 2129}# #{e1 2126}#))
                                              #{ids 2083}#
                                              #{labels 2084}#
                                              #{var-ids 2085}#
                                              #{vars 2086}#
                                              #{vals 2087}#
                                              #{bindings 2088}#))
                                          #{tmp 2124}#)
                                        (syntax-violation
                                          #f
                                          "source expression failed to match any pattern"
                                          #{tmp 2123}#))))
                                  (if (memv #{type 2096}# '(local-syntax-form))
                                    (#{chi-local-syntax 433}#
                                      #{value 2097}#
                                      #{e 2098}#
                                      #{er 2094}#
                                      #{w 2099}#
                                      #{s 2100}#
                                      #{mod 2101}#
                                      (lambda (#{forms 2133}#
                                               #{er 2134}#
                                               #{w 2135}#
                                               #{s 2136}#
                                               #{mod 2137}#)
                                        (#{parse 2081}#
                                          (letrec*
                                            ((#{f 2145}#
                                               (lambda (#{forms 2146}#)
                                                 (if (null? #{forms 2146}#)
                                                   (cdr #{body 2082}#)
                                                   (cons (cons #{er 2134}#
                                                               (#{wrap 409}#
                                                                 (car #{forms 2146}#)
                                                                 #{w 2135}#
                                                                 #{mod 2137}#))
                                                         (#{f 2145}#
                                                           (cdr #{forms 2146}#)))))))
                                            (#{f 2145}# #{forms 2133}#))
                                          #{ids 2083}#
                                          #{labels 2084}#
                                          #{var-ids 2085}#
                                          #{vars 2086}#
                                          #{vals 2087}#
                                          #{bindings 2088}#)))
                                    (if (null? #{ids 2083}#)
                                      (#{build-sequence 297}#
                                        #f
                                        (map (lambda (#{x 2149}#)
                                               (#{chi 423}#
                                                 (cdr #{x 2149}#)
                                                 (car #{x 2149}#)
                                                 '(())
                                                 #{mod 2101}#))
                                             (cons (cons #{er 2094}#
                                                         (#{source-wrap 411}#
                                                           #{e 2098}#
                                                           #{w 2099}#
                                                           #{s 2100}#
                                                           #{mod 2101}#))
                                                   (cdr #{body 2082}#))))
                                      (begin
                                        (if (not (#{valid-bound-ids? 403}#
                                                   #{ids 2083}#))
                                          (syntax-violation
                                            #f
                                            "invalid or duplicate identifier in definition"
                                            #{outer-form 2053}#))
                                        (letrec*
                                          ((#{loop 2156}#
                                             (lambda (#{bs 2157}#
                                                      #{er-cache 2158}#
                                                      #{r-cache 2159}#)
                                               (if (not (null? #{bs 2157}#))
                                                 (let ((#{b 2162}#
                                                         (car #{bs 2157}#)))
                                                   (if (eq? (car #{b 2162}#)
                                                            'macro)
                                                     (let ((#{er 2165}#
                                                             (car (cdr #{b 2162}#))))
                                                       (let ((#{r-cache 2167}#
                                                               (if (eq? #{er 2165}#
                                                                        #{er-cache 2158}#)
                                                                 #{r-cache 2159}#
                                                                 (#{macros-only-env 335}#
                                                                   #{er 2165}#))))
                                                         (begin
                                                           (set-cdr!
                                                             #{b 2162}#
                                                             (#{eval-local-transformer 435}#
                                                               (#{chi 423}#
                                                                 (cdr (cdr #{b 2162}#))
                                                                 #{r-cache 2167}#
                                                                 '(())
                                                                 #{mod 2101}#)
                                                               #{mod 2101}#))
                                                           (#{loop 2156}#
                                                             (cdr #{bs 2157}#)
                                                             #{er 2165}#
                                                             #{r-cache 2167}#))))
                                                     (#{loop 2156}#
                                                       (cdr #{bs 2157}#)
                                                       #{er-cache 2158}#
                                                       #{r-cache 2159}#)))))))
                                          (#{loop 2156}#
                                            #{bindings 2088}#
                                            #f
                                            #f))
                                        (set-cdr!
                                          #{r 2064}#
                                          (#{extend-env 331}#
                                            #{labels 2084}#
                                            #{bindings 2088}#
                                            (cdr #{r 2064}#)))
                                        (#{build-letrec 303}#
                                          #f
                                          #t
                                          (reverse
                                            (map syntax->datum
                                                 #{var-ids 2085}#))
                                          (reverse #{vars 2086}#)
                                          (map (lambda (#{x 2170}#)
                                                 (#{chi 423}#
                                                   (cdr #{x 2170}#)
                                                   (car #{x 2170}#)
                                                   '(())
                                                   #{mod 2101}#))
                                               (reverse #{vals 2087}#))
                                          (#{build-sequence 297}#
                                            #f
                                            (map (lambda (#{x 2174}#)
                                                   (#{chi 423}#
                                                     (cdr #{x 2174}#)
                                                     (car #{x 2174}#)
                                                     '(())
                                                     #{mod 2101}#))
                                                 (cons (cons #{er 2094}#
                                                             (#{source-wrap 411}#
                                                               #{e 2098}#
                                                               #{w 2099}#
                                                               #{s 2100}#
                                                               #{mod 2101}#))
                                                       (cdr #{body 2082}#))))))))))))))))))
               (#{parse 2081}#
                 (map (lambda (#{x 2089}#)
                        (cons #{r 2064}#
                              (#{wrap 409}#
                                #{x 2089}#
                                #{w 2069}#
                                #{mod 2056}#)))
                      #{body 2052}#)
                 '()
                 '()
                 '()
                 '()
                 '()
                 '())))))))
   (#{chi-local-syntax 433}#
     (lambda (#{rec? 2177}#
              #{e 2178}#
              #{r 2179}#
              #{w 2180}#
              #{s 2181}#
              #{mod 2182}#
              #{k 2183}#)
       (let ((#{tmp 2191}# #{e 2178}#))
         (let ((#{tmp 2192}#
                 ($sc-dispatch
                   #{tmp 2191}#
                   '(_ #(each (any any)) any . each-any))))
           (if #{tmp 2192}#
             (@apply
               (lambda (#{id 2197}#
                        #{val 2198}#
                        #{e1 2199}#
                        #{e2 2200}#)
                 (let ((#{ids 2202}# #{id 2197}#))
                   (if (not (#{valid-bound-ids? 403}# #{ids 2202}#))
                     (syntax-violation
                       #f
                       "duplicate bound keyword"
                       #{e 2178}#)
                     (let ((#{labels 2205}#
                             (#{gen-labels 358}# #{ids 2202}#)))
                       (let ((#{new-w 2207}#
                               (#{make-binding-wrap 387}#
                                 #{ids 2202}#
                                 #{labels 2205}#
                                 #{w 2180}#)))
                         (#{k 2183}#
                           (cons #{e1 2199}# #{e2 2200}#)
                           (#{extend-env 331}#
                             #{labels 2205}#
                             (let ((#{w 2211}#
                                     (if #{rec? 2177}#
                                       #{new-w 2207}#
                                       #{w 2180}#))
                                   (#{trans-r 2212}#
                                     (#{macros-only-env 335}# #{r 2179}#)))
                               (map (lambda (#{x 2213}#)
                                      (cons 'macro
                                            (#{eval-local-transformer 435}#
                                              (#{chi 423}#
                                                #{x 2213}#
                                                #{trans-r 2212}#
                                                #{w 2211}#
                                                #{mod 2182}#)
                                              #{mod 2182}#)))
                                    #{val 2198}#))
                             #{r 2179}#)
                           #{new-w 2207}#
                           #{s 2181}#
                           #{mod 2182}#))))))
               #{tmp 2192}#)
             (let ((#{_ 2218}# #{tmp 2191}#))
               (syntax-violation
                 #f
                 "bad local syntax definition"
                 (#{source-wrap 411}#
                   #{e 2178}#
                   #{w 2180}#
                   #{s 2181}#
                   #{mod 2182}#))))))))
   (#{eval-local-transformer 435}#
     (lambda (#{expanded 2219}# #{mod 2220}#)
       (let ((#{p 2224}#
               (#{local-eval-hook 254}#
                 #{expanded 2219}#
                 #{mod 2220}#)))
         (if (procedure? #{p 2224}#)
           #{p 2224}#
           (syntax-violation
             #f
             "nonprocedure transformer"
             #{p 2224}#)))))
   (#{chi-void 437}#
     (lambda () (#{build-void 265}# #f)))
   (#{ellipsis? 439}#
     (lambda (#{x 2226}#)
       (if (#{nonsymbol-id? 341}# #{x 2226}#)
         (#{free-id=? 399}#
           #{x 2226}#
           '#(syntax-object
              ...
              ((top)
               #(ribcage () () ())
               #(ribcage () () ())
               #(ribcage #(x) #((top)) #("i2227"))
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
     (lambda (#{orig-args 2230}#)
       (letrec*
         ((#{req 2233}#
            (lambda (#{args 2236}# #{rreq 2237}#)
              (let ((#{tmp 2240}# #{args 2236}#))
                (let ((#{tmp 2241}# ($sc-dispatch #{tmp 2240}# '())))
                  (if #{tmp 2241}#
                    (@apply
                      (lambda ()
                        (#{check 2235}# (reverse #{rreq 2237}#) #f))
                      #{tmp 2241}#)
                    (let ((#{tmp 2242}#
                            ($sc-dispatch #{tmp 2240}# '(any . any))))
                      (if (if #{tmp 2242}#
                            (@apply
                              (lambda (#{a 2245}# #{b 2246}#)
                                (#{id? 343}# #{a 2245}#))
                              #{tmp 2242}#)
                            #f)
                        (@apply
                          (lambda (#{a 2249}# #{b 2250}#)
                            (#{req 2233}#
                              #{b 2250}#
                              (cons #{a 2249}# #{rreq 2237}#)))
                          #{tmp 2242}#)
                        (let ((#{tmp 2251}# (list #{tmp 2240}#)))
                          (if (if #{tmp 2251}#
                                (@apply
                                  (lambda (#{r 2253}#)
                                    (#{id? 343}# #{r 2253}#))
                                  #{tmp 2251}#)
                                #f)
                            (@apply
                              (lambda (#{r 2255}#)
                                (#{check 2235}#
                                  (reverse #{rreq 2237}#)
                                  #{r 2255}#))
                              #{tmp 2251}#)
                            (let ((#{else 2257}# #{tmp 2240}#))
                              (syntax-violation
                                'lambda
                                "invalid argument list"
                                #{orig-args 2230}#
                                #{args 2236}#)))))))))))
          (#{check 2235}#
            (lambda (#{req 2258}# #{rest 2259}#)
              (if (#{distinct-bound-ids? 405}#
                    (if #{rest 2259}#
                      (cons #{rest 2259}# #{req 2258}#)
                      #{req 2258}#))
                (values #{req 2258}# #f #{rest 2259}# #f)
                (syntax-violation
                  'lambda
                  "duplicate identifier in argument list"
                  #{orig-args 2230}#)))))
         (#{req 2233}# #{orig-args 2230}# '()))))
   (#{chi-simple-lambda 443}#
     (lambda (#{e 2265}#
              #{r 2266}#
              #{w 2267}#
              #{s 2268}#
              #{mod 2269}#
              #{req 2270}#
              #{rest 2271}#
              #{meta 2272}#
              #{body 2273}#)
       (let ((#{ids 2285}#
               (if #{rest 2271}#
                 (append #{req 2270}# (list #{rest 2271}#))
                 #{req 2270}#)))
         (let ((#{vars 2287}#
                 (map #{gen-var 451}# #{ids 2285}#)))
           (let ((#{labels 2289}#
                   (#{gen-labels 358}# #{ids 2285}#)))
             (#{build-simple-lambda 285}#
               #{s 2268}#
               (map syntax->datum #{req 2270}#)
               (if #{rest 2271}#
                 (syntax->datum #{rest 2271}#)
                 #f)
               #{vars 2287}#
               #{meta 2272}#
               (#{chi-body 431}#
                 #{body 2273}#
                 (#{source-wrap 411}#
                   #{e 2265}#
                   #{w 2267}#
                   #{s 2268}#
                   #{mod 2269}#)
                 (#{extend-var-env 333}#
                   #{labels 2289}#
                   #{vars 2287}#
                   #{r 2266}#)
                 (#{make-binding-wrap 387}#
                   #{ids 2285}#
                   #{labels 2289}#
                   #{w 2267}#)
                 #{mod 2269}#)))))))
   (#{lambda*-formals 445}#
     (lambda (#{orig-args 2292}#)
       (letrec*
         ((#{req 2295}#
            (lambda (#{args 2304}# #{rreq 2305}#)
              (let ((#{tmp 2308}# #{args 2304}#))
                (let ((#{tmp 2309}# ($sc-dispatch #{tmp 2308}# '())))
                  (if #{tmp 2309}#
                    (@apply
                      (lambda ()
                        (#{check 2303}#
                          (reverse #{rreq 2305}#)
                          '()
                          #f
                          '()))
                      #{tmp 2309}#)
                    (let ((#{tmp 2310}#
                            ($sc-dispatch #{tmp 2308}# '(any . any))))
                      (if (if #{tmp 2310}#
                            (@apply
                              (lambda (#{a 2313}# #{b 2314}#)
                                (#{id? 343}# #{a 2313}#))
                              #{tmp 2310}#)
                            #f)
                        (@apply
                          (lambda (#{a 2317}# #{b 2318}#)
                            (#{req 2295}#
                              #{b 2318}#
                              (cons #{a 2317}# #{rreq 2305}#)))
                          #{tmp 2310}#)
                        (let ((#{tmp 2319}#
                                ($sc-dispatch #{tmp 2308}# '(any . any))))
                          (if (if #{tmp 2319}#
                                (@apply
                                  (lambda (#{a 2322}# #{b 2323}#)
                                    (eq? (syntax->datum #{a 2322}#)
                                         #:optional))
                                  #{tmp 2319}#)
                                #f)
                            (@apply
                              (lambda (#{a 2326}# #{b 2327}#)
                                (#{opt 2297}#
                                  #{b 2327}#
                                  (reverse #{rreq 2305}#)
                                  '()))
                              #{tmp 2319}#)
                            (let ((#{tmp 2328}#
                                    ($sc-dispatch #{tmp 2308}# '(any . any))))
                              (if (if #{tmp 2328}#
                                    (@apply
                                      (lambda (#{a 2331}# #{b 2332}#)
                                        (eq? (syntax->datum #{a 2331}#) #:key))
                                      #{tmp 2328}#)
                                    #f)
                                (@apply
                                  (lambda (#{a 2335}# #{b 2336}#)
                                    (#{key 2299}#
                                      #{b 2336}#
                                      (reverse #{rreq 2305}#)
                                      '()
                                      '()))
                                  #{tmp 2328}#)
                                (let ((#{tmp 2337}#
                                        ($sc-dispatch
                                          #{tmp 2308}#
                                          '(any any))))
                                  (if (if #{tmp 2337}#
                                        (@apply
                                          (lambda (#{a 2340}# #{b 2341}#)
                                            (eq? (syntax->datum #{a 2340}#)
                                                 #:rest))
                                          #{tmp 2337}#)
                                        #f)
                                    (@apply
                                      (lambda (#{a 2344}# #{b 2345}#)
                                        (#{rest 2301}#
                                          #{b 2345}#
                                          (reverse #{rreq 2305}#)
                                          '()
                                          '()))
                                      #{tmp 2337}#)
                                    (let ((#{tmp 2346}# (list #{tmp 2308}#)))
                                      (if (if #{tmp 2346}#
                                            (@apply
                                              (lambda (#{r 2348}#)
                                                (#{id? 343}# #{r 2348}#))
                                              #{tmp 2346}#)
                                            #f)
                                        (@apply
                                          (lambda (#{r 2350}#)
                                            (#{rest 2301}#
                                              #{r 2350}#
                                              (reverse #{rreq 2305}#)
                                              '()
                                              '()))
                                          #{tmp 2346}#)
                                        (let ((#{else 2352}# #{tmp 2308}#))
                                          (syntax-violation
                                            'lambda*
                                            "invalid argument list"
                                            #{orig-args 2292}#
                                            #{args 2304}#)))))))))))))))))
          (#{opt 2297}#
            (lambda (#{args 2353}# #{req 2354}# #{ropt 2355}#)
              (let ((#{tmp 2359}# #{args 2353}#))
                (let ((#{tmp 2360}# ($sc-dispatch #{tmp 2359}# '())))
                  (if #{tmp 2360}#
                    (@apply
                      (lambda ()
                        (#{check 2303}#
                          #{req 2354}#
                          (reverse #{ropt 2355}#)
                          #f
                          '()))
                      #{tmp 2360}#)
                    (let ((#{tmp 2361}#
                            ($sc-dispatch #{tmp 2359}# '(any . any))))
                      (if (if #{tmp 2361}#
                            (@apply
                              (lambda (#{a 2364}# #{b 2365}#)
                                (#{id? 343}# #{a 2364}#))
                              #{tmp 2361}#)
                            #f)
                        (@apply
                          (lambda (#{a 2368}# #{b 2369}#)
                            (#{opt 2297}#
                              #{b 2369}#
                              #{req 2354}#
                              (cons (cons #{a 2368}#
                                          '(#(syntax-object
                                              #f
                                              ((top)
                                               #(ribcage
                                                 #(a b)
                                                 #((top) (top))
                                                 #("i2366" "i2367"))
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(args req ropt)
                                                 #((top) (top) (top))
                                                 #("i2356" "i2357" "i2358"))
                                               #(ribcage
                                                 (check rest key opt req)
                                                 ((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                 ("i2302"
                                                  "i2300"
                                                  "i2298"
                                                  "i2296"
                                                  "i2294"))
                                               #(ribcage
                                                 #(orig-args)
                                                 #((top))
                                                 #("i2293"))
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
                                    #{ropt 2355}#)))
                          #{tmp 2361}#)
                        (let ((#{tmp 2370}#
                                ($sc-dispatch
                                  #{tmp 2359}#
                                  '((any any) . any))))
                          (if (if #{tmp 2370}#
                                (@apply
                                  (lambda (#{a 2374}# #{init 2375}# #{b 2376}#)
                                    (#{id? 343}# #{a 2374}#))
                                  #{tmp 2370}#)
                                #f)
                            (@apply
                              (lambda (#{a 2380}# #{init 2381}# #{b 2382}#)
                                (#{opt 2297}#
                                  #{b 2382}#
                                  #{req 2354}#
                                  (cons (list #{a 2380}# #{init 2381}#)
                                        #{ropt 2355}#)))
                              #{tmp 2370}#)
                            (let ((#{tmp 2383}#
                                    ($sc-dispatch #{tmp 2359}# '(any . any))))
                              (if (if #{tmp 2383}#
                                    (@apply
                                      (lambda (#{a 2386}# #{b 2387}#)
                                        (eq? (syntax->datum #{a 2386}#) #:key))
                                      #{tmp 2383}#)
                                    #f)
                                (@apply
                                  (lambda (#{a 2390}# #{b 2391}#)
                                    (#{key 2299}#
                                      #{b 2391}#
                                      #{req 2354}#
                                      (reverse #{ropt 2355}#)
                                      '()))
                                  #{tmp 2383}#)
                                (let ((#{tmp 2392}#
                                        ($sc-dispatch
                                          #{tmp 2359}#
                                          '(any any))))
                                  (if (if #{tmp 2392}#
                                        (@apply
                                          (lambda (#{a 2395}# #{b 2396}#)
                                            (eq? (syntax->datum #{a 2395}#)
                                                 #:rest))
                                          #{tmp 2392}#)
                                        #f)
                                    (@apply
                                      (lambda (#{a 2399}# #{b 2400}#)
                                        (#{rest 2301}#
                                          #{b 2400}#
                                          #{req 2354}#
                                          (reverse #{ropt 2355}#)
                                          '()))
                                      #{tmp 2392}#)
                                    (let ((#{tmp 2401}# (list #{tmp 2359}#)))
                                      (if (if #{tmp 2401}#
                                            (@apply
                                              (lambda (#{r 2403}#)
                                                (#{id? 343}# #{r 2403}#))
                                              #{tmp 2401}#)
                                            #f)
                                        (@apply
                                          (lambda (#{r 2405}#)
                                            (#{rest 2301}#
                                              #{r 2405}#
                                              #{req 2354}#
                                              (reverse #{ropt 2355}#)
                                              '()))
                                          #{tmp 2401}#)
                                        (let ((#{else 2407}# #{tmp 2359}#))
                                          (syntax-violation
                                            'lambda*
                                            "invalid optional argument list"
                                            #{orig-args 2292}#
                                            #{args 2353}#)))))))))))))))))
          (#{key 2299}#
            (lambda (#{args 2408}#
                     #{req 2409}#
                     #{opt 2410}#
                     #{rkey 2411}#)
              (let ((#{tmp 2416}# #{args 2408}#))
                (let ((#{tmp 2417}# ($sc-dispatch #{tmp 2416}# '())))
                  (if #{tmp 2417}#
                    (@apply
                      (lambda ()
                        (#{check 2303}#
                          #{req 2409}#
                          #{opt 2410}#
                          #f
                          (cons #f (reverse #{rkey 2411}#))))
                      #{tmp 2417}#)
                    (let ((#{tmp 2418}#
                            ($sc-dispatch #{tmp 2416}# '(any . any))))
                      (if (if #{tmp 2418}#
                            (@apply
                              (lambda (#{a 2421}# #{b 2422}#)
                                (#{id? 343}# #{a 2421}#))
                              #{tmp 2418}#)
                            #f)
                        (@apply
                          (lambda (#{a 2425}# #{b 2426}#)
                            (let ((#{tmp 2428}#
                                    (symbol->keyword
                                      (syntax->datum #{a 2425}#))))
                              (let ((#{k 2430}# #{tmp 2428}#))
                                (#{key 2299}#
                                  #{b 2426}#
                                  #{req 2409}#
                                  #{opt 2410}#
                                  (cons (cons #{k 2430}#
                                              (cons #{a 2425}#
                                                    '(#(syntax-object
                                                        #f
                                                        ((top)
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(k)
                                                           #((top))
                                                           #("i2429"))
                                                         #(ribcage
                                                           #(a b)
                                                           #((top) (top))
                                                           #("i2423" "i2424"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(args req opt rkey)
                                                           #((top)
                                                             (top)
                                                             (top)
                                                             (top))
                                                           #("i2412"
                                                             "i2413"
                                                             "i2414"
                                                             "i2415"))
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
                                                           ("i2302"
                                                            "i2300"
                                                            "i2298"
                                                            "i2296"
                                                            "i2294"))
                                                         #(ribcage
                                                           #(orig-args)
                                                           #((top))
                                                           #("i2293"))
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
                                        #{rkey 2411}#)))))
                          #{tmp 2418}#)
                        (let ((#{tmp 2431}#
                                ($sc-dispatch
                                  #{tmp 2416}#
                                  '((any any) . any))))
                          (if (if #{tmp 2431}#
                                (@apply
                                  (lambda (#{a 2435}# #{init 2436}# #{b 2437}#)
                                    (#{id? 343}# #{a 2435}#))
                                  #{tmp 2431}#)
                                #f)
                            (@apply
                              (lambda (#{a 2441}# #{init 2442}# #{b 2443}#)
                                (let ((#{tmp 2445}#
                                        (symbol->keyword
                                          (syntax->datum #{a 2441}#))))
                                  (let ((#{k 2447}# #{tmp 2445}#))
                                    (#{key 2299}#
                                      #{b 2443}#
                                      #{req 2409}#
                                      #{opt 2410}#
                                      (cons (list #{k 2447}#
                                                  #{a 2441}#
                                                  #{init 2442}#)
                                            #{rkey 2411}#)))))
                              #{tmp 2431}#)
                            (let ((#{tmp 2448}#
                                    ($sc-dispatch
                                      #{tmp 2416}#
                                      '((any any any) . any))))
                              (if (if #{tmp 2448}#
                                    (@apply
                                      (lambda (#{a 2453}#
                                               #{init 2454}#
                                               #{k 2455}#
                                               #{b 2456}#)
                                        (if (#{id? 343}# #{a 2453}#)
                                          (keyword? (syntax->datum #{k 2455}#))
                                          #f))
                                      #{tmp 2448}#)
                                    #f)
                                (@apply
                                  (lambda (#{a 2463}#
                                           #{init 2464}#
                                           #{k 2465}#
                                           #{b 2466}#)
                                    (#{key 2299}#
                                      #{b 2466}#
                                      #{req 2409}#
                                      #{opt 2410}#
                                      (cons (list #{k 2465}#
                                                  #{a 2463}#
                                                  #{init 2464}#)
                                            #{rkey 2411}#)))
                                  #{tmp 2448}#)
                                (let ((#{tmp 2467}#
                                        ($sc-dispatch #{tmp 2416}# '(any))))
                                  (if (if #{tmp 2467}#
                                        (@apply
                                          (lambda (#{aok 2469}#)
                                            (eq? (syntax->datum #{aok 2469}#)
                                                 #:allow-other-keys))
                                          #{tmp 2467}#)
                                        #f)
                                    (@apply
                                      (lambda (#{aok 2471}#)
                                        (#{check 2303}#
                                          #{req 2409}#
                                          #{opt 2410}#
                                          #f
                                          (cons #t (reverse #{rkey 2411}#))))
                                      #{tmp 2467}#)
                                    (let ((#{tmp 2472}#
                                            ($sc-dispatch
                                              #{tmp 2416}#
                                              '(any any any))))
                                      (if (if #{tmp 2472}#
                                            (@apply
                                              (lambda (#{aok 2476}#
                                                       #{a 2477}#
                                                       #{b 2478}#)
                                                (if (eq? (syntax->datum
                                                           #{aok 2476}#)
                                                         #:allow-other-keys)
                                                  (eq? (syntax->datum
                                                         #{a 2477}#)
                                                       #:rest)
                                                  #f))
                                              #{tmp 2472}#)
                                            #f)
                                        (@apply
                                          (lambda (#{aok 2484}#
                                                   #{a 2485}#
                                                   #{b 2486}#)
                                            (#{rest 2301}#
                                              #{b 2486}#
                                              #{req 2409}#
                                              #{opt 2410}#
                                              (cons #t
                                                    (reverse #{rkey 2411}#))))
                                          #{tmp 2472}#)
                                        (let ((#{tmp 2487}#
                                                ($sc-dispatch
                                                  #{tmp 2416}#
                                                  '(any . any))))
                                          (if (if #{tmp 2487}#
                                                (@apply
                                                  (lambda (#{aok 2490}#
                                                           #{r 2491}#)
                                                    (if (eq? (syntax->datum
                                                               #{aok 2490}#)
                                                             #:allow-other-keys)
                                                      (#{id? 343}# #{r 2491}#)
                                                      #f))
                                                  #{tmp 2487}#)
                                                #f)
                                            (@apply
                                              (lambda (#{aok 2496}# #{r 2497}#)
                                                (#{rest 2301}#
                                                  #{r 2497}#
                                                  #{req 2409}#
                                                  #{opt 2410}#
                                                  (cons #t
                                                        (reverse
                                                          #{rkey 2411}#))))
                                              #{tmp 2487}#)
                                            (let ((#{tmp 2498}#
                                                    ($sc-dispatch
                                                      #{tmp 2416}#
                                                      '(any any))))
                                              (if (if #{tmp 2498}#
                                                    (@apply
                                                      (lambda (#{a 2501}#
                                                               #{b 2502}#)
                                                        (eq? (syntax->datum
                                                               #{a 2501}#)
                                                             #:rest))
                                                      #{tmp 2498}#)
                                                    #f)
                                                (@apply
                                                  (lambda (#{a 2505}#
                                                           #{b 2506}#)
                                                    (#{rest 2301}#
                                                      #{b 2506}#
                                                      #{req 2409}#
                                                      #{opt 2410}#
                                                      (cons #f
                                                            (reverse
                                                              #{rkey 2411}#))))
                                                  #{tmp 2498}#)
                                                (let ((#{tmp 2507}#
                                                        (list #{tmp 2416}#)))
                                                  (if (if #{tmp 2507}#
                                                        (@apply
                                                          (lambda (#{r 2509}#)
                                                            (#{id? 343}#
                                                              #{r 2509}#))
                                                          #{tmp 2507}#)
                                                        #f)
                                                    (@apply
                                                      (lambda (#{r 2511}#)
                                                        (#{rest 2301}#
                                                          #{r 2511}#
                                                          #{req 2409}#
                                                          #{opt 2410}#
                                                          (cons #f
                                                                (reverse
                                                                  #{rkey 2411}#))))
                                                      #{tmp 2507}#)
                                                    (let ((#{else 2513}#
                                                            #{tmp 2416}#))
                                                      (syntax-violation
                                                        'lambda*
                                                        "invalid keyword argument list"
                                                        #{orig-args 2292}#
                                                        #{args 2408}#)))))))))))))))))))))))
          (#{rest 2301}#
            (lambda (#{args 2514}#
                     #{req 2515}#
                     #{opt 2516}#
                     #{kw 2517}#)
              (let ((#{tmp 2522}# #{args 2514}#))
                (let ((#{tmp 2523}# (list #{tmp 2522}#)))
                  (if (if #{tmp 2523}#
                        (@apply
                          (lambda (#{r 2525}#) (#{id? 343}# #{r 2525}#))
                          #{tmp 2523}#)
                        #f)
                    (@apply
                      (lambda (#{r 2527}#)
                        (#{check 2303}#
                          #{req 2515}#
                          #{opt 2516}#
                          #{r 2527}#
                          #{kw 2517}#))
                      #{tmp 2523}#)
                    (let ((#{else 2529}# #{tmp 2522}#))
                      (syntax-violation
                        'lambda*
                        "invalid rest argument"
                        #{orig-args 2292}#
                        #{args 2514}#)))))))
          (#{check 2303}#
            (lambda (#{req 2530}#
                     #{opt 2531}#
                     #{rest 2532}#
                     #{kw 2533}#)
              (if (#{distinct-bound-ids? 405}#
                    (append
                      #{req 2530}#
                      (map car #{opt 2531}#)
                      (if #{rest 2532}# (list #{rest 2532}#) '())
                      (if (pair? #{kw 2533}#)
                        (map cadr (cdr #{kw 2533}#))
                        '())))
                (values
                  #{req 2530}#
                  #{opt 2531}#
                  #{rest 2532}#
                  #{kw 2533}#)
                (syntax-violation
                  'lambda*
                  "duplicate identifier in argument list"
                  #{orig-args 2292}#)))))
         (#{req 2295}# #{orig-args 2292}# '()))))
   (#{chi-lambda-case 447}#
     (lambda (#{e 2541}#
              #{r 2542}#
              #{w 2543}#
              #{s 2544}#
              #{mod 2545}#
              #{get-formals 2546}#
              #{clauses 2547}#)
       (letrec*
         ((#{expand-req 2556}#
            (lambda (#{req 2563}#
                     #{opt 2564}#
                     #{rest 2565}#
                     #{kw 2566}#
                     #{body 2567}#)
              (let ((#{vars 2575}#
                      (map #{gen-var 451}# #{req 2563}#))
                    (#{labels 2576}#
                      (#{gen-labels 358}# #{req 2563}#)))
                (let ((#{r* 2579}#
                        (#{extend-var-env 333}#
                          #{labels 2576}#
                          #{vars 2575}#
                          #{r 2542}#))
                      (#{w* 2580}#
                        (#{make-binding-wrap 387}#
                          #{req 2563}#
                          #{labels 2576}#
                          #{w 2543}#)))
                  (#{expand-opt 2558}#
                    (map syntax->datum #{req 2563}#)
                    #{opt 2564}#
                    #{rest 2565}#
                    #{kw 2566}#
                    #{body 2567}#
                    (reverse #{vars 2575}#)
                    #{r* 2579}#
                    #{w* 2580}#
                    '()
                    '())))))
          (#{expand-opt 2558}#
            (lambda (#{req 2581}#
                     #{opt 2582}#
                     #{rest 2583}#
                     #{kw 2584}#
                     #{body 2585}#
                     #{vars 2586}#
                     #{r* 2587}#
                     #{w* 2588}#
                     #{out 2589}#
                     #{inits 2590}#)
              (if (pair? #{opt 2582}#)
                (let ((#{tmp 2603}# (car #{opt 2582}#)))
                  (let ((#{tmp 2604}#
                          ($sc-dispatch #{tmp 2603}# '(any any))))
                    (if #{tmp 2604}#
                      (@apply
                        (lambda (#{id 2607}# #{i 2608}#)
                          (let ((#{v 2611}# (#{gen-var 451}# #{id 2607}#)))
                            (let ((#{l 2613}#
                                    (#{gen-labels 358}# (list #{v 2611}#))))
                              (let ((#{r** 2615}#
                                      (#{extend-var-env 333}#
                                        #{l 2613}#
                                        (list #{v 2611}#)
                                        #{r* 2587}#)))
                                (let ((#{w** 2617}#
                                        (#{make-binding-wrap 387}#
                                          (list #{id 2607}#)
                                          #{l 2613}#
                                          #{w* 2588}#)))
                                  (#{expand-opt 2558}#
                                    #{req 2581}#
                                    (cdr #{opt 2582}#)
                                    #{rest 2583}#
                                    #{kw 2584}#
                                    #{body 2585}#
                                    (cons #{v 2611}# #{vars 2586}#)
                                    #{r** 2615}#
                                    #{w** 2617}#
                                    (cons (syntax->datum #{id 2607}#)
                                          #{out 2589}#)
                                    (cons (#{chi 423}#
                                            #{i 2608}#
                                            #{r* 2587}#
                                            #{w* 2588}#
                                            #{mod 2545}#)
                                          #{inits 2590}#)))))))
                        #{tmp 2604}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp 2603}#))))
                (if #{rest 2583}#
                  (let ((#{v 2622}# (#{gen-var 451}# #{rest 2583}#)))
                    (let ((#{l 2624}#
                            (#{gen-labels 358}# (list #{v 2622}#))))
                      (let ((#{r* 2626}#
                              (#{extend-var-env 333}#
                                #{l 2624}#
                                (list #{v 2622}#)
                                #{r* 2587}#)))
                        (let ((#{w* 2628}#
                                (#{make-binding-wrap 387}#
                                  (list #{rest 2583}#)
                                  #{l 2624}#
                                  #{w* 2588}#)))
                          (#{expand-kw 2560}#
                            #{req 2581}#
                            (if (pair? #{out 2589}#)
                              (reverse #{out 2589}#)
                              #f)
                            (syntax->datum #{rest 2583}#)
                            (if (pair? #{kw 2584}#)
                              (cdr #{kw 2584}#)
                              #{kw 2584}#)
                            #{body 2585}#
                            (cons #{v 2622}# #{vars 2586}#)
                            #{r* 2626}#
                            #{w* 2628}#
                            (if (pair? #{kw 2584}#) (car #{kw 2584}#) #f)
                            '()
                            #{inits 2590}#)))))
                  (#{expand-kw 2560}#
                    #{req 2581}#
                    (if (pair? #{out 2589}#)
                      (reverse #{out 2589}#)
                      #f)
                    #f
                    (if (pair? #{kw 2584}#)
                      (cdr #{kw 2584}#)
                      #{kw 2584}#)
                    #{body 2585}#
                    #{vars 2586}#
                    #{r* 2587}#
                    #{w* 2588}#
                    (if (pair? #{kw 2584}#) (car #{kw 2584}#) #f)
                    '()
                    #{inits 2590}#)))))
          (#{expand-kw 2560}#
            (lambda (#{req 2630}#
                     #{opt 2631}#
                     #{rest 2632}#
                     #{kw 2633}#
                     #{body 2634}#
                     #{vars 2635}#
                     #{r* 2636}#
                     #{w* 2637}#
                     #{aok 2638}#
                     #{out 2639}#
                     #{inits 2640}#)
              (if (pair? #{kw 2633}#)
                (let ((#{tmp 2654}# (car #{kw 2633}#)))
                  (let ((#{tmp 2655}#
                          ($sc-dispatch #{tmp 2654}# '(any any any))))
                    (if #{tmp 2655}#
                      (@apply
                        (lambda (#{k 2659}# #{id 2660}# #{i 2661}#)
                          (let ((#{v 2664}# (#{gen-var 451}# #{id 2660}#)))
                            (let ((#{l 2666}#
                                    (#{gen-labels 358}# (list #{v 2664}#))))
                              (let ((#{r** 2668}#
                                      (#{extend-var-env 333}#
                                        #{l 2666}#
                                        (list #{v 2664}#)
                                        #{r* 2636}#)))
                                (let ((#{w** 2670}#
                                        (#{make-binding-wrap 387}#
                                          (list #{id 2660}#)
                                          #{l 2666}#
                                          #{w* 2637}#)))
                                  (#{expand-kw 2560}#
                                    #{req 2630}#
                                    #{opt 2631}#
                                    #{rest 2632}#
                                    (cdr #{kw 2633}#)
                                    #{body 2634}#
                                    (cons #{v 2664}# #{vars 2635}#)
                                    #{r** 2668}#
                                    #{w** 2670}#
                                    #{aok 2638}#
                                    (cons (list (syntax->datum #{k 2659}#)
                                                (syntax->datum #{id 2660}#)
                                                #{v 2664}#)
                                          #{out 2639}#)
                                    (cons (#{chi 423}#
                                            #{i 2661}#
                                            #{r* 2636}#
                                            #{w* 2637}#
                                            #{mod 2545}#)
                                          #{inits 2640}#)))))))
                        #{tmp 2655}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp 2654}#))))
                (#{expand-body 2562}#
                  #{req 2630}#
                  #{opt 2631}#
                  #{rest 2632}#
                  (if (let ((#{t 2674}# #{aok 2638}#))
                        (if #{t 2674}# #{t 2674}# (pair? #{out 2639}#)))
                    (cons #{aok 2638}# (reverse #{out 2639}#))
                    #f)
                  #{body 2634}#
                  (reverse #{vars 2635}#)
                  #{r* 2636}#
                  #{w* 2637}#
                  (reverse #{inits 2640}#)
                  '()))))
          (#{expand-body 2562}#
            (lambda (#{req 2676}#
                     #{opt 2677}#
                     #{rest 2678}#
                     #{kw 2679}#
                     #{body 2680}#
                     #{vars 2681}#
                     #{r* 2682}#
                     #{w* 2683}#
                     #{inits 2684}#
                     #{meta 2685}#)
              (let ((#{tmp 2696}# #{body 2680}#))
                (let ((#{tmp 2697}#
                        ($sc-dispatch #{tmp 2696}# '(any any . each-any))))
                  (if (if #{tmp 2697}#
                        (@apply
                          (lambda (#{docstring 2701}# #{e1 2702}# #{e2 2703}#)
                            (string? (syntax->datum #{docstring 2701}#)))
                          #{tmp 2697}#)
                        #f)
                    (@apply
                      (lambda (#{docstring 2707}# #{e1 2708}# #{e2 2709}#)
                        (#{expand-body 2562}#
                          #{req 2676}#
                          #{opt 2677}#
                          #{rest 2678}#
                          #{kw 2679}#
                          (cons #{e1 2708}# #{e2 2709}#)
                          #{vars 2681}#
                          #{r* 2682}#
                          #{w* 2683}#
                          #{inits 2684}#
                          (append
                            #{meta 2685}#
                            (list (cons 'documentation
                                        (syntax->datum #{docstring 2707}#))))))
                      #{tmp 2697}#)
                    (let ((#{tmp 2712}#
                            ($sc-dispatch
                              #{tmp 2696}#
                              '(#(vector #(each (any . any)))
                                any
                                .
                                each-any))))
                      (if #{tmp 2712}#
                        (@apply
                          (lambda (#{k 2717}#
                                   #{v 2718}#
                                   #{e1 2719}#
                                   #{e2 2720}#)
                            (#{expand-body 2562}#
                              #{req 2676}#
                              #{opt 2677}#
                              #{rest 2678}#
                              #{kw 2679}#
                              (cons #{e1 2719}# #{e2 2720}#)
                              #{vars 2681}#
                              #{r* 2682}#
                              #{w* 2683}#
                              #{inits 2684}#
                              (append
                                #{meta 2685}#
                                (syntax->datum
                                  (map cons #{k 2717}# #{v 2718}#)))))
                          #{tmp 2712}#)
                        (let ((#{tmp 2724}#
                                ($sc-dispatch #{tmp 2696}# '(any . each-any))))
                          (if #{tmp 2724}#
                            (@apply
                              (lambda (#{e1 2727}# #{e2 2728}#)
                                (values
                                  #{meta 2685}#
                                  #{req 2676}#
                                  #{opt 2677}#
                                  #{rest 2678}#
                                  #{kw 2679}#
                                  #{inits 2684}#
                                  #{vars 2681}#
                                  (#{chi-body 431}#
                                    (cons #{e1 2727}# #{e2 2728}#)
                                    (#{source-wrap 411}#
                                      #{e 2541}#
                                      #{w 2543}#
                                      #{s 2544}#
                                      #{mod 2545}#)
                                    #{r* 2682}#
                                    #{w* 2683}#
                                    #{mod 2545}#)))
                              #{tmp 2724}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp 2696}#)))))))))))
         (let ((#{tmp 2730}# #{clauses 2547}#))
           (let ((#{tmp 2731}# ($sc-dispatch #{tmp 2730}# '())))
             (if #{tmp 2731}#
               (@apply (lambda () (values '() #f)) #{tmp 2731}#)
               (let ((#{tmp 2732}#
                       ($sc-dispatch
                         #{tmp 2730}#
                         '((any any . each-any)
                           .
                           #(each (any any . each-any))))))
                 (if #{tmp 2732}#
                   (@apply
                     (lambda (#{args 2739}#
                              #{e1 2740}#
                              #{e2 2741}#
                              #{args* 2742}#
                              #{e1* 2743}#
                              #{e2* 2744}#)
                       (call-with-values
                         (lambda () (#{get-formals 2546}# #{args 2739}#))
                         (lambda (#{req 2745}#
                                  #{opt 2746}#
                                  #{rest 2747}#
                                  #{kw 2748}#)
                           (call-with-values
                             (lambda ()
                               (#{expand-req 2556}#
                                 #{req 2745}#
                                 #{opt 2746}#
                                 #{rest 2747}#
                                 #{kw 2748}#
                                 (cons #{e1 2740}# #{e2 2741}#)))
                             (lambda (#{meta 2754}#
                                      #{req 2755}#
                                      #{opt 2756}#
                                      #{rest 2757}#
                                      #{kw 2758}#
                                      #{inits 2759}#
                                      #{vars 2760}#
                                      #{body 2761}#)
                               (call-with-values
                                 (lambda ()
                                   (#{chi-lambda-case 447}#
                                     #{e 2541}#
                                     #{r 2542}#
                                     #{w 2543}#
                                     #{s 2544}#
                                     #{mod 2545}#
                                     #{get-formals 2546}#
                                     (map (lambda (#{tmp 2772}#
                                                   #{tmp 2771}#
                                                   #{tmp 2770}#)
                                            (cons #{tmp 2770}#
                                                  (cons #{tmp 2771}#
                                                        #{tmp 2772}#)))
                                          #{e2* 2744}#
                                          #{e1* 2743}#
                                          #{args* 2742}#)))
                                 (lambda (#{meta* 2774}# #{else* 2775}#)
                                   (values
                                     (append #{meta 2754}# #{meta* 2774}#)
                                     (#{build-lambda-case 289}#
                                       #{s 2544}#
                                       #{req 2755}#
                                       #{opt 2756}#
                                       #{rest 2757}#
                                       #{kw 2758}#
                                       #{inits 2759}#
                                       #{vars 2760}#
                                       #{body 2761}#
                                       #{else* 2775}#)))))))))
                     #{tmp 2732}#)
                   (syntax-violation
                     #f
                     "source expression failed to match any pattern"
                     #{tmp 2730}#)))))))))
   (#{strip 449}#
     (lambda (#{x 2778}# #{w 2779}#)
       (if (memq 'top (car #{w 2779}#))
         #{x 2778}#
         (letrec*
           ((#{f 2786}#
              (lambda (#{x 2787}#)
                (if (#{syntax-object? 309}# #{x 2787}#)
                  (#{strip 449}#
                    (#{syntax-object-expression 311}# #{x 2787}#)
                    (#{syntax-object-wrap 313}# #{x 2787}#))
                  (if (pair? #{x 2787}#)
                    (let ((#{a 2794}# (#{f 2786}# (car #{x 2787}#)))
                          (#{d 2795}# (#{f 2786}# (cdr #{x 2787}#))))
                      (if (if (eq? #{a 2794}# (car #{x 2787}#))
                            (eq? #{d 2795}# (cdr #{x 2787}#))
                            #f)
                        #{x 2787}#
                        (cons #{a 2794}# #{d 2795}#)))
                    (if (vector? #{x 2787}#)
                      (let ((#{old 2801}# (vector->list #{x 2787}#)))
                        (let ((#{new 2803}# (map #{f 2786}# #{old 2801}#)))
                          (letrec*
                            ((#{lp 2807}#
                               (lambda (#{l1 2808}# #{l2 2809}#)
                                 (if (null? #{l1 2808}#)
                                   #{x 2787}#
                                   (if (eq? (car #{l1 2808}#)
                                            (car #{l2 2809}#))
                                     (#{lp 2807}#
                                       (cdr #{l1 2808}#)
                                       (cdr #{l2 2809}#))
                                     (list->vector #{new 2803}#))))))
                            (#{lp 2807}# #{old 2801}# #{new 2803}#))))
                      #{x 2787}#))))))
           (#{f 2786}# #{x 2778}#)))))
   (#{gen-var 451}#
     (lambda (#{id 2811}#)
       (let ((#{id 2814}#
               (if (#{syntax-object? 309}# #{id 2811}#)
                 (#{syntax-object-expression 311}# #{id 2811}#)
                 #{id 2811}#)))
         (gensym
           (string-append (symbol->string #{id 2814}#) " ")))))
   (#{lambda-var-list 453}#
     (lambda (#{vars 2816}#)
       (letrec*
         ((#{lvl 2822}#
            (lambda (#{vars 2823}# #{ls 2824}# #{w 2825}#)
              (if (pair? #{vars 2823}#)
                (#{lvl 2822}#
                  (cdr #{vars 2823}#)
                  (cons (#{wrap 409}# (car #{vars 2823}#) #{w 2825}# #f)
                        #{ls 2824}#)
                  #{w 2825}#)
                (if (#{id? 343}# #{vars 2823}#)
                  (cons (#{wrap 409}# #{vars 2823}# #{w 2825}# #f)
                        #{ls 2824}#)
                  (if (null? #{vars 2823}#)
                    #{ls 2824}#
                    (if (#{syntax-object? 309}# #{vars 2823}#)
                      (#{lvl 2822}#
                        (#{syntax-object-expression 311}# #{vars 2823}#)
                        #{ls 2824}#
                        (#{join-wraps 391}#
                          #{w 2825}#
                          (#{syntax-object-wrap 313}# #{vars 2823}#)))
                      (cons #{vars 2823}# #{ls 2824}#))))))))
         (#{lvl 2822}# #{vars 2816}# '() '(()))))))
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
      (lambda (#{e 2836}#
               #{r 2837}#
               #{w 2838}#
               #{s 2839}#
               #{mod 2840}#)
        (let ((#{tmp 2846}# #{e 2836}#))
          (let ((#{tmp 2847}#
                  ($sc-dispatch
                    #{tmp 2846}#
                    '(_ #(each (any any)) any . each-any))))
            (if (if #{tmp 2847}#
                  (@apply
                    (lambda (#{var 2852}#
                             #{val 2853}#
                             #{e1 2854}#
                             #{e2 2855}#)
                      (#{valid-bound-ids? 403}# #{var 2852}#))
                    #{tmp 2847}#)
                  #f)
              (@apply
                (lambda (#{var 2861}#
                         #{val 2862}#
                         #{e1 2863}#
                         #{e2 2864}#)
                  (let ((#{names 2866}#
                          (map (lambda (#{x 2867}#)
                                 (#{id-var-name 397}# #{x 2867}# #{w 2838}#))
                               #{var 2861}#)))
                    (begin
                      (for-each
                        (lambda (#{id 2870}# #{n 2871}#)
                          (let ((#{atom-key 2876}#
                                  (car (#{lookup 337}#
                                         #{n 2871}#
                                         #{r 2837}#
                                         #{mod 2840}#))))
                            (if (memv #{atom-key 2876}# '(displaced-lexical))
                              (syntax-violation
                                'fluid-let-syntax
                                "identifier out of context"
                                #{e 2836}#
                                (#{source-wrap 411}#
                                  #{id 2870}#
                                  #{w 2838}#
                                  #{s 2839}#
                                  #{mod 2840}#)))))
                        #{var 2861}#
                        #{names 2866}#)
                      (#{chi-body 431}#
                        (cons #{e1 2863}# #{e2 2864}#)
                        (#{source-wrap 411}#
                          #{e 2836}#
                          #{w 2838}#
                          #{s 2839}#
                          #{mod 2840}#)
                        (#{extend-env 331}#
                          #{names 2866}#
                          (let ((#{trans-r 2882}#
                                  (#{macros-only-env 335}# #{r 2837}#)))
                            (map (lambda (#{x 2883}#)
                                   (cons 'macro
                                         (#{eval-local-transformer 435}#
                                           (#{chi 423}#
                                             #{x 2883}#
                                             #{trans-r 2882}#
                                             #{w 2838}#
                                             #{mod 2840}#)
                                           #{mod 2840}#)))
                                 #{val 2862}#))
                          #{r 2837}#)
                        #{w 2838}#
                        #{mod 2840}#))))
                #{tmp 2847}#)
              (let ((#{_ 2888}# #{tmp 2846}#))
                (syntax-violation
                  'fluid-let-syntax
                  "bad syntax"
                  (#{source-wrap 411}#
                    #{e 2836}#
                    #{w 2838}#
                    #{s 2839}#
                    #{mod 2840}#))))))))
    (#{global-extend 339}#
      'core
      'quote
      (lambda (#{e 2889}#
               #{r 2890}#
               #{w 2891}#
               #{s 2892}#
               #{mod 2893}#)
        (let ((#{tmp 2899}# #{e 2889}#))
          (let ((#{tmp 2900}#
                  ($sc-dispatch #{tmp 2899}# '(_ any))))
            (if #{tmp 2900}#
              (@apply
                (lambda (#{e 2902}#)
                  (#{build-data 295}#
                    #{s 2892}#
                    (#{strip 449}# #{e 2902}# #{w 2891}#)))
                #{tmp 2900}#)
              (let ((#{_ 2904}# #{tmp 2899}#))
                (syntax-violation
                  'quote
                  "bad syntax"
                  (#{source-wrap 411}#
                    #{e 2889}#
                    #{w 2891}#
                    #{s 2892}#
                    #{mod 2893}#))))))))
    (#{global-extend 339}#
      'core
      'syntax
      (letrec*
        ((#{gen-syntax 2906}#
           (lambda (#{src 2921}#
                    #{e 2922}#
                    #{r 2923}#
                    #{maps 2924}#
                    #{ellipsis? 2925}#
                    #{mod 2926}#)
             (if (#{id? 343}# #{e 2922}#)
               (let ((#{label 2934}#
                       (#{id-var-name 397}# #{e 2922}# '(()))))
                 (let ((#{b 2937}#
                         (#{lookup 337}#
                           #{label 2934}#
                           #{r 2923}#
                           #{mod 2926}#)))
                   (if (eq? (car #{b 2937}#) 'syntax)
                     (call-with-values
                       (lambda ()
                         (let ((#{var.lev 2940}# (cdr #{b 2937}#)))
                           (#{gen-ref 2908}#
                             #{src 2921}#
                             (car #{var.lev 2940}#)
                             (cdr #{var.lev 2940}#)
                             #{maps 2924}#)))
                       (lambda (#{var 2942}# #{maps 2943}#)
                         (values (list 'ref #{var 2942}#) #{maps 2943}#)))
                     (if (#{ellipsis? 2925}# #{e 2922}#)
                       (syntax-violation
                         'syntax
                         "misplaced ellipsis"
                         #{src 2921}#)
                       (values (list 'quote #{e 2922}#) #{maps 2924}#)))))
               (let ((#{tmp 2948}# #{e 2922}#))
                 (let ((#{tmp 2949}#
                         ($sc-dispatch #{tmp 2948}# '(any any))))
                   (if (if #{tmp 2949}#
                         (@apply
                           (lambda (#{dots 2952}# #{e 2953}#)
                             (#{ellipsis? 2925}# #{dots 2952}#))
                           #{tmp 2949}#)
                         #f)
                     (@apply
                       (lambda (#{dots 2956}# #{e 2957}#)
                         (#{gen-syntax 2906}#
                           #{src 2921}#
                           #{e 2957}#
                           #{r 2923}#
                           #{maps 2924}#
                           (lambda (#{x 2958}#) #f)
                           #{mod 2926}#))
                       #{tmp 2949}#)
                     (let ((#{tmp 2960}#
                             ($sc-dispatch #{tmp 2948}# '(any any . any))))
                       (if (if #{tmp 2960}#
                             (@apply
                               (lambda (#{x 2964}# #{dots 2965}# #{y 2966}#)
                                 (#{ellipsis? 2925}# #{dots 2965}#))
                               #{tmp 2960}#)
                             #f)
                         (@apply
                           (lambda (#{x 2970}# #{dots 2971}# #{y 2972}#)
                             (letrec*
                               ((#{f 2976}#
                                  (lambda (#{y 2977}# #{k 2978}#)
                                    (let ((#{tmp 2985}# #{y 2977}#))
                                      (let ((#{tmp 2986}#
                                              ($sc-dispatch
                                                #{tmp 2985}#
                                                '(any . any))))
                                        (if (if #{tmp 2986}#
                                              (@apply
                                                (lambda (#{dots 2989}#
                                                         #{y 2990}#)
                                                  (#{ellipsis? 2925}#
                                                    #{dots 2989}#))
                                                #{tmp 2986}#)
                                              #f)
                                          (@apply
                                            (lambda (#{dots 2993}# #{y 2994}#)
                                              (#{f 2976}#
                                                #{y 2994}#
                                                (lambda (#{maps 2995}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{k 2978}#
                                                        (cons '()
                                                              #{maps 2995}#)))
                                                    (lambda (#{x 2997}#
                                                             #{maps 2998}#)
                                                      (if (null? (car #{maps 2998}#))
                                                        (syntax-violation
                                                          'syntax
                                                          "extra ellipsis"
                                                          #{src 2921}#)
                                                        (values
                                                          (#{gen-mappend 2910}#
                                                            #{x 2997}#
                                                            (car #{maps 2998}#))
                                                          (cdr #{maps 2998}#))))))))
                                            #{tmp 2986}#)
                                          (let ((#{_ 3002}# #{tmp 2985}#))
                                            (call-with-values
                                              (lambda ()
                                                (#{gen-syntax 2906}#
                                                  #{src 2921}#
                                                  #{y 2977}#
                                                  #{r 2923}#
                                                  #{maps 2924}#
                                                  #{ellipsis? 2925}#
                                                  #{mod 2926}#))
                                              (lambda (#{y 3003}#
                                                       #{maps 3004}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{k 2978}# #{maps 3004}#))
                                                  (lambda (#{x 3007}#
                                                           #{maps 3008}#)
                                                    (values
                                                      (#{gen-append 2916}#
                                                        #{x 3007}#
                                                        #{y 3003}#)
                                                      #{maps 3008}#))))))))))))
                               (#{f 2976}#
                                 #{y 2972}#
                                 (lambda (#{maps 2979}#)
                                   (call-with-values
                                     (lambda ()
                                       (#{gen-syntax 2906}#
                                         #{src 2921}#
                                         #{x 2970}#
                                         #{r 2923}#
                                         (cons '() #{maps 2979}#)
                                         #{ellipsis? 2925}#
                                         #{mod 2926}#))
                                     (lambda (#{x 2981}# #{maps 2982}#)
                                       (if (null? (car #{maps 2982}#))
                                         (syntax-violation
                                           'syntax
                                           "extra ellipsis"
                                           #{src 2921}#)
                                         (values
                                           (#{gen-map 2912}#
                                             #{x 2981}#
                                             (car #{maps 2982}#))
                                           (cdr #{maps 2982}#)))))))))
                           #{tmp 2960}#)
                         (let ((#{tmp 3011}#
                                 ($sc-dispatch #{tmp 2948}# '(any . any))))
                           (if #{tmp 3011}#
                             (@apply
                               (lambda (#{x 3014}# #{y 3015}#)
                                 (call-with-values
                                   (lambda ()
                                     (#{gen-syntax 2906}#
                                       #{src 2921}#
                                       #{x 3014}#
                                       #{r 2923}#
                                       #{maps 2924}#
                                       #{ellipsis? 2925}#
                                       #{mod 2926}#))
                                   (lambda (#{x 3016}# #{maps 3017}#)
                                     (call-with-values
                                       (lambda ()
                                         (#{gen-syntax 2906}#
                                           #{src 2921}#
                                           #{y 3015}#
                                           #{r 2923}#
                                           #{maps 3017}#
                                           #{ellipsis? 2925}#
                                           #{mod 2926}#))
                                       (lambda (#{y 3020}# #{maps 3021}#)
                                         (values
                                           (#{gen-cons 2914}#
                                             #{x 3016}#
                                             #{y 3020}#)
                                           #{maps 3021}#))))))
                               #{tmp 3011}#)
                             (let ((#{tmp 3024}#
                                     ($sc-dispatch
                                       #{tmp 2948}#
                                       '#(vector (any . each-any)))))
                               (if #{tmp 3024}#
                                 (@apply
                                   (lambda (#{e1 3027}# #{e2 3028}#)
                                     (call-with-values
                                       (lambda ()
                                         (#{gen-syntax 2906}#
                                           #{src 2921}#
                                           (cons #{e1 3027}# #{e2 3028}#)
                                           #{r 2923}#
                                           #{maps 2924}#
                                           #{ellipsis? 2925}#
                                           #{mod 2926}#))
                                       (lambda (#{e 3030}# #{maps 3031}#)
                                         (values
                                           (#{gen-vector 2918}# #{e 3030}#)
                                           #{maps 3031}#))))
                                   #{tmp 3024}#)
                                 (let ((#{_ 3035}# #{tmp 2948}#))
                                   (values
                                     (list 'quote #{e 2922}#)
                                     #{maps 2924}#))))))))))))))
         (#{gen-ref 2908}#
           (lambda (#{src 3037}#
                    #{var 3038}#
                    #{level 3039}#
                    #{maps 3040}#)
             (if (= #{level 3039}# 0)
               (values #{var 3038}# #{maps 3040}#)
               (if (null? #{maps 3040}#)
                 (syntax-violation
                   'syntax
                   "missing ellipsis"
                   #{src 3037}#)
                 (call-with-values
                   (lambda ()
                     (#{gen-ref 2908}#
                       #{src 3037}#
                       #{var 3038}#
                       (#{1-}# #{level 3039}#)
                       (cdr #{maps 3040}#)))
                   (lambda (#{outer-var 3047}# #{outer-maps 3048}#)
                     (let ((#{b 3052}#
                             (assq #{outer-var 3047}# (car #{maps 3040}#))))
                       (if #{b 3052}#
                         (values (cdr #{b 3052}#) #{maps 3040}#)
                         (let ((#{inner-var 3054}# (#{gen-var 451}# 'tmp)))
                           (values
                             #{inner-var 3054}#
                             (cons (cons (cons #{outer-var 3047}#
                                               #{inner-var 3054}#)
                                         (car #{maps 3040}#))
                                   #{outer-maps 3048}#)))))))))))
         (#{gen-mappend 2910}#
           (lambda (#{e 3055}# #{map-env 3056}#)
             (list 'apply
                   '(primitive append)
                   (#{gen-map 2912}# #{e 3055}# #{map-env 3056}#))))
         (#{gen-map 2912}#
           (lambda (#{e 3060}# #{map-env 3061}#)
             (let ((#{formals 3066}# (map cdr #{map-env 3061}#))
                   (#{actuals 3067}#
                     (map (lambda (#{x 3068}#)
                            (list 'ref (car #{x 3068}#)))
                          #{map-env 3061}#)))
               (if (eq? (car #{e 3060}#) 'ref)
                 (car #{actuals 3067}#)
                 (if (and-map
                       (lambda (#{x 3075}#)
                         (if (eq? (car #{x 3075}#) 'ref)
                           (memq (car (cdr #{x 3075}#)) #{formals 3066}#)
                           #f))
                       (cdr #{e 3060}#))
                   (cons 'map
                         (cons (list 'primitive (car #{e 3060}#))
                               (map (let ((#{r 3081}#
                                            (map cons
                                                 #{formals 3066}#
                                                 #{actuals 3067}#)))
                                      (lambda (#{x 3082}#)
                                        (cdr (assq (car (cdr #{x 3082}#))
                                                   #{r 3081}#))))
                                    (cdr #{e 3060}#))))
                   (cons 'map
                         (cons (list 'lambda #{formals 3066}# #{e 3060}#)
                               #{actuals 3067}#)))))))
         (#{gen-cons 2914}#
           (lambda (#{x 3086}# #{y 3087}#)
             (let ((#{atom-key 3092}# (car #{y 3087}#)))
               (if (memv #{atom-key 3092}# '(quote))
                 (if (eq? (car #{x 3086}#) 'quote)
                   (list 'quote
                         (cons (car (cdr #{x 3086}#))
                               (car (cdr #{y 3087}#))))
                   (if (eq? (car (cdr #{y 3087}#)) '())
                     (list 'list #{x 3086}#)
                     (list 'cons #{x 3086}# #{y 3087}#)))
                 (if (memv #{atom-key 3092}# '(list))
                   (cons 'list (cons #{x 3086}# (cdr #{y 3087}#)))
                   (list 'cons #{x 3086}# #{y 3087}#))))))
         (#{gen-append 2916}#
           (lambda (#{x 3101}# #{y 3102}#)
             (if (equal? #{y 3102}# ''())
               #{x 3101}#
               (list 'append #{x 3101}# #{y 3102}#))))
         (#{gen-vector 2918}#
           (lambda (#{x 3106}#)
             (if (eq? (car #{x 3106}#) 'list)
               (cons 'vector (cdr #{x 3106}#))
               (if (eq? (car #{x 3106}#) 'quote)
                 (list 'quote
                       (list->vector (car (cdr #{x 3106}#))))
                 (list 'list->vector #{x 3106}#)))))
         (#{regen 2920}#
           (lambda (#{x 3116}#)
             (let ((#{atom-key 3120}# (car #{x 3116}#)))
               (if (memv #{atom-key 3120}# '(ref))
                 (#{build-lexical-reference 273}#
                   'value
                   #f
                   (car (cdr #{x 3116}#))
                   (car (cdr #{x 3116}#)))
                 (if (memv #{atom-key 3120}# '(primitive))
                   (#{build-primref 293}# #f (car (cdr #{x 3116}#)))
                   (if (memv #{atom-key 3120}# '(quote))
                     (#{build-data 295}# #f (car (cdr #{x 3116}#)))
                     (if (memv #{atom-key 3120}# '(lambda))
                       (if (list? (car (cdr #{x 3116}#)))
                         (#{build-simple-lambda 285}#
                           #f
                           (car (cdr #{x 3116}#))
                           #f
                           (car (cdr #{x 3116}#))
                           '()
                           (#{regen 2920}# (car (cdr (cdr #{x 3116}#)))))
                         (error "how did we get here" #{x 3116}#))
                       (#{build-primcall 291}#
                         #f
                         (car #{x 3116}#)
                         (map #{regen 2920}# (cdr #{x 3116}#)))))))))))
        (lambda (#{e 3131}#
                 #{r 3132}#
                 #{w 3133}#
                 #{s 3134}#
                 #{mod 3135}#)
          (let ((#{e 3142}#
                  (#{source-wrap 411}#
                    #{e 3131}#
                    #{w 3133}#
                    #{s 3134}#
                    #{mod 3135}#)))
            (let ((#{tmp 3143}# #{e 3142}#))
              (let ((#{tmp 3144}#
                      ($sc-dispatch #{tmp 3143}# '(_ any))))
                (if #{tmp 3144}#
                  (@apply
                    (lambda (#{x 3146}#)
                      (call-with-values
                        (lambda ()
                          (#{gen-syntax 2906}#
                            #{e 3142}#
                            #{x 3146}#
                            #{r 3132}#
                            '()
                            #{ellipsis? 439}#
                            #{mod 3135}#))
                        (lambda (#{e 3147}# #{maps 3148}#)
                          (#{regen 2920}# #{e 3147}#))))
                    #{tmp 3144}#)
                  (let ((#{_ 3152}# #{tmp 3143}#))
                    (syntax-violation
                      'syntax
                      "bad `syntax' form"
                      #{e 3142}#)))))))))
    (#{global-extend 339}#
      'core
      'lambda
      (lambda (#{e 3153}#
               #{r 3154}#
               #{w 3155}#
               #{s 3156}#
               #{mod 3157}#)
        (let ((#{tmp 3163}# #{e 3153}#))
          (let ((#{tmp 3164}#
                  ($sc-dispatch
                    #{tmp 3163}#
                    '(_ any any . each-any))))
            (if #{tmp 3164}#
              (@apply
                (lambda (#{args 3168}# #{e1 3169}# #{e2 3170}#)
                  (call-with-values
                    (lambda ()
                      (#{lambda-formals 441}# #{args 3168}#))
                    (lambda (#{req 3171}#
                             #{opt 3172}#
                             #{rest 3173}#
                             #{kw 3174}#)
                      (letrec*
                        ((#{lp 3182}#
                           (lambda (#{body 3183}# #{meta 3184}#)
                             (let ((#{tmp 3186}# #{body 3183}#))
                               (let ((#{tmp 3187}#
                                       ($sc-dispatch
                                         #{tmp 3186}#
                                         '(any any . each-any))))
                                 (if (if #{tmp 3187}#
                                       (@apply
                                         (lambda (#{docstring 3191}#
                                                  #{e1 3192}#
                                                  #{e2 3193}#)
                                           (string?
                                             (syntax->datum
                                               #{docstring 3191}#)))
                                         #{tmp 3187}#)
                                       #f)
                                   (@apply
                                     (lambda (#{docstring 3197}#
                                              #{e1 3198}#
                                              #{e2 3199}#)
                                       (#{lp 3182}#
                                         (cons #{e1 3198}# #{e2 3199}#)
                                         (append
                                           #{meta 3184}#
                                           (list (cons 'documentation
                                                       (syntax->datum
                                                         #{docstring 3197}#))))))
                                     #{tmp 3187}#)
                                   (let ((#{tmp 3202}#
                                           ($sc-dispatch
                                             #{tmp 3186}#
                                             '(#(vector #(each (any . any)))
                                               any
                                               .
                                               each-any))))
                                     (if #{tmp 3202}#
                                       (@apply
                                         (lambda (#{k 3207}#
                                                  #{v 3208}#
                                                  #{e1 3209}#
                                                  #{e2 3210}#)
                                           (#{lp 3182}#
                                             (cons #{e1 3209}# #{e2 3210}#)
                                             (append
                                               #{meta 3184}#
                                               (syntax->datum
                                                 (map cons
                                                      #{k 3207}#
                                                      #{v 3208}#)))))
                                         #{tmp 3202}#)
                                       (let ((#{_ 3215}# #{tmp 3186}#))
                                         (#{chi-simple-lambda 443}#
                                           #{e 3153}#
                                           #{r 3154}#
                                           #{w 3155}#
                                           #{s 3156}#
                                           #{mod 3157}#
                                           #{req 3171}#
                                           #{rest 3173}#
                                           #{meta 3184}#
                                           #{body 3183}#))))))))))
                        (#{lp 3182}# (cons #{e1 3169}# #{e2 3170}#) '())))))
                #{tmp 3164}#)
              (let ((#{_ 3217}# #{tmp 3163}#))
                (syntax-violation
                  'lambda
                  "bad lambda"
                  #{e 3153}#)))))))
    (#{global-extend 339}#
      'core
      'lambda*
      (lambda (#{e 3218}#
               #{r 3219}#
               #{w 3220}#
               #{s 3221}#
               #{mod 3222}#)
        (let ((#{tmp 3228}# #{e 3218}#))
          (let ((#{tmp 3229}#
                  ($sc-dispatch
                    #{tmp 3228}#
                    '(_ any any . each-any))))
            (if #{tmp 3229}#
              (@apply
                (lambda (#{args 3233}# #{e1 3234}# #{e2 3235}#)
                  (call-with-values
                    (lambda ()
                      (#{chi-lambda-case 447}#
                        #{e 3218}#
                        #{r 3219}#
                        #{w 3220}#
                        #{s 3221}#
                        #{mod 3222}#
                        #{lambda*-formals 445}#
                        (list (cons #{args 3233}#
                                    (cons #{e1 3234}# #{e2 3235}#)))))
                    (lambda (#{meta 3237}# #{lcase 3238}#)
                      (#{build-case-lambda 287}#
                        #{s 3221}#
                        #{meta 3237}#
                        #{lcase 3238}#))))
                #{tmp 3229}#)
              (let ((#{_ 3242}# #{tmp 3228}#))
                (syntax-violation
                  'lambda
                  "bad lambda*"
                  #{e 3218}#)))))))
    (#{global-extend 339}#
      'core
      'case-lambda
      (lambda (#{e 3243}#
               #{r 3244}#
               #{w 3245}#
               #{s 3246}#
               #{mod 3247}#)
        (let ((#{tmp 3253}# #{e 3243}#))
          (let ((#{tmp 3254}#
                  ($sc-dispatch
                    #{tmp 3253}#
                    '(_ (any any . each-any)
                        .
                        #(each (any any . each-any))))))
            (if #{tmp 3254}#
              (@apply
                (lambda (#{args 3261}#
                         #{e1 3262}#
                         #{e2 3263}#
                         #{args* 3264}#
                         #{e1* 3265}#
                         #{e2* 3266}#)
                  (call-with-values
                    (lambda ()
                      (#{chi-lambda-case 447}#
                        #{e 3243}#
                        #{r 3244}#
                        #{w 3245}#
                        #{s 3246}#
                        #{mod 3247}#
                        #{lambda-formals 441}#
                        (cons (cons #{args 3261}#
                                    (cons #{e1 3262}# #{e2 3263}#))
                              (map (lambda (#{tmp 3270}#
                                            #{tmp 3269}#
                                            #{tmp 3268}#)
                                     (cons #{tmp 3268}#
                                           (cons #{tmp 3269}# #{tmp 3270}#)))
                                   #{e2* 3266}#
                                   #{e1* 3265}#
                                   #{args* 3264}#))))
                    (lambda (#{meta 3272}# #{lcase 3273}#)
                      (#{build-case-lambda 287}#
                        #{s 3246}#
                        #{meta 3272}#
                        #{lcase 3273}#))))
                #{tmp 3254}#)
              (let ((#{_ 3277}# #{tmp 3253}#))
                (syntax-violation
                  'case-lambda
                  "bad case-lambda"
                  #{e 3243}#)))))))
    (#{global-extend 339}#
      'core
      'case-lambda*
      (lambda (#{e 3278}#
               #{r 3279}#
               #{w 3280}#
               #{s 3281}#
               #{mod 3282}#)
        (let ((#{tmp 3288}# #{e 3278}#))
          (let ((#{tmp 3289}#
                  ($sc-dispatch
                    #{tmp 3288}#
                    '(_ (any any . each-any)
                        .
                        #(each (any any . each-any))))))
            (if #{tmp 3289}#
              (@apply
                (lambda (#{args 3296}#
                         #{e1 3297}#
                         #{e2 3298}#
                         #{args* 3299}#
                         #{e1* 3300}#
                         #{e2* 3301}#)
                  (call-with-values
                    (lambda ()
                      (#{chi-lambda-case 447}#
                        #{e 3278}#
                        #{r 3279}#
                        #{w 3280}#
                        #{s 3281}#
                        #{mod 3282}#
                        #{lambda*-formals 445}#
                        (cons (cons #{args 3296}#
                                    (cons #{e1 3297}# #{e2 3298}#))
                              (map (lambda (#{tmp 3305}#
                                            #{tmp 3304}#
                                            #{tmp 3303}#)
                                     (cons #{tmp 3303}#
                                           (cons #{tmp 3304}# #{tmp 3305}#)))
                                   #{e2* 3301}#
                                   #{e1* 3300}#
                                   #{args* 3299}#))))
                    (lambda (#{meta 3307}# #{lcase 3308}#)
                      (#{build-case-lambda 287}#
                        #{s 3281}#
                        #{meta 3307}#
                        #{lcase 3308}#))))
                #{tmp 3289}#)
              (let ((#{_ 3312}# #{tmp 3288}#))
                (syntax-violation
                  'case-lambda
                  "bad case-lambda*"
                  #{e 3278}#)))))))
    (#{global-extend 339}#
      'core
      'let
      (letrec*
        ((#{chi-let 3314}#
           (lambda (#{e 3315}#
                    #{r 3316}#
                    #{w 3317}#
                    #{s 3318}#
                    #{mod 3319}#
                    #{constructor 3320}#
                    #{ids 3321}#
                    #{vals 3322}#
                    #{exps 3323}#)
             (if (not (#{valid-bound-ids? 403}# #{ids 3321}#))
               (syntax-violation
                 'let
                 "duplicate bound variable"
                 #{e 3315}#)
               (let ((#{labels 3335}#
                       (#{gen-labels 358}# #{ids 3321}#))
                     (#{new-vars 3336}#
                       (map #{gen-var 451}# #{ids 3321}#)))
                 (let ((#{nw 3339}#
                         (#{make-binding-wrap 387}#
                           #{ids 3321}#
                           #{labels 3335}#
                           #{w 3317}#))
                       (#{nr 3340}#
                         (#{extend-var-env 333}#
                           #{labels 3335}#
                           #{new-vars 3336}#
                           #{r 3316}#)))
                   (#{constructor 3320}#
                     #{s 3318}#
                     (map syntax->datum #{ids 3321}#)
                     #{new-vars 3336}#
                     (map (lambda (#{x 3341}#)
                            (#{chi 423}#
                              #{x 3341}#
                              #{r 3316}#
                              #{w 3317}#
                              #{mod 3319}#))
                          #{vals 3322}#)
                     (#{chi-body 431}#
                       #{exps 3323}#
                       (#{source-wrap 411}#
                         #{e 3315}#
                         #{nw 3339}#
                         #{s 3318}#
                         #{mod 3319}#)
                       #{nr 3340}#
                       #{nw 3339}#
                       #{mod 3319}#))))))))
        (lambda (#{e 3343}#
                 #{r 3344}#
                 #{w 3345}#
                 #{s 3346}#
                 #{mod 3347}#)
          (let ((#{tmp 3353}# #{e 3343}#))
            (let ((#{tmp 3354}#
                    ($sc-dispatch
                      #{tmp 3353}#
                      '(_ #(each (any any)) any . each-any))))
              (if (if #{tmp 3354}#
                    (@apply
                      (lambda (#{id 3359}#
                               #{val 3360}#
                               #{e1 3361}#
                               #{e2 3362}#)
                        (and-map #{id? 343}# #{id 3359}#))
                      #{tmp 3354}#)
                    #f)
                (@apply
                  (lambda (#{id 3368}#
                           #{val 3369}#
                           #{e1 3370}#
                           #{e2 3371}#)
                    (#{chi-let 3314}#
                      #{e 3343}#
                      #{r 3344}#
                      #{w 3345}#
                      #{s 3346}#
                      #{mod 3347}#
                      #{build-let 299}#
                      #{id 3368}#
                      #{val 3369}#
                      (cons #{e1 3370}# #{e2 3371}#)))
                  #{tmp 3354}#)
                (let ((#{tmp 3375}#
                        ($sc-dispatch
                          #{tmp 3353}#
                          '(_ any #(each (any any)) any . each-any))))
                  (if (if #{tmp 3375}#
                        (@apply
                          (lambda (#{f 3381}#
                                   #{id 3382}#
                                   #{val 3383}#
                                   #{e1 3384}#
                                   #{e2 3385}#)
                            (if (#{id? 343}# #{f 3381}#)
                              (and-map #{id? 343}# #{id 3382}#)
                              #f))
                          #{tmp 3375}#)
                        #f)
                    (@apply
                      (lambda (#{f 3394}#
                               #{id 3395}#
                               #{val 3396}#
                               #{e1 3397}#
                               #{e2 3398}#)
                        (#{chi-let 3314}#
                          #{e 3343}#
                          #{r 3344}#
                          #{w 3345}#
                          #{s 3346}#
                          #{mod 3347}#
                          #{build-named-let 301}#
                          (cons #{f 3394}# #{id 3395}#)
                          #{val 3396}#
                          (cons #{e1 3397}# #{e2 3398}#)))
                      #{tmp 3375}#)
                    (let ((#{_ 3403}# #{tmp 3353}#))
                      (syntax-violation
                        'let
                        "bad let"
                        (#{source-wrap 411}#
                          #{e 3343}#
                          #{w 3345}#
                          #{s 3346}#
                          #{mod 3347}#)))))))))))
    (#{global-extend 339}#
      'core
      'letrec
      (lambda (#{e 3404}#
               #{r 3405}#
               #{w 3406}#
               #{s 3407}#
               #{mod 3408}#)
        (let ((#{tmp 3414}# #{e 3404}#))
          (let ((#{tmp 3415}#
                  ($sc-dispatch
                    #{tmp 3414}#
                    '(_ #(each (any any)) any . each-any))))
            (if (if #{tmp 3415}#
                  (@apply
                    (lambda (#{id 3420}#
                             #{val 3421}#
                             #{e1 3422}#
                             #{e2 3423}#)
                      (and-map #{id? 343}# #{id 3420}#))
                    #{tmp 3415}#)
                  #f)
              (@apply
                (lambda (#{id 3429}#
                         #{val 3430}#
                         #{e1 3431}#
                         #{e2 3432}#)
                  (let ((#{ids 3434}# #{id 3429}#))
                    (if (not (#{valid-bound-ids? 403}# #{ids 3434}#))
                      (syntax-violation
                        'letrec
                        "duplicate bound variable"
                        #{e 3404}#)
                      (let ((#{labels 3438}#
                              (#{gen-labels 358}# #{ids 3434}#))
                            (#{new-vars 3439}#
                              (map #{gen-var 451}# #{ids 3434}#)))
                        (let ((#{w 3442}#
                                (#{make-binding-wrap 387}#
                                  #{ids 3434}#
                                  #{labels 3438}#
                                  #{w 3406}#))
                              (#{r 3443}#
                                (#{extend-var-env 333}#
                                  #{labels 3438}#
                                  #{new-vars 3439}#
                                  #{r 3405}#)))
                          (#{build-letrec 303}#
                            #{s 3407}#
                            #f
                            (map syntax->datum #{ids 3434}#)
                            #{new-vars 3439}#
                            (map (lambda (#{x 3444}#)
                                   (#{chi 423}#
                                     #{x 3444}#
                                     #{r 3443}#
                                     #{w 3442}#
                                     #{mod 3408}#))
                                 #{val 3430}#)
                            (#{chi-body 431}#
                              (cons #{e1 3431}# #{e2 3432}#)
                              (#{source-wrap 411}#
                                #{e 3404}#
                                #{w 3442}#
                                #{s 3407}#
                                #{mod 3408}#)
                              #{r 3443}#
                              #{w 3442}#
                              #{mod 3408}#)))))))
                #{tmp 3415}#)
              (let ((#{_ 3449}# #{tmp 3414}#))
                (syntax-violation
                  'letrec
                  "bad letrec"
                  (#{source-wrap 411}#
                    #{e 3404}#
                    #{w 3406}#
                    #{s 3407}#
                    #{mod 3408}#))))))))
    (#{global-extend 339}#
      'core
      'letrec*
      (lambda (#{e 3450}#
               #{r 3451}#
               #{w 3452}#
               #{s 3453}#
               #{mod 3454}#)
        (let ((#{tmp 3460}# #{e 3450}#))
          (let ((#{tmp 3461}#
                  ($sc-dispatch
                    #{tmp 3460}#
                    '(_ #(each (any any)) any . each-any))))
            (if (if #{tmp 3461}#
                  (@apply
                    (lambda (#{id 3466}#
                             #{val 3467}#
                             #{e1 3468}#
                             #{e2 3469}#)
                      (and-map #{id? 343}# #{id 3466}#))
                    #{tmp 3461}#)
                  #f)
              (@apply
                (lambda (#{id 3475}#
                         #{val 3476}#
                         #{e1 3477}#
                         #{e2 3478}#)
                  (let ((#{ids 3480}# #{id 3475}#))
                    (if (not (#{valid-bound-ids? 403}# #{ids 3480}#))
                      (syntax-violation
                        'letrec*
                        "duplicate bound variable"
                        #{e 3450}#)
                      (let ((#{labels 3484}#
                              (#{gen-labels 358}# #{ids 3480}#))
                            (#{new-vars 3485}#
                              (map #{gen-var 451}# #{ids 3480}#)))
                        (let ((#{w 3488}#
                                (#{make-binding-wrap 387}#
                                  #{ids 3480}#
                                  #{labels 3484}#
                                  #{w 3452}#))
                              (#{r 3489}#
                                (#{extend-var-env 333}#
                                  #{labels 3484}#
                                  #{new-vars 3485}#
                                  #{r 3451}#)))
                          (#{build-letrec 303}#
                            #{s 3453}#
                            #t
                            (map syntax->datum #{ids 3480}#)
                            #{new-vars 3485}#
                            (map (lambda (#{x 3490}#)
                                   (#{chi 423}#
                                     #{x 3490}#
                                     #{r 3489}#
                                     #{w 3488}#
                                     #{mod 3454}#))
                                 #{val 3476}#)
                            (#{chi-body 431}#
                              (cons #{e1 3477}# #{e2 3478}#)
                              (#{source-wrap 411}#
                                #{e 3450}#
                                #{w 3488}#
                                #{s 3453}#
                                #{mod 3454}#)
                              #{r 3489}#
                              #{w 3488}#
                              #{mod 3454}#)))))))
                #{tmp 3461}#)
              (let ((#{_ 3495}# #{tmp 3460}#))
                (syntax-violation
                  'letrec*
                  "bad letrec*"
                  (#{source-wrap 411}#
                    #{e 3450}#
                    #{w 3452}#
                    #{s 3453}#
                    #{mod 3454}#))))))))
    (#{global-extend 339}#
      'core
      'set!
      (lambda (#{e 3496}#
               #{r 3497}#
               #{w 3498}#
               #{s 3499}#
               #{mod 3500}#)
        (let ((#{tmp 3506}# #{e 3496}#))
          (let ((#{tmp 3507}#
                  ($sc-dispatch #{tmp 3506}# '(_ any any))))
            (if (if #{tmp 3507}#
                  (@apply
                    (lambda (#{id 3510}# #{val 3511}#)
                      (#{id? 343}# #{id 3510}#))
                    #{tmp 3507}#)
                  #f)
              (@apply
                (lambda (#{id 3514}# #{val 3515}#)
                  (let ((#{n 3518}#
                          (#{id-var-name 397}# #{id 3514}# #{w 3498}#))
                        (#{id-mod 3519}#
                          (if (#{syntax-object? 309}# #{id 3514}#)
                            (#{syntax-object-module 315}# #{id 3514}#)
                            #{mod 3500}#)))
                    (let ((#{b 3521}#
                            (#{lookup 337}#
                              #{n 3518}#
                              #{r 3497}#
                              #{id-mod 3519}#)))
                      (let ((#{atom-key 3524}# (car #{b 3521}#)))
                        (if (memv #{atom-key 3524}# '(lexical))
                          (#{build-lexical-assignment 275}#
                            #{s 3499}#
                            (syntax->datum #{id 3514}#)
                            (cdr #{b 3521}#)
                            (#{chi 423}#
                              #{val 3515}#
                              #{r 3497}#
                              #{w 3498}#
                              #{mod 3500}#))
                          (if (memv #{atom-key 3524}# '(global))
                            (#{build-global-assignment 281}#
                              #{s 3499}#
                              #{n 3518}#
                              (#{chi 423}#
                                #{val 3515}#
                                #{r 3497}#
                                #{w 3498}#
                                #{mod 3500}#)
                              #{id-mod 3519}#)
                            (if (memv #{atom-key 3524}# '(macro))
                              (let ((#{p 3531}# (cdr #{b 3521}#)))
                                (if (procedure-property
                                      #{p 3531}#
                                      'variable-transformer)
                                  (#{chi 423}#
                                    (#{chi-macro 429}#
                                      #{p 3531}#
                                      #{e 3496}#
                                      #{r 3497}#
                                      #{w 3498}#
                                      #{s 3499}#
                                      #f
                                      #{mod 3500}#)
                                    #{r 3497}#
                                    '(())
                                    #{mod 3500}#)
                                  (syntax-violation
                                    'set!
                                    "not a variable transformer"
                                    (#{wrap 409}#
                                      #{e 3496}#
                                      #{w 3498}#
                                      #{mod 3500}#)
                                    (#{wrap 409}#
                                      #{id 3514}#
                                      #{w 3498}#
                                      #{id-mod 3519}#))))
                              (if (memv #{atom-key 3524}# '(displaced-lexical))
                                (syntax-violation
                                  'set!
                                  "identifier out of context"
                                  (#{wrap 409}#
                                    #{id 3514}#
                                    #{w 3498}#
                                    #{mod 3500}#))
                                (syntax-violation
                                  'set!
                                  "bad set!"
                                  (#{source-wrap 411}#
                                    #{e 3496}#
                                    #{w 3498}#
                                    #{s 3499}#
                                    #{mod 3500}#))))))))))
                #{tmp 3507}#)
              (let ((#{tmp 3536}#
                      ($sc-dispatch
                        #{tmp 3506}#
                        '(_ (any . each-any) any))))
                (if #{tmp 3536}#
                  (@apply
                    (lambda (#{head 3540}# #{tail 3541}# #{val 3542}#)
                      (call-with-values
                        (lambda ()
                          (#{syntax-type 421}#
                            #{head 3540}#
                            #{r 3497}#
                            '(())
                            #f
                            #f
                            #{mod 3500}#
                            #t))
                        (lambda (#{type 3545}#
                                 #{value 3546}#
                                 #{ee 3547}#
                                 #{ww 3548}#
                                 #{ss 3549}#
                                 #{modmod 3550}#)
                          (if (memv #{type 3545}# '(module-ref))
                            (let ((#{val 3559}#
                                    (#{chi 423}#
                                      #{val 3542}#
                                      #{r 3497}#
                                      #{w 3498}#
                                      #{mod 3500}#)))
                              (call-with-values
                                (lambda ()
                                  (#{value 3546}#
                                    (cons #{head 3540}# #{tail 3541}#)
                                    #{r 3497}#
                                    #{w 3498}#))
                                (lambda (#{e 3561}#
                                         #{r 3562}#
                                         #{w 3563}#
                                         #{s* 3564}#
                                         #{mod 3565}#)
                                  (let ((#{tmp 3571}# #{e 3561}#))
                                    (let ((#{tmp 3572}# (list #{tmp 3571}#)))
                                      (if (if #{tmp 3572}#
                                            (@apply
                                              (lambda (#{e 3574}#)
                                                (#{id? 343}# #{e 3574}#))
                                              #{tmp 3572}#)
                                            #f)
                                        (@apply
                                          (lambda (#{e 3576}#)
                                            (#{build-global-assignment 281}#
                                              #{s 3499}#
                                              (syntax->datum #{e 3576}#)
                                              #{val 3559}#
                                              #{mod 3565}#))
                                          #{tmp 3572}#)
                                        (syntax-violation
                                          #f
                                          "source expression failed to match any pattern"
                                          #{tmp 3571}#)))))))
                            (#{build-call 267}#
                              #{s 3499}#
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
                                            #("i3551"
                                              "i3552"
                                              "i3553"
                                              "i3554"
                                              "i3555"
                                              "i3556"))
                                          #(ribcage
                                            #(head tail val)
                                            #((top) (top) (top))
                                            #("i3537" "i3538" "i3539"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(e r w s mod)
                                            #((top) (top) (top) (top) (top))
                                            #("i3501"
                                              "i3502"
                                              "i3503"
                                              "i3504"
                                              "i3505"))
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
                                      #{head 3540}#)
                                #{r 3497}#
                                #{w 3498}#
                                #{mod 3500}#)
                              (map (lambda (#{e 3578}#)
                                     (#{chi 423}#
                                       #{e 3578}#
                                       #{r 3497}#
                                       #{w 3498}#
                                       #{mod 3500}#))
                                   (append
                                     #{tail 3541}#
                                     (list #{val 3542}#))))))))
                    #{tmp 3536}#)
                  (let ((#{_ 3582}# #{tmp 3506}#))
                    (syntax-violation
                      'set!
                      "bad set!"
                      (#{source-wrap 411}#
                        #{e 3496}#
                        #{w 3498}#
                        #{s 3499}#
                        #{mod 3500}#))))))))))
    (#{global-extend 339}#
      'module-ref
      '@
      (lambda (#{e 3583}# #{r 3584}# #{w 3585}#)
        (let ((#{tmp 3589}# #{e 3583}#))
          (let ((#{tmp 3590}#
                  ($sc-dispatch #{tmp 3589}# '(_ each-any any))))
            (if (if #{tmp 3590}#
                  (@apply
                    (lambda (#{mod 3593}# #{id 3594}#)
                      (if (and-map #{id? 343}# #{mod 3593}#)
                        (#{id? 343}# #{id 3594}#)
                        #f))
                    #{tmp 3590}#)
                  #f)
              (@apply
                (lambda (#{mod 3600}# #{id 3601}#)
                  (values
                    (syntax->datum #{id 3601}#)
                    #{r 3584}#
                    #{w 3585}#
                    #f
                    (syntax->datum
                      (cons '#(syntax-object
                               public
                               ((top)
                                #(ribcage
                                  #(mod id)
                                  #((top) (top))
                                  #("i3598" "i3599"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(e r w)
                                  #((top) (top) (top))
                                  #("i3586" "i3587" "i3588"))
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
                            #{mod 3600}#))))
                #{tmp 3590}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 3589}#))))))
    (#{global-extend 339}#
      'module-ref
      '@@
      (lambda (#{e 3603}# #{r 3604}# #{w 3605}#)
        (letrec*
          ((#{remodulate 3610}#
             (lambda (#{x 3611}# #{mod 3612}#)
               (if (pair? #{x 3611}#)
                 (cons (#{remodulate 3610}#
                         (car #{x 3611}#)
                         #{mod 3612}#)
                       (#{remodulate 3610}#
                         (cdr #{x 3611}#)
                         #{mod 3612}#))
                 (if (#{syntax-object? 309}# #{x 3611}#)
                   (#{make-syntax-object 307}#
                     (#{remodulate 3610}#
                       (#{syntax-object-expression 311}# #{x 3611}#)
                       #{mod 3612}#)
                     (#{syntax-object-wrap 313}# #{x 3611}#)
                     #{mod 3612}#)
                   (if (vector? #{x 3611}#)
                     (let ((#{n 3623}# (vector-length #{x 3611}#)))
                       (let ((#{v 3625}# (make-vector #{n 3623}#)))
                         (letrec*
                           ((#{loop 3628}#
                              (lambda (#{i 3629}#)
                                (if (= #{i 3629}# #{n 3623}#)
                                  (begin (if #f #f) #{v 3625}#)
                                  (begin
                                    (vector-set!
                                      #{v 3625}#
                                      #{i 3629}#
                                      (#{remodulate 3610}#
                                        (vector-ref #{x 3611}# #{i 3629}#)
                                        #{mod 3612}#))
                                    (#{loop 3628}# (#{1+}# #{i 3629}#)))))))
                           (#{loop 3628}# 0))))
                     #{x 3611}#))))))
          (let ((#{tmp 3635}# #{e 3603}#))
            (let ((#{tmp 3636}#
                    ($sc-dispatch #{tmp 3635}# '(_ each-any any))))
              (if (if #{tmp 3636}#
                    (@apply
                      (lambda (#{mod 3639}# #{exp 3640}#)
                        (and-map #{id? 343}# #{mod 3639}#))
                      #{tmp 3636}#)
                    #f)
                (@apply
                  (lambda (#{mod 3644}# #{exp 3645}#)
                    (let ((#{mod 3647}#
                            (syntax->datum
                              (cons '#(syntax-object
                                       private
                                       ((top)
                                        #(ribcage
                                          #(mod exp)
                                          #((top) (top))
                                          #("i3642" "i3643"))
                                        #(ribcage
                                          (remodulate)
                                          ((top))
                                          ("i3609"))
                                        #(ribcage
                                          #(e r w)
                                          #((top) (top) (top))
                                          #("i3606" "i3607" "i3608"))
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
                                    #{mod 3644}#))))
                      (values
                        (#{remodulate 3610}# #{exp 3645}# #{mod 3647}#)
                        #{r 3604}#
                        #{w 3605}#
                        (#{source-annotation 324}# #{exp 3645}#)
                        #{mod 3647}#)))
                  #{tmp 3636}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 3635}#)))))))
    (#{global-extend 339}#
      'core
      'if
      (lambda (#{e 3649}#
               #{r 3650}#
               #{w 3651}#
               #{s 3652}#
               #{mod 3653}#)
        (let ((#{tmp 3659}# #{e 3649}#))
          (let ((#{tmp 3660}#
                  ($sc-dispatch #{tmp 3659}# '(_ any any))))
            (if #{tmp 3660}#
              (@apply
                (lambda (#{test 3663}# #{then 3664}#)
                  (#{build-conditional 269}#
                    #{s 3652}#
                    (#{chi 423}#
                      #{test 3663}#
                      #{r 3650}#
                      #{w 3651}#
                      #{mod 3653}#)
                    (#{chi 423}#
                      #{then 3664}#
                      #{r 3650}#
                      #{w 3651}#
                      #{mod 3653}#)
                    (#{build-void 265}# #f)))
                #{tmp 3660}#)
              (let ((#{tmp 3666}#
                      ($sc-dispatch #{tmp 3659}# '(_ any any any))))
                (if #{tmp 3666}#
                  (@apply
                    (lambda (#{test 3670}# #{then 3671}# #{else 3672}#)
                      (#{build-conditional 269}#
                        #{s 3652}#
                        (#{chi 423}#
                          #{test 3670}#
                          #{r 3650}#
                          #{w 3651}#
                          #{mod 3653}#)
                        (#{chi 423}#
                          #{then 3671}#
                          #{r 3650}#
                          #{w 3651}#
                          #{mod 3653}#)
                        (#{chi 423}#
                          #{else 3672}#
                          #{r 3650}#
                          #{w 3651}#
                          #{mod 3653}#)))
                    #{tmp 3666}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp 3659}#))))))))
    (#{global-extend 339}#
      'core
      'with-fluids
      (lambda (#{e 3673}#
               #{r 3674}#
               #{w 3675}#
               #{s 3676}#
               #{mod 3677}#)
        (let ((#{tmp 3683}# #{e 3673}#))
          (let ((#{tmp 3684}#
                  ($sc-dispatch
                    #{tmp 3683}#
                    '(_ #(each (any any)) any . each-any))))
            (if #{tmp 3684}#
              (@apply
                (lambda (#{fluid 3689}#
                         #{val 3690}#
                         #{b 3691}#
                         #{b* 3692}#)
                  (#{build-dynlet 271}#
                    #{s 3676}#
                    (map (lambda (#{x 3693}#)
                           (#{chi 423}#
                             #{x 3693}#
                             #{r 3674}#
                             #{w 3675}#
                             #{mod 3677}#))
                         #{fluid 3689}#)
                    (map (lambda (#{x 3696}#)
                           (#{chi 423}#
                             #{x 3696}#
                             #{r 3674}#
                             #{w 3675}#
                             #{mod 3677}#))
                         #{val 3690}#)
                    (#{chi-body 431}#
                      (cons #{b 3691}# #{b* 3692}#)
                      (#{source-wrap 411}#
                        #{e 3673}#
                        #{w 3675}#
                        #{s 3676}#
                        #{mod 3677}#)
                      #{r 3674}#
                      #{w 3675}#
                      #{mod 3677}#)))
                #{tmp 3684}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 3683}#))))))
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
        ((#{convert-pattern 3701}#
           (lambda (#{pattern 3708}# #{keys 3709}#)
             (letrec*
               ((#{cvt* 3713}#
                  (lambda (#{p* 3718}# #{n 3719}# #{ids 3720}#)
                    (if (not (pair? #{p* 3718}#))
                      (#{cvt 3717}#
                        #{p* 3718}#
                        #{n 3719}#
                        #{ids 3720}#)
                      (call-with-values
                        (lambda ()
                          (#{cvt* 3713}#
                            (cdr #{p* 3718}#)
                            #{n 3719}#
                            #{ids 3720}#))
                        (lambda (#{y 3724}# #{ids 3725}#)
                          (call-with-values
                            (lambda ()
                              (#{cvt 3717}#
                                (car #{p* 3718}#)
                                #{n 3719}#
                                #{ids 3725}#))
                            (lambda (#{x 3728}# #{ids 3729}#)
                              (values
                                (cons #{x 3728}# #{y 3724}#)
                                #{ids 3729}#))))))))
                (#{v-reverse 3715}#
                  (lambda (#{x 3732}#)
                    (letrec*
                      ((#{loop 3737}#
                         (lambda (#{r 3738}# #{x 3739}#)
                           (if (not (pair? #{x 3739}#))
                             (values #{r 3738}# #{x 3739}#)
                             (#{loop 3737}#
                               (cons (car #{x 3739}#) #{r 3738}#)
                               (cdr #{x 3739}#))))))
                      (#{loop 3737}# '() #{x 3732}#))))
                (#{cvt 3717}#
                  (lambda (#{p 3740}# #{n 3741}# #{ids 3742}#)
                    (if (#{id? 343}# #{p 3740}#)
                      (if (#{bound-id-member? 407}#
                            #{p 3740}#
                            #{keys 3709}#)
                        (values
                          (vector 'free-id #{p 3740}#)
                          #{ids 3742}#)
                        (if (#{free-id=? 399}#
                              #{p 3740}#
                              '#(syntax-object
                                 _
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage
                                    #(p n ids)
                                    #((top) (top) (top))
                                    #("i3743" "i3744" "i3745"))
                                  #(ribcage
                                    (cvt v-reverse cvt*)
                                    ((top) (top) (top))
                                    ("i3716" "i3714" "i3712"))
                                  #(ribcage
                                    #(pattern keys)
                                    #((top) (top))
                                    #("i3710" "i3711"))
                                  #(ribcage
                                    (gen-syntax-case
                                      gen-clause
                                      build-dispatch-call
                                      convert-pattern)
                                    ((top) (top) (top) (top))
                                    ("i3706" "i3704" "i3702" "i3700"))
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
                          (values '_ #{ids 3742}#)
                          (values
                            'any
                            (cons (cons #{p 3740}# #{n 3741}#) #{ids 3742}#))))
                      (let ((#{tmp 3751}# #{p 3740}#))
                        (let ((#{tmp 3752}#
                                ($sc-dispatch #{tmp 3751}# '(any any))))
                          (if (if #{tmp 3752}#
                                (@apply
                                  (lambda (#{x 3755}# #{dots 3756}#)
                                    (#{ellipsis? 439}# #{dots 3756}#))
                                  #{tmp 3752}#)
                                #f)
                            (@apply
                              (lambda (#{x 3759}# #{dots 3760}#)
                                (call-with-values
                                  (lambda ()
                                    (#{cvt 3717}#
                                      #{x 3759}#
                                      (#{1+}# #{n 3741}#)
                                      #{ids 3742}#))
                                  (lambda (#{p 3762}# #{ids 3763}#)
                                    (values
                                      (if (eq? #{p 3762}# 'any)
                                        'each-any
                                        (vector 'each #{p 3762}#))
                                      #{ids 3763}#))))
                              #{tmp 3752}#)
                            (let ((#{tmp 3766}#
                                    ($sc-dispatch
                                      #{tmp 3751}#
                                      '(any any . any))))
                              (if (if #{tmp 3766}#
                                    (@apply
                                      (lambda (#{x 3770}#
                                               #{dots 3771}#
                                               #{ys 3772}#)
                                        (#{ellipsis? 439}# #{dots 3771}#))
                                      #{tmp 3766}#)
                                    #f)
                                (@apply
                                  (lambda (#{x 3776}#
                                           #{dots 3777}#
                                           #{ys 3778}#)
                                    (call-with-values
                                      (lambda ()
                                        (#{cvt* 3713}#
                                          #{ys 3778}#
                                          #{n 3741}#
                                          #{ids 3742}#))
                                      (lambda (#{ys 3779}# #{ids 3780}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt 3717}#
                                              #{x 3776}#
                                              (#{1+}# #{n 3741}#)
                                              #{ids 3780}#))
                                          (lambda (#{x 3783}# #{ids 3784}#)
                                            (call-with-values
                                              (lambda ()
                                                (#{v-reverse 3715}#
                                                  #{ys 3779}#))
                                              (lambda (#{ys 3787}# #{e 3788}#)
                                                (values
                                                  (vector
                                                    'each+
                                                    #{x 3783}#
                                                    #{ys 3787}#
                                                    #{e 3788}#)
                                                  #{ids 3784}#))))))))
                                  #{tmp 3766}#)
                                (let ((#{tmp 3792}#
                                        ($sc-dispatch
                                          #{tmp 3751}#
                                          '(any . any))))
                                  (if #{tmp 3792}#
                                    (@apply
                                      (lambda (#{x 3795}# #{y 3796}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt 3717}#
                                              #{y 3796}#
                                              #{n 3741}#
                                              #{ids 3742}#))
                                          (lambda (#{y 3797}# #{ids 3798}#)
                                            (call-with-values
                                              (lambda ()
                                                (#{cvt 3717}#
                                                  #{x 3795}#
                                                  #{n 3741}#
                                                  #{ids 3798}#))
                                              (lambda (#{x 3801}# #{ids 3802}#)
                                                (values
                                                  (cons #{x 3801}# #{y 3797}#)
                                                  #{ids 3802}#))))))
                                      #{tmp 3792}#)
                                    (let ((#{tmp 3805}#
                                            ($sc-dispatch #{tmp 3751}# '())))
                                      (if #{tmp 3805}#
                                        (@apply
                                          (lambda () (values '() #{ids 3742}#))
                                          #{tmp 3805}#)
                                        (let ((#{tmp 3806}#
                                                ($sc-dispatch
                                                  #{tmp 3751}#
                                                  '#(vector each-any))))
                                          (if #{tmp 3806}#
                                            (@apply
                                              (lambda (#{x 3808}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt 3717}#
                                                      #{x 3808}#
                                                      #{n 3741}#
                                                      #{ids 3742}#))
                                                  (lambda (#{p 3810}#
                                                           #{ids 3811}#)
                                                    (values
                                                      (vector
                                                        'vector
                                                        #{p 3810}#)
                                                      #{ids 3811}#))))
                                              #{tmp 3806}#)
                                            (let ((#{x 3815}# #{tmp 3751}#))
                                              (values
                                                (vector
                                                  'atom
                                                  (#{strip 449}#
                                                    #{p 3740}#
                                                    '(())))
                                                #{ids 3742}#)))))))))))))))))
               (#{cvt 3717}# #{pattern 3708}# 0 '()))))
         (#{build-dispatch-call 3703}#
           (lambda (#{pvars 3817}#
                    #{exp 3818}#
                    #{y 3819}#
                    #{r 3820}#
                    #{mod 3821}#)
             (begin
               (map cdr #{pvars 3817}#)
               (let ((#{ids 3829}# (map car #{pvars 3817}#)))
                 (let ((#{labels 3833}#
                         (#{gen-labels 358}# #{ids 3829}#))
                       (#{new-vars 3834}#
                         (map #{gen-var 451}# #{ids 3829}#)))
                   (#{build-primcall 291}#
                     #f
                     'apply
                     (list (#{build-simple-lambda 285}#
                             #f
                             (map syntax->datum #{ids 3829}#)
                             #f
                             #{new-vars 3834}#
                             '()
                             (#{chi 423}#
                               #{exp 3818}#
                               (#{extend-env 331}#
                                 #{labels 3833}#
                                 (map (lambda (#{var 3837}# #{level 3838}#)
                                        (cons 'syntax
                                              (cons #{var 3837}#
                                                    #{level 3838}#)))
                                      #{new-vars 3834}#
                                      (map cdr #{pvars 3817}#))
                                 #{r 3820}#)
                               (#{make-binding-wrap 387}#
                                 #{ids 3829}#
                                 #{labels 3833}#
                                 '(()))
                               #{mod 3821}#))
                           #{y 3819}#)))))))
         (#{gen-clause 3705}#
           (lambda (#{x 3844}#
                    #{keys 3845}#
                    #{clauses 3846}#
                    #{r 3847}#
                    #{pat 3848}#
                    #{fender 3849}#
                    #{exp 3850}#
                    #{mod 3851}#)
             (call-with-values
               (lambda ()
                 (#{convert-pattern 3701}#
                   #{pat 3848}#
                   #{keys 3845}#))
               (lambda (#{p 3860}# #{pvars 3861}#)
                 (if (not (#{distinct-bound-ids? 405}#
                            (map car #{pvars 3861}#)))
                   (syntax-violation
                     'syntax-case
                     "duplicate pattern variable"
                     #{pat 3848}#)
                   (if (not (and-map
                              (lambda (#{x 3868}#)
                                (not (#{ellipsis? 439}# (car #{x 3868}#))))
                              #{pvars 3861}#))
                     (syntax-violation
                       'syntax-case
                       "misplaced ellipsis"
                       #{pat 3848}#)
                     (let ((#{y 3872}# (#{gen-var 451}# 'tmp)))
                       (#{build-call 267}#
                         #f
                         (#{build-simple-lambda 285}#
                           #f
                           (list 'tmp)
                           #f
                           (list #{y 3872}#)
                           '()
                           (let ((#{y 3876}#
                                   (#{build-lexical-reference 273}#
                                     'value
                                     #f
                                     'tmp
                                     #{y 3872}#)))
                             (#{build-conditional 269}#
                               #f
                               (let ((#{tmp 3879}# #{fender 3849}#))
                                 (let ((#{tmp 3880}#
                                         ($sc-dispatch
                                           #{tmp 3879}#
                                           '#(atom #t))))
                                   (if #{tmp 3880}#
                                     (@apply
                                       (lambda () #{y 3876}#)
                                       #{tmp 3880}#)
                                     (let ((#{_ 3882}# #{tmp 3879}#))
                                       (#{build-conditional 269}#
                                         #f
                                         #{y 3876}#
                                         (#{build-dispatch-call 3703}#
                                           #{pvars 3861}#
                                           #{fender 3849}#
                                           #{y 3876}#
                                           #{r 3847}#
                                           #{mod 3851}#)
                                         (#{build-data 295}# #f #f))))))
                               (#{build-dispatch-call 3703}#
                                 #{pvars 3861}#
                                 #{exp 3850}#
                                 #{y 3876}#
                                 #{r 3847}#
                                 #{mod 3851}#)
                               (#{gen-syntax-case 3707}#
                                 #{x 3844}#
                                 #{keys 3845}#
                                 #{clauses 3846}#
                                 #{r 3847}#
                                 #{mod 3851}#))))
                         (list (if (eq? #{p 3860}# 'any)
                                 (#{build-primcall 291}#
                                   #f
                                   'list
                                   (list #{x 3844}#))
                                 (#{build-primcall 291}#
                                   #f
                                   '$sc-dispatch
                                   (list #{x 3844}#
                                         (#{build-data 295}#
                                           #f
                                           #{p 3860}#)))))))))))))
         (#{gen-syntax-case 3707}#
           (lambda (#{x 3888}#
                    #{keys 3889}#
                    #{clauses 3890}#
                    #{r 3891}#
                    #{mod 3892}#)
             (if (null? #{clauses 3890}#)
               (#{build-primcall 291}#
                 #f
                 'syntax-violation
                 (list (#{build-data 295}# #f #f)
                       (#{build-data 295}#
                         #f
                         "source expression failed to match any pattern")
                       #{x 3888}#))
               (let ((#{tmp 3901}# (car #{clauses 3890}#)))
                 (let ((#{tmp 3902}#
                         ($sc-dispatch #{tmp 3901}# '(any any))))
                   (if #{tmp 3902}#
                     (@apply
                       (lambda (#{pat 3905}# #{exp 3906}#)
                         (if (if (#{id? 343}# #{pat 3905}#)
                               (and-map
                                 (lambda (#{x 3909}#)
                                   (not (#{free-id=? 399}#
                                          #{pat 3905}#
                                          #{x 3909}#)))
                                 (cons '#(syntax-object
                                          ...
                                          ((top)
                                           #(ribcage
                                             #(pat exp)
                                             #((top) (top))
                                             #("i3903" "i3904"))
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(x keys clauses r mod)
                                             #((top) (top) (top) (top) (top))
                                             #("i3893"
                                               "i3894"
                                               "i3895"
                                               "i3896"
                                               "i3897"))
                                           #(ribcage
                                             (gen-syntax-case
                                               gen-clause
                                               build-dispatch-call
                                               convert-pattern)
                                             ((top) (top) (top) (top))
                                             ("i3706" "i3704" "i3702" "i3700"))
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
                                       #{keys 3889}#))
                               #f)
                           (if (#{free-id=? 399}#
                                 '#(syntax-object
                                    pad
                                    ((top)
                                     #(ribcage
                                       #(pat exp)
                                       #((top) (top))
                                       #("i3903" "i3904"))
                                     #(ribcage () () ())
                                     #(ribcage
                                       #(x keys clauses r mod)
                                       #((top) (top) (top) (top) (top))
                                       #("i3893"
                                         "i3894"
                                         "i3895"
                                         "i3896"
                                         "i3897"))
                                     #(ribcage
                                       (gen-syntax-case
                                         gen-clause
                                         build-dispatch-call
                                         convert-pattern)
                                       ((top) (top) (top) (top))
                                       ("i3706" "i3704" "i3702" "i3700"))
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
                                       #("i3903" "i3904"))
                                     #(ribcage () () ())
                                     #(ribcage
                                       #(x keys clauses r mod)
                                       #((top) (top) (top) (top) (top))
                                       #("i3893"
                                         "i3894"
                                         "i3895"
                                         "i3896"
                                         "i3897"))
                                     #(ribcage
                                       (gen-syntax-case
                                         gen-clause
                                         build-dispatch-call
                                         convert-pattern)
                                       ((top) (top) (top) (top))
                                       ("i3706" "i3704" "i3702" "i3700"))
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
                               #{exp 3906}#
                               #{r 3891}#
                               '(())
                               #{mod 3892}#)
                             (let ((#{labels 3914}# (list (#{gen-label 356}#)))
                                   (#{var 3915}#
                                     (#{gen-var 451}# #{pat 3905}#)))
                               (#{build-call 267}#
                                 #f
                                 (#{build-simple-lambda 285}#
                                   #f
                                   (list (syntax->datum #{pat 3905}#))
                                   #f
                                   (list #{var 3915}#)
                                   '()
                                   (#{chi 423}#
                                     #{exp 3906}#
                                     (#{extend-env 331}#
                                       #{labels 3914}#
                                       (list (cons 'syntax
                                                   (cons #{var 3915}# 0)))
                                       #{r 3891}#)
                                     (#{make-binding-wrap 387}#
                                       (list #{pat 3905}#)
                                       #{labels 3914}#
                                       '(()))
                                     #{mod 3892}#))
                                 (list #{x 3888}#))))
                           (#{gen-clause 3705}#
                             #{x 3888}#
                             #{keys 3889}#
                             (cdr #{clauses 3890}#)
                             #{r 3891}#
                             #{pat 3905}#
                             #t
                             #{exp 3906}#
                             #{mod 3892}#)))
                       #{tmp 3902}#)
                     (let ((#{tmp 3921}#
                             ($sc-dispatch #{tmp 3901}# '(any any any))))
                       (if #{tmp 3921}#
                         (@apply
                           (lambda (#{pat 3925}# #{fender 3926}# #{exp 3927}#)
                             (#{gen-clause 3705}#
                               #{x 3888}#
                               #{keys 3889}#
                               (cdr #{clauses 3890}#)
                               #{r 3891}#
                               #{pat 3925}#
                               #{fender 3926}#
                               #{exp 3927}#
                               #{mod 3892}#))
                           #{tmp 3921}#)
                         (let ((#{_ 3929}# #{tmp 3901}#))
                           (syntax-violation
                             'syntax-case
                             "invalid clause"
                             (car #{clauses 3890}#))))))))))))
        (lambda (#{e 3930}#
                 #{r 3931}#
                 #{w 3932}#
                 #{s 3933}#
                 #{mod 3934}#)
          (let ((#{e 3941}#
                  (#{source-wrap 411}#
                    #{e 3930}#
                    #{w 3932}#
                    #{s 3933}#
                    #{mod 3934}#)))
            (let ((#{tmp 3942}# #{e 3941}#))
              (let ((#{tmp 3943}#
                      ($sc-dispatch
                        #{tmp 3942}#
                        '(_ any each-any . each-any))))
                (if #{tmp 3943}#
                  (@apply
                    (lambda (#{val 3947}# #{key 3948}# #{m 3949}#)
                      (if (and-map
                            (lambda (#{x 3950}#)
                              (if (#{id? 343}# #{x 3950}#)
                                (not (#{ellipsis? 439}# #{x 3950}#))
                                #f))
                            #{key 3948}#)
                        (let ((#{x 3956}# (#{gen-var 451}# 'tmp)))
                          (#{build-call 267}#
                            #{s 3933}#
                            (#{build-simple-lambda 285}#
                              #f
                              (list 'tmp)
                              #f
                              (list #{x 3956}#)
                              '()
                              (#{gen-syntax-case 3707}#
                                (#{build-lexical-reference 273}#
                                  'value
                                  #f
                                  'tmp
                                  #{x 3956}#)
                                #{key 3948}#
                                #{m 3949}#
                                #{r 3931}#
                                #{mod 3934}#))
                            (list (#{chi 423}#
                                    #{val 3947}#
                                    #{r 3931}#
                                    '(())
                                    #{mod 3934}#))))
                        (syntax-violation
                          'syntax-case
                          "invalid literals list"
                          #{e 3941}#)))
                    #{tmp 3943}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp 3942}#))))))))
    (set! macroexpand
      (lambda*
        (#{x 3962}#
          #:optional
          (#{m 3964}# 'e)
          (#{esew 3966}# '(eval)))
        (#{chi-top-sequence 415}#
          (list #{x 3962}#)
          '()
          '((top))
          #f
          #{m 3964}#
          #{esew 3966}#
          (cons 'hygiene (module-name (current-module))))))
    (set! identifier?
      (lambda (#{x 3970}#)
        (#{nonsymbol-id? 341}# #{x 3970}#)))
    (set! datum->syntax
      (lambda (#{id 3972}# #{datum 3973}#)
        (#{make-syntax-object 307}#
          #{datum 3973}#
          (#{syntax-object-wrap 313}# #{id 3972}#)
          (#{syntax-object-module 315}# #{id 3972}#))))
    (set! syntax->datum
      (lambda (#{x 3976}#)
        (#{strip 449}# #{x 3976}# '(()))))
    (set! syntax-source
      (lambda (#{x 3979}#)
        (#{source-annotation 324}# #{x 3979}#)))
    (set! generate-temporaries
      (lambda (#{ls 3981}#)
        (begin
          (let ((#{x 3985}# #{ls 3981}#))
            (if (not (list? #{x 3985}#))
              (syntax-violation
                'generate-temporaries
                "invalid argument"
                #{x 3985}#)))
          (let ((#{mod 3987}#
                  (cons 'hygiene (module-name (current-module)))))
            (map (lambda (#{x 3988}#)
                   (#{wrap 409}# (gensym) '((top)) #{mod 3987}#))
                 #{ls 3981}#)))))
    (set! free-identifier=?
      (lambda (#{x 3992}# #{y 3993}#)
        (begin
          (let ((#{x 3998}# #{x 3992}#))
            (if (not (#{nonsymbol-id? 341}# #{x 3998}#))
              (syntax-violation
                'free-identifier=?
                "invalid argument"
                #{x 3998}#)))
          (let ((#{x 4001}# #{y 3993}#))
            (if (not (#{nonsymbol-id? 341}# #{x 4001}#))
              (syntax-violation
                'free-identifier=?
                "invalid argument"
                #{x 4001}#)))
          (#{free-id=? 399}# #{x 3992}# #{y 3993}#))))
    (set! bound-identifier=?
      (lambda (#{x 4002}# #{y 4003}#)
        (begin
          (let ((#{x 4008}# #{x 4002}#))
            (if (not (#{nonsymbol-id? 341}# #{x 4008}#))
              (syntax-violation
                'bound-identifier=?
                "invalid argument"
                #{x 4008}#)))
          (let ((#{x 4011}# #{y 4003}#))
            (if (not (#{nonsymbol-id? 341}# #{x 4011}#))
              (syntax-violation
                'bound-identifier=?
                "invalid argument"
                #{x 4011}#)))
          (#{bound-id=? 401}# #{x 4002}# #{y 4003}#))))
    (set! syntax-violation
      (lambda*
        (#{who 4012}#
          #{message 4013}#
          #{form 4014}#
          #:optional
          (#{subform 4018}# #f))
        (begin
          (let ((#{x 4022}# #{who 4012}#))
            (if (not (let ((#{x 4023}# #{x 4022}#))
                       (let ((#{t 4027}# (not #{x 4023}#)))
                         (if #{t 4027}#
                           #{t 4027}#
                           (let ((#{t 4030}# (string? #{x 4023}#)))
                             (if #{t 4030}#
                               #{t 4030}#
                               (symbol? #{x 4023}#)))))))
              (syntax-violation
                'syntax-violation
                "invalid argument"
                #{x 4022}#)))
          (let ((#{x 4034}# #{message 4013}#))
            (if (not (string? #{x 4034}#))
              (syntax-violation
                'syntax-violation
                "invalid argument"
                #{x 4034}#)))
          (throw 'syntax-error
                 #{who 4012}#
                 #{message 4013}#
                 (#{source-annotation 324}#
                   (let ((#{t 4037}# #{form 4014}#))
                     (if #{t 4037}# #{t 4037}# #{subform 4018}#)))
                 (#{strip 449}# #{form 4014}# '(()))
                 (if #{subform 4018}#
                   (#{strip 449}# #{subform 4018}# '(()))
                   #f)))))
    (letrec*
      ((#{match-each 4044}#
         (lambda (#{e 4057}# #{p 4058}# #{w 4059}# #{mod 4060}#)
           (if (pair? #{e 4057}#)
             (let ((#{first 4068}#
                     (#{match 4056}#
                       (car #{e 4057}#)
                       #{p 4058}#
                       #{w 4059}#
                       '()
                       #{mod 4060}#)))
               (if #{first 4068}#
                 (let ((#{rest 4072}#
                         (#{match-each 4044}#
                           (cdr #{e 4057}#)
                           #{p 4058}#
                           #{w 4059}#
                           #{mod 4060}#)))
                   (if #{rest 4072}#
                     (cons #{first 4068}# #{rest 4072}#)
                     #f))
                 #f))
             (if (null? #{e 4057}#)
               '()
               (if (#{syntax-object? 309}# #{e 4057}#)
                 (#{match-each 4044}#
                   (#{syntax-object-expression 311}# #{e 4057}#)
                   #{p 4058}#
                   (#{join-wraps 391}#
                     #{w 4059}#
                     (#{syntax-object-wrap 313}# #{e 4057}#))
                   (#{syntax-object-module 315}# #{e 4057}#))
                 #f)))))
       (#{match-each+ 4046}#
         (lambda (#{e 4080}#
                  #{x-pat 4081}#
                  #{y-pat 4082}#
                  #{z-pat 4083}#
                  #{w 4084}#
                  #{r 4085}#
                  #{mod 4086}#)
           (letrec*
             ((#{f 4097}#
                (lambda (#{e 4098}# #{w 4099}#)
                  (if (pair? #{e 4098}#)
                    (call-with-values
                      (lambda ()
                        (#{f 4097}# (cdr #{e 4098}#) #{w 4099}#))
                      (lambda (#{xr* 4102}# #{y-pat 4103}# #{r 4104}#)
                        (if #{r 4104}#
                          (if (null? #{y-pat 4103}#)
                            (let ((#{xr 4109}#
                                    (#{match 4056}#
                                      (car #{e 4098}#)
                                      #{x-pat 4081}#
                                      #{w 4099}#
                                      '()
                                      #{mod 4086}#)))
                              (if #{xr 4109}#
                                (values
                                  (cons #{xr 4109}# #{xr* 4102}#)
                                  #{y-pat 4103}#
                                  #{r 4104}#)
                                (values #f #f #f)))
                            (values
                              '()
                              (cdr #{y-pat 4103}#)
                              (#{match 4056}#
                                (car #{e 4098}#)
                                (car #{y-pat 4103}#)
                                #{w 4099}#
                                #{r 4104}#
                                #{mod 4086}#)))
                          (values #f #f #f))))
                    (if (#{syntax-object? 309}# #{e 4098}#)
                      (#{f 4097}#
                        (#{syntax-object-expression 311}# #{e 4098}#)
                        (#{join-wraps 391}# #{w 4099}# #{e 4098}#))
                      (values
                        '()
                        #{y-pat 4082}#
                        (#{match 4056}#
                          #{e 4098}#
                          #{z-pat 4083}#
                          #{w 4099}#
                          #{r 4085}#
                          #{mod 4086}#)))))))
             (#{f 4097}# #{e 4080}# #{w 4084}#))))
       (#{match-each-any 4048}#
         (lambda (#{e 4113}# #{w 4114}# #{mod 4115}#)
           (if (pair? #{e 4113}#)
             (let ((#{l 4122}#
                     (#{match-each-any 4048}#
                       (cdr #{e 4113}#)
                       #{w 4114}#
                       #{mod 4115}#)))
               (if #{l 4122}#
                 (cons (#{wrap 409}#
                         (car #{e 4113}#)
                         #{w 4114}#
                         #{mod 4115}#)
                       #{l 4122}#)
                 #f))
             (if (null? #{e 4113}#)
               '()
               (if (#{syntax-object? 309}# #{e 4113}#)
                 (#{match-each-any 4048}#
                   (#{syntax-object-expression 311}# #{e 4113}#)
                   (#{join-wraps 391}#
                     #{w 4114}#
                     (#{syntax-object-wrap 313}# #{e 4113}#))
                   #{mod 4115}#)
                 #f)))))
       (#{match-empty 4050}#
         (lambda (#{p 4130}# #{r 4131}#)
           (if (null? #{p 4130}#)
             #{r 4131}#
             (if (eq? #{p 4130}# '_)
               #{r 4131}#
               (if (eq? #{p 4130}# 'any)
                 (cons '() #{r 4131}#)
                 (if (pair? #{p 4130}#)
                   (#{match-empty 4050}#
                     (car #{p 4130}#)
                     (#{match-empty 4050}#
                       (cdr #{p 4130}#)
                       #{r 4131}#))
                   (if (eq? #{p 4130}# 'each-any)
                     (cons '() #{r 4131}#)
                     (let ((#{atom-key 4147}# (vector-ref #{p 4130}# 0)))
                       (if (memv #{atom-key 4147}# '(each))
                         (#{match-empty 4050}#
                           (vector-ref #{p 4130}# 1)
                           #{r 4131}#)
                         (if (memv #{atom-key 4147}# '(each+))
                           (#{match-empty 4050}#
                             (vector-ref #{p 4130}# 1)
                             (#{match-empty 4050}#
                               (reverse (vector-ref #{p 4130}# 2))
                               (#{match-empty 4050}#
                                 (vector-ref #{p 4130}# 3)
                                 #{r 4131}#)))
                           (if (memv #{atom-key 4147}# '(free-id atom))
                             #{r 4131}#
                             (if (memv #{atom-key 4147}# '(vector))
                               (#{match-empty 4050}#
                                 (vector-ref #{p 4130}# 1)
                                 #{r 4131}#)))))))))))))
       (#{combine 4052}#
         (lambda (#{r* 4152}# #{r 4153}#)
           (if (null? (car #{r* 4152}#))
             #{r 4153}#
             (cons (map car #{r* 4152}#)
                   (#{combine 4052}#
                     (map cdr #{r* 4152}#)
                     #{r 4153}#)))))
       (#{match* 4054}#
         (lambda (#{e 4156}#
                  #{p 4157}#
                  #{w 4158}#
                  #{r 4159}#
                  #{mod 4160}#)
           (if (null? #{p 4157}#)
             (if (null? #{e 4156}#) #{r 4159}# #f)
             (if (pair? #{p 4157}#)
               (if (pair? #{e 4156}#)
                 (#{match 4056}#
                   (car #{e 4156}#)
                   (car #{p 4157}#)
                   #{w 4158}#
                   (#{match 4056}#
                     (cdr #{e 4156}#)
                     (cdr #{p 4157}#)
                     #{w 4158}#
                     #{r 4159}#
                     #{mod 4160}#)
                   #{mod 4160}#)
                 #f)
               (if (eq? #{p 4157}# 'each-any)
                 (let ((#{l 4177}#
                         (#{match-each-any 4048}#
                           #{e 4156}#
                           #{w 4158}#
                           #{mod 4160}#)))
                   (if #{l 4177}# (cons #{l 4177}# #{r 4159}#) #f))
                 (let ((#{atom-key 4183}# (vector-ref #{p 4157}# 0)))
                   (if (memv #{atom-key 4183}# '(each))
                     (if (null? #{e 4156}#)
                       (#{match-empty 4050}#
                         (vector-ref #{p 4157}# 1)
                         #{r 4159}#)
                       (let ((#{l 4186}#
                               (#{match-each 4044}#
                                 #{e 4156}#
                                 (vector-ref #{p 4157}# 1)
                                 #{w 4158}#
                                 #{mod 4160}#)))
                         (if #{l 4186}#
                           (letrec*
                             ((#{collect 4191}#
                                (lambda (#{l 4192}#)
                                  (if (null? (car #{l 4192}#))
                                    #{r 4159}#
                                    (cons (map car #{l 4192}#)
                                          (#{collect 4191}#
                                            (map cdr #{l 4192}#)))))))
                             (#{collect 4191}# #{l 4186}#))
                           #f)))
                     (if (memv #{atom-key 4183}# '(each+))
                       (call-with-values
                         (lambda ()
                           (#{match-each+ 4046}#
                             #{e 4156}#
                             (vector-ref #{p 4157}# 1)
                             (vector-ref #{p 4157}# 2)
                             (vector-ref #{p 4157}# 3)
                             #{w 4158}#
                             #{r 4159}#
                             #{mod 4160}#))
                         (lambda (#{xr* 4194}# #{y-pat 4195}# #{r 4196}#)
                           (if #{r 4196}#
                             (if (null? #{y-pat 4195}#)
                               (if (null? #{xr* 4194}#)
                                 (#{match-empty 4050}#
                                   (vector-ref #{p 4157}# 1)
                                   #{r 4196}#)
                                 (#{combine 4052}# #{xr* 4194}# #{r 4196}#))
                               #f)
                             #f)))
                       (if (memv #{atom-key 4183}# '(free-id))
                         (if (#{id? 343}# #{e 4156}#)
                           (if (#{free-id=? 399}#
                                 (#{wrap 409}#
                                   #{e 4156}#
                                   #{w 4158}#
                                   #{mod 4160}#)
                                 (vector-ref #{p 4157}# 1))
                             #{r 4159}#
                             #f)
                           #f)
                         (if (memv #{atom-key 4183}# '(atom))
                           (if (equal?
                                 (vector-ref #{p 4157}# 1)
                                 (#{strip 449}# #{e 4156}# #{w 4158}#))
                             #{r 4159}#
                             #f)
                           (if (memv #{atom-key 4183}# '(vector))
                             (if (vector? #{e 4156}#)
                               (#{match 4056}#
                                 (vector->list #{e 4156}#)
                                 (vector-ref #{p 4157}# 1)
                                 #{w 4158}#
                                 #{r 4159}#
                                 #{mod 4160}#)
                               #f))))))))))))
       (#{match 4056}#
         (lambda (#{e 4213}#
                  #{p 4214}#
                  #{w 4215}#
                  #{r 4216}#
                  #{mod 4217}#)
           (if (not #{r 4216}#)
             #f
             (if (eq? #{p 4214}# '_)
               #{r 4216}#
               (if (eq? #{p 4214}# 'any)
                 (cons (#{wrap 409}# #{e 4213}# #{w 4215}# #{mod 4217}#)
                       #{r 4216}#)
                 (if (#{syntax-object? 309}# #{e 4213}#)
                   (#{match* 4054}#
                     (#{syntax-object-expression 311}# #{e 4213}#)
                     #{p 4214}#
                     (#{join-wraps 391}#
                       #{w 4215}#
                       (#{syntax-object-wrap 313}# #{e 4213}#))
                     #{r 4216}#
                     (#{syntax-object-module 315}# #{e 4213}#))
                   (#{match* 4054}#
                     #{e 4213}#
                     #{p 4214}#
                     #{w 4215}#
                     #{r 4216}#
                     #{mod 4217}#))))))))
      (set! $sc-dispatch
        (lambda (#{e 4232}# #{p 4233}#)
          (if (eq? #{p 4233}# 'any)
            (list #{e 4232}#)
            (if (eq? #{p 4233}# '_)
              '()
              (if (#{syntax-object? 309}# #{e 4232}#)
                (#{match* 4054}#
                  (#{syntax-object-expression 311}# #{e 4232}#)
                  #{p 4233}#
                  (#{syntax-object-wrap 313}# #{e 4232}#)
                  '()
                  (#{syntax-object-module 315}# #{e 4232}#))
                (#{match* 4054}#
                  #{e 4232}#
                  #{p 4233}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syntax-transformer
    'with-syntax
    'macro
    (lambda (#{x 4244}#)
      (let ((#{tmp 4246}# #{x 4244}#))
        (let ((#{tmp 4247}#
                ($sc-dispatch
                  #{tmp 4246}#
                  '(_ () any . each-any))))
          (if #{tmp 4247}#
            (@apply
              (lambda (#{e1 4250}# #{e2 4251}#)
                (cons '#(syntax-object
                         let
                         ((top)
                          #(ribcage
                            #(e1 e2)
                            #((top) (top))
                            #("i4248" "i4249"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4245")))
                         (hygiene guile))
                      (cons '() (cons #{e1 4250}# #{e2 4251}#))))
              #{tmp 4247}#)
            (let ((#{tmp 4253}#
                    ($sc-dispatch
                      #{tmp 4246}#
                      '(_ ((any any)) any . each-any))))
              (if #{tmp 4253}#
                (@apply
                  (lambda (#{out 4258}#
                           #{in 4259}#
                           #{e1 4260}#
                           #{e2 4261}#)
                    (list '#(syntax-object
                             syntax-case
                             ((top)
                              #(ribcage
                                #(out in e1 e2)
                                #((top) (top) (top) (top))
                                #("i4254" "i4255" "i4256" "i4257"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4245")))
                             (hygiene guile))
                          #{in 4259}#
                          '()
                          (list #{out 4258}#
                                (cons '#(syntax-object
                                         let
                                         ((top)
                                          #(ribcage
                                            #(out in e1 e2)
                                            #((top) (top) (top) (top))
                                            #("i4254" "i4255" "i4256" "i4257"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4245")))
                                         (hygiene guile))
                                      (cons '()
                                            (cons #{e1 4260}# #{e2 4261}#))))))
                  #{tmp 4253}#)
                (let ((#{tmp 4263}#
                        ($sc-dispatch
                          #{tmp 4246}#
                          '(_ #(each (any any)) any . each-any))))
                  (if #{tmp 4263}#
                    (@apply
                      (lambda (#{out 4268}#
                               #{in 4269}#
                               #{e1 4270}#
                               #{e2 4271}#)
                        (list '#(syntax-object
                                 syntax-case
                                 ((top)
                                  #(ribcage
                                    #(out in e1 e2)
                                    #((top) (top) (top) (top))
                                    #("i4264" "i4265" "i4266" "i4267"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4245")))
                                 (hygiene guile))
                              (cons '#(syntax-object
                                       list
                                       ((top)
                                        #(ribcage
                                          #(out in e1 e2)
                                          #((top) (top) (top) (top))
                                          #("i4264" "i4265" "i4266" "i4267"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4245")))
                                       (hygiene guile))
                                    #{in 4269}#)
                              '()
                              (list #{out 4268}#
                                    (cons '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage
                                                #(out in e1 e2)
                                                #((top) (top) (top) (top))
                                                #("i4264"
                                                  "i4265"
                                                  "i4266"
                                                  "i4267"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i4245")))
                                             (hygiene guile))
                                          (cons '()
                                                (cons #{e1 4270}#
                                                      #{e2 4271}#))))))
                      #{tmp 4263}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp 4246}#)))))))))))

(define syntax-rules
  (make-syntax-transformer
    'syntax-rules
    'macro
    (lambda (#{x 4275}#)
      (let ((#{tmp 4277}# #{x 4275}#))
        (let ((#{tmp 4278}#
                ($sc-dispatch
                  #{tmp 4277}#
                  '(_ each-any . #(each ((any . any) any))))))
          (if #{tmp 4278}#
            (@apply
              (lambda (#{k 4283}#
                       #{keyword 4284}#
                       #{pattern 4285}#
                       #{template 4286}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(k keyword pattern template)
                            #((top) (top) (top) (top))
                            #("i4279" "i4280" "i4281" "i4282"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4276")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(k keyword pattern template)
                             #((top) (top) (top) (top))
                             #("i4279" "i4280" "i4281" "i4282"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4276")))
                          (hygiene guile)))
                      (vector
                        '(#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(k keyword pattern template)
                               #((top) (top) (top) (top))
                               #("i4279" "i4280" "i4281" "i4282"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4276")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            syntax-rules
                            ((top)
                             #(ribcage
                               #(k keyword pattern template)
                               #((top) (top) (top) (top))
                               #("i4279" "i4280" "i4281" "i4282"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4276")))
                            (hygiene guile)))
                        (cons '#(syntax-object
                                 patterns
                                 ((top)
                                  #(ribcage
                                    #(k keyword pattern template)
                                    #((top) (top) (top) (top))
                                    #("i4279" "i4280" "i4281" "i4282"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4276")))
                                 (hygiene guile))
                              #{pattern 4285}#))
                      (cons '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(k keyword pattern template)
                                  #((top) (top) (top) (top))
                                  #("i4279" "i4280" "i4281" "i4282"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4276")))
                               (hygiene guile))
                            (cons '#(syntax-object
                                     x
                                     ((top)
                                      #(ribcage
                                        #(k keyword pattern template)
                                        #((top) (top) (top) (top))
                                        #("i4279" "i4280" "i4281" "i4282"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4276")))
                                     (hygiene guile))
                                  (cons #{k 4283}#
                                        (map (lambda (#{tmp 4290}#
                                                      #{tmp 4289}#)
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
                                                                 #("i4279"
                                                                   "i4280"
                                                                   "i4281"
                                                                   "i4282"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4276")))
                                                              (hygiene guile))
                                                           #{tmp 4289}#)
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
                                                                 #("i4279"
                                                                   "i4280"
                                                                   "i4281"
                                                                   "i4282"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4276")))
                                                              (hygiene guile))
                                                           #{tmp 4290}#)))
                                             #{template 4286}#
                                             #{pattern 4285}#))))))
              #{tmp 4278}#)
            (let ((#{tmp 4291}#
                    ($sc-dispatch
                      #{tmp 4277}#
                      '(_ each-any any . #(each ((any . any) any))))))
              (if (if #{tmp 4291}#
                    (@apply
                      (lambda (#{k 4297}#
                               #{docstring 4298}#
                               #{keyword 4299}#
                               #{pattern 4300}#
                               #{template 4301}#)
                        (string? (syntax->datum #{docstring 4298}#)))
                      #{tmp 4291}#)
                    #f)
                (@apply
                  (lambda (#{k 4307}#
                           #{docstring 4308}#
                           #{keyword 4309}#
                           #{pattern 4310}#
                           #{template 4311}#)
                    (list '#(syntax-object
                             lambda
                             ((top)
                              #(ribcage
                                #(k docstring keyword pattern template)
                                #((top) (top) (top) (top) (top))
                                #("i4302" "i4303" "i4304" "i4305" "i4306"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4276")))
                             (hygiene guile))
                          '(#(syntax-object
                              x
                              ((top)
                               #(ribcage
                                 #(k docstring keyword pattern template)
                                 #((top) (top) (top) (top) (top))
                                 #("i4302" "i4303" "i4304" "i4305" "i4306"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4276")))
                              (hygiene guile)))
                          #{docstring 4308}#
                          (vector
                            '(#(syntax-object
                                macro-type
                                ((top)
                                 #(ribcage
                                   #(k docstring keyword pattern template)
                                   #((top) (top) (top) (top) (top))
                                   #("i4302" "i4303" "i4304" "i4305" "i4306"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4276")))
                                (hygiene guile))
                              .
                              #(syntax-object
                                syntax-rules
                                ((top)
                                 #(ribcage
                                   #(k docstring keyword pattern template)
                                   #((top) (top) (top) (top) (top))
                                   #("i4302" "i4303" "i4304" "i4305" "i4306"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4276")))
                                (hygiene guile)))
                            (cons '#(syntax-object
                                     patterns
                                     ((top)
                                      #(ribcage
                                        #(k docstring keyword pattern template)
                                        #((top) (top) (top) (top) (top))
                                        #("i4302"
                                          "i4303"
                                          "i4304"
                                          "i4305"
                                          "i4306"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4276")))
                                     (hygiene guile))
                                  #{pattern 4310}#))
                          (cons '#(syntax-object
                                   syntax-case
                                   ((top)
                                    #(ribcage
                                      #(k docstring keyword pattern template)
                                      #((top) (top) (top) (top) (top))
                                      #("i4302"
                                        "i4303"
                                        "i4304"
                                        "i4305"
                                        "i4306"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i4276")))
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
                                            #("i4302"
                                              "i4303"
                                              "i4304"
                                              "i4305"
                                              "i4306"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4276")))
                                         (hygiene guile))
                                      (cons #{k 4307}#
                                            (map (lambda (#{tmp 4315}#
                                                          #{tmp 4314}#)
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
                                                                     #("i4302"
                                                                       "i4303"
                                                                       "i4304"
                                                                       "i4305"
                                                                       "i4306"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(x)
                                                                     #((top))
                                                                     #("i4276")))
                                                                  (hygiene
                                                                    guile))
                                                               #{tmp 4314}#)
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
                                                                     #("i4302"
                                                                       "i4303"
                                                                       "i4304"
                                                                       "i4305"
                                                                       "i4306"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(x)
                                                                     #((top))
                                                                     #("i4276")))
                                                                  (hygiene
                                                                    guile))
                                                               #{tmp 4315}#)))
                                                 #{template 4311}#
                                                 #{pattern 4310}#))))))
                  #{tmp 4291}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4277}#)))))))))

(define let*
  (make-syntax-transformer
    'let*
    'macro
    (lambda (#{x 4316}#)
      (let ((#{tmp 4318}# #{x 4316}#))
        (let ((#{tmp 4319}#
                ($sc-dispatch
                  #{tmp 4318}#
                  '(any #(each (any any)) any . each-any))))
          (if (if #{tmp 4319}#
                (@apply
                  (lambda (#{let* 4325}#
                           #{x 4326}#
                           #{v 4327}#
                           #{e1 4328}#
                           #{e2 4329}#)
                    (and-map identifier? #{x 4326}#))
                  #{tmp 4319}#)
                #f)
            (@apply
              (lambda (#{let* 4336}#
                       #{x 4337}#
                       #{v 4338}#
                       #{e1 4339}#
                       #{e2 4340}#)
                (letrec*
                  ((#{f 4343}#
                     (lambda (#{bindings 4344}#)
                       (if (null? #{bindings 4344}#)
                         (cons '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(f bindings)
                                     #((top) (top))
                                     #("i4341" "i4342"))
                                   #(ribcage
                                     #(let* x v e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("i4331"
                                       "i4332"
                                       "i4333"
                                       "i4334"
                                       "i4335"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i4317")))
                                  (hygiene guile))
                               (cons '() (cons #{e1 4339}# #{e2 4340}#)))
                         (let ((#{tmp 4349}#
                                 (list (#{f 4343}# (cdr #{bindings 4344}#))
                                       (car #{bindings 4344}#))))
                           (let ((#{tmp 4350}#
                                   ($sc-dispatch #{tmp 4349}# '(any any))))
                             (if #{tmp 4350}#
                               (@apply
                                 (lambda (#{body 4353}# #{binding 4354}#)
                                   (list '#(syntax-object
                                            let
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(body binding)
                                               #((top) (top))
                                               #("i4351" "i4352"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(f bindings)
                                               #((top) (top))
                                               #("i4341" "i4342"))
                                             #(ribcage
                                               #(let* x v e1 e2)
                                               #((top) (top) (top) (top) (top))
                                               #("i4331"
                                                 "i4332"
                                                 "i4333"
                                                 "i4334"
                                                 "i4335"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4317")))
                                            (hygiene guile))
                                         (list #{binding 4354}#)
                                         #{body 4353}#))
                                 #{tmp 4350}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4349}#))))))))
                  (#{f 4343}# (map list #{x 4337}# #{v 4338}#))))
              #{tmp 4319}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4318}#)))))))

(define do
  (make-syntax-transformer
    'do
    'macro
    (lambda (#{orig-x 4355}#)
      (let ((#{tmp 4357}# #{orig-x 4355}#))
        (let ((#{tmp 4358}#
                ($sc-dispatch
                  #{tmp 4357}#
                  '(_ #(each (any any . any))
                      (any . each-any)
                      .
                      each-any))))
          (if #{tmp 4358}#
            (@apply
              (lambda (#{var 4365}#
                       #{init 4366}#
                       #{step 4367}#
                       #{e0 4368}#
                       #{e1 4369}#
                       #{c 4370}#)
                (let ((#{tmp 4372}#
                        (map (lambda (#{v 4393}# #{s 4394}#)
                               (let ((#{tmp 4397}# #{s 4394}#))
                                 (let ((#{tmp 4398}#
                                         ($sc-dispatch #{tmp 4397}# '())))
                                   (if #{tmp 4398}#
                                     (@apply
                                       (lambda () #{v 4393}#)
                                       #{tmp 4398}#)
                                     (let ((#{tmp 4399}#
                                             ($sc-dispatch
                                               #{tmp 4397}#
                                               '(any))))
                                       (if #{tmp 4399}#
                                         (@apply
                                           (lambda (#{e 4401}#) #{e 4401}#)
                                           #{tmp 4399}#)
                                         (let ((#{_ 4403}# #{tmp 4397}#))
                                           (syntax-violation
                                             'do
                                             "bad step expression"
                                             #{orig-x 4355}#
                                             #{s 4394}#))))))))
                             #{var 4365}#
                             #{step 4367}#)))
                  (let ((#{tmp 4373}#
                          ($sc-dispatch #{tmp 4372}# 'each-any)))
                    (if #{tmp 4373}#
                      (@apply
                        (lambda (#{step 4375}#)
                          (let ((#{tmp 4376}# #{e1 4369}#))
                            (let ((#{tmp 4377}#
                                    ($sc-dispatch #{tmp 4376}# '())))
                              (if #{tmp 4377}#
                                (@apply
                                  (lambda ()
                                    (list '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4374"))
                                              #(ribcage
                                                #(var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4359"
                                                  "i4360"
                                                  "i4361"
                                                  "i4362"
                                                  "i4363"
                                                  "i4364"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4356")))
                                             (hygiene guile))
                                          '#(syntax-object
                                             doloop
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4374"))
                                              #(ribcage
                                                #(var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4359"
                                                  "i4360"
                                                  "i4361"
                                                  "i4362"
                                                  "i4363"
                                                  "i4364"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4356")))
                                             (hygiene guile))
                                          (map list #{var 4365}# #{init 4366}#)
                                          (list '#(syntax-object
                                                   if
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(step)
                                                      #((top))
                                                      #("i4374"))
                                                    #(ribcage
                                                      #(var init step e0 e1 c)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i4359"
                                                        "i4360"
                                                        "i4361"
                                                        "i4362"
                                                        "i4363"
                                                        "i4364"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(orig-x)
                                                      #((top))
                                                      #("i4356")))
                                                   (hygiene guile))
                                                (list '#(syntax-object
                                                         not
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4374"))
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
                                                            #("i4359"
                                                              "i4360"
                                                              "i4361"
                                                              "i4362"
                                                              "i4363"
                                                              "i4364"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4356")))
                                                         (hygiene guile))
                                                      #{e0 4368}#)
                                                (cons '#(syntax-object
                                                         begin
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4374"))
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
                                                            #("i4359"
                                                              "i4360"
                                                              "i4361"
                                                              "i4362"
                                                              "i4363"
                                                              "i4364"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4356")))
                                                         (hygiene guile))
                                                      (append
                                                        #{c 4370}#
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
                                                                          #("i4374"))
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
                                                                          #("i4359"
                                                                            "i4360"
                                                                            "i4361"
                                                                            "i4362"
                                                                            "i4363"
                                                                            "i4364"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(orig-x)
                                                                          #((top))
                                                                          #("i4356")))
                                                                       (hygiene
                                                                         guile))
                                                                    #{step 4375}#)))))))
                                  #{tmp 4377}#)
                                (let ((#{tmp 4382}#
                                        ($sc-dispatch
                                          #{tmp 4376}#
                                          '(any . each-any))))
                                  (if #{tmp 4382}#
                                    (@apply
                                      (lambda (#{e1 4385}# #{e2 4386}#)
                                        (list '#(syntax-object
                                                 let
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4383" "i4384"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4374"))
                                                  #(ribcage
                                                    #(var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4359"
                                                      "i4360"
                                                      "i4361"
                                                      "i4362"
                                                      "i4363"
                                                      "i4364"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4356")))
                                                 (hygiene guile))
                                              '#(syntax-object
                                                 doloop
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4383" "i4384"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4374"))
                                                  #(ribcage
                                                    #(var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4359"
                                                      "i4360"
                                                      "i4361"
                                                      "i4362"
                                                      "i4363"
                                                      "i4364"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4356")))
                                                 (hygiene guile))
                                              (map list
                                                   #{var 4365}#
                                                   #{init 4366}#)
                                              (list '#(syntax-object
                                                       if
                                                       ((top)
                                                        #(ribcage
                                                          #(e1 e2)
                                                          #((top) (top))
                                                          #("i4383" "i4384"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(step)
                                                          #((top))
                                                          #("i4374"))
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
                                                          #("i4359"
                                                            "i4360"
                                                            "i4361"
                                                            "i4362"
                                                            "i4363"
                                                            "i4364"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(orig-x)
                                                          #((top))
                                                          #("i4356")))
                                                       (hygiene guile))
                                                    #{e0 4368}#
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4383"
                                                                  "i4384"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4374"))
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
                                                                #("i4359"
                                                                  "i4360"
                                                                  "i4361"
                                                                  "i4362"
                                                                  "i4363"
                                                                  "i4364"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4356")))
                                                             (hygiene guile))
                                                          (cons #{e1 4385}#
                                                                #{e2 4386}#))
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4383"
                                                                  "i4384"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4374"))
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
                                                                #("i4359"
                                                                  "i4360"
                                                                  "i4361"
                                                                  "i4362"
                                                                  "i4363"
                                                                  "i4364"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4356")))
                                                             (hygiene guile))
                                                          (append
                                                            #{c 4370}#
                                                            (list (cons '#(syntax-object
                                                                           doloop
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(e1
                                                                                e2)
                                                                              #((top)
                                                                                (top))
                                                                              #("i4383"
                                                                                "i4384"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(step)
                                                                              #((top))
                                                                              #("i4374"))
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
                                                                              #("i4359"
                                                                                "i4360"
                                                                                "i4361"
                                                                                "i4362"
                                                                                "i4363"
                                                                                "i4364"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(orig-x)
                                                                              #((top))
                                                                              #("i4356")))
                                                                           (hygiene
                                                                             guile))
                                                                        #{step 4375}#)))))))
                                      #{tmp 4382}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp 4376}#)))))))
                        #{tmp 4373}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp 4372}#)))))
              #{tmp 4358}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4357}#)))))))

(define quasiquote
  (make-syntax-transformer
    'quasiquote
    'macro
    (letrec*
      ((#{quasi 4407}#
         (lambda (#{p 4420}# #{lev 4421}#)
           (let ((#{tmp 4424}# #{p 4420}#))
             (let ((#{tmp 4425}#
                     ($sc-dispatch
                       #{tmp 4424}#
                       '(#(free-id
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4422" "i4423"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4418"
                                 "i4416"
                                 "i4414"
                                 "i4412"
                                 "i4410"
                                 "i4408"
                                 "i4406")))
                             (hygiene guile)))
                         any))))
               (if #{tmp 4425}#
                 (@apply
                   (lambda (#{p 4427}#)
                     (if (= #{lev 4421}# 0)
                       (list '#(syntax-object
                                "value"
                                ((top)
                                 #(ribcage #(p) #((top)) #("i4426"))
                                 #(ribcage () () ())
                                 #(ribcage
                                   #(p lev)
                                   #((top) (top))
                                   #("i4422" "i4423"))
                                 #(ribcage
                                   (emit quasivector
                                         quasilist*
                                         quasiappend
                                         quasicons
                                         vquasi
                                         quasi)
                                   ((top) (top) (top) (top) (top) (top) (top))
                                   ("i4418"
                                    "i4416"
                                    "i4414"
                                    "i4412"
                                    "i4410"
                                    "i4408"
                                    "i4406")))
                                (hygiene guile))
                             #{p 4427}#)
                       (#{quasicons 4411}#
                         '(#(syntax-object
                             "quote"
                             ((top)
                              #(ribcage #(p) #((top)) #("i4426"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4422" "i4423"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4418"
                                 "i4416"
                                 "i4414"
                                 "i4412"
                                 "i4410"
                                 "i4408"
                                 "i4406")))
                             (hygiene guile))
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage #(p) #((top)) #("i4426"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4422" "i4423"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4418"
                                 "i4416"
                                 "i4414"
                                 "i4412"
                                 "i4410"
                                 "i4408"
                                 "i4406")))
                             (hygiene guile)))
                         (#{quasi 4407}#
                           (list #{p 4427}#)
                           (#{1-}# #{lev 4421}#)))))
                   #{tmp 4425}#)
                 (let ((#{tmp 4428}#
                         ($sc-dispatch
                           #{tmp 4424}#
                           '(#(free-id
                               #(syntax-object
                                 quasiquote
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage
                                    #(p lev)
                                    #((top) (top))
                                    #("i4422" "i4423"))
                                  #(ribcage
                                    (emit quasivector
                                          quasilist*
                                          quasiappend
                                          quasicons
                                          vquasi
                                          quasi)
                                    ((top) (top) (top) (top) (top) (top) (top))
                                    ("i4418"
                                     "i4416"
                                     "i4414"
                                     "i4412"
                                     "i4410"
                                     "i4408"
                                     "i4406")))
                                 (hygiene guile)))
                             any))))
                   (if #{tmp 4428}#
                     (@apply
                       (lambda (#{p 4430}#)
                         (#{quasicons 4411}#
                           '(#(syntax-object
                               "quote"
                               ((top)
                                #(ribcage #(p) #((top)) #("i4429"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(p lev)
                                  #((top) (top))
                                  #("i4422" "i4423"))
                                #(ribcage
                                  (emit quasivector
                                        quasilist*
                                        quasiappend
                                        quasicons
                                        vquasi
                                        quasi)
                                  ((top) (top) (top) (top) (top) (top) (top))
                                  ("i4418"
                                   "i4416"
                                   "i4414"
                                   "i4412"
                                   "i4410"
                                   "i4408"
                                   "i4406")))
                               (hygiene guile))
                             #(syntax-object
                               quasiquote
                               ((top)
                                #(ribcage #(p) #((top)) #("i4429"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(p lev)
                                  #((top) (top))
                                  #("i4422" "i4423"))
                                #(ribcage
                                  (emit quasivector
                                        quasilist*
                                        quasiappend
                                        quasicons
                                        vquasi
                                        quasi)
                                  ((top) (top) (top) (top) (top) (top) (top))
                                  ("i4418"
                                   "i4416"
                                   "i4414"
                                   "i4412"
                                   "i4410"
                                   "i4408"
                                   "i4406")))
                               (hygiene guile)))
                           (#{quasi 4407}#
                             (list #{p 4430}#)
                             (#{1+}# #{lev 4421}#))))
                       #{tmp 4428}#)
                     (let ((#{tmp 4431}#
                             ($sc-dispatch #{tmp 4424}# '(any . any))))
                       (if #{tmp 4431}#
                         (@apply
                           (lambda (#{p 4434}# #{q 4435}#)
                             (let ((#{tmp 4436}# #{p 4434}#))
                               (let ((#{tmp 4437}#
                                       ($sc-dispatch
                                         #{tmp 4436}#
                                         '(#(free-id
                                             #(syntax-object
                                               unquote
                                               ((top)
                                                #(ribcage
                                                  #(p q)
                                                  #((top) (top))
                                                  #("i4432" "i4433"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(p lev)
                                                  #((top) (top))
                                                  #("i4422" "i4423"))
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
                                                  ("i4418"
                                                   "i4416"
                                                   "i4414"
                                                   "i4412"
                                                   "i4410"
                                                   "i4408"
                                                   "i4406")))
                                               (hygiene guile)))
                                           .
                                           each-any))))
                                 (if #{tmp 4437}#
                                   (@apply
                                     (lambda (#{p 4439}#)
                                       (if (= #{lev 4421}# 0)
                                         (#{quasilist* 4415}#
                                           (map (lambda (#{tmp 4440}#)
                                                  (list '#(syntax-object
                                                           "value"
                                                           ((top)
                                                            #(ribcage
                                                              #(p)
                                                              #((top))
                                                              #("i4438"))
                                                            #(ribcage
                                                              #(p q)
                                                              #((top) (top))
                                                              #("i4432"
                                                                "i4433"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(p lev)
                                                              #((top) (top))
                                                              #("i4422"
                                                                "i4423"))
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
                                                              ("i4418"
                                                               "i4416"
                                                               "i4414"
                                                               "i4412"
                                                               "i4410"
                                                               "i4408"
                                                               "i4406")))
                                                           (hygiene guile))
                                                        #{tmp 4440}#))
                                                #{p 4439}#)
                                           (#{quasi 4407}#
                                             #{q 4435}#
                                             #{lev 4421}#))
                                         (#{quasicons 4411}#
                                           (#{quasicons 4411}#
                                             '(#(syntax-object
                                                 "quote"
                                                 ((top)
                                                  #(ribcage
                                                    #(p)
                                                    #((top))
                                                    #("i4438"))
                                                  #(ribcage
                                                    #(p q)
                                                    #((top) (top))
                                                    #("i4432" "i4433"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(p lev)
                                                    #((top) (top))
                                                    #("i4422" "i4423"))
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
                                                    ("i4418"
                                                     "i4416"
                                                     "i4414"
                                                     "i4412"
                                                     "i4410"
                                                     "i4408"
                                                     "i4406")))
                                                 (hygiene guile))
                                               #(syntax-object
                                                 unquote
                                                 ((top)
                                                  #(ribcage
                                                    #(p)
                                                    #((top))
                                                    #("i4438"))
                                                  #(ribcage
                                                    #(p q)
                                                    #((top) (top))
                                                    #("i4432" "i4433"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(p lev)
                                                    #((top) (top))
                                                    #("i4422" "i4423"))
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
                                                    ("i4418"
                                                     "i4416"
                                                     "i4414"
                                                     "i4412"
                                                     "i4410"
                                                     "i4408"
                                                     "i4406")))
                                                 (hygiene guile)))
                                             (#{quasi 4407}#
                                               #{p 4439}#
                                               (#{1-}# #{lev 4421}#)))
                                           (#{quasi 4407}#
                                             #{q 4435}#
                                             #{lev 4421}#))))
                                     #{tmp 4437}#)
                                   (let ((#{tmp 4442}#
                                           ($sc-dispatch
                                             #{tmp 4436}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   unquote-splicing
                                                   ((top)
                                                    #(ribcage
                                                      #(p q)
                                                      #((top) (top))
                                                      #("i4432" "i4433"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(p lev)
                                                      #((top) (top))
                                                      #("i4422" "i4423"))
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
                                                      ("i4418"
                                                       "i4416"
                                                       "i4414"
                                                       "i4412"
                                                       "i4410"
                                                       "i4408"
                                                       "i4406")))
                                                   (hygiene guile)))
                                               .
                                               each-any))))
                                     (if #{tmp 4442}#
                                       (@apply
                                         (lambda (#{p 4444}#)
                                           (if (= #{lev 4421}# 0)
                                             (#{quasiappend 4413}#
                                               (map (lambda (#{tmp 4445}#)
                                                      (list '#(syntax-object
                                                               "value"
                                                               ((top)
                                                                #(ribcage
                                                                  #(p)
                                                                  #((top))
                                                                  #("i4443"))
                                                                #(ribcage
                                                                  #(p q)
                                                                  #((top)
                                                                    (top))
                                                                  #("i4432"
                                                                    "i4433"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(p lev)
                                                                  #((top)
                                                                    (top))
                                                                  #("i4422"
                                                                    "i4423"))
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
                                                                  ("i4418"
                                                                   "i4416"
                                                                   "i4414"
                                                                   "i4412"
                                                                   "i4410"
                                                                   "i4408"
                                                                   "i4406")))
                                                               (hygiene guile))
                                                            #{tmp 4445}#))
                                                    #{p 4444}#)
                                               (#{quasi 4407}#
                                                 #{q 4435}#
                                                 #{lev 4421}#))
                                             (#{quasicons 4411}#
                                               (#{quasicons 4411}#
                                                 '(#(syntax-object
                                                     "quote"
                                                     ((top)
                                                      #(ribcage
                                                        #(p)
                                                        #((top))
                                                        #("i4443"))
                                                      #(ribcage
                                                        #(p q)
                                                        #((top) (top))
                                                        #("i4432" "i4433"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(p lev)
                                                        #((top) (top))
                                                        #("i4422" "i4423"))
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
                                                        ("i4418"
                                                         "i4416"
                                                         "i4414"
                                                         "i4412"
                                                         "i4410"
                                                         "i4408"
                                                         "i4406")))
                                                     (hygiene guile))
                                                   #(syntax-object
                                                     unquote-splicing
                                                     ((top)
                                                      #(ribcage
                                                        #(p)
                                                        #((top))
                                                        #("i4443"))
                                                      #(ribcage
                                                        #(p q)
                                                        #((top) (top))
                                                        #("i4432" "i4433"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(p lev)
                                                        #((top) (top))
                                                        #("i4422" "i4423"))
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
                                                        ("i4418"
                                                         "i4416"
                                                         "i4414"
                                                         "i4412"
                                                         "i4410"
                                                         "i4408"
                                                         "i4406")))
                                                     (hygiene guile)))
                                                 (#{quasi 4407}#
                                                   #{p 4444}#
                                                   (#{1-}# #{lev 4421}#)))
                                               (#{quasi 4407}#
                                                 #{q 4435}#
                                                 #{lev 4421}#))))
                                         #{tmp 4442}#)
                                       (let ((#{_ 4448}# #{tmp 4436}#))
                                         (#{quasicons 4411}#
                                           (#{quasi 4407}#
                                             #{p 4434}#
                                             #{lev 4421}#)
                                           (#{quasi 4407}#
                                             #{q 4435}#
                                             #{lev 4421}#)))))))))
                           #{tmp 4431}#)
                         (let ((#{tmp 4449}#
                                 ($sc-dispatch
                                   #{tmp 4424}#
                                   '#(vector each-any))))
                           (if #{tmp 4449}#
                             (@apply
                               (lambda (#{x 4451}#)
                                 (#{quasivector 4417}#
                                   (#{vquasi 4409}# #{x 4451}# #{lev 4421}#)))
                               #{tmp 4449}#)
                             (let ((#{p 4454}# #{tmp 4424}#))
                               (list '#(syntax-object
                                        "quote"
                                        ((top)
                                         #(ribcage #(p) #((top)) #("i4453"))
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p lev)
                                           #((top) (top))
                                           #("i4422" "i4423"))
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
                                           ("i4418"
                                            "i4416"
                                            "i4414"
                                            "i4412"
                                            "i4410"
                                            "i4408"
                                            "i4406")))
                                        (hygiene guile))
                                     #{p 4454}#)))))))))))))
       (#{vquasi 4409}#
         (lambda (#{p 4455}# #{lev 4456}#)
           (let ((#{tmp 4459}# #{p 4455}#))
             (let ((#{tmp 4460}#
                     ($sc-dispatch #{tmp 4459}# '(any . any))))
               (if #{tmp 4460}#
                 (@apply
                   (lambda (#{p 4463}# #{q 4464}#)
                     (let ((#{tmp 4465}# #{p 4463}#))
                       (let ((#{tmp 4466}#
                               ($sc-dispatch
                                 #{tmp 4465}#
                                 '(#(free-id
                                     #(syntax-object
                                       unquote
                                       ((top)
                                        #(ribcage
                                          #(p q)
                                          #((top) (top))
                                          #("i4461" "i4462"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p lev)
                                          #((top) (top))
                                          #("i4457" "i4458"))
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
                                          ("i4418"
                                           "i4416"
                                           "i4414"
                                           "i4412"
                                           "i4410"
                                           "i4408"
                                           "i4406")))
                                       (hygiene guile)))
                                   .
                                   each-any))))
                         (if #{tmp 4466}#
                           (@apply
                             (lambda (#{p 4468}#)
                               (if (= #{lev 4456}# 0)
                                 (#{quasilist* 4415}#
                                   (map (lambda (#{tmp 4469}#)
                                          (list '#(syntax-object
                                                   "value"
                                                   ((top)
                                                    #(ribcage
                                                      #(p)
                                                      #((top))
                                                      #("i4467"))
                                                    #(ribcage
                                                      #(p q)
                                                      #((top) (top))
                                                      #("i4461" "i4462"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(p lev)
                                                      #((top) (top))
                                                      #("i4457" "i4458"))
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
                                                      ("i4418"
                                                       "i4416"
                                                       "i4414"
                                                       "i4412"
                                                       "i4410"
                                                       "i4408"
                                                       "i4406")))
                                                   (hygiene guile))
                                                #{tmp 4469}#))
                                        #{p 4468}#)
                                   (#{vquasi 4409}# #{q 4464}# #{lev 4456}#))
                                 (#{quasicons 4411}#
                                   (#{quasicons 4411}#
                                     '(#(syntax-object
                                         "quote"
                                         ((top)
                                          #(ribcage #(p) #((top)) #("i4467"))
                                          #(ribcage
                                            #(p q)
                                            #((top) (top))
                                            #("i4461" "i4462"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(p lev)
                                            #((top) (top))
                                            #("i4457" "i4458"))
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
                                            ("i4418"
                                             "i4416"
                                             "i4414"
                                             "i4412"
                                             "i4410"
                                             "i4408"
                                             "i4406")))
                                         (hygiene guile))
                                       #(syntax-object
                                         unquote
                                         ((top)
                                          #(ribcage #(p) #((top)) #("i4467"))
                                          #(ribcage
                                            #(p q)
                                            #((top) (top))
                                            #("i4461" "i4462"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(p lev)
                                            #((top) (top))
                                            #("i4457" "i4458"))
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
                                            ("i4418"
                                             "i4416"
                                             "i4414"
                                             "i4412"
                                             "i4410"
                                             "i4408"
                                             "i4406")))
                                         (hygiene guile)))
                                     (#{quasi 4407}#
                                       #{p 4468}#
                                       (#{1-}# #{lev 4456}#)))
                                   (#{vquasi 4409}# #{q 4464}# #{lev 4456}#))))
                             #{tmp 4466}#)
                           (let ((#{tmp 4471}#
                                   ($sc-dispatch
                                     #{tmp 4465}#
                                     '(#(free-id
                                         #(syntax-object
                                           unquote-splicing
                                           ((top)
                                            #(ribcage
                                              #(p q)
                                              #((top) (top))
                                              #("i4461" "i4462"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(p lev)
                                              #((top) (top))
                                              #("i4457" "i4458"))
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
                                              ("i4418"
                                               "i4416"
                                               "i4414"
                                               "i4412"
                                               "i4410"
                                               "i4408"
                                               "i4406")))
                                           (hygiene guile)))
                                       .
                                       each-any))))
                             (if #{tmp 4471}#
                               (@apply
                                 (lambda (#{p 4473}#)
                                   (if (= #{lev 4456}# 0)
                                     (#{quasiappend 4413}#
                                       (map (lambda (#{tmp 4474}#)
                                              (list '#(syntax-object
                                                       "value"
                                                       ((top)
                                                        #(ribcage
                                                          #(p)
                                                          #((top))
                                                          #("i4472"))
                                                        #(ribcage
                                                          #(p q)
                                                          #((top) (top))
                                                          #("i4461" "i4462"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(p lev)
                                                          #((top) (top))
                                                          #("i4457" "i4458"))
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
                                                          ("i4418"
                                                           "i4416"
                                                           "i4414"
                                                           "i4412"
                                                           "i4410"
                                                           "i4408"
                                                           "i4406")))
                                                       (hygiene guile))
                                                    #{tmp 4474}#))
                                            #{p 4473}#)
                                       (#{vquasi 4409}#
                                         #{q 4464}#
                                         #{lev 4456}#))
                                     (#{quasicons 4411}#
                                       (#{quasicons 4411}#
                                         '(#(syntax-object
                                             "quote"
                                             ((top)
                                              #(ribcage
                                                #(p)
                                                #((top))
                                                #("i4472"))
                                              #(ribcage
                                                #(p q)
                                                #((top) (top))
                                                #("i4461" "i4462"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(p lev)
                                                #((top) (top))
                                                #("i4457" "i4458"))
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
                                                ("i4418"
                                                 "i4416"
                                                 "i4414"
                                                 "i4412"
                                                 "i4410"
                                                 "i4408"
                                                 "i4406")))
                                             (hygiene guile))
                                           #(syntax-object
                                             unquote-splicing
                                             ((top)
                                              #(ribcage
                                                #(p)
                                                #((top))
                                                #("i4472"))
                                              #(ribcage
                                                #(p q)
                                                #((top) (top))
                                                #("i4461" "i4462"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(p lev)
                                                #((top) (top))
                                                #("i4457" "i4458"))
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
                                                ("i4418"
                                                 "i4416"
                                                 "i4414"
                                                 "i4412"
                                                 "i4410"
                                                 "i4408"
                                                 "i4406")))
                                             (hygiene guile)))
                                         (#{quasi 4407}#
                                           #{p 4473}#
                                           (#{1-}# #{lev 4456}#)))
                                       (#{vquasi 4409}#
                                         #{q 4464}#
                                         #{lev 4456}#))))
                                 #{tmp 4471}#)
                               (let ((#{_ 4477}# #{tmp 4465}#))
                                 (#{quasicons 4411}#
                                   (#{quasi 4407}# #{p 4463}# #{lev 4456}#)
                                   (#{vquasi 4409}#
                                     #{q 4464}#
                                     #{lev 4456}#)))))))))
                   #{tmp 4460}#)
                 (let ((#{tmp 4478}# ($sc-dispatch #{tmp 4459}# '())))
                   (if #{tmp 4478}#
                     (@apply
                       (lambda ()
                         '(#(syntax-object
                             "quote"
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4457" "i4458"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4418"
                                 "i4416"
                                 "i4414"
                                 "i4412"
                                 "i4410"
                                 "i4408"
                                 "i4406")))
                             (hygiene guile))
                           ()))
                       #{tmp 4478}#)
                     (syntax-violation
                       #f
                       "source expression failed to match any pattern"
                       #{tmp 4459}#))))))))
       (#{quasicons 4411}#
         (lambda (#{x 4479}# #{y 4480}#)
           (let ((#{tmp 4484}# (list #{x 4479}# #{y 4480}#)))
             (let ((#{tmp 4485}#
                     ($sc-dispatch #{tmp 4484}# '(any any))))
               (if #{tmp 4485}#
                 (@apply
                   (lambda (#{x 4488}# #{y 4489}#)
                     (let ((#{tmp 4490}# #{y 4489}#))
                       (let ((#{tmp 4491}#
                               ($sc-dispatch
                                 #{tmp 4490}#
                                 '(#(atom "quote") any))))
                         (if #{tmp 4491}#
                           (@apply
                             (lambda (#{dy 4493}#)
                               (let ((#{tmp 4494}# #{x 4488}#))
                                 (let ((#{tmp 4495}#
                                         ($sc-dispatch
                                           #{tmp 4494}#
                                           '(#(atom "quote") any))))
                                   (if #{tmp 4495}#
                                     (@apply
                                       (lambda (#{dx 4497}#)
                                         (list '#(syntax-object
                                                  "quote"
                                                  ((top)
                                                   #(ribcage
                                                     #(dx)
                                                     #((top))
                                                     #("i4496"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4492"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4486" "i4487"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4481" "i4482"))
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
                                                     ("i4418"
                                                      "i4416"
                                                      "i4414"
                                                      "i4412"
                                                      "i4410"
                                                      "i4408"
                                                      "i4406")))
                                                  (hygiene guile))
                                               (cons #{dx 4497}# #{dy 4493}#)))
                                       #{tmp 4495}#)
                                     (let ((#{_ 4499}# #{tmp 4494}#))
                                       (if (null? #{dy 4493}#)
                                         (list '#(syntax-object
                                                  "list"
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4498"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4492"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4486" "i4487"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4481" "i4482"))
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
                                                     ("i4418"
                                                      "i4416"
                                                      "i4414"
                                                      "i4412"
                                                      "i4410"
                                                      "i4408"
                                                      "i4406")))
                                                  (hygiene guile))
                                               #{x 4488}#)
                                         (list '#(syntax-object
                                                  "list*"
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4498"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4492"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4486" "i4487"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4481" "i4482"))
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
                                                     ("i4418"
                                                      "i4416"
                                                      "i4414"
                                                      "i4412"
                                                      "i4410"
                                                      "i4408"
                                                      "i4406")))
                                                  (hygiene guile))
                                               #{x 4488}#
                                               #{y 4489}#)))))))
                             #{tmp 4491}#)
                           (let ((#{tmp 4500}#
                                   ($sc-dispatch
                                     #{tmp 4490}#
                                     '(#(atom "list") . any))))
                             (if #{tmp 4500}#
                               (@apply
                                 (lambda (#{stuff 4502}#)
                                   (cons '#(syntax-object
                                            "list"
                                            ((top)
                                             #(ribcage
                                               #(stuff)
                                               #((top))
                                               #("i4501"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4486" "i4487"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4481" "i4482"))
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
                                               ("i4418"
                                                "i4416"
                                                "i4414"
                                                "i4412"
                                                "i4410"
                                                "i4408"
                                                "i4406")))
                                            (hygiene guile))
                                         (cons #{x 4488}# #{stuff 4502}#)))
                                 #{tmp 4500}#)
                               (let ((#{tmp 4503}#
                                       ($sc-dispatch
                                         #{tmp 4490}#
                                         '(#(atom "list*") . any))))
                                 (if #{tmp 4503}#
                                   (@apply
                                     (lambda (#{stuff 4505}#)
                                       (cons '#(syntax-object
                                                "list*"
                                                ((top)
                                                 #(ribcage
                                                   #(stuff)
                                                   #((top))
                                                   #("i4504"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x y)
                                                   #((top) (top))
                                                   #("i4486" "i4487"))
                                                 #(ribcage () () ())
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x y)
                                                   #((top) (top))
                                                   #("i4481" "i4482"))
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
                                                   ("i4418"
                                                    "i4416"
                                                    "i4414"
                                                    "i4412"
                                                    "i4410"
                                                    "i4408"
                                                    "i4406")))
                                                (hygiene guile))
                                             (cons #{x 4488}# #{stuff 4505}#)))
                                     #{tmp 4503}#)
                                   (let ((#{_ 4507}# #{tmp 4490}#))
                                     (list '#(syntax-object
                                              "list*"
                                              ((top)
                                               #(ribcage
                                                 #(_)
                                                 #((top))
                                                 #("i4506"))
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i4486" "i4487"))
                                               #(ribcage () () ())
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i4481" "i4482"))
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
                                                 ("i4418"
                                                  "i4416"
                                                  "i4414"
                                                  "i4412"
                                                  "i4410"
                                                  "i4408"
                                                  "i4406")))
                                              (hygiene guile))
                                           #{x 4488}#
                                           #{y 4489}#))))))))))
                   #{tmp 4485}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp 4484}#))))))
       (#{quasiappend 4413}#
         (lambda (#{x 4508}# #{y 4509}#)
           (let ((#{tmp 4512}# #{y 4509}#))
             (let ((#{tmp 4513}#
                     ($sc-dispatch #{tmp 4512}# '(#(atom "quote") ()))))
               (if #{tmp 4513}#
                 (@apply
                   (lambda ()
                     (if (null? #{x 4508}#)
                       '(#(syntax-object
                           "quote"
                           ((top)
                            #(ribcage () () ())
                            #(ribcage
                              #(x y)
                              #((top) (top))
                              #("i4510" "i4511"))
                            #(ribcage
                              (emit quasivector
                                    quasilist*
                                    quasiappend
                                    quasicons
                                    vquasi
                                    quasi)
                              ((top) (top) (top) (top) (top) (top) (top))
                              ("i4418"
                               "i4416"
                               "i4414"
                               "i4412"
                               "i4410"
                               "i4408"
                               "i4406")))
                           (hygiene guile))
                         ())
                       (if (null? (cdr #{x 4508}#))
                         (car #{x 4508}#)
                         (let ((#{tmp 4520}# #{x 4508}#))
                           (let ((#{tmp 4521}#
                                   ($sc-dispatch #{tmp 4520}# 'each-any)))
                             (if #{tmp 4521}#
                               (@apply
                                 (lambda (#{p 4523}#)
                                   (cons '#(syntax-object
                                            "append"
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(p)
                                               #((top))
                                               #("i4522"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4510" "i4511"))
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
                                               ("i4418"
                                                "i4416"
                                                "i4414"
                                                "i4412"
                                                "i4410"
                                                "i4408"
                                                "i4406")))
                                            (hygiene guile))
                                         #{p 4523}#))
                                 #{tmp 4521}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4520}#)))))))
                   #{tmp 4513}#)
                 (let ((#{_ 4526}# #{tmp 4512}#))
                   (if (null? #{x 4508}#)
                     #{y 4509}#
                     (let ((#{tmp 4531}# (list #{x 4508}# #{y 4509}#)))
                       (let ((#{tmp 4532}#
                               ($sc-dispatch #{tmp 4531}# '(each-any any))))
                         (if #{tmp 4532}#
                           (@apply
                             (lambda (#{p 4535}# #{y 4536}#)
                               (cons '#(syntax-object
                                        "append"
                                        ((top)
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p y)
                                           #((top) (top))
                                           #("i4533" "i4534"))
                                         #(ribcage #(_) #((top)) #("i4525"))
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(x y)
                                           #((top) (top))
                                           #("i4510" "i4511"))
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
                                           ("i4418"
                                            "i4416"
                                            "i4414"
                                            "i4412"
                                            "i4410"
                                            "i4408"
                                            "i4406")))
                                        (hygiene guile))
                                     (append #{p 4535}# (list #{y 4536}#))))
                             #{tmp 4532}#)
                           (syntax-violation
                             #f
                             "source expression failed to match any pattern"
                             #{tmp 4531}#)))))))))))
       (#{quasilist* 4415}#
         (lambda (#{x 4538}# #{y 4539}#)
           (letrec*
             ((#{f 4544}#
                (lambda (#{x 4545}#)
                  (if (null? #{x 4545}#)
                    #{y 4539}#
                    (#{quasicons 4411}#
                      (car #{x 4545}#)
                      (#{f 4544}# (cdr #{x 4545}#)))))))
             (#{f 4544}# #{x 4538}#))))
       (#{quasivector 4417}#
         (lambda (#{x 4546}#)
           (let ((#{tmp 4548}# #{x 4546}#))
             (let ((#{tmp 4549}#
                     ($sc-dispatch
                       #{tmp 4548}#
                       '(#(atom "quote") each-any))))
               (if #{tmp 4549}#
                 (@apply
                   (lambda (#{x 4551}#)
                     (list '#(syntax-object
                              "quote"
                              ((top)
                               #(ribcage #(x) #((top)) #("i4550"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4547"))
                               #(ribcage
                                 (emit quasivector
                                       quasilist*
                                       quasiappend
                                       quasicons
                                       vquasi
                                       quasi)
                                 ((top) (top) (top) (top) (top) (top) (top))
                                 ("i4418"
                                  "i4416"
                                  "i4414"
                                  "i4412"
                                  "i4410"
                                  "i4408"
                                  "i4406")))
                              (hygiene guile))
                           (list->vector #{x 4551}#)))
                   #{tmp 4549}#)
                 (let ((#{_ 4554}# #{tmp 4548}#))
                   (letrec*
                     ((#{f 4558}#
                        (lambda (#{y 4559}# #{k 4560}#)
                          (let ((#{tmp 4571}# #{y 4559}#))
                            (let ((#{tmp 4572}#
                                    ($sc-dispatch
                                      #{tmp 4571}#
                                      '(#(atom "quote") each-any))))
                              (if #{tmp 4572}#
                                (@apply
                                  (lambda (#{y 4574}#)
                                    (#{k 4560}#
                                      (map (lambda (#{tmp 4575}#)
                                             (list '#(syntax-object
                                                      "quote"
                                                      ((top)
                                                       #(ribcage
                                                         #(y)
                                                         #((top))
                                                         #("i4573"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(f y k)
                                                         #((top) (top) (top))
                                                         #("i4555"
                                                           "i4556"
                                                           "i4557"))
                                                       #(ribcage
                                                         #(_)
                                                         #((top))
                                                         #("i4553"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4547"))
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
                                                         ("i4418"
                                                          "i4416"
                                                          "i4414"
                                                          "i4412"
                                                          "i4410"
                                                          "i4408"
                                                          "i4406")))
                                                      (hygiene guile))
                                                   #{tmp 4575}#))
                                           #{y 4574}#)))
                                  #{tmp 4572}#)
                                (let ((#{tmp 4576}#
                                        ($sc-dispatch
                                          #{tmp 4571}#
                                          '(#(atom "list") . each-any))))
                                  (if #{tmp 4576}#
                                    (@apply
                                      (lambda (#{y 4578}#)
                                        (#{k 4560}# #{y 4578}#))
                                      #{tmp 4576}#)
                                    (let ((#{tmp 4580}#
                                            ($sc-dispatch
                                              #{tmp 4571}#
                                              '(#(atom "list*")
                                                .
                                                #(each+ any (any) ())))))
                                      (if #{tmp 4580}#
                                        (@apply
                                          (lambda (#{y 4583}# #{z 4584}#)
                                            (#{f 4558}#
                                              #{z 4584}#
                                              (lambda (#{ls 4585}#)
                                                (#{k 4560}#
                                                  (append
                                                    #{y 4583}#
                                                    #{ls 4585}#)))))
                                          #{tmp 4580}#)
                                        (let ((#{else 4589}# #{tmp 4571}#))
                                          (let ((#{tmp 4593}# #{x 4546}#))
                                            (let ((#{ g4590 4595}#
                                                    #{tmp 4593}#))
                                              (list '#(syntax-object
                                                       "list->vector"
                                                       ((top)
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(#{ g4590}#)
                                                          #((m4591 top))
                                                          #("i4594"))
                                                        #(ribcage
                                                          #(else)
                                                          #((top))
                                                          #("i4588"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(f y k)
                                                          #((top) (top) (top))
                                                          #("i4555"
                                                            "i4556"
                                                            "i4557"))
                                                        #(ribcage
                                                          #(_)
                                                          #((top))
                                                          #("i4553"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(x)
                                                          #((top))
                                                          #("i4547"))
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
                                                          ("i4418"
                                                           "i4416"
                                                           "i4414"
                                                           "i4412"
                                                           "i4410"
                                                           "i4408"
                                                           "i4406")))
                                                       (hygiene guile))
                                                    #{ g4590 4595}#))))))))))))))
                     (#{f 4558}#
                       #{x 4546}#
                       (lambda (#{ls 4561}#)
                         (let ((#{tmp 4566}# #{ls 4561}#))
                           (let ((#{tmp 4567}#
                                   ($sc-dispatch #{tmp 4566}# 'each-any)))
                             (if #{tmp 4567}#
                               (@apply
                                 (lambda (#{ g4563 4569}#)
                                   (cons '#(syntax-object
                                            "vector"
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(#{ g4563}#)
                                               #((m4564 top))
                                               #("i4568"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(ls)
                                               #((top))
                                               #("i4562"))
                                             #(ribcage
                                               #(_)
                                               #((top))
                                               #("i4553"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4547"))
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
                                               ("i4418"
                                                "i4416"
                                                "i4414"
                                                "i4412"
                                                "i4410"
                                                "i4408"
                                                "i4406")))
                                            (hygiene guile))
                                         #{ g4563 4569}#))
                                 #{tmp 4567}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4566}#)))))))))))))
       (#{emit 4419}#
         (lambda (#{x 4596}#)
           (let ((#{tmp 4598}# #{x 4596}#))
             (let ((#{tmp 4599}#
                     ($sc-dispatch
                       #{tmp 4598}#
                       '(#(atom "quote") any))))
               (if #{tmp 4599}#
                 (@apply
                   (lambda (#{x 4601}#)
                     (list '#(syntax-object
                              quote
                              ((top)
                               #(ribcage #(x) #((top)) #("i4600"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4597"))
                               #(ribcage
                                 (emit quasivector
                                       quasilist*
                                       quasiappend
                                       quasicons
                                       vquasi
                                       quasi)
                                 ((top) (top) (top) (top) (top) (top) (top))
                                 ("i4418"
                                  "i4416"
                                  "i4414"
                                  "i4412"
                                  "i4410"
                                  "i4408"
                                  "i4406")))
                              (hygiene guile))
                           #{x 4601}#))
                   #{tmp 4599}#)
                 (let ((#{tmp 4602}#
                         ($sc-dispatch
                           #{tmp 4598}#
                           '(#(atom "list") . each-any))))
                   (if #{tmp 4602}#
                     (@apply
                       (lambda (#{x 4604}#)
                         (let ((#{tmp 4608}# (map #{emit 4419}# #{x 4604}#)))
                           (let ((#{tmp 4609}#
                                   ($sc-dispatch #{tmp 4608}# 'each-any)))
                             (if #{tmp 4609}#
                               (@apply
                                 (lambda (#{ g4605 4611}#)
                                   (cons '#(syntax-object
                                            list
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(#{ g4605}#)
                                               #((m4606 top))
                                               #("i4610"))
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4603"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4597"))
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
                                               ("i4418"
                                                "i4416"
                                                "i4414"
                                                "i4412"
                                                "i4410"
                                                "i4408"
                                                "i4406")))
                                            (hygiene guile))
                                         #{ g4605 4611}#))
                                 #{tmp 4609}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4608}#)))))
                       #{tmp 4602}#)
                     (let ((#{tmp 4614}#
                             ($sc-dispatch
                               #{tmp 4598}#
                               '(#(atom "list*") . #(each+ any (any) ())))))
                       (if #{tmp 4614}#
                         (@apply
                           (lambda (#{x 4617}# #{y 4618}#)
                             (letrec*
                               ((#{f 4621}#
                                  (lambda (#{x* 4622}#)
                                    (if (null? #{x* 4622}#)
                                      (#{emit 4419}# #{y 4618}#)
                                      (let ((#{tmp 4628}#
                                              (list (#{emit 4419}#
                                                      (car #{x* 4622}#))
                                                    (#{f 4621}#
                                                      (cdr #{x* 4622}#)))))
                                        (let ((#{tmp 4629}#
                                                ($sc-dispatch
                                                  #{tmp 4628}#
                                                  '(any any))))
                                          (if #{tmp 4629}#
                                            (@apply
                                              (lambda (#{ g4625 4632}#
                                                       #{ g4624 4633}#)
                                                (list '#(syntax-object
                                                         cons
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(#{ g4625}#
                                                              #{ g4624}#)
                                                            #((m4626 top)
                                                              (m4626 top))
                                                            #("i4630" "i4631"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(f x*)
                                                            #((top) (top))
                                                            #("i4619" "i4620"))
                                                          #(ribcage
                                                            #(x y)
                                                            #((top) (top))
                                                            #("i4615" "i4616"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x)
                                                            #((top))
                                                            #("i4597"))
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
                                                            ("i4418"
                                                             "i4416"
                                                             "i4414"
                                                             "i4412"
                                                             "i4410"
                                                             "i4408"
                                                             "i4406")))
                                                         (hygiene guile))
                                                      #{ g4625 4632}#
                                                      #{ g4624 4633}#))
                                              #{tmp 4629}#)
                                            (syntax-violation
                                              #f
                                              "source expression failed to match any pattern"
                                              #{tmp 4628}#))))))))
                               (#{f 4621}# #{x 4617}#)))
                           #{tmp 4614}#)
                         (let ((#{tmp 4634}#
                                 ($sc-dispatch
                                   #{tmp 4598}#
                                   '(#(atom "append") . each-any))))
                           (if #{tmp 4634}#
                             (@apply
                               (lambda (#{x 4636}#)
                                 (let ((#{tmp 4640}#
                                         (map #{emit 4419}# #{x 4636}#)))
                                   (let ((#{tmp 4641}#
                                           ($sc-dispatch
                                             #{tmp 4640}#
                                             'each-any)))
                                     (if #{tmp 4641}#
                                       (@apply
                                         (lambda (#{ g4637 4643}#)
                                           (cons '#(syntax-object
                                                    append
                                                    ((top)
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(#{ g4637}#)
                                                       #((m4638 top))
                                                       #("i4642"))
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4635"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4597"))
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
                                                       ("i4418"
                                                        "i4416"
                                                        "i4414"
                                                        "i4412"
                                                        "i4410"
                                                        "i4408"
                                                        "i4406")))
                                                    (hygiene guile))
                                                 #{ g4637 4643}#))
                                         #{tmp 4641}#)
                                       (syntax-violation
                                         #f
                                         "source expression failed to match any pattern"
                                         #{tmp 4640}#)))))
                               #{tmp 4634}#)
                             (let ((#{tmp 4646}#
                                     ($sc-dispatch
                                       #{tmp 4598}#
                                       '(#(atom "vector") . each-any))))
                               (if #{tmp 4646}#
                                 (@apply
                                   (lambda (#{x 4648}#)
                                     (let ((#{tmp 4652}#
                                             (map #{emit 4419}# #{x 4648}#)))
                                       (let ((#{tmp 4653}#
                                               ($sc-dispatch
                                                 #{tmp 4652}#
                                                 'each-any)))
                                         (if #{tmp 4653}#
                                           (@apply
                                             (lambda (#{ g4649 4655}#)
                                               (cons '#(syntax-object
                                                        vector
                                                        ((top)
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(#{ g4649}#)
                                                           #((m4650 top))
                                                           #("i4654"))
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4647"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4597"))
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
                                                           ("i4418"
                                                            "i4416"
                                                            "i4414"
                                                            "i4412"
                                                            "i4410"
                                                            "i4408"
                                                            "i4406")))
                                                        (hygiene guile))
                                                     #{ g4649 4655}#))
                                             #{tmp 4653}#)
                                           (syntax-violation
                                             #f
                                             "source expression failed to match any pattern"
                                             #{tmp 4652}#)))))
                                   #{tmp 4646}#)
                                 (let ((#{tmp 4658}#
                                         ($sc-dispatch
                                           #{tmp 4598}#
                                           '(#(atom "list->vector") any))))
                                   (if #{tmp 4658}#
                                     (@apply
                                       (lambda (#{x 4660}#)
                                         (let ((#{tmp 4664}#
                                                 (#{emit 4419}# #{x 4660}#)))
                                           (let ((#{ g4661 4666}#
                                                   #{tmp 4664}#))
                                             (list '#(syntax-object
                                                      list->vector
                                                      ((top)
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(#{ g4661}#)
                                                         #((m4662 top))
                                                         #("i4665"))
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4659"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4597"))
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
                                                         ("i4418"
                                                          "i4416"
                                                          "i4414"
                                                          "i4412"
                                                          "i4410"
                                                          "i4408"
                                                          "i4406")))
                                                      (hygiene guile))
                                                   #{ g4661 4666}#))))
                                       #{tmp 4658}#)
                                     (let ((#{tmp 4667}#
                                             ($sc-dispatch
                                               #{tmp 4598}#
                                               '(#(atom "value") any))))
                                       (if #{tmp 4667}#
                                         (@apply
                                           (lambda (#{x 4669}#) #{x 4669}#)
                                           #{tmp 4667}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp 4598}#)))))))))))))))))))
      (lambda (#{x 4670}#)
        (let ((#{tmp 4672}# #{x 4670}#))
          (let ((#{tmp 4673}#
                  ($sc-dispatch #{tmp 4672}# '(_ any))))
            (if #{tmp 4673}#
              (@apply
                (lambda (#{e 4675}#)
                  (#{emit 4419}# (#{quasi 4407}# #{e 4675}# 0)))
                #{tmp 4673}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 4672}#))))))))

(define include
  (make-syntax-transformer
    'include
    'macro
    (lambda (#{x 4676}#)
      (letrec*
        ((#{read-file 4679}#
           (lambda (#{fn 4680}# #{k 4681}#)
             (let ((#{p 4685}# (open-input-file #{fn 4680}#)))
               (letrec*
                 ((#{f 4689}#
                    (lambda (#{x 4690}# #{result 4691}#)
                      (if (eof-object? #{x 4690}#)
                        (begin
                          (close-input-port #{p 4685}#)
                          (reverse #{result 4691}#))
                        (#{f 4689}#
                          (read #{p 4685}#)
                          (cons (datum->syntax #{k 4681}# #{x 4690}#)
                                #{result 4691}#))))))
                 (#{f 4689}# (read #{p 4685}#) '()))))))
        (let ((#{tmp 4692}# #{x 4676}#))
          (let ((#{tmp 4693}#
                  ($sc-dispatch #{tmp 4692}# '(any any))))
            (if #{tmp 4693}#
              (@apply
                (lambda (#{k 4696}# #{filename 4697}#)
                  (let ((#{fn 4699}# (syntax->datum #{filename 4697}#)))
                    (let ((#{tmp 4701}#
                            (#{read-file 4679}#
                              #{fn 4699}#
                              #{filename 4697}#)))
                      (let ((#{tmp 4702}#
                              ($sc-dispatch #{tmp 4701}# 'each-any)))
                        (if #{tmp 4702}#
                          (@apply
                            (lambda (#{exp 4704}#)
                              (cons '#(syntax-object
                                       begin
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(exp) #((top)) #("i4703"))
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage #(fn) #((top)) #("i4698"))
                                        #(ribcage
                                          #(k filename)
                                          #((top) (top))
                                          #("i4694" "i4695"))
                                        #(ribcage
                                          (read-file)
                                          ((top))
                                          ("i4678"))
                                        #(ribcage #(x) #((top)) #("i4677")))
                                       (hygiene guile))
                                    #{exp 4704}#))
                            #{tmp 4702}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp 4701}#))))))
                #{tmp 4693}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 4692}#))))))))

(define include-from-path
  (make-syntax-transformer
    'include-from-path
    'macro
    (lambda (#{x 4706}#)
      (let ((#{tmp 4708}# #{x 4706}#))
        (let ((#{tmp 4709}#
                ($sc-dispatch #{tmp 4708}# '(any any))))
          (if #{tmp 4709}#
            (@apply
              (lambda (#{k 4712}# #{filename 4713}#)
                (let ((#{fn 4715}# (syntax->datum #{filename 4713}#)))
                  (let ((#{tmp 4717}#
                          (datum->syntax
                            #{filename 4713}#
                            (let ((#{t 4722}# (%search-load-path #{fn 4715}#)))
                              (if #{t 4722}#
                                #{t 4722}#
                                (syntax-violation
                                  'include-from-path
                                  "file not found in path"
                                  #{x 4706}#
                                  #{filename 4713}#))))))
                    (let ((#{fn 4719}# #{tmp 4717}#))
                      (list '#(syntax-object
                               include
                               ((top)
                                #(ribcage () () ())
                                #(ribcage #(fn) #((top)) #("i4718"))
                                #(ribcage () () ())
                                #(ribcage () () ())
                                #(ribcage #(fn) #((top)) #("i4714"))
                                #(ribcage
                                  #(k filename)
                                  #((top) (top))
                                  #("i4710" "i4711"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4707")))
                               (hygiene guile))
                            #{fn 4719}#)))))
              #{tmp 4709}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4708}#)))))))

(define unquote
  (make-syntax-transformer
    'unquote
    'macro
    (lambda (#{x 4724}#)
      (syntax-violation
        'unquote
        "expression not valid outside of quasiquote"
        #{x 4724}#))))

(define unquote-splicing
  (make-syntax-transformer
    'unquote-splicing
    'macro
    (lambda (#{x 4726}#)
      (syntax-violation
        'unquote-splicing
        "expression not valid outside of quasiquote"
        #{x 4726}#))))

(define case
  (make-syntax-transformer
    'case
    'macro
    (lambda (#{x 4728}#)
      (let ((#{tmp 4730}# #{x 4728}#))
        (let ((#{tmp 4731}#
                ($sc-dispatch
                  #{tmp 4730}#
                  '(_ any any . each-any))))
          (if #{tmp 4731}#
            (@apply
              (lambda (#{e 4735}# #{m1 4736}# #{m2 4737}#)
                (let ((#{tmp 4739}#
                        (letrec*
                          ((#{f 4745}#
                             (lambda (#{clause 4746}# #{clauses 4747}#)
                               (if (null? #{clauses 4747}#)
                                 (let ((#{tmp 4749}# #{clause 4746}#))
                                   (let ((#{tmp 4750}#
                                           ($sc-dispatch
                                             #{tmp 4749}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   else
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(f clause clauses)
                                                      #((top) (top) (top))
                                                      #("i4742"
                                                        "i4743"
                                                        "i4744"))
                                                    #(ribcage
                                                      #(e m1 m2)
                                                      #((top) (top) (top))
                                                      #("i4732"
                                                        "i4733"
                                                        "i4734"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i4729")))
                                                   (hygiene guile)))
                                               any
                                               .
                                               each-any))))
                                     (if #{tmp 4750}#
                                       (@apply
                                         (lambda (#{e1 4753}# #{e2 4754}#)
                                           (cons '#(syntax-object
                                                    begin
                                                    ((top)
                                                     #(ribcage
                                                       #(e1 e2)
                                                       #((top) (top))
                                                       #("i4751" "i4752"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(f clause clauses)
                                                       #((top) (top) (top))
                                                       #("i4742"
                                                         "i4743"
                                                         "i4744"))
                                                     #(ribcage
                                                       #(e m1 m2)
                                                       #((top) (top) (top))
                                                       #("i4732"
                                                         "i4733"
                                                         "i4734"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4729")))
                                                    (hygiene guile))
                                                 (cons #{e1 4753}#
                                                       #{e2 4754}#)))
                                         #{tmp 4750}#)
                                       (let ((#{tmp 4756}#
                                               ($sc-dispatch
                                                 #{tmp 4749}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp 4756}#
                                           (@apply
                                             (lambda (#{k 4760}#
                                                      #{e1 4761}#
                                                      #{e2 4762}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4757"
                                                             "i4758"
                                                             "i4759"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4742"
                                                             "i4743"
                                                             "i4744"))
                                                         #(ribcage
                                                           #(e m1 m2)
                                                           #((top) (top) (top))
                                                           #("i4732"
                                                             "i4733"
                                                             "i4734"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4729")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4757"
                                                                   "i4758"
                                                                   "i4759"))
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
                                                                 #("i4742"
                                                                   "i4743"
                                                                   "i4744"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4732"
                                                                   "i4733"
                                                                   "i4734"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4729")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4757"
                                                                   "i4758"
                                                                   "i4759"))
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
                                                                 #("i4742"
                                                                   "i4743"
                                                                   "i4744"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4732"
                                                                   "i4733"
                                                                   "i4734"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4729")))
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
                                                                       #("i4757"
                                                                         "i4758"
                                                                         "i4759"))
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
                                                                       #("i4742"
                                                                         "i4743"
                                                                         "i4744"))
                                                                     #(ribcage
                                                                       #(e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4732"
                                                                         "i4733"
                                                                         "i4734"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4729")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k 4760}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4757"
                                                                   "i4758"
                                                                   "i4759"))
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
                                                                 #("i4742"
                                                                   "i4743"
                                                                   "i4744"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4732"
                                                                   "i4733"
                                                                   "i4734"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4729")))
                                                              (hygiene guile))
                                                           (cons #{e1 4761}#
                                                                 #{e2 4762}#))))
                                             #{tmp 4756}#)
                                           (let ((#{_ 4766}# #{tmp 4749}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x 4728}#
                                               #{clause 4746}#)))))))
                                 (let ((#{tmp 4768}#
                                         (#{f 4745}#
                                           (car #{clauses 4747}#)
                                           (cdr #{clauses 4747}#))))
                                   (let ((#{rest 4770}# #{tmp 4768}#))
                                     (let ((#{tmp 4771}# #{clause 4746}#))
                                       (let ((#{tmp 4772}#
                                               ($sc-dispatch
                                                 #{tmp 4771}#
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
                                                           #(rest)
                                                           #((top))
                                                           #("i4769"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4742"
                                                             "i4743"
                                                             "i4744"))
                                                         #(ribcage
                                                           #(e m1 m2)
                                                           #((top) (top) (top))
                                                           #("i4732"
                                                             "i4733"
                                                             "i4734"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4729")))
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
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4769"))
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
                                                                 #("i4742"
                                                                   "i4743"
                                                                   "i4744"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4732"
                                                                   "i4733"
                                                                   "i4734"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4729")))
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
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4769"))
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
                                                                 #("i4742"
                                                                   "i4743"
                                                                   "i4744"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4732"
                                                                   "i4733"
                                                                   "i4734"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4729")))
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
                                                                       #(rest)
                                                                       #((top))
                                                                       #("i4769"))
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
                                                                       #("i4742"
                                                                         "i4743"
                                                                         "i4744"))
                                                                     #(ribcage
                                                                       #(e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4732"
                                                                         "i4733"
                                                                         "i4734"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4729")))
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
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4769"))
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
                                                                 #("i4742"
                                                                   "i4743"
                                                                   "i4744"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4732"
                                                                   "i4733"
                                                                   "i4734"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4729")))
                                                              (hygiene guile))
                                                           (cons #{e1 4777}#
                                                                 #{e2 4778}#))
                                                     #{rest 4770}#))
                                             #{tmp 4772}#)
                                           (let ((#{_ 4782}# #{tmp 4771}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x 4728}#
                                               #{clause 4746}#)))))))))))
                          (#{f 4745}# #{m1 4736}# #{m2 4737}#))))
                  (let ((#{body 4741}# #{tmp 4739}#))
                    (list '#(syntax-object
                             let
                             ((top)
                              #(ribcage () () ())
                              #(ribcage #(body) #((top)) #("i4740"))
                              #(ribcage
                                #(e m1 m2)
                                #((top) (top) (top))
                                #("i4732" "i4733" "i4734"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4729")))
                             (hygiene guile))
                          (list (list '#(syntax-object
                                         t
                                         ((top)
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(body)
                                            #((top))
                                            #("i4740"))
                                          #(ribcage
                                            #(e m1 m2)
                                            #((top) (top) (top))
                                            #("i4732" "i4733" "i4734"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4729")))
                                         (hygiene guile))
                                      #{e 4735}#))
                          #{body 4741}#))))
              #{tmp 4731}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4730}#)))))))

(define make-variable-transformer
  (lambda (#{proc 4783}#)
    (if (procedure? #{proc 4783}#)
      (letrec*
        ((#{trans 4786}#
           (lambda (#{x 4787}#) (#{proc 4783}# #{x 4787}#))))
        (begin
          (set-procedure-property!
            #{trans 4786}#
            'variable-transformer
            #t)
          #{trans 4786}#))
      (error "variable transformer not a procedure"
             #{proc 4783}#))))

(define identifier-syntax
  (make-syntax-transformer
    'identifier-syntax
    'macro
    (lambda (#{x 4789}#)
      (let ((#{tmp 4791}# #{x 4789}#))
        (let ((#{tmp 4792}#
                ($sc-dispatch #{tmp 4791}# '(_ any))))
          (if #{tmp 4792}#
            (@apply
              (lambda (#{e 4794}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage #(e) #((top)) #("i4793"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4790")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage #(e) #((top)) #("i4793"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4790")))
                          (hygiene guile)))
                      '#((#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage #(e) #((top)) #("i4793"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4790")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            identifier-syntax
                            ((top)
                             #(ribcage #(e) #((top)) #("i4793"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4790")))
                            (hygiene guile))))
                      (list '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage #(e) #((top)) #("i4793"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4790")))
                               (hygiene guile))
                            '#(syntax-object
                               x
                               ((top)
                                #(ribcage #(e) #((top)) #("i4793"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4790")))
                               (hygiene guile))
                            '()
                            (list '#(syntax-object
                                     id
                                     ((top)
                                      #(ribcage #(e) #((top)) #("i4793"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4790")))
                                     (hygiene guile))
                                  '(#(syntax-object
                                      identifier?
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4793"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4790")))
                                      (hygiene guile))
                                    (#(syntax-object
                                       syntax
                                       ((top)
                                        #(ribcage #(e) #((top)) #("i4793"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4790")))
                                       (hygiene guile))
                                     #(syntax-object
                                       id
                                       ((top)
                                        #(ribcage #(e) #((top)) #("i4793"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4790")))
                                       (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage #(e) #((top)) #("i4793"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4790")))
                                           (hygiene guile))
                                        #{e 4794}#))
                            (list '(#(syntax-object
                                      _
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4793"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4790")))
                                      (hygiene guile))
                                    #(syntax-object
                                      x
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4793"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4790")))
                                      (hygiene guile))
                                    #(syntax-object
                                      ...
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4793"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4790")))
                                      (hygiene guile)))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage #(e) #((top)) #("i4793"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4790")))
                                           (hygiene guile))
                                        (cons #{e 4794}#
                                              '(#(syntax-object
                                                  x
                                                  ((top)
                                                   #(ribcage
                                                     #(e)
                                                     #((top))
                                                     #("i4793"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4790")))
                                                  (hygiene guile))
                                                #(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(e)
                                                     #((top))
                                                     #("i4793"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4790")))
                                                  (hygiene guile)))))))))
              #{tmp 4792}#)
            (let ((#{tmp 4795}#
                    ($sc-dispatch
                      #{tmp 4791}#
                      '(_ (any any)
                          ((#(free-id
                              #(syntax-object
                                set!
                                ((top)
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4790")))
                                (hygiene guile)))
                            any
                            any)
                           any)))))
              (if (if #{tmp 4795}#
                    (@apply
                      (lambda (#{id 4801}#
                               #{exp1 4802}#
                               #{var 4803}#
                               #{val 4804}#
                               #{exp2 4805}#)
                        (if (identifier? #{id 4801}#)
                          (identifier? #{var 4803}#)
                          #f))
                      #{tmp 4795}#)
                    #f)
                (@apply
                  (lambda (#{id 4813}#
                           #{exp1 4814}#
                           #{var 4815}#
                           #{val 4816}#
                           #{exp2 4817}#)
                    (list '#(syntax-object
                             make-variable-transformer
                             ((top)
                              #(ribcage
                                #(id exp1 var val exp2)
                                #((top) (top) (top) (top) (top))
                                #("i4808" "i4809" "i4810" "i4811" "i4812"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4790")))
                             (hygiene guile))
                          (list '#(syntax-object
                                   lambda
                                   ((top)
                                    #(ribcage
                                      #(id exp1 var val exp2)
                                      #((top) (top) (top) (top) (top))
                                      #("i4808"
                                        "i4809"
                                        "i4810"
                                        "i4811"
                                        "i4812"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i4790")))
                                   (hygiene guile))
                                '(#(syntax-object
                                    x
                                    ((top)
                                     #(ribcage
                                       #(id exp1 var val exp2)
                                       #((top) (top) (top) (top) (top))
                                       #("i4808"
                                         "i4809"
                                         "i4810"
                                         "i4811"
                                         "i4812"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i4790")))
                                    (hygiene guile)))
                                '#((#(syntax-object
                                      macro-type
                                      ((top)
                                       #(ribcage
                                         #(id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top))
                                         #("i4808"
                                           "i4809"
                                           "i4810"
                                           "i4811"
                                           "i4812"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4790")))
                                      (hygiene guile))
                                    .
                                    #(syntax-object
                                      variable-transformer
                                      ((top)
                                       #(ribcage
                                         #(id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top))
                                         #("i4808"
                                           "i4809"
                                           "i4810"
                                           "i4811"
                                           "i4812"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4790")))
                                      (hygiene guile))))
                                (list '#(syntax-object
                                         syntax-case
                                         ((top)
                                          #(ribcage
                                            #(id exp1 var val exp2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4808"
                                              "i4809"
                                              "i4810"
                                              "i4811"
                                              "i4812"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4790")))
                                         (hygiene guile))
                                      '#(syntax-object
                                         x
                                         ((top)
                                          #(ribcage
                                            #(id exp1 var val exp2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4808"
                                              "i4809"
                                              "i4810"
                                              "i4811"
                                              "i4812"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4790")))
                                         (hygiene guile))
                                      '(#(syntax-object
                                          set!
                                          ((top)
                                           #(ribcage
                                             #(id exp1 var val exp2)
                                             #((top) (top) (top) (top) (top))
                                             #("i4808"
                                               "i4809"
                                               "i4810"
                                               "i4811"
                                               "i4812"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i4790")))
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
                                                        #("i4808"
                                                          "i4809"
                                                          "i4810"
                                                          "i4811"
                                                          "i4812"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4790")))
                                                     (hygiene guile))
                                                  #{var 4815}#
                                                  #{val 4816}#)
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
                                                        #("i4808"
                                                          "i4809"
                                                          "i4810"
                                                          "i4811"
                                                          "i4812"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4790")))
                                                     (hygiene guile))
                                                  #{exp2 4817}#))
                                      (list (cons #{id 4813}#
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
                                                         #("i4808"
                                                           "i4809"
                                                           "i4810"
                                                           "i4811"
                                                           "i4812"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4790")))
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
                                                         #("i4808"
                                                           "i4809"
                                                           "i4810"
                                                           "i4811"
                                                           "i4812"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4790")))
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
                                                        #("i4808"
                                                          "i4809"
                                                          "i4810"
                                                          "i4811"
                                                          "i4812"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4790")))
                                                     (hygiene guile))
                                                  (cons #{exp1 4814}#
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
                                                               #("i4808"
                                                                 "i4809"
                                                                 "i4810"
                                                                 "i4811"
                                                                 "i4812"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4790")))
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
                                                               #("i4808"
                                                                 "i4809"
                                                                 "i4810"
                                                                 "i4811"
                                                                 "i4812"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4790")))
                                                            (hygiene
                                                              guile))))))
                                      (list #{id 4813}#
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
                                                        #("i4808"
                                                          "i4809"
                                                          "i4810"
                                                          "i4811"
                                                          "i4812"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4790")))
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
                                                              #("i4808"
                                                                "i4809"
                                                                "i4810"
                                                                "i4811"
                                                                "i4812"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(x)
                                                              #((top))
                                                              #("i4790")))
                                                           (hygiene guile))
                                                        #{id 4813}#))
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
                                                        #("i4808"
                                                          "i4809"
                                                          "i4810"
                                                          "i4811"
                                                          "i4812"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4790")))
                                                     (hygiene guile))
                                                  #{exp1 4814}#))))))
                  #{tmp 4795}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4791}#)))))))))

(define define*
  (make-syntax-transformer
    'define*
    'macro
    (lambda (#{x 4818}#)
      (let ((#{tmp 4820}# #{x 4818}#))
        (let ((#{tmp 4821}#
                ($sc-dispatch
                  #{tmp 4820}#
                  '(_ (any . any) any . each-any))))
          (if #{tmp 4821}#
            (@apply
              (lambda (#{id 4826}#
                       #{args 4827}#
                       #{b0 4828}#
                       #{b1 4829}#)
                (list '#(syntax-object
                         define
                         ((top)
                          #(ribcage
                            #(id args b0 b1)
                            #((top) (top) (top) (top))
                            #("i4822" "i4823" "i4824" "i4825"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4819")))
                         (hygiene guile))
                      #{id 4826}#
                      (cons '#(syntax-object
                               lambda*
                               ((top)
                                #(ribcage
                                  #(id args b0 b1)
                                  #((top) (top) (top) (top))
                                  #("i4822" "i4823" "i4824" "i4825"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4819")))
                               (hygiene guile))
                            (cons #{args 4827}#
                                  (cons #{b0 4828}# #{b1 4829}#)))))
              #{tmp 4821}#)
            (let ((#{tmp 4831}#
                    ($sc-dispatch #{tmp 4820}# '(_ any any))))
              (if (if #{tmp 4831}#
                    (@apply
                      (lambda (#{id 4834}# #{val 4835}#)
                        (identifier?
                          '#(syntax-object
                             x
                             ((top)
                              #(ribcage
                                #(id val)
                                #((top) (top))
                                #("i4832" "i4833"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4819")))
                             (hygiene guile))))
                      #{tmp 4831}#)
                    #f)
                (@apply
                  (lambda (#{id 4838}# #{val 4839}#)
                    (list '#(syntax-object
                             define
                             ((top)
                              #(ribcage
                                #(id val)
                                #((top) (top))
                                #("i4836" "i4837"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4819")))
                             (hygiene guile))
                          #{id 4838}#
                          #{val 4839}#))
                  #{tmp 4831}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4820}#)))))))))

