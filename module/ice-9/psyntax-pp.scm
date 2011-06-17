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
                  (lambda (#{p* 3716}# #{n 3717}# #{ids 3718}#)
                    (if (null? #{p* 3716}#)
                      (values '() #{ids 3718}#)
                      (call-with-values
                        (lambda ()
                          (#{cvt* 3713}#
                            (cdr #{p* 3716}#)
                            #{n 3717}#
                            #{ids 3718}#))
                        (lambda (#{y 3722}# #{ids 3723}#)
                          (call-with-values
                            (lambda ()
                              (#{cvt 3715}#
                                (car #{p* 3716}#)
                                #{n 3717}#
                                #{ids 3723}#))
                            (lambda (#{x 3726}# #{ids 3727}#)
                              (values
                                (cons #{x 3726}# #{y 3722}#)
                                #{ids 3727}#))))))))
                (#{cvt 3715}#
                  (lambda (#{p 3730}# #{n 3731}# #{ids 3732}#)
                    (if (#{id? 343}# #{p 3730}#)
                      (if (#{bound-id-member? 407}#
                            #{p 3730}#
                            #{keys 3709}#)
                        (values
                          (vector 'free-id #{p 3730}#)
                          #{ids 3732}#)
                        (if (#{free-id=? 399}#
                              #{p 3730}#
                              '#(syntax-object
                                 _
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage
                                    #(p n ids)
                                    #((top) (top) (top))
                                    #("i3733" "i3734" "i3735"))
                                  #(ribcage
                                    (cvt cvt*)
                                    ((top) (top))
                                    ("i3714" "i3712"))
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
                          (values '_ #{ids 3732}#)
                          (values
                            'any
                            (cons (cons #{p 3730}# #{n 3731}#) #{ids 3732}#))))
                      (let ((#{tmp 3741}# #{p 3730}#))
                        (let ((#{tmp 3742}#
                                ($sc-dispatch #{tmp 3741}# '(any any))))
                          (if (if #{tmp 3742}#
                                (@apply
                                  (lambda (#{x 3745}# #{dots 3746}#)
                                    (#{ellipsis? 439}# #{dots 3746}#))
                                  #{tmp 3742}#)
                                #f)
                            (@apply
                              (lambda (#{x 3749}# #{dots 3750}#)
                                (call-with-values
                                  (lambda ()
                                    (#{cvt 3715}#
                                      #{x 3749}#
                                      (#{1+}# #{n 3731}#)
                                      #{ids 3732}#))
                                  (lambda (#{p 3752}# #{ids 3753}#)
                                    (values
                                      (if (eq? #{p 3752}# 'any)
                                        'each-any
                                        (vector 'each #{p 3752}#))
                                      #{ids 3753}#))))
                              #{tmp 3742}#)
                            (let ((#{tmp 3756}#
                                    ($sc-dispatch
                                      #{tmp 3741}#
                                      '(any any . each-any))))
                              (if (if #{tmp 3756}#
                                    (@apply
                                      (lambda (#{x 3760}#
                                               #{dots 3761}#
                                               #{ys 3762}#)
                                        (#{ellipsis? 439}# #{dots 3761}#))
                                      #{tmp 3756}#)
                                    #f)
                                (@apply
                                  (lambda (#{x 3766}#
                                           #{dots 3767}#
                                           #{ys 3768}#)
                                    (call-with-values
                                      (lambda ()
                                        (#{cvt* 3713}#
                                          #{ys 3768}#
                                          #{n 3731}#
                                          #{ids 3732}#))
                                      (lambda (#{ys 3770}# #{ids 3771}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt 3715}#
                                              #{x 3766}#
                                              (#{1+}# #{n 3731}#)
                                              #{ids 3771}#))
                                          (lambda (#{x 3774}# #{ids 3775}#)
                                            (values
                                              (vector
                                                'each+
                                                #{x 3774}#
                                                (reverse #{ys 3770}#)
                                                '())
                                              #{ids 3775}#))))))
                                  #{tmp 3756}#)
                                (let ((#{tmp 3779}#
                                        ($sc-dispatch
                                          #{tmp 3741}#
                                          '(any . any))))
                                  (if #{tmp 3779}#
                                    (@apply
                                      (lambda (#{x 3782}# #{y 3783}#)
                                        (call-with-values
                                          (lambda ()
                                            (#{cvt 3715}#
                                              #{y 3783}#
                                              #{n 3731}#
                                              #{ids 3732}#))
                                          (lambda (#{y 3784}# #{ids 3785}#)
                                            (call-with-values
                                              (lambda ()
                                                (#{cvt 3715}#
                                                  #{x 3782}#
                                                  #{n 3731}#
                                                  #{ids 3785}#))
                                              (lambda (#{x 3788}# #{ids 3789}#)
                                                (values
                                                  (cons #{x 3788}# #{y 3784}#)
                                                  #{ids 3789}#))))))
                                      #{tmp 3779}#)
                                    (let ((#{tmp 3792}#
                                            ($sc-dispatch #{tmp 3741}# '())))
                                      (if #{tmp 3792}#
                                        (@apply
                                          (lambda () (values '() #{ids 3732}#))
                                          #{tmp 3792}#)
                                        (let ((#{tmp 3793}#
                                                ($sc-dispatch
                                                  #{tmp 3741}#
                                                  '#(vector each-any))))
                                          (if #{tmp 3793}#
                                            (@apply
                                              (lambda (#{x 3795}#)
                                                (call-with-values
                                                  (lambda ()
                                                    (#{cvt 3715}#
                                                      #{x 3795}#
                                                      #{n 3731}#
                                                      #{ids 3732}#))
                                                  (lambda (#{p 3797}#
                                                           #{ids 3798}#)
                                                    (values
                                                      (vector
                                                        'vector
                                                        #{p 3797}#)
                                                      #{ids 3798}#))))
                                              #{tmp 3793}#)
                                            (let ((#{x 3802}# #{tmp 3741}#))
                                              (values
                                                (vector
                                                  'atom
                                                  (#{strip 449}#
                                                    #{p 3730}#
                                                    '(())))
                                                #{ids 3732}#)))))))))))))))))
               (#{cvt 3715}# #{pattern 3708}# 0 '()))))
         (#{build-dispatch-call 3703}#
           (lambda (#{pvars 3804}#
                    #{exp 3805}#
                    #{y 3806}#
                    #{r 3807}#
                    #{mod 3808}#)
             (begin
               (map cdr #{pvars 3804}#)
               (let ((#{ids 3816}# (map car #{pvars 3804}#)))
                 (let ((#{labels 3820}#
                         (#{gen-labels 358}# #{ids 3816}#))
                       (#{new-vars 3821}#
                         (map #{gen-var 451}# #{ids 3816}#)))
                   (#{build-primcall 291}#
                     #f
                     'apply
                     (list (#{build-simple-lambda 285}#
                             #f
                             (map syntax->datum #{ids 3816}#)
                             #f
                             #{new-vars 3821}#
                             '()
                             (#{chi 423}#
                               #{exp 3805}#
                               (#{extend-env 331}#
                                 #{labels 3820}#
                                 (map (lambda (#{var 3824}# #{level 3825}#)
                                        (cons 'syntax
                                              (cons #{var 3824}#
                                                    #{level 3825}#)))
                                      #{new-vars 3821}#
                                      (map cdr #{pvars 3804}#))
                                 #{r 3807}#)
                               (#{make-binding-wrap 387}#
                                 #{ids 3816}#
                                 #{labels 3820}#
                                 '(()))
                               #{mod 3808}#))
                           #{y 3806}#)))))))
         (#{gen-clause 3705}#
           (lambda (#{x 3831}#
                    #{keys 3832}#
                    #{clauses 3833}#
                    #{r 3834}#
                    #{pat 3835}#
                    #{fender 3836}#
                    #{exp 3837}#
                    #{mod 3838}#)
             (call-with-values
               (lambda ()
                 (#{convert-pattern 3701}#
                   #{pat 3835}#
                   #{keys 3832}#))
               (lambda (#{p 3847}# #{pvars 3848}#)
                 (if (not (#{distinct-bound-ids? 405}#
                            (map car #{pvars 3848}#)))
                   (syntax-violation
                     'syntax-case
                     "duplicate pattern variable"
                     #{pat 3835}#)
                   (if (not (and-map
                              (lambda (#{x 3855}#)
                                (not (#{ellipsis? 439}# (car #{x 3855}#))))
                              #{pvars 3848}#))
                     (syntax-violation
                       'syntax-case
                       "misplaced ellipsis"
                       #{pat 3835}#)
                     (let ((#{y 3859}# (#{gen-var 451}# 'tmp)))
                       (#{build-call 267}#
                         #f
                         (#{build-simple-lambda 285}#
                           #f
                           (list 'tmp)
                           #f
                           (list #{y 3859}#)
                           '()
                           (let ((#{y 3863}#
                                   (#{build-lexical-reference 273}#
                                     'value
                                     #f
                                     'tmp
                                     #{y 3859}#)))
                             (#{build-conditional 269}#
                               #f
                               (let ((#{tmp 3866}# #{fender 3836}#))
                                 (let ((#{tmp 3867}#
                                         ($sc-dispatch
                                           #{tmp 3866}#
                                           '#(atom #t))))
                                   (if #{tmp 3867}#
                                     (@apply
                                       (lambda () #{y 3863}#)
                                       #{tmp 3867}#)
                                     (let ((#{_ 3869}# #{tmp 3866}#))
                                       (#{build-conditional 269}#
                                         #f
                                         #{y 3863}#
                                         (#{build-dispatch-call 3703}#
                                           #{pvars 3848}#
                                           #{fender 3836}#
                                           #{y 3863}#
                                           #{r 3834}#
                                           #{mod 3838}#)
                                         (#{build-data 295}# #f #f))))))
                               (#{build-dispatch-call 3703}#
                                 #{pvars 3848}#
                                 #{exp 3837}#
                                 #{y 3863}#
                                 #{r 3834}#
                                 #{mod 3838}#)
                               (#{gen-syntax-case 3707}#
                                 #{x 3831}#
                                 #{keys 3832}#
                                 #{clauses 3833}#
                                 #{r 3834}#
                                 #{mod 3838}#))))
                         (list (if (eq? #{p 3847}# 'any)
                                 (#{build-primcall 291}#
                                   #f
                                   'list
                                   (list #{x 3831}#))
                                 (#{build-primcall 291}#
                                   #f
                                   '$sc-dispatch
                                   (list #{x 3831}#
                                         (#{build-data 295}#
                                           #f
                                           #{p 3847}#)))))))))))))
         (#{gen-syntax-case 3707}#
           (lambda (#{x 3875}#
                    #{keys 3876}#
                    #{clauses 3877}#
                    #{r 3878}#
                    #{mod 3879}#)
             (if (null? #{clauses 3877}#)
               (#{build-primcall 291}#
                 #f
                 'syntax-violation
                 (list (#{build-data 295}# #f #f)
                       (#{build-data 295}#
                         #f
                         "source expression failed to match any pattern")
                       #{x 3875}#))
               (let ((#{tmp 3888}# (car #{clauses 3877}#)))
                 (let ((#{tmp 3889}#
                         ($sc-dispatch #{tmp 3888}# '(any any))))
                   (if #{tmp 3889}#
                     (@apply
                       (lambda (#{pat 3892}# #{exp 3893}#)
                         (if (if (#{id? 343}# #{pat 3892}#)
                               (and-map
                                 (lambda (#{x 3896}#)
                                   (not (#{free-id=? 399}#
                                          #{pat 3892}#
                                          #{x 3896}#)))
                                 (cons '#(syntax-object
                                          ...
                                          ((top)
                                           #(ribcage
                                             #(pat exp)
                                             #((top) (top))
                                             #("i3890" "i3891"))
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(x keys clauses r mod)
                                             #((top) (top) (top) (top) (top))
                                             #("i3880"
                                               "i3881"
                                               "i3882"
                                               "i3883"
                                               "i3884"))
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
                                       #{keys 3876}#))
                               #f)
                           (if (#{free-id=? 399}#
                                 '#(syntax-object
                                    pad
                                    ((top)
                                     #(ribcage
                                       #(pat exp)
                                       #((top) (top))
                                       #("i3890" "i3891"))
                                     #(ribcage () () ())
                                     #(ribcage
                                       #(x keys clauses r mod)
                                       #((top) (top) (top) (top) (top))
                                       #("i3880"
                                         "i3881"
                                         "i3882"
                                         "i3883"
                                         "i3884"))
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
                                       #("i3890" "i3891"))
                                     #(ribcage () () ())
                                     #(ribcage
                                       #(x keys clauses r mod)
                                       #((top) (top) (top) (top) (top))
                                       #("i3880"
                                         "i3881"
                                         "i3882"
                                         "i3883"
                                         "i3884"))
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
                               #{exp 3893}#
                               #{r 3878}#
                               '(())
                               #{mod 3879}#)
                             (let ((#{labels 3901}# (list (#{gen-label 356}#)))
                                   (#{var 3902}#
                                     (#{gen-var 451}# #{pat 3892}#)))
                               (#{build-call 267}#
                                 #f
                                 (#{build-simple-lambda 285}#
                                   #f
                                   (list (syntax->datum #{pat 3892}#))
                                   #f
                                   (list #{var 3902}#)
                                   '()
                                   (#{chi 423}#
                                     #{exp 3893}#
                                     (#{extend-env 331}#
                                       #{labels 3901}#
                                       (list (cons 'syntax
                                                   (cons #{var 3902}# 0)))
                                       #{r 3878}#)
                                     (#{make-binding-wrap 387}#
                                       (list #{pat 3892}#)
                                       #{labels 3901}#
                                       '(()))
                                     #{mod 3879}#))
                                 (list #{x 3875}#))))
                           (#{gen-clause 3705}#
                             #{x 3875}#
                             #{keys 3876}#
                             (cdr #{clauses 3877}#)
                             #{r 3878}#
                             #{pat 3892}#
                             #t
                             #{exp 3893}#
                             #{mod 3879}#)))
                       #{tmp 3889}#)
                     (let ((#{tmp 3908}#
                             ($sc-dispatch #{tmp 3888}# '(any any any))))
                       (if #{tmp 3908}#
                         (@apply
                           (lambda (#{pat 3912}# #{fender 3913}# #{exp 3914}#)
                             (#{gen-clause 3705}#
                               #{x 3875}#
                               #{keys 3876}#
                               (cdr #{clauses 3877}#)
                               #{r 3878}#
                               #{pat 3912}#
                               #{fender 3913}#
                               #{exp 3914}#
                               #{mod 3879}#))
                           #{tmp 3908}#)
                         (let ((#{_ 3916}# #{tmp 3888}#))
                           (syntax-violation
                             'syntax-case
                             "invalid clause"
                             (car #{clauses 3877}#))))))))))))
        (lambda (#{e 3917}#
                 #{r 3918}#
                 #{w 3919}#
                 #{s 3920}#
                 #{mod 3921}#)
          (let ((#{e 3928}#
                  (#{source-wrap 411}#
                    #{e 3917}#
                    #{w 3919}#
                    #{s 3920}#
                    #{mod 3921}#)))
            (let ((#{tmp 3929}# #{e 3928}#))
              (let ((#{tmp 3930}#
                      ($sc-dispatch
                        #{tmp 3929}#
                        '(_ any each-any . each-any))))
                (if #{tmp 3930}#
                  (@apply
                    (lambda (#{val 3934}# #{key 3935}# #{m 3936}#)
                      (if (and-map
                            (lambda (#{x 3937}#)
                              (if (#{id? 343}# #{x 3937}#)
                                (not (#{ellipsis? 439}# #{x 3937}#))
                                #f))
                            #{key 3935}#)
                        (let ((#{x 3943}# (#{gen-var 451}# 'tmp)))
                          (#{build-call 267}#
                            #{s 3920}#
                            (#{build-simple-lambda 285}#
                              #f
                              (list 'tmp)
                              #f
                              (list #{x 3943}#)
                              '()
                              (#{gen-syntax-case 3707}#
                                (#{build-lexical-reference 273}#
                                  'value
                                  #f
                                  'tmp
                                  #{x 3943}#)
                                #{key 3935}#
                                #{m 3936}#
                                #{r 3918}#
                                #{mod 3921}#))
                            (list (#{chi 423}#
                                    #{val 3934}#
                                    #{r 3918}#
                                    '(())
                                    #{mod 3921}#))))
                        (syntax-violation
                          'syntax-case
                          "invalid literals list"
                          #{e 3928}#)))
                    #{tmp 3930}#)
                  (syntax-violation
                    #f
                    "source expression failed to match any pattern"
                    #{tmp 3929}#))))))))
    (set! macroexpand
      (lambda*
        (#{x 3949}#
          #:optional
          (#{m 3951}# 'e)
          (#{esew 3953}# '(eval)))
        (#{chi-top-sequence 415}#
          (list #{x 3949}#)
          '()
          '((top))
          #f
          #{m 3951}#
          #{esew 3953}#
          (cons 'hygiene (module-name (current-module))))))
    (set! identifier?
      (lambda (#{x 3957}#)
        (#{nonsymbol-id? 341}# #{x 3957}#)))
    (set! datum->syntax
      (lambda (#{id 3959}# #{datum 3960}#)
        (#{make-syntax-object 307}#
          #{datum 3960}#
          (#{syntax-object-wrap 313}# #{id 3959}#)
          (#{syntax-object-module 315}# #{id 3959}#))))
    (set! syntax->datum
      (lambda (#{x 3963}#)
        (#{strip 449}# #{x 3963}# '(()))))
    (set! syntax-source
      (lambda (#{x 3966}#)
        (#{source-annotation 324}# #{x 3966}#)))
    (set! generate-temporaries
      (lambda (#{ls 3968}#)
        (begin
          (let ((#{x 3972}# #{ls 3968}#))
            (if (not (list? #{x 3972}#))
              (syntax-violation
                'generate-temporaries
                "invalid argument"
                #{x 3972}#)))
          (let ((#{mod 3974}#
                  (cons 'hygiene (module-name (current-module)))))
            (map (lambda (#{x 3975}#)
                   (#{wrap 409}# (gensym) '((top)) #{mod 3974}#))
                 #{ls 3968}#)))))
    (set! free-identifier=?
      (lambda (#{x 3979}# #{y 3980}#)
        (begin
          (let ((#{x 3985}# #{x 3979}#))
            (if (not (#{nonsymbol-id? 341}# #{x 3985}#))
              (syntax-violation
                'free-identifier=?
                "invalid argument"
                #{x 3985}#)))
          (let ((#{x 3988}# #{y 3980}#))
            (if (not (#{nonsymbol-id? 341}# #{x 3988}#))
              (syntax-violation
                'free-identifier=?
                "invalid argument"
                #{x 3988}#)))
          (#{free-id=? 399}# #{x 3979}# #{y 3980}#))))
    (set! bound-identifier=?
      (lambda (#{x 3989}# #{y 3990}#)
        (begin
          (let ((#{x 3995}# #{x 3989}#))
            (if (not (#{nonsymbol-id? 341}# #{x 3995}#))
              (syntax-violation
                'bound-identifier=?
                "invalid argument"
                #{x 3995}#)))
          (let ((#{x 3998}# #{y 3990}#))
            (if (not (#{nonsymbol-id? 341}# #{x 3998}#))
              (syntax-violation
                'bound-identifier=?
                "invalid argument"
                #{x 3998}#)))
          (#{bound-id=? 401}# #{x 3989}# #{y 3990}#))))
    (set! syntax-violation
      (lambda*
        (#{who 3999}#
          #{message 4000}#
          #{form 4001}#
          #:optional
          (#{subform 4005}# #f))
        (begin
          (let ((#{x 4009}# #{who 3999}#))
            (if (not (let ((#{x 4010}# #{x 4009}#))
                       (let ((#{t 4014}# (not #{x 4010}#)))
                         (if #{t 4014}#
                           #{t 4014}#
                           (let ((#{t 4017}# (string? #{x 4010}#)))
                             (if #{t 4017}#
                               #{t 4017}#
                               (symbol? #{x 4010}#)))))))
              (syntax-violation
                'syntax-violation
                "invalid argument"
                #{x 4009}#)))
          (let ((#{x 4021}# #{message 4000}#))
            (if (not (string? #{x 4021}#))
              (syntax-violation
                'syntax-violation
                "invalid argument"
                #{x 4021}#)))
          (throw 'syntax-error
                 #{who 3999}#
                 #{message 4000}#
                 (#{source-annotation 324}#
                   (let ((#{t 4024}# #{form 4001}#))
                     (if #{t 4024}# #{t 4024}# #{subform 4005}#)))
                 (#{strip 449}# #{form 4001}# '(()))
                 (if #{subform 4005}#
                   (#{strip 449}# #{subform 4005}# '(()))
                   #f)))))
    (letrec*
      ((#{match-each 4031}#
         (lambda (#{e 4044}# #{p 4045}# #{w 4046}# #{mod 4047}#)
           (if (pair? #{e 4044}#)
             (let ((#{first 4055}#
                     (#{match 4043}#
                       (car #{e 4044}#)
                       #{p 4045}#
                       #{w 4046}#
                       '()
                       #{mod 4047}#)))
               (if #{first 4055}#
                 (let ((#{rest 4059}#
                         (#{match-each 4031}#
                           (cdr #{e 4044}#)
                           #{p 4045}#
                           #{w 4046}#
                           #{mod 4047}#)))
                   (if #{rest 4059}#
                     (cons #{first 4055}# #{rest 4059}#)
                     #f))
                 #f))
             (if (null? #{e 4044}#)
               '()
               (if (#{syntax-object? 309}# #{e 4044}#)
                 (#{match-each 4031}#
                   (#{syntax-object-expression 311}# #{e 4044}#)
                   #{p 4045}#
                   (#{join-wraps 391}#
                     #{w 4046}#
                     (#{syntax-object-wrap 313}# #{e 4044}#))
                   (#{syntax-object-module 315}# #{e 4044}#))
                 #f)))))
       (#{match-each+ 4033}#
         (lambda (#{e 4067}#
                  #{x-pat 4068}#
                  #{y-pat 4069}#
                  #{z-pat 4070}#
                  #{w 4071}#
                  #{r 4072}#
                  #{mod 4073}#)
           (letrec*
             ((#{f 4084}#
                (lambda (#{e 4085}# #{w 4086}#)
                  (if (pair? #{e 4085}#)
                    (call-with-values
                      (lambda ()
                        (#{f 4084}# (cdr #{e 4085}#) #{w 4086}#))
                      (lambda (#{xr* 4089}# #{y-pat 4090}# #{r 4091}#)
                        (if #{r 4091}#
                          (if (null? #{y-pat 4090}#)
                            (let ((#{xr 4096}#
                                    (#{match 4043}#
                                      (car #{e 4085}#)
                                      #{x-pat 4068}#
                                      #{w 4086}#
                                      '()
                                      #{mod 4073}#)))
                              (if #{xr 4096}#
                                (values
                                  (cons #{xr 4096}# #{xr* 4089}#)
                                  #{y-pat 4090}#
                                  #{r 4091}#)
                                (values #f #f #f)))
                            (values
                              '()
                              (cdr #{y-pat 4090}#)
                              (#{match 4043}#
                                (car #{e 4085}#)
                                (car #{y-pat 4090}#)
                                #{w 4086}#
                                #{r 4091}#
                                #{mod 4073}#)))
                          (values #f #f #f))))
                    (if (#{syntax-object? 309}# #{e 4085}#)
                      (#{f 4084}#
                        (#{syntax-object-expression 311}# #{e 4085}#)
                        (#{join-wraps 391}# #{w 4086}# #{e 4085}#))
                      (values
                        '()
                        #{y-pat 4069}#
                        (#{match 4043}#
                          #{e 4085}#
                          #{z-pat 4070}#
                          #{w 4086}#
                          #{r 4072}#
                          #{mod 4073}#)))))))
             (#{f 4084}# #{e 4067}# #{w 4071}#))))
       (#{match-each-any 4035}#
         (lambda (#{e 4100}# #{w 4101}# #{mod 4102}#)
           (if (pair? #{e 4100}#)
             (let ((#{l 4109}#
                     (#{match-each-any 4035}#
                       (cdr #{e 4100}#)
                       #{w 4101}#
                       #{mod 4102}#)))
               (if #{l 4109}#
                 (cons (#{wrap 409}#
                         (car #{e 4100}#)
                         #{w 4101}#
                         #{mod 4102}#)
                       #{l 4109}#)
                 #f))
             (if (null? #{e 4100}#)
               '()
               (if (#{syntax-object? 309}# #{e 4100}#)
                 (#{match-each-any 4035}#
                   (#{syntax-object-expression 311}# #{e 4100}#)
                   (#{join-wraps 391}#
                     #{w 4101}#
                     (#{syntax-object-wrap 313}# #{e 4100}#))
                   #{mod 4102}#)
                 #f)))))
       (#{match-empty 4037}#
         (lambda (#{p 4117}# #{r 4118}#)
           (if (null? #{p 4117}#)
             #{r 4118}#
             (if (eq? #{p 4117}# '_)
               #{r 4118}#
               (if (eq? #{p 4117}# 'any)
                 (cons '() #{r 4118}#)
                 (if (pair? #{p 4117}#)
                   (#{match-empty 4037}#
                     (car #{p 4117}#)
                     (#{match-empty 4037}#
                       (cdr #{p 4117}#)
                       #{r 4118}#))
                   (if (eq? #{p 4117}# 'each-any)
                     (cons '() #{r 4118}#)
                     (let ((#{atom-key 4134}# (vector-ref #{p 4117}# 0)))
                       (if (memv #{atom-key 4134}# '(each))
                         (#{match-empty 4037}#
                           (vector-ref #{p 4117}# 1)
                           #{r 4118}#)
                         (if (memv #{atom-key 4134}# '(each+))
                           (#{match-empty 4037}#
                             (vector-ref #{p 4117}# 1)
                             (#{match-empty 4037}#
                               (reverse (vector-ref #{p 4117}# 2))
                               (#{match-empty 4037}#
                                 (vector-ref #{p 4117}# 3)
                                 #{r 4118}#)))
                           (if (memv #{atom-key 4134}# '(free-id atom))
                             #{r 4118}#
                             (if (memv #{atom-key 4134}# '(vector))
                               (#{match-empty 4037}#
                                 (vector-ref #{p 4117}# 1)
                                 #{r 4118}#)))))))))))))
       (#{combine 4039}#
         (lambda (#{r* 4139}# #{r 4140}#)
           (if (null? (car #{r* 4139}#))
             #{r 4140}#
             (cons (map car #{r* 4139}#)
                   (#{combine 4039}#
                     (map cdr #{r* 4139}#)
                     #{r 4140}#)))))
       (#{match* 4041}#
         (lambda (#{e 4143}#
                  #{p 4144}#
                  #{w 4145}#
                  #{r 4146}#
                  #{mod 4147}#)
           (if (null? #{p 4144}#)
             (if (null? #{e 4143}#) #{r 4146}# #f)
             (if (pair? #{p 4144}#)
               (if (pair? #{e 4143}#)
                 (#{match 4043}#
                   (car #{e 4143}#)
                   (car #{p 4144}#)
                   #{w 4145}#
                   (#{match 4043}#
                     (cdr #{e 4143}#)
                     (cdr #{p 4144}#)
                     #{w 4145}#
                     #{r 4146}#
                     #{mod 4147}#)
                   #{mod 4147}#)
                 #f)
               (if (eq? #{p 4144}# 'each-any)
                 (let ((#{l 4164}#
                         (#{match-each-any 4035}#
                           #{e 4143}#
                           #{w 4145}#
                           #{mod 4147}#)))
                   (if #{l 4164}# (cons #{l 4164}# #{r 4146}#) #f))
                 (let ((#{atom-key 4170}# (vector-ref #{p 4144}# 0)))
                   (if (memv #{atom-key 4170}# '(each))
                     (if (null? #{e 4143}#)
                       (#{match-empty 4037}#
                         (vector-ref #{p 4144}# 1)
                         #{r 4146}#)
                       (let ((#{l 4173}#
                               (#{match-each 4031}#
                                 #{e 4143}#
                                 (vector-ref #{p 4144}# 1)
                                 #{w 4145}#
                                 #{mod 4147}#)))
                         (if #{l 4173}#
                           (letrec*
                             ((#{collect 4178}#
                                (lambda (#{l 4179}#)
                                  (if (null? (car #{l 4179}#))
                                    #{r 4146}#
                                    (cons (map car #{l 4179}#)
                                          (#{collect 4178}#
                                            (map cdr #{l 4179}#)))))))
                             (#{collect 4178}# #{l 4173}#))
                           #f)))
                     (if (memv #{atom-key 4170}# '(each+))
                       (call-with-values
                         (lambda ()
                           (#{match-each+ 4033}#
                             #{e 4143}#
                             (vector-ref #{p 4144}# 1)
                             (vector-ref #{p 4144}# 2)
                             (vector-ref #{p 4144}# 3)
                             #{w 4145}#
                             #{r 4146}#
                             #{mod 4147}#))
                         (lambda (#{xr* 4181}# #{y-pat 4182}# #{r 4183}#)
                           (if #{r 4183}#
                             (if (null? #{y-pat 4182}#)
                               (if (null? #{xr* 4181}#)
                                 (#{match-empty 4037}#
                                   (vector-ref #{p 4144}# 1)
                                   #{r 4183}#)
                                 (#{combine 4039}# #{xr* 4181}# #{r 4183}#))
                               #f)
                             #f)))
                       (if (memv #{atom-key 4170}# '(free-id))
                         (if (#{id? 343}# #{e 4143}#)
                           (if (#{free-id=? 399}#
                                 (#{wrap 409}#
                                   #{e 4143}#
                                   #{w 4145}#
                                   #{mod 4147}#)
                                 (vector-ref #{p 4144}# 1))
                             #{r 4146}#
                             #f)
                           #f)
                         (if (memv #{atom-key 4170}# '(atom))
                           (if (equal?
                                 (vector-ref #{p 4144}# 1)
                                 (#{strip 449}# #{e 4143}# #{w 4145}#))
                             #{r 4146}#
                             #f)
                           (if (memv #{atom-key 4170}# '(vector))
                             (if (vector? #{e 4143}#)
                               (#{match 4043}#
                                 (vector->list #{e 4143}#)
                                 (vector-ref #{p 4144}# 1)
                                 #{w 4145}#
                                 #{r 4146}#
                                 #{mod 4147}#)
                               #f))))))))))))
       (#{match 4043}#
         (lambda (#{e 4200}#
                  #{p 4201}#
                  #{w 4202}#
                  #{r 4203}#
                  #{mod 4204}#)
           (if (not #{r 4203}#)
             #f
             (if (eq? #{p 4201}# '_)
               #{r 4203}#
               (if (eq? #{p 4201}# 'any)
                 (cons (#{wrap 409}# #{e 4200}# #{w 4202}# #{mod 4204}#)
                       #{r 4203}#)
                 (if (#{syntax-object? 309}# #{e 4200}#)
                   (#{match* 4041}#
                     (#{syntax-object-expression 311}# #{e 4200}#)
                     #{p 4201}#
                     (#{join-wraps 391}#
                       #{w 4202}#
                       (#{syntax-object-wrap 313}# #{e 4200}#))
                     #{r 4203}#
                     (#{syntax-object-module 315}# #{e 4200}#))
                   (#{match* 4041}#
                     #{e 4200}#
                     #{p 4201}#
                     #{w 4202}#
                     #{r 4203}#
                     #{mod 4204}#))))))))
      (set! $sc-dispatch
        (lambda (#{e 4219}# #{p 4220}#)
          (if (eq? #{p 4220}# 'any)
            (list #{e 4219}#)
            (if (eq? #{p 4220}# '_)
              '()
              (if (#{syntax-object? 309}# #{e 4219}#)
                (#{match* 4041}#
                  (#{syntax-object-expression 311}# #{e 4219}#)
                  #{p 4220}#
                  (#{syntax-object-wrap 313}# #{e 4219}#)
                  '()
                  (#{syntax-object-module 315}# #{e 4219}#))
                (#{match* 4041}#
                  #{e 4219}#
                  #{p 4220}#
                  '(())
                  '()
                  #f)))))))))

(define with-syntax
  (make-syntax-transformer
    'with-syntax
    'macro
    (lambda (#{x 4231}#)
      (let ((#{tmp 4233}# #{x 4231}#))
        (let ((#{tmp 4234}#
                ($sc-dispatch
                  #{tmp 4233}#
                  '(_ () any . each-any))))
          (if #{tmp 4234}#
            (@apply
              (lambda (#{e1 4237}# #{e2 4238}#)
                (cons '#(syntax-object
                         let
                         ((top)
                          #(ribcage
                            #(e1 e2)
                            #((top) (top))
                            #("i4235" "i4236"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4232")))
                         (hygiene guile))
                      (cons '() (cons #{e1 4237}# #{e2 4238}#))))
              #{tmp 4234}#)
            (let ((#{tmp 4240}#
                    ($sc-dispatch
                      #{tmp 4233}#
                      '(_ ((any any)) any . each-any))))
              (if #{tmp 4240}#
                (@apply
                  (lambda (#{out 4245}#
                           #{in 4246}#
                           #{e1 4247}#
                           #{e2 4248}#)
                    (list '#(syntax-object
                             syntax-case
                             ((top)
                              #(ribcage
                                #(out in e1 e2)
                                #((top) (top) (top) (top))
                                #("i4241" "i4242" "i4243" "i4244"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4232")))
                             (hygiene guile))
                          #{in 4246}#
                          '()
                          (list #{out 4245}#
                                (cons '#(syntax-object
                                         let
                                         ((top)
                                          #(ribcage
                                            #(out in e1 e2)
                                            #((top) (top) (top) (top))
                                            #("i4241" "i4242" "i4243" "i4244"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4232")))
                                         (hygiene guile))
                                      (cons '()
                                            (cons #{e1 4247}# #{e2 4248}#))))))
                  #{tmp 4240}#)
                (let ((#{tmp 4250}#
                        ($sc-dispatch
                          #{tmp 4233}#
                          '(_ #(each (any any)) any . each-any))))
                  (if #{tmp 4250}#
                    (@apply
                      (lambda (#{out 4255}#
                               #{in 4256}#
                               #{e1 4257}#
                               #{e2 4258}#)
                        (list '#(syntax-object
                                 syntax-case
                                 ((top)
                                  #(ribcage
                                    #(out in e1 e2)
                                    #((top) (top) (top) (top))
                                    #("i4251" "i4252" "i4253" "i4254"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4232")))
                                 (hygiene guile))
                              (cons '#(syntax-object
                                       list
                                       ((top)
                                        #(ribcage
                                          #(out in e1 e2)
                                          #((top) (top) (top) (top))
                                          #("i4251" "i4252" "i4253" "i4254"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4232")))
                                       (hygiene guile))
                                    #{in 4256}#)
                              '()
                              (list #{out 4255}#
                                    (cons '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage
                                                #(out in e1 e2)
                                                #((top) (top) (top) (top))
                                                #("i4251"
                                                  "i4252"
                                                  "i4253"
                                                  "i4254"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i4232")))
                                             (hygiene guile))
                                          (cons '()
                                                (cons #{e1 4257}#
                                                      #{e2 4258}#))))))
                      #{tmp 4250}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp 4233}#)))))))))))

(define syntax-rules
  (make-syntax-transformer
    'syntax-rules
    'macro
    (lambda (#{x 4262}#)
      (let ((#{tmp 4264}# #{x 4262}#))
        (let ((#{tmp 4265}#
                ($sc-dispatch
                  #{tmp 4264}#
                  '(_ each-any . #(each ((any . any) any))))))
          (if #{tmp 4265}#
            (@apply
              (lambda (#{k 4270}#
                       #{keyword 4271}#
                       #{pattern 4272}#
                       #{template 4273}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(k keyword pattern template)
                            #((top) (top) (top) (top))
                            #("i4266" "i4267" "i4268" "i4269"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4263")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(k keyword pattern template)
                             #((top) (top) (top) (top))
                             #("i4266" "i4267" "i4268" "i4269"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4263")))
                          (hygiene guile)))
                      (vector
                        '(#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(k keyword pattern template)
                               #((top) (top) (top) (top))
                               #("i4266" "i4267" "i4268" "i4269"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4263")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            syntax-rules
                            ((top)
                             #(ribcage
                               #(k keyword pattern template)
                               #((top) (top) (top) (top))
                               #("i4266" "i4267" "i4268" "i4269"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4263")))
                            (hygiene guile)))
                        (cons '#(syntax-object
                                 patterns
                                 ((top)
                                  #(ribcage
                                    #(k keyword pattern template)
                                    #((top) (top) (top) (top))
                                    #("i4266" "i4267" "i4268" "i4269"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4263")))
                                 (hygiene guile))
                              #{pattern 4272}#))
                      (cons '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(k keyword pattern template)
                                  #((top) (top) (top) (top))
                                  #("i4266" "i4267" "i4268" "i4269"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4263")))
                               (hygiene guile))
                            (cons '#(syntax-object
                                     x
                                     ((top)
                                      #(ribcage
                                        #(k keyword pattern template)
                                        #((top) (top) (top) (top))
                                        #("i4266" "i4267" "i4268" "i4269"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4263")))
                                     (hygiene guile))
                                  (cons #{k 4270}#
                                        (map (lambda (#{tmp 4277}#
                                                      #{tmp 4276}#)
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
                                                                 #("i4266"
                                                                   "i4267"
                                                                   "i4268"
                                                                   "i4269"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4263")))
                                                              (hygiene guile))
                                                           #{tmp 4276}#)
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
                                                                 #("i4266"
                                                                   "i4267"
                                                                   "i4268"
                                                                   "i4269"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4263")))
                                                              (hygiene guile))
                                                           #{tmp 4277}#)))
                                             #{template 4273}#
                                             #{pattern 4272}#))))))
              #{tmp 4265}#)
            (let ((#{tmp 4278}#
                    ($sc-dispatch
                      #{tmp 4264}#
                      '(_ each-any any . #(each ((any . any) any))))))
              (if (if #{tmp 4278}#
                    (@apply
                      (lambda (#{k 4284}#
                               #{docstring 4285}#
                               #{keyword 4286}#
                               #{pattern 4287}#
                               #{template 4288}#)
                        (string? (syntax->datum #{docstring 4285}#)))
                      #{tmp 4278}#)
                    #f)
                (@apply
                  (lambda (#{k 4294}#
                           #{docstring 4295}#
                           #{keyword 4296}#
                           #{pattern 4297}#
                           #{template 4298}#)
                    (list '#(syntax-object
                             lambda
                             ((top)
                              #(ribcage
                                #(k docstring keyword pattern template)
                                #((top) (top) (top) (top) (top))
                                #("i4289" "i4290" "i4291" "i4292" "i4293"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4263")))
                             (hygiene guile))
                          '(#(syntax-object
                              x
                              ((top)
                               #(ribcage
                                 #(k docstring keyword pattern template)
                                 #((top) (top) (top) (top) (top))
                                 #("i4289" "i4290" "i4291" "i4292" "i4293"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4263")))
                              (hygiene guile)))
                          #{docstring 4295}#
                          (vector
                            '(#(syntax-object
                                macro-type
                                ((top)
                                 #(ribcage
                                   #(k docstring keyword pattern template)
                                   #((top) (top) (top) (top) (top))
                                   #("i4289" "i4290" "i4291" "i4292" "i4293"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4263")))
                                (hygiene guile))
                              .
                              #(syntax-object
                                syntax-rules
                                ((top)
                                 #(ribcage
                                   #(k docstring keyword pattern template)
                                   #((top) (top) (top) (top) (top))
                                   #("i4289" "i4290" "i4291" "i4292" "i4293"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4263")))
                                (hygiene guile)))
                            (cons '#(syntax-object
                                     patterns
                                     ((top)
                                      #(ribcage
                                        #(k docstring keyword pattern template)
                                        #((top) (top) (top) (top) (top))
                                        #("i4289"
                                          "i4290"
                                          "i4291"
                                          "i4292"
                                          "i4293"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4263")))
                                     (hygiene guile))
                                  #{pattern 4297}#))
                          (cons '#(syntax-object
                                   syntax-case
                                   ((top)
                                    #(ribcage
                                      #(k docstring keyword pattern template)
                                      #((top) (top) (top) (top) (top))
                                      #("i4289"
                                        "i4290"
                                        "i4291"
                                        "i4292"
                                        "i4293"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i4263")))
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
                                            #("i4289"
                                              "i4290"
                                              "i4291"
                                              "i4292"
                                              "i4293"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4263")))
                                         (hygiene guile))
                                      (cons #{k 4294}#
                                            (map (lambda (#{tmp 4302}#
                                                          #{tmp 4301}#)
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
                                                                     #("i4289"
                                                                       "i4290"
                                                                       "i4291"
                                                                       "i4292"
                                                                       "i4293"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(x)
                                                                     #((top))
                                                                     #("i4263")))
                                                                  (hygiene
                                                                    guile))
                                                               #{tmp 4301}#)
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
                                                                     #("i4289"
                                                                       "i4290"
                                                                       "i4291"
                                                                       "i4292"
                                                                       "i4293"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(x)
                                                                     #((top))
                                                                     #("i4263")))
                                                                  (hygiene
                                                                    guile))
                                                               #{tmp 4302}#)))
                                                 #{template 4298}#
                                                 #{pattern 4297}#))))))
                  #{tmp 4278}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4264}#)))))))))

(define let*
  (make-syntax-transformer
    'let*
    'macro
    (lambda (#{x 4303}#)
      (let ((#{tmp 4305}# #{x 4303}#))
        (let ((#{tmp 4306}#
                ($sc-dispatch
                  #{tmp 4305}#
                  '(any #(each (any any)) any . each-any))))
          (if (if #{tmp 4306}#
                (@apply
                  (lambda (#{let* 4312}#
                           #{x 4313}#
                           #{v 4314}#
                           #{e1 4315}#
                           #{e2 4316}#)
                    (and-map identifier? #{x 4313}#))
                  #{tmp 4306}#)
                #f)
            (@apply
              (lambda (#{let* 4323}#
                       #{x 4324}#
                       #{v 4325}#
                       #{e1 4326}#
                       #{e2 4327}#)
                (letrec*
                  ((#{f 4330}#
                     (lambda (#{bindings 4331}#)
                       (if (null? #{bindings 4331}#)
                         (cons '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(f bindings)
                                     #((top) (top))
                                     #("i4328" "i4329"))
                                   #(ribcage
                                     #(let* x v e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("i4318"
                                       "i4319"
                                       "i4320"
                                       "i4321"
                                       "i4322"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i4304")))
                                  (hygiene guile))
                               (cons '() (cons #{e1 4326}# #{e2 4327}#)))
                         (let ((#{tmp 4336}#
                                 (list (#{f 4330}# (cdr #{bindings 4331}#))
                                       (car #{bindings 4331}#))))
                           (let ((#{tmp 4337}#
                                   ($sc-dispatch #{tmp 4336}# '(any any))))
                             (if #{tmp 4337}#
                               (@apply
                                 (lambda (#{body 4340}# #{binding 4341}#)
                                   (list '#(syntax-object
                                            let
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(body binding)
                                               #((top) (top))
                                               #("i4338" "i4339"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(f bindings)
                                               #((top) (top))
                                               #("i4328" "i4329"))
                                             #(ribcage
                                               #(let* x v e1 e2)
                                               #((top) (top) (top) (top) (top))
                                               #("i4318"
                                                 "i4319"
                                                 "i4320"
                                                 "i4321"
                                                 "i4322"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4304")))
                                            (hygiene guile))
                                         (list #{binding 4341}#)
                                         #{body 4340}#))
                                 #{tmp 4337}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4336}#))))))))
                  (#{f 4330}# (map list #{x 4324}# #{v 4325}#))))
              #{tmp 4306}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4305}#)))))))

(define do
  (make-syntax-transformer
    'do
    'macro
    (lambda (#{orig-x 4342}#)
      (let ((#{tmp 4344}# #{orig-x 4342}#))
        (let ((#{tmp 4345}#
                ($sc-dispatch
                  #{tmp 4344}#
                  '(_ #(each (any any . any))
                      (any . each-any)
                      .
                      each-any))))
          (if #{tmp 4345}#
            (@apply
              (lambda (#{var 4352}#
                       #{init 4353}#
                       #{step 4354}#
                       #{e0 4355}#
                       #{e1 4356}#
                       #{c 4357}#)
                (let ((#{tmp 4359}#
                        (map (lambda (#{v 4380}# #{s 4381}#)
                               (let ((#{tmp 4384}# #{s 4381}#))
                                 (let ((#{tmp 4385}#
                                         ($sc-dispatch #{tmp 4384}# '())))
                                   (if #{tmp 4385}#
                                     (@apply
                                       (lambda () #{v 4380}#)
                                       #{tmp 4385}#)
                                     (let ((#{tmp 4386}#
                                             ($sc-dispatch
                                               #{tmp 4384}#
                                               '(any))))
                                       (if #{tmp 4386}#
                                         (@apply
                                           (lambda (#{e 4388}#) #{e 4388}#)
                                           #{tmp 4386}#)
                                         (let ((#{_ 4390}# #{tmp 4384}#))
                                           (syntax-violation
                                             'do
                                             "bad step expression"
                                             #{orig-x 4342}#
                                             #{s 4381}#))))))))
                             #{var 4352}#
                             #{step 4354}#)))
                  (let ((#{tmp 4360}#
                          ($sc-dispatch #{tmp 4359}# 'each-any)))
                    (if #{tmp 4360}#
                      (@apply
                        (lambda (#{step 4362}#)
                          (let ((#{tmp 4363}# #{e1 4356}#))
                            (let ((#{tmp 4364}#
                                    ($sc-dispatch #{tmp 4363}# '())))
                              (if #{tmp 4364}#
                                (@apply
                                  (lambda ()
                                    (list '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4361"))
                                              #(ribcage
                                                #(var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4346"
                                                  "i4347"
                                                  "i4348"
                                                  "i4349"
                                                  "i4350"
                                                  "i4351"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4343")))
                                             (hygiene guile))
                                          '#(syntax-object
                                             doloop
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4361"))
                                              #(ribcage
                                                #(var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4346"
                                                  "i4347"
                                                  "i4348"
                                                  "i4349"
                                                  "i4350"
                                                  "i4351"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4343")))
                                             (hygiene guile))
                                          (map list #{var 4352}# #{init 4353}#)
                                          (list '#(syntax-object
                                                   if
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(step)
                                                      #((top))
                                                      #("i4361"))
                                                    #(ribcage
                                                      #(var init step e0 e1 c)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i4346"
                                                        "i4347"
                                                        "i4348"
                                                        "i4349"
                                                        "i4350"
                                                        "i4351"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(orig-x)
                                                      #((top))
                                                      #("i4343")))
                                                   (hygiene guile))
                                                (list '#(syntax-object
                                                         not
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4361"))
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
                                                            #("i4346"
                                                              "i4347"
                                                              "i4348"
                                                              "i4349"
                                                              "i4350"
                                                              "i4351"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4343")))
                                                         (hygiene guile))
                                                      #{e0 4355}#)
                                                (cons '#(syntax-object
                                                         begin
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4361"))
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
                                                            #("i4346"
                                                              "i4347"
                                                              "i4348"
                                                              "i4349"
                                                              "i4350"
                                                              "i4351"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4343")))
                                                         (hygiene guile))
                                                      (append
                                                        #{c 4357}#
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
                                                                          #("i4361"))
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
                                                                          #("i4346"
                                                                            "i4347"
                                                                            "i4348"
                                                                            "i4349"
                                                                            "i4350"
                                                                            "i4351"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(orig-x)
                                                                          #((top))
                                                                          #("i4343")))
                                                                       (hygiene
                                                                         guile))
                                                                    #{step 4362}#)))))))
                                  #{tmp 4364}#)
                                (let ((#{tmp 4369}#
                                        ($sc-dispatch
                                          #{tmp 4363}#
                                          '(any . each-any))))
                                  (if #{tmp 4369}#
                                    (@apply
                                      (lambda (#{e1 4372}# #{e2 4373}#)
                                        (list '#(syntax-object
                                                 let
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4370" "i4371"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4361"))
                                                  #(ribcage
                                                    #(var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4346"
                                                      "i4347"
                                                      "i4348"
                                                      "i4349"
                                                      "i4350"
                                                      "i4351"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4343")))
                                                 (hygiene guile))
                                              '#(syntax-object
                                                 doloop
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4370" "i4371"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4361"))
                                                  #(ribcage
                                                    #(var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4346"
                                                      "i4347"
                                                      "i4348"
                                                      "i4349"
                                                      "i4350"
                                                      "i4351"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4343")))
                                                 (hygiene guile))
                                              (map list
                                                   #{var 4352}#
                                                   #{init 4353}#)
                                              (list '#(syntax-object
                                                       if
                                                       ((top)
                                                        #(ribcage
                                                          #(e1 e2)
                                                          #((top) (top))
                                                          #("i4370" "i4371"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(step)
                                                          #((top))
                                                          #("i4361"))
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
                                                          #("i4346"
                                                            "i4347"
                                                            "i4348"
                                                            "i4349"
                                                            "i4350"
                                                            "i4351"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(orig-x)
                                                          #((top))
                                                          #("i4343")))
                                                       (hygiene guile))
                                                    #{e0 4355}#
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4370"
                                                                  "i4371"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4361"))
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
                                                                #("i4346"
                                                                  "i4347"
                                                                  "i4348"
                                                                  "i4349"
                                                                  "i4350"
                                                                  "i4351"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4343")))
                                                             (hygiene guile))
                                                          (cons #{e1 4372}#
                                                                #{e2 4373}#))
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4370"
                                                                  "i4371"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4361"))
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
                                                                #("i4346"
                                                                  "i4347"
                                                                  "i4348"
                                                                  "i4349"
                                                                  "i4350"
                                                                  "i4351"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4343")))
                                                             (hygiene guile))
                                                          (append
                                                            #{c 4357}#
                                                            (list (cons '#(syntax-object
                                                                           doloop
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(e1
                                                                                e2)
                                                                              #((top)
                                                                                (top))
                                                                              #("i4370"
                                                                                "i4371"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(step)
                                                                              #((top))
                                                                              #("i4361"))
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
                                                                              #("i4346"
                                                                                "i4347"
                                                                                "i4348"
                                                                                "i4349"
                                                                                "i4350"
                                                                                "i4351"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(orig-x)
                                                                              #((top))
                                                                              #("i4343")))
                                                                           (hygiene
                                                                             guile))
                                                                        #{step 4362}#)))))))
                                      #{tmp 4369}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp 4363}#)))))))
                        #{tmp 4360}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp 4359}#)))))
              #{tmp 4345}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4344}#)))))))

(define quasiquote
  (make-syntax-transformer
    'quasiquote
    'macro
    (letrec*
      ((#{quasi 4394}#
         (lambda (#{p 4407}# #{lev 4408}#)
           (let ((#{tmp 4411}# #{p 4407}#))
             (let ((#{tmp 4412}#
                     ($sc-dispatch
                       #{tmp 4411}#
                       '(#(free-id
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4409" "i4410"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4405"
                                 "i4403"
                                 "i4401"
                                 "i4399"
                                 "i4397"
                                 "i4395"
                                 "i4393")))
                             (hygiene guile)))
                         any))))
               (if #{tmp 4412}#
                 (@apply
                   (lambda (#{p 4414}#)
                     (if (= #{lev 4408}# 0)
                       (list '#(syntax-object
                                "value"
                                ((top)
                                 #(ribcage #(p) #((top)) #("i4413"))
                                 #(ribcage () () ())
                                 #(ribcage
                                   #(p lev)
                                   #((top) (top))
                                   #("i4409" "i4410"))
                                 #(ribcage
                                   (emit quasivector
                                         quasilist*
                                         quasiappend
                                         quasicons
                                         vquasi
                                         quasi)
                                   ((top) (top) (top) (top) (top) (top) (top))
                                   ("i4405"
                                    "i4403"
                                    "i4401"
                                    "i4399"
                                    "i4397"
                                    "i4395"
                                    "i4393")))
                                (hygiene guile))
                             #{p 4414}#)
                       (#{quasicons 4398}#
                         '(#(syntax-object
                             "quote"
                             ((top)
                              #(ribcage #(p) #((top)) #("i4413"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4409" "i4410"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4405"
                                 "i4403"
                                 "i4401"
                                 "i4399"
                                 "i4397"
                                 "i4395"
                                 "i4393")))
                             (hygiene guile))
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage #(p) #((top)) #("i4413"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4409" "i4410"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4405"
                                 "i4403"
                                 "i4401"
                                 "i4399"
                                 "i4397"
                                 "i4395"
                                 "i4393")))
                             (hygiene guile)))
                         (#{quasi 4394}#
                           (list #{p 4414}#)
                           (#{1-}# #{lev 4408}#)))))
                   #{tmp 4412}#)
                 (let ((#{tmp 4415}#
                         ($sc-dispatch
                           #{tmp 4411}#
                           '(#(free-id
                               #(syntax-object
                                 quasiquote
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage
                                    #(p lev)
                                    #((top) (top))
                                    #("i4409" "i4410"))
                                  #(ribcage
                                    (emit quasivector
                                          quasilist*
                                          quasiappend
                                          quasicons
                                          vquasi
                                          quasi)
                                    ((top) (top) (top) (top) (top) (top) (top))
                                    ("i4405"
                                     "i4403"
                                     "i4401"
                                     "i4399"
                                     "i4397"
                                     "i4395"
                                     "i4393")))
                                 (hygiene guile)))
                             any))))
                   (if #{tmp 4415}#
                     (@apply
                       (lambda (#{p 4417}#)
                         (#{quasicons 4398}#
                           '(#(syntax-object
                               "quote"
                               ((top)
                                #(ribcage #(p) #((top)) #("i4416"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(p lev)
                                  #((top) (top))
                                  #("i4409" "i4410"))
                                #(ribcage
                                  (emit quasivector
                                        quasilist*
                                        quasiappend
                                        quasicons
                                        vquasi
                                        quasi)
                                  ((top) (top) (top) (top) (top) (top) (top))
                                  ("i4405"
                                   "i4403"
                                   "i4401"
                                   "i4399"
                                   "i4397"
                                   "i4395"
                                   "i4393")))
                               (hygiene guile))
                             #(syntax-object
                               quasiquote
                               ((top)
                                #(ribcage #(p) #((top)) #("i4416"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(p lev)
                                  #((top) (top))
                                  #("i4409" "i4410"))
                                #(ribcage
                                  (emit quasivector
                                        quasilist*
                                        quasiappend
                                        quasicons
                                        vquasi
                                        quasi)
                                  ((top) (top) (top) (top) (top) (top) (top))
                                  ("i4405"
                                   "i4403"
                                   "i4401"
                                   "i4399"
                                   "i4397"
                                   "i4395"
                                   "i4393")))
                               (hygiene guile)))
                           (#{quasi 4394}#
                             (list #{p 4417}#)
                             (#{1+}# #{lev 4408}#))))
                       #{tmp 4415}#)
                     (let ((#{tmp 4418}#
                             ($sc-dispatch #{tmp 4411}# '(any . any))))
                       (if #{tmp 4418}#
                         (@apply
                           (lambda (#{p 4421}# #{q 4422}#)
                             (let ((#{tmp 4423}# #{p 4421}#))
                               (let ((#{tmp 4424}#
                                       ($sc-dispatch
                                         #{tmp 4423}#
                                         '(#(free-id
                                             #(syntax-object
                                               unquote
                                               ((top)
                                                #(ribcage
                                                  #(p q)
                                                  #((top) (top))
                                                  #("i4419" "i4420"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(p lev)
                                                  #((top) (top))
                                                  #("i4409" "i4410"))
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
                                                  ("i4405"
                                                   "i4403"
                                                   "i4401"
                                                   "i4399"
                                                   "i4397"
                                                   "i4395"
                                                   "i4393")))
                                               (hygiene guile)))
                                           .
                                           each-any))))
                                 (if #{tmp 4424}#
                                   (@apply
                                     (lambda (#{p 4426}#)
                                       (if (= #{lev 4408}# 0)
                                         (#{quasilist* 4402}#
                                           (map (lambda (#{tmp 4427}#)
                                                  (list '#(syntax-object
                                                           "value"
                                                           ((top)
                                                            #(ribcage
                                                              #(p)
                                                              #((top))
                                                              #("i4425"))
                                                            #(ribcage
                                                              #(p q)
                                                              #((top) (top))
                                                              #("i4419"
                                                                "i4420"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(p lev)
                                                              #((top) (top))
                                                              #("i4409"
                                                                "i4410"))
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
                                                              ("i4405"
                                                               "i4403"
                                                               "i4401"
                                                               "i4399"
                                                               "i4397"
                                                               "i4395"
                                                               "i4393")))
                                                           (hygiene guile))
                                                        #{tmp 4427}#))
                                                #{p 4426}#)
                                           (#{quasi 4394}#
                                             #{q 4422}#
                                             #{lev 4408}#))
                                         (#{quasicons 4398}#
                                           (#{quasicons 4398}#
                                             '(#(syntax-object
                                                 "quote"
                                                 ((top)
                                                  #(ribcage
                                                    #(p)
                                                    #((top))
                                                    #("i4425"))
                                                  #(ribcage
                                                    #(p q)
                                                    #((top) (top))
                                                    #("i4419" "i4420"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(p lev)
                                                    #((top) (top))
                                                    #("i4409" "i4410"))
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
                                                    ("i4405"
                                                     "i4403"
                                                     "i4401"
                                                     "i4399"
                                                     "i4397"
                                                     "i4395"
                                                     "i4393")))
                                                 (hygiene guile))
                                               #(syntax-object
                                                 unquote
                                                 ((top)
                                                  #(ribcage
                                                    #(p)
                                                    #((top))
                                                    #("i4425"))
                                                  #(ribcage
                                                    #(p q)
                                                    #((top) (top))
                                                    #("i4419" "i4420"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(p lev)
                                                    #((top) (top))
                                                    #("i4409" "i4410"))
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
                                                    ("i4405"
                                                     "i4403"
                                                     "i4401"
                                                     "i4399"
                                                     "i4397"
                                                     "i4395"
                                                     "i4393")))
                                                 (hygiene guile)))
                                             (#{quasi 4394}#
                                               #{p 4426}#
                                               (#{1-}# #{lev 4408}#)))
                                           (#{quasi 4394}#
                                             #{q 4422}#
                                             #{lev 4408}#))))
                                     #{tmp 4424}#)
                                   (let ((#{tmp 4429}#
                                           ($sc-dispatch
                                             #{tmp 4423}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   unquote-splicing
                                                   ((top)
                                                    #(ribcage
                                                      #(p q)
                                                      #((top) (top))
                                                      #("i4419" "i4420"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(p lev)
                                                      #((top) (top))
                                                      #("i4409" "i4410"))
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
                                                      ("i4405"
                                                       "i4403"
                                                       "i4401"
                                                       "i4399"
                                                       "i4397"
                                                       "i4395"
                                                       "i4393")))
                                                   (hygiene guile)))
                                               .
                                               each-any))))
                                     (if #{tmp 4429}#
                                       (@apply
                                         (lambda (#{p 4431}#)
                                           (if (= #{lev 4408}# 0)
                                             (#{quasiappend 4400}#
                                               (map (lambda (#{tmp 4432}#)
                                                      (list '#(syntax-object
                                                               "value"
                                                               ((top)
                                                                #(ribcage
                                                                  #(p)
                                                                  #((top))
                                                                  #("i4430"))
                                                                #(ribcage
                                                                  #(p q)
                                                                  #((top)
                                                                    (top))
                                                                  #("i4419"
                                                                    "i4420"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(p lev)
                                                                  #((top)
                                                                    (top))
                                                                  #("i4409"
                                                                    "i4410"))
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
                                                                  ("i4405"
                                                                   "i4403"
                                                                   "i4401"
                                                                   "i4399"
                                                                   "i4397"
                                                                   "i4395"
                                                                   "i4393")))
                                                               (hygiene guile))
                                                            #{tmp 4432}#))
                                                    #{p 4431}#)
                                               (#{quasi 4394}#
                                                 #{q 4422}#
                                                 #{lev 4408}#))
                                             (#{quasicons 4398}#
                                               (#{quasicons 4398}#
                                                 '(#(syntax-object
                                                     "quote"
                                                     ((top)
                                                      #(ribcage
                                                        #(p)
                                                        #((top))
                                                        #("i4430"))
                                                      #(ribcage
                                                        #(p q)
                                                        #((top) (top))
                                                        #("i4419" "i4420"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(p lev)
                                                        #((top) (top))
                                                        #("i4409" "i4410"))
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
                                                        ("i4405"
                                                         "i4403"
                                                         "i4401"
                                                         "i4399"
                                                         "i4397"
                                                         "i4395"
                                                         "i4393")))
                                                     (hygiene guile))
                                                   #(syntax-object
                                                     unquote-splicing
                                                     ((top)
                                                      #(ribcage
                                                        #(p)
                                                        #((top))
                                                        #("i4430"))
                                                      #(ribcage
                                                        #(p q)
                                                        #((top) (top))
                                                        #("i4419" "i4420"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(p lev)
                                                        #((top) (top))
                                                        #("i4409" "i4410"))
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
                                                        ("i4405"
                                                         "i4403"
                                                         "i4401"
                                                         "i4399"
                                                         "i4397"
                                                         "i4395"
                                                         "i4393")))
                                                     (hygiene guile)))
                                                 (#{quasi 4394}#
                                                   #{p 4431}#
                                                   (#{1-}# #{lev 4408}#)))
                                               (#{quasi 4394}#
                                                 #{q 4422}#
                                                 #{lev 4408}#))))
                                         #{tmp 4429}#)
                                       (let ((#{_ 4435}# #{tmp 4423}#))
                                         (#{quasicons 4398}#
                                           (#{quasi 4394}#
                                             #{p 4421}#
                                             #{lev 4408}#)
                                           (#{quasi 4394}#
                                             #{q 4422}#
                                             #{lev 4408}#)))))))))
                           #{tmp 4418}#)
                         (let ((#{tmp 4436}#
                                 ($sc-dispatch
                                   #{tmp 4411}#
                                   '#(vector each-any))))
                           (if #{tmp 4436}#
                             (@apply
                               (lambda (#{x 4438}#)
                                 (#{quasivector 4404}#
                                   (#{vquasi 4396}# #{x 4438}# #{lev 4408}#)))
                               #{tmp 4436}#)
                             (let ((#{p 4441}# #{tmp 4411}#))
                               (list '#(syntax-object
                                        "quote"
                                        ((top)
                                         #(ribcage #(p) #((top)) #("i4440"))
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p lev)
                                           #((top) (top))
                                           #("i4409" "i4410"))
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
                                           ("i4405"
                                            "i4403"
                                            "i4401"
                                            "i4399"
                                            "i4397"
                                            "i4395"
                                            "i4393")))
                                        (hygiene guile))
                                     #{p 4441}#)))))))))))))
       (#{vquasi 4396}#
         (lambda (#{p 4442}# #{lev 4443}#)
           (let ((#{tmp 4446}# #{p 4442}#))
             (let ((#{tmp 4447}#
                     ($sc-dispatch #{tmp 4446}# '(any . any))))
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
                                          #("i4444" "i4445"))
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
                                          ("i4405"
                                           "i4403"
                                           "i4401"
                                           "i4399"
                                           "i4397"
                                           "i4395"
                                           "i4393")))
                                       (hygiene guile)))
                                   .
                                   each-any))))
                         (if #{tmp 4453}#
                           (@apply
                             (lambda (#{p 4455}#)
                               (if (= #{lev 4443}# 0)
                                 (#{quasilist* 4402}#
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
                                                      #("i4448" "i4449"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(p lev)
                                                      #((top) (top))
                                                      #("i4444" "i4445"))
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
                                                      ("i4405"
                                                       "i4403"
                                                       "i4401"
                                                       "i4399"
                                                       "i4397"
                                                       "i4395"
                                                       "i4393")))
                                                   (hygiene guile))
                                                #{tmp 4456}#))
                                        #{p 4455}#)
                                   (#{vquasi 4396}# #{q 4451}# #{lev 4443}#))
                                 (#{quasicons 4398}#
                                   (#{quasicons 4398}#
                                     '(#(syntax-object
                                         "quote"
                                         ((top)
                                          #(ribcage #(p) #((top)) #("i4454"))
                                          #(ribcage
                                            #(p q)
                                            #((top) (top))
                                            #("i4448" "i4449"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(p lev)
                                            #((top) (top))
                                            #("i4444" "i4445"))
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
                                            ("i4405"
                                             "i4403"
                                             "i4401"
                                             "i4399"
                                             "i4397"
                                             "i4395"
                                             "i4393")))
                                         (hygiene guile))
                                       #(syntax-object
                                         unquote
                                         ((top)
                                          #(ribcage #(p) #((top)) #("i4454"))
                                          #(ribcage
                                            #(p q)
                                            #((top) (top))
                                            #("i4448" "i4449"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(p lev)
                                            #((top) (top))
                                            #("i4444" "i4445"))
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
                                            ("i4405"
                                             "i4403"
                                             "i4401"
                                             "i4399"
                                             "i4397"
                                             "i4395"
                                             "i4393")))
                                         (hygiene guile)))
                                     (#{quasi 4394}#
                                       #{p 4455}#
                                       (#{1-}# #{lev 4443}#)))
                                   (#{vquasi 4396}# #{q 4451}# #{lev 4443}#))))
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
                                              #("i4444" "i4445"))
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
                                              ("i4405"
                                               "i4403"
                                               "i4401"
                                               "i4399"
                                               "i4397"
                                               "i4395"
                                               "i4393")))
                                           (hygiene guile)))
                                       .
                                       each-any))))
                             (if #{tmp 4458}#
                               (@apply
                                 (lambda (#{p 4460}#)
                                   (if (= #{lev 4443}# 0)
                                     (#{quasiappend 4400}#
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
                                                          #((top) (top))
                                                          #("i4448" "i4449"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(p lev)
                                                          #((top) (top))
                                                          #("i4444" "i4445"))
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
                                                          ("i4405"
                                                           "i4403"
                                                           "i4401"
                                                           "i4399"
                                                           "i4397"
                                                           "i4395"
                                                           "i4393")))
                                                       (hygiene guile))
                                                    #{tmp 4461}#))
                                            #{p 4460}#)
                                       (#{vquasi 4396}#
                                         #{q 4451}#
                                         #{lev 4443}#))
                                     (#{quasicons 4398}#
                                       (#{quasicons 4398}#
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
                                                #("i4444" "i4445"))
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
                                                ("i4405"
                                                 "i4403"
                                                 "i4401"
                                                 "i4399"
                                                 "i4397"
                                                 "i4395"
                                                 "i4393")))
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
                                                #("i4444" "i4445"))
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
                                                ("i4405"
                                                 "i4403"
                                                 "i4401"
                                                 "i4399"
                                                 "i4397"
                                                 "i4395"
                                                 "i4393")))
                                             (hygiene guile)))
                                         (#{quasi 4394}#
                                           #{p 4460}#
                                           (#{1-}# #{lev 4443}#)))
                                       (#{vquasi 4396}#
                                         #{q 4451}#
                                         #{lev 4443}#))))
                                 #{tmp 4458}#)
                               (let ((#{_ 4464}# #{tmp 4452}#))
                                 (#{quasicons 4398}#
                                   (#{quasi 4394}# #{p 4450}# #{lev 4443}#)
                                   (#{vquasi 4396}#
                                     #{q 4451}#
                                     #{lev 4443}#)))))))))
                   #{tmp 4447}#)
                 (let ((#{tmp 4465}# ($sc-dispatch #{tmp 4446}# '())))
                   (if #{tmp 4465}#
                     (@apply
                       (lambda ()
                         '(#(syntax-object
                             "quote"
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4444" "i4445"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4405"
                                 "i4403"
                                 "i4401"
                                 "i4399"
                                 "i4397"
                                 "i4395"
                                 "i4393")))
                             (hygiene guile))
                           ()))
                       #{tmp 4465}#)
                     (syntax-violation
                       #f
                       "source expression failed to match any pattern"
                       #{tmp 4446}#))))))))
       (#{quasicons 4398}#
         (lambda (#{x 4466}# #{y 4467}#)
           (let ((#{tmp 4471}# (list #{x 4466}# #{y 4467}#)))
             (let ((#{tmp 4472}#
                     ($sc-dispatch #{tmp 4471}# '(any any))))
               (if #{tmp 4472}#
                 (@apply
                   (lambda (#{x 4475}# #{y 4476}#)
                     (let ((#{tmp 4477}# #{y 4476}#))
                       (let ((#{tmp 4478}#
                               ($sc-dispatch
                                 #{tmp 4477}#
                                 '(#(atom "quote") any))))
                         (if #{tmp 4478}#
                           (@apply
                             (lambda (#{dy 4480}#)
                               (let ((#{tmp 4481}# #{x 4475}#))
                                 (let ((#{tmp 4482}#
                                         ($sc-dispatch
                                           #{tmp 4481}#
                                           '(#(atom "quote") any))))
                                   (if #{tmp 4482}#
                                     (@apply
                                       (lambda (#{dx 4484}#)
                                         (list '#(syntax-object
                                                  "quote"
                                                  ((top)
                                                   #(ribcage
                                                     #(dx)
                                                     #((top))
                                                     #("i4483"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4479"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4473" "i4474"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4468" "i4469"))
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
                                                     ("i4405"
                                                      "i4403"
                                                      "i4401"
                                                      "i4399"
                                                      "i4397"
                                                      "i4395"
                                                      "i4393")))
                                                  (hygiene guile))
                                               (cons #{dx 4484}# #{dy 4480}#)))
                                       #{tmp 4482}#)
                                     (let ((#{_ 4486}# #{tmp 4481}#))
                                       (if (null? #{dy 4480}#)
                                         (list '#(syntax-object
                                                  "list"
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4485"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4479"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4473" "i4474"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4468" "i4469"))
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
                                                     ("i4405"
                                                      "i4403"
                                                      "i4401"
                                                      "i4399"
                                                      "i4397"
                                                      "i4395"
                                                      "i4393")))
                                                  (hygiene guile))
                                               #{x 4475}#)
                                         (list '#(syntax-object
                                                  "list*"
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4485"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4479"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4473" "i4474"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4468" "i4469"))
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
                                                     ("i4405"
                                                      "i4403"
                                                      "i4401"
                                                      "i4399"
                                                      "i4397"
                                                      "i4395"
                                                      "i4393")))
                                                  (hygiene guile))
                                               #{x 4475}#
                                               #{y 4476}#)))))))
                             #{tmp 4478}#)
                           (let ((#{tmp 4487}#
                                   ($sc-dispatch
                                     #{tmp 4477}#
                                     '(#(atom "list") . any))))
                             (if #{tmp 4487}#
                               (@apply
                                 (lambda (#{stuff 4489}#)
                                   (cons '#(syntax-object
                                            "list"
                                            ((top)
                                             #(ribcage
                                               #(stuff)
                                               #((top))
                                               #("i4488"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4473" "i4474"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4468" "i4469"))
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
                                               ("i4405"
                                                "i4403"
                                                "i4401"
                                                "i4399"
                                                "i4397"
                                                "i4395"
                                                "i4393")))
                                            (hygiene guile))
                                         (cons #{x 4475}# #{stuff 4489}#)))
                                 #{tmp 4487}#)
                               (let ((#{tmp 4490}#
                                       ($sc-dispatch
                                         #{tmp 4477}#
                                         '(#(atom "list*") . any))))
                                 (if #{tmp 4490}#
                                   (@apply
                                     (lambda (#{stuff 4492}#)
                                       (cons '#(syntax-object
                                                "list*"
                                                ((top)
                                                 #(ribcage
                                                   #(stuff)
                                                   #((top))
                                                   #("i4491"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x y)
                                                   #((top) (top))
                                                   #("i4473" "i4474"))
                                                 #(ribcage () () ())
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x y)
                                                   #((top) (top))
                                                   #("i4468" "i4469"))
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
                                                   ("i4405"
                                                    "i4403"
                                                    "i4401"
                                                    "i4399"
                                                    "i4397"
                                                    "i4395"
                                                    "i4393")))
                                                (hygiene guile))
                                             (cons #{x 4475}# #{stuff 4492}#)))
                                     #{tmp 4490}#)
                                   (let ((#{_ 4494}# #{tmp 4477}#))
                                     (list '#(syntax-object
                                              "list*"
                                              ((top)
                                               #(ribcage
                                                 #(_)
                                                 #((top))
                                                 #("i4493"))
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i4473" "i4474"))
                                               #(ribcage () () ())
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i4468" "i4469"))
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
                                                 ("i4405"
                                                  "i4403"
                                                  "i4401"
                                                  "i4399"
                                                  "i4397"
                                                  "i4395"
                                                  "i4393")))
                                              (hygiene guile))
                                           #{x 4475}#
                                           #{y 4476}#))))))))))
                   #{tmp 4472}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp 4471}#))))))
       (#{quasiappend 4400}#
         (lambda (#{x 4495}# #{y 4496}#)
           (let ((#{tmp 4499}# #{y 4496}#))
             (let ((#{tmp 4500}#
                     ($sc-dispatch #{tmp 4499}# '(#(atom "quote") ()))))
               (if #{tmp 4500}#
                 (@apply
                   (lambda ()
                     (if (null? #{x 4495}#)
                       '(#(syntax-object
                           "quote"
                           ((top)
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
                              ((top) (top) (top) (top) (top) (top) (top))
                              ("i4405"
                               "i4403"
                               "i4401"
                               "i4399"
                               "i4397"
                               "i4395"
                               "i4393")))
                           (hygiene guile))
                         ())
                       (if (null? (cdr #{x 4495}#))
                         (car #{x 4495}#)
                         (let ((#{tmp 4507}# #{x 4495}#))
                           (let ((#{tmp 4508}#
                                   ($sc-dispatch #{tmp 4507}# 'each-any)))
                             (if #{tmp 4508}#
                               (@apply
                                 (lambda (#{p 4510}#)
                                   (cons '#(syntax-object
                                            "append"
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(p)
                                               #((top))
                                               #("i4509"))
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
                                               ("i4405"
                                                "i4403"
                                                "i4401"
                                                "i4399"
                                                "i4397"
                                                "i4395"
                                                "i4393")))
                                            (hygiene guile))
                                         #{p 4510}#))
                                 #{tmp 4508}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4507}#)))))))
                   #{tmp 4500}#)
                 (let ((#{_ 4513}# #{tmp 4499}#))
                   (if (null? #{x 4495}#)
                     #{y 4496}#
                     (let ((#{tmp 4518}# (list #{x 4495}# #{y 4496}#)))
                       (let ((#{tmp 4519}#
                               ($sc-dispatch #{tmp 4518}# '(each-any any))))
                         (if #{tmp 4519}#
                           (@apply
                             (lambda (#{p 4522}# #{y 4523}#)
                               (cons '#(syntax-object
                                        "append"
                                        ((top)
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p y)
                                           #((top) (top))
                                           #("i4520" "i4521"))
                                         #(ribcage #(_) #((top)) #("i4512"))
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
                                           ("i4405"
                                            "i4403"
                                            "i4401"
                                            "i4399"
                                            "i4397"
                                            "i4395"
                                            "i4393")))
                                        (hygiene guile))
                                     (append #{p 4522}# (list #{y 4523}#))))
                             #{tmp 4519}#)
                           (syntax-violation
                             #f
                             "source expression failed to match any pattern"
                             #{tmp 4518}#)))))))))))
       (#{quasilist* 4402}#
         (lambda (#{x 4525}# #{y 4526}#)
           (letrec*
             ((#{f 4531}#
                (lambda (#{x 4532}#)
                  (if (null? #{x 4532}#)
                    #{y 4526}#
                    (#{quasicons 4398}#
                      (car #{x 4532}#)
                      (#{f 4531}# (cdr #{x 4532}#)))))))
             (#{f 4531}# #{x 4525}#))))
       (#{quasivector 4404}#
         (lambda (#{x 4533}#)
           (let ((#{tmp 4535}# #{x 4533}#))
             (let ((#{tmp 4536}#
                     ($sc-dispatch
                       #{tmp 4535}#
                       '(#(atom "quote") each-any))))
               (if #{tmp 4536}#
                 (@apply
                   (lambda (#{x 4538}#)
                     (list '#(syntax-object
                              "quote"
                              ((top)
                               #(ribcage #(x) #((top)) #("i4537"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4534"))
                               #(ribcage
                                 (emit quasivector
                                       quasilist*
                                       quasiappend
                                       quasicons
                                       vquasi
                                       quasi)
                                 ((top) (top) (top) (top) (top) (top) (top))
                                 ("i4405"
                                  "i4403"
                                  "i4401"
                                  "i4399"
                                  "i4397"
                                  "i4395"
                                  "i4393")))
                              (hygiene guile))
                           (list->vector #{x 4538}#)))
                   #{tmp 4536}#)
                 (let ((#{_ 4541}# #{tmp 4535}#))
                   (letrec*
                     ((#{f 4545}#
                        (lambda (#{y 4546}# #{k 4547}#)
                          (let ((#{tmp 4558}# #{y 4546}#))
                            (let ((#{tmp 4559}#
                                    ($sc-dispatch
                                      #{tmp 4558}#
                                      '(#(atom "quote") each-any))))
                              (if #{tmp 4559}#
                                (@apply
                                  (lambda (#{y 4561}#)
                                    (#{k 4547}#
                                      (map (lambda (#{tmp 4562}#)
                                             (list '#(syntax-object
                                                      "quote"
                                                      ((top)
                                                       #(ribcage
                                                         #(y)
                                                         #((top))
                                                         #("i4560"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(f y k)
                                                         #((top) (top) (top))
                                                         #("i4542"
                                                           "i4543"
                                                           "i4544"))
                                                       #(ribcage
                                                         #(_)
                                                         #((top))
                                                         #("i4540"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4534"))
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
                                                         ("i4405"
                                                          "i4403"
                                                          "i4401"
                                                          "i4399"
                                                          "i4397"
                                                          "i4395"
                                                          "i4393")))
                                                      (hygiene guile))
                                                   #{tmp 4562}#))
                                           #{y 4561}#)))
                                  #{tmp 4559}#)
                                (let ((#{tmp 4563}#
                                        ($sc-dispatch
                                          #{tmp 4558}#
                                          '(#(atom "list") . each-any))))
                                  (if #{tmp 4563}#
                                    (@apply
                                      (lambda (#{y 4565}#)
                                        (#{k 4547}# #{y 4565}#))
                                      #{tmp 4563}#)
                                    (let ((#{tmp 4567}#
                                            ($sc-dispatch
                                              #{tmp 4558}#
                                              '(#(atom "list*")
                                                .
                                                #(each+ any (any) ())))))
                                      (if #{tmp 4567}#
                                        (@apply
                                          (lambda (#{y 4570}# #{z 4571}#)
                                            (#{f 4545}#
                                              #{z 4571}#
                                              (lambda (#{ls 4572}#)
                                                (#{k 4547}#
                                                  (append
                                                    #{y 4570}#
                                                    #{ls 4572}#)))))
                                          #{tmp 4567}#)
                                        (let ((#{else 4576}# #{tmp 4558}#))
                                          (let ((#{tmp 4580}# #{x 4533}#))
                                            (let ((#{ g4577 4582}#
                                                    #{tmp 4580}#))
                                              (list '#(syntax-object
                                                       "list->vector"
                                                       ((top)
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(#{ g4577}#)
                                                          #((m4578 top))
                                                          #("i4581"))
                                                        #(ribcage
                                                          #(else)
                                                          #((top))
                                                          #("i4575"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(f y k)
                                                          #((top) (top) (top))
                                                          #("i4542"
                                                            "i4543"
                                                            "i4544"))
                                                        #(ribcage
                                                          #(_)
                                                          #((top))
                                                          #("i4540"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(x)
                                                          #((top))
                                                          #("i4534"))
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
                                                          ("i4405"
                                                           "i4403"
                                                           "i4401"
                                                           "i4399"
                                                           "i4397"
                                                           "i4395"
                                                           "i4393")))
                                                       (hygiene guile))
                                                    #{ g4577 4582}#))))))))))))))
                     (#{f 4545}#
                       #{x 4533}#
                       (lambda (#{ls 4548}#)
                         (let ((#{tmp 4553}# #{ls 4548}#))
                           (let ((#{tmp 4554}#
                                   ($sc-dispatch #{tmp 4553}# 'each-any)))
                             (if #{tmp 4554}#
                               (@apply
                                 (lambda (#{ g4550 4556}#)
                                   (cons '#(syntax-object
                                            "vector"
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(#{ g4550}#)
                                               #((m4551 top))
                                               #("i4555"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(ls)
                                               #((top))
                                               #("i4549"))
                                             #(ribcage
                                               #(_)
                                               #((top))
                                               #("i4540"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4534"))
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
                                               ("i4405"
                                                "i4403"
                                                "i4401"
                                                "i4399"
                                                "i4397"
                                                "i4395"
                                                "i4393")))
                                            (hygiene guile))
                                         #{ g4550 4556}#))
                                 #{tmp 4554}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4553}#)))))))))))))
       (#{emit 4406}#
         (lambda (#{x 4583}#)
           (let ((#{tmp 4585}# #{x 4583}#))
             (let ((#{tmp 4586}#
                     ($sc-dispatch
                       #{tmp 4585}#
                       '(#(atom "quote") any))))
               (if #{tmp 4586}#
                 (@apply
                   (lambda (#{x 4588}#)
                     (list '#(syntax-object
                              quote
                              ((top)
                               #(ribcage #(x) #((top)) #("i4587"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4584"))
                               #(ribcage
                                 (emit quasivector
                                       quasilist*
                                       quasiappend
                                       quasicons
                                       vquasi
                                       quasi)
                                 ((top) (top) (top) (top) (top) (top) (top))
                                 ("i4405"
                                  "i4403"
                                  "i4401"
                                  "i4399"
                                  "i4397"
                                  "i4395"
                                  "i4393")))
                              (hygiene guile))
                           #{x 4588}#))
                   #{tmp 4586}#)
                 (let ((#{tmp 4589}#
                         ($sc-dispatch
                           #{tmp 4585}#
                           '(#(atom "list") . each-any))))
                   (if #{tmp 4589}#
                     (@apply
                       (lambda (#{x 4591}#)
                         (let ((#{tmp 4595}# (map #{emit 4406}# #{x 4591}#)))
                           (let ((#{tmp 4596}#
                                   ($sc-dispatch #{tmp 4595}# 'each-any)))
                             (if #{tmp 4596}#
                               (@apply
                                 (lambda (#{ g4592 4598}#)
                                   (cons '#(syntax-object
                                            list
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(#{ g4592}#)
                                               #((m4593 top))
                                               #("i4597"))
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4590"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4584"))
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
                                               ("i4405"
                                                "i4403"
                                                "i4401"
                                                "i4399"
                                                "i4397"
                                                "i4395"
                                                "i4393")))
                                            (hygiene guile))
                                         #{ g4592 4598}#))
                                 #{tmp 4596}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4595}#)))))
                       #{tmp 4589}#)
                     (let ((#{tmp 4601}#
                             ($sc-dispatch
                               #{tmp 4585}#
                               '(#(atom "list*") . #(each+ any (any) ())))))
                       (if #{tmp 4601}#
                         (@apply
                           (lambda (#{x 4604}# #{y 4605}#)
                             (letrec*
                               ((#{f 4608}#
                                  (lambda (#{x* 4609}#)
                                    (if (null? #{x* 4609}#)
                                      (#{emit 4406}# #{y 4605}#)
                                      (let ((#{tmp 4615}#
                                              (list (#{emit 4406}#
                                                      (car #{x* 4609}#))
                                                    (#{f 4608}#
                                                      (cdr #{x* 4609}#)))))
                                        (let ((#{tmp 4616}#
                                                ($sc-dispatch
                                                  #{tmp 4615}#
                                                  '(any any))))
                                          (if #{tmp 4616}#
                                            (@apply
                                              (lambda (#{ g4612 4619}#
                                                       #{ g4611 4620}#)
                                                (list '#(syntax-object
                                                         cons
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(#{ g4612}#
                                                              #{ g4611}#)
                                                            #((m4613 top)
                                                              (m4613 top))
                                                            #("i4617" "i4618"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(f x*)
                                                            #((top) (top))
                                                            #("i4606" "i4607"))
                                                          #(ribcage
                                                            #(x y)
                                                            #((top) (top))
                                                            #("i4602" "i4603"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x)
                                                            #((top))
                                                            #("i4584"))
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
                                                            ("i4405"
                                                             "i4403"
                                                             "i4401"
                                                             "i4399"
                                                             "i4397"
                                                             "i4395"
                                                             "i4393")))
                                                         (hygiene guile))
                                                      #{ g4612 4619}#
                                                      #{ g4611 4620}#))
                                              #{tmp 4616}#)
                                            (syntax-violation
                                              #f
                                              "source expression failed to match any pattern"
                                              #{tmp 4615}#))))))))
                               (#{f 4608}# #{x 4604}#)))
                           #{tmp 4601}#)
                         (let ((#{tmp 4621}#
                                 ($sc-dispatch
                                   #{tmp 4585}#
                                   '(#(atom "append") . each-any))))
                           (if #{tmp 4621}#
                             (@apply
                               (lambda (#{x 4623}#)
                                 (let ((#{tmp 4627}#
                                         (map #{emit 4406}# #{x 4623}#)))
                                   (let ((#{tmp 4628}#
                                           ($sc-dispatch
                                             #{tmp 4627}#
                                             'each-any)))
                                     (if #{tmp 4628}#
                                       (@apply
                                         (lambda (#{ g4624 4630}#)
                                           (cons '#(syntax-object
                                                    append
                                                    ((top)
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(#{ g4624}#)
                                                       #((m4625 top))
                                                       #("i4629"))
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4622"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4584"))
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
                                                       ("i4405"
                                                        "i4403"
                                                        "i4401"
                                                        "i4399"
                                                        "i4397"
                                                        "i4395"
                                                        "i4393")))
                                                    (hygiene guile))
                                                 #{ g4624 4630}#))
                                         #{tmp 4628}#)
                                       (syntax-violation
                                         #f
                                         "source expression failed to match any pattern"
                                         #{tmp 4627}#)))))
                               #{tmp 4621}#)
                             (let ((#{tmp 4633}#
                                     ($sc-dispatch
                                       #{tmp 4585}#
                                       '(#(atom "vector") . each-any))))
                               (if #{tmp 4633}#
                                 (@apply
                                   (lambda (#{x 4635}#)
                                     (let ((#{tmp 4639}#
                                             (map #{emit 4406}# #{x 4635}#)))
                                       (let ((#{tmp 4640}#
                                               ($sc-dispatch
                                                 #{tmp 4639}#
                                                 'each-any)))
                                         (if #{tmp 4640}#
                                           (@apply
                                             (lambda (#{ g4636 4642}#)
                                               (cons '#(syntax-object
                                                        vector
                                                        ((top)
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(#{ g4636}#)
                                                           #((m4637 top))
                                                           #("i4641"))
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4634"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4584"))
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
                                                           ("i4405"
                                                            "i4403"
                                                            "i4401"
                                                            "i4399"
                                                            "i4397"
                                                            "i4395"
                                                            "i4393")))
                                                        (hygiene guile))
                                                     #{ g4636 4642}#))
                                             #{tmp 4640}#)
                                           (syntax-violation
                                             #f
                                             "source expression failed to match any pattern"
                                             #{tmp 4639}#)))))
                                   #{tmp 4633}#)
                                 (let ((#{tmp 4645}#
                                         ($sc-dispatch
                                           #{tmp 4585}#
                                           '(#(atom "list->vector") any))))
                                   (if #{tmp 4645}#
                                     (@apply
                                       (lambda (#{x 4647}#)
                                         (let ((#{tmp 4651}#
                                                 (#{emit 4406}# #{x 4647}#)))
                                           (let ((#{ g4648 4653}#
                                                   #{tmp 4651}#))
                                             (list '#(syntax-object
                                                      list->vector
                                                      ((top)
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(#{ g4648}#)
                                                         #((m4649 top))
                                                         #("i4652"))
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4646"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4584"))
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
                                                         ("i4405"
                                                          "i4403"
                                                          "i4401"
                                                          "i4399"
                                                          "i4397"
                                                          "i4395"
                                                          "i4393")))
                                                      (hygiene guile))
                                                   #{ g4648 4653}#))))
                                       #{tmp 4645}#)
                                     (let ((#{tmp 4654}#
                                             ($sc-dispatch
                                               #{tmp 4585}#
                                               '(#(atom "value") any))))
                                       (if #{tmp 4654}#
                                         (@apply
                                           (lambda (#{x 4656}#) #{x 4656}#)
                                           #{tmp 4654}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp 4585}#)))))))))))))))))))
      (lambda (#{x 4657}#)
        (let ((#{tmp 4659}# #{x 4657}#))
          (let ((#{tmp 4660}#
                  ($sc-dispatch #{tmp 4659}# '(_ any))))
            (if #{tmp 4660}#
              (@apply
                (lambda (#{e 4662}#)
                  (#{emit 4406}# (#{quasi 4394}# #{e 4662}# 0)))
                #{tmp 4660}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 4659}#))))))))

(define include
  (make-syntax-transformer
    'include
    'macro
    (lambda (#{x 4663}#)
      (letrec*
        ((#{read-file 4666}#
           (lambda (#{fn 4667}# #{k 4668}#)
             (let ((#{p 4672}# (open-input-file #{fn 4667}#)))
               (letrec*
                 ((#{f 4676}#
                    (lambda (#{x 4677}# #{result 4678}#)
                      (if (eof-object? #{x 4677}#)
                        (begin
                          (close-input-port #{p 4672}#)
                          (reverse #{result 4678}#))
                        (#{f 4676}#
                          (read #{p 4672}#)
                          (cons (datum->syntax #{k 4668}# #{x 4677}#)
                                #{result 4678}#))))))
                 (#{f 4676}# (read #{p 4672}#) '()))))))
        (let ((#{tmp 4679}# #{x 4663}#))
          (let ((#{tmp 4680}#
                  ($sc-dispatch #{tmp 4679}# '(any any))))
            (if #{tmp 4680}#
              (@apply
                (lambda (#{k 4683}# #{filename 4684}#)
                  (let ((#{fn 4686}# (syntax->datum #{filename 4684}#)))
                    (let ((#{tmp 4688}#
                            (#{read-file 4666}#
                              #{fn 4686}#
                              #{filename 4684}#)))
                      (let ((#{tmp 4689}#
                              ($sc-dispatch #{tmp 4688}# 'each-any)))
                        (if #{tmp 4689}#
                          (@apply
                            (lambda (#{exp 4691}#)
                              (cons '#(syntax-object
                                       begin
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage #(exp) #((top)) #("i4690"))
                                        #(ribcage () () ())
                                        #(ribcage () () ())
                                        #(ribcage #(fn) #((top)) #("i4685"))
                                        #(ribcage
                                          #(k filename)
                                          #((top) (top))
                                          #("i4681" "i4682"))
                                        #(ribcage
                                          (read-file)
                                          ((top))
                                          ("i4665"))
                                        #(ribcage #(x) #((top)) #("i4664")))
                                       (hygiene guile))
                                    #{exp 4691}#))
                            #{tmp 4689}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp 4688}#))))))
                #{tmp 4680}#)
              (syntax-violation
                #f
                "source expression failed to match any pattern"
                #{tmp 4679}#))))))))

(define include-from-path
  (make-syntax-transformer
    'include-from-path
    'macro
    (lambda (#{x 4693}#)
      (let ((#{tmp 4695}# #{x 4693}#))
        (let ((#{tmp 4696}#
                ($sc-dispatch #{tmp 4695}# '(any any))))
          (if #{tmp 4696}#
            (@apply
              (lambda (#{k 4699}# #{filename 4700}#)
                (let ((#{fn 4702}# (syntax->datum #{filename 4700}#)))
                  (let ((#{tmp 4704}#
                          (datum->syntax
                            #{filename 4700}#
                            (let ((#{t 4709}# (%search-load-path #{fn 4702}#)))
                              (if #{t 4709}#
                                #{t 4709}#
                                (syntax-violation
                                  'include-from-path
                                  "file not found in path"
                                  #{x 4693}#
                                  #{filename 4700}#))))))
                    (let ((#{fn 4706}# #{tmp 4704}#))
                      (list '#(syntax-object
                               include
                               ((top)
                                #(ribcage () () ())
                                #(ribcage #(fn) #((top)) #("i4705"))
                                #(ribcage () () ())
                                #(ribcage () () ())
                                #(ribcage #(fn) #((top)) #("i4701"))
                                #(ribcage
                                  #(k filename)
                                  #((top) (top))
                                  #("i4697" "i4698"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4694")))
                               (hygiene guile))
                            #{fn 4706}#)))))
              #{tmp 4696}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4695}#)))))))

(define unquote
  (make-syntax-transformer
    'unquote
    'macro
    (lambda (#{x 4711}#)
      (syntax-violation
        'unquote
        "expression not valid outside of quasiquote"
        #{x 4711}#))))

(define unquote-splicing
  (make-syntax-transformer
    'unquote-splicing
    'macro
    (lambda (#{x 4713}#)
      (syntax-violation
        'unquote-splicing
        "expression not valid outside of quasiquote"
        #{x 4713}#))))

(define case
  (make-syntax-transformer
    'case
    'macro
    (lambda (#{x 4715}#)
      (let ((#{tmp 4717}# #{x 4715}#))
        (let ((#{tmp 4718}#
                ($sc-dispatch
                  #{tmp 4717}#
                  '(_ any any . each-any))))
          (if #{tmp 4718}#
            (@apply
              (lambda (#{e 4722}# #{m1 4723}# #{m2 4724}#)
                (let ((#{tmp 4726}#
                        (letrec*
                          ((#{f 4732}#
                             (lambda (#{clause 4733}# #{clauses 4734}#)
                               (if (null? #{clauses 4734}#)
                                 (let ((#{tmp 4736}# #{clause 4733}#))
                                   (let ((#{tmp 4737}#
                                           ($sc-dispatch
                                             #{tmp 4736}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   else
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(f clause clauses)
                                                      #((top) (top) (top))
                                                      #("i4729"
                                                        "i4730"
                                                        "i4731"))
                                                    #(ribcage
                                                      #(e m1 m2)
                                                      #((top) (top) (top))
                                                      #("i4719"
                                                        "i4720"
                                                        "i4721"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i4716")))
                                                   (hygiene guile)))
                                               any
                                               .
                                               each-any))))
                                     (if #{tmp 4737}#
                                       (@apply
                                         (lambda (#{e1 4740}# #{e2 4741}#)
                                           (cons '#(syntax-object
                                                    begin
                                                    ((top)
                                                     #(ribcage
                                                       #(e1 e2)
                                                       #((top) (top))
                                                       #("i4738" "i4739"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(f clause clauses)
                                                       #((top) (top) (top))
                                                       #("i4729"
                                                         "i4730"
                                                         "i4731"))
                                                     #(ribcage
                                                       #(e m1 m2)
                                                       #((top) (top) (top))
                                                       #("i4719"
                                                         "i4720"
                                                         "i4721"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4716")))
                                                    (hygiene guile))
                                                 (cons #{e1 4740}#
                                                       #{e2 4741}#)))
                                         #{tmp 4737}#)
                                       (let ((#{tmp 4743}#
                                               ($sc-dispatch
                                                 #{tmp 4736}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp 4743}#
                                           (@apply
                                             (lambda (#{k 4747}#
                                                      #{e1 4748}#
                                                      #{e2 4749}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4744"
                                                             "i4745"
                                                             "i4746"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4729"
                                                             "i4730"
                                                             "i4731"))
                                                         #(ribcage
                                                           #(e m1 m2)
                                                           #((top) (top) (top))
                                                           #("i4719"
                                                             "i4720"
                                                             "i4721"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4716")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4744"
                                                                   "i4745"
                                                                   "i4746"))
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
                                                                 #("i4729"
                                                                   "i4730"
                                                                   "i4731"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4719"
                                                                   "i4720"
                                                                   "i4721"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4716")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4744"
                                                                   "i4745"
                                                                   "i4746"))
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
                                                                 #("i4729"
                                                                   "i4730"
                                                                   "i4731"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4719"
                                                                   "i4720"
                                                                   "i4721"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4716")))
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
                                                                       #("i4744"
                                                                         "i4745"
                                                                         "i4746"))
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
                                                                       #("i4729"
                                                                         "i4730"
                                                                         "i4731"))
                                                                     #(ribcage
                                                                       #(e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4719"
                                                                         "i4720"
                                                                         "i4721"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4716")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k 4747}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4744"
                                                                   "i4745"
                                                                   "i4746"))
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
                                                                 #("i4729"
                                                                   "i4730"
                                                                   "i4731"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4719"
                                                                   "i4720"
                                                                   "i4721"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4716")))
                                                              (hygiene guile))
                                                           (cons #{e1 4748}#
                                                                 #{e2 4749}#))))
                                             #{tmp 4743}#)
                                           (let ((#{_ 4753}# #{tmp 4736}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x 4715}#
                                               #{clause 4733}#)))))))
                                 (let ((#{tmp 4755}#
                                         (#{f 4732}#
                                           (car #{clauses 4734}#)
                                           (cdr #{clauses 4734}#))))
                                   (let ((#{rest 4757}# #{tmp 4755}#))
                                     (let ((#{tmp 4758}# #{clause 4733}#))
                                       (let ((#{tmp 4759}#
                                               ($sc-dispatch
                                                 #{tmp 4758}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp 4759}#
                                           (@apply
                                             (lambda (#{k 4763}#
                                                      #{e1 4764}#
                                                      #{e2 4765}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4760"
                                                             "i4761"
                                                             "i4762"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(rest)
                                                           #((top))
                                                           #("i4756"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4729"
                                                             "i4730"
                                                             "i4731"))
                                                         #(ribcage
                                                           #(e m1 m2)
                                                           #((top) (top) (top))
                                                           #("i4719"
                                                             "i4720"
                                                             "i4721"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4716")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4760"
                                                                   "i4761"
                                                                   "i4762"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4756"))
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
                                                                 #("i4729"
                                                                   "i4730"
                                                                   "i4731"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4719"
                                                                   "i4720"
                                                                   "i4721"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4716")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4760"
                                                                   "i4761"
                                                                   "i4762"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4756"))
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
                                                                 #("i4729"
                                                                   "i4730"
                                                                   "i4731"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4719"
                                                                   "i4720"
                                                                   "i4721"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4716")))
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
                                                                       #("i4760"
                                                                         "i4761"
                                                                         "i4762"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(rest)
                                                                       #((top))
                                                                       #("i4756"))
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
                                                                       #("i4729"
                                                                         "i4730"
                                                                         "i4731"))
                                                                     #(ribcage
                                                                       #(e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4719"
                                                                         "i4720"
                                                                         "i4721"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4716")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k 4763}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4760"
                                                                   "i4761"
                                                                   "i4762"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4756"))
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
                                                                 #("i4729"
                                                                   "i4730"
                                                                   "i4731"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4719"
                                                                   "i4720"
                                                                   "i4721"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4716")))
                                                              (hygiene guile))
                                                           (cons #{e1 4764}#
                                                                 #{e2 4765}#))
                                                     #{rest 4757}#))
                                             #{tmp 4759}#)
                                           (let ((#{_ 4769}# #{tmp 4758}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x 4715}#
                                               #{clause 4733}#)))))))))))
                          (#{f 4732}# #{m1 4723}# #{m2 4724}#))))
                  (let ((#{body 4728}# #{tmp 4726}#))
                    (list '#(syntax-object
                             let
                             ((top)
                              #(ribcage () () ())
                              #(ribcage #(body) #((top)) #("i4727"))
                              #(ribcage
                                #(e m1 m2)
                                #((top) (top) (top))
                                #("i4719" "i4720" "i4721"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4716")))
                             (hygiene guile))
                          (list (list '#(syntax-object
                                         t
                                         ((top)
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(body)
                                            #((top))
                                            #("i4727"))
                                          #(ribcage
                                            #(e m1 m2)
                                            #((top) (top) (top))
                                            #("i4719" "i4720" "i4721"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4716")))
                                         (hygiene guile))
                                      #{e 4722}#))
                          #{body 4728}#))))
              #{tmp 4718}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4717}#)))))))

(define make-variable-transformer
  (lambda (#{proc 4770}#)
    (if (procedure? #{proc 4770}#)
      (letrec*
        ((#{trans 4773}#
           (lambda (#{x 4774}#) (#{proc 4770}# #{x 4774}#))))
        (begin
          (set-procedure-property!
            #{trans 4773}#
            'variable-transformer
            #t)
          #{trans 4773}#))
      (error "variable transformer not a procedure"
             #{proc 4770}#))))

(define identifier-syntax
  (make-syntax-transformer
    'identifier-syntax
    'macro
    (lambda (#{x 4776}#)
      (let ((#{tmp 4778}# #{x 4776}#))
        (let ((#{tmp 4779}#
                ($sc-dispatch #{tmp 4778}# '(_ any))))
          (if #{tmp 4779}#
            (@apply
              (lambda (#{e 4781}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage #(e) #((top)) #("i4780"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4777")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage #(e) #((top)) #("i4780"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4777")))
                          (hygiene guile)))
                      '#((#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage #(e) #((top)) #("i4780"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4777")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            identifier-syntax
                            ((top)
                             #(ribcage #(e) #((top)) #("i4780"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4777")))
                            (hygiene guile))))
                      (list '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage #(e) #((top)) #("i4780"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4777")))
                               (hygiene guile))
                            '#(syntax-object
                               x
                               ((top)
                                #(ribcage #(e) #((top)) #("i4780"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4777")))
                               (hygiene guile))
                            '()
                            (list '#(syntax-object
                                     id
                                     ((top)
                                      #(ribcage #(e) #((top)) #("i4780"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4777")))
                                     (hygiene guile))
                                  '(#(syntax-object
                                      identifier?
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4780"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4777")))
                                      (hygiene guile))
                                    (#(syntax-object
                                       syntax
                                       ((top)
                                        #(ribcage #(e) #((top)) #("i4780"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4777")))
                                       (hygiene guile))
                                     #(syntax-object
                                       id
                                       ((top)
                                        #(ribcage #(e) #((top)) #("i4780"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4777")))
                                       (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage #(e) #((top)) #("i4780"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4777")))
                                           (hygiene guile))
                                        #{e 4781}#))
                            (list '(#(syntax-object
                                      _
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4780"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4777")))
                                      (hygiene guile))
                                    #(syntax-object
                                      x
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4780"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4777")))
                                      (hygiene guile))
                                    #(syntax-object
                                      ...
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4780"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4777")))
                                      (hygiene guile)))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage #(e) #((top)) #("i4780"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4777")))
                                           (hygiene guile))
                                        (cons #{e 4781}#
                                              '(#(syntax-object
                                                  x
                                                  ((top)
                                                   #(ribcage
                                                     #(e)
                                                     #((top))
                                                     #("i4780"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4777")))
                                                  (hygiene guile))
                                                #(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(e)
                                                     #((top))
                                                     #("i4780"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4777")))
                                                  (hygiene guile)))))))))
              #{tmp 4779}#)
            (let ((#{tmp 4782}#
                    ($sc-dispatch
                      #{tmp 4778}#
                      '(_ (any any)
                          ((#(free-id
                              #(syntax-object
                                set!
                                ((top)
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4777")))
                                (hygiene guile)))
                            any
                            any)
                           any)))))
              (if (if #{tmp 4782}#
                    (@apply
                      (lambda (#{id 4788}#
                               #{exp1 4789}#
                               #{var 4790}#
                               #{val 4791}#
                               #{exp2 4792}#)
                        (if (identifier? #{id 4788}#)
                          (identifier? #{var 4790}#)
                          #f))
                      #{tmp 4782}#)
                    #f)
                (@apply
                  (lambda (#{id 4800}#
                           #{exp1 4801}#
                           #{var 4802}#
                           #{val 4803}#
                           #{exp2 4804}#)
                    (list '#(syntax-object
                             make-variable-transformer
                             ((top)
                              #(ribcage
                                #(id exp1 var val exp2)
                                #((top) (top) (top) (top) (top))
                                #("i4795" "i4796" "i4797" "i4798" "i4799"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4777")))
                             (hygiene guile))
                          (list '#(syntax-object
                                   lambda
                                   ((top)
                                    #(ribcage
                                      #(id exp1 var val exp2)
                                      #((top) (top) (top) (top) (top))
                                      #("i4795"
                                        "i4796"
                                        "i4797"
                                        "i4798"
                                        "i4799"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i4777")))
                                   (hygiene guile))
                                '(#(syntax-object
                                    x
                                    ((top)
                                     #(ribcage
                                       #(id exp1 var val exp2)
                                       #((top) (top) (top) (top) (top))
                                       #("i4795"
                                         "i4796"
                                         "i4797"
                                         "i4798"
                                         "i4799"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i4777")))
                                    (hygiene guile)))
                                '#((#(syntax-object
                                      macro-type
                                      ((top)
                                       #(ribcage
                                         #(id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top))
                                         #("i4795"
                                           "i4796"
                                           "i4797"
                                           "i4798"
                                           "i4799"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4777")))
                                      (hygiene guile))
                                    .
                                    #(syntax-object
                                      variable-transformer
                                      ((top)
                                       #(ribcage
                                         #(id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top))
                                         #("i4795"
                                           "i4796"
                                           "i4797"
                                           "i4798"
                                           "i4799"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4777")))
                                      (hygiene guile))))
                                (list '#(syntax-object
                                         syntax-case
                                         ((top)
                                          #(ribcage
                                            #(id exp1 var val exp2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4795"
                                              "i4796"
                                              "i4797"
                                              "i4798"
                                              "i4799"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4777")))
                                         (hygiene guile))
                                      '#(syntax-object
                                         x
                                         ((top)
                                          #(ribcage
                                            #(id exp1 var val exp2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4795"
                                              "i4796"
                                              "i4797"
                                              "i4798"
                                              "i4799"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4777")))
                                         (hygiene guile))
                                      '(#(syntax-object
                                          set!
                                          ((top)
                                           #(ribcage
                                             #(id exp1 var val exp2)
                                             #((top) (top) (top) (top) (top))
                                             #("i4795"
                                               "i4796"
                                               "i4797"
                                               "i4798"
                                               "i4799"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i4777")))
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
                                                        #("i4795"
                                                          "i4796"
                                                          "i4797"
                                                          "i4798"
                                                          "i4799"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4777")))
                                                     (hygiene guile))
                                                  #{var 4802}#
                                                  #{val 4803}#)
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
                                                        #("i4795"
                                                          "i4796"
                                                          "i4797"
                                                          "i4798"
                                                          "i4799"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4777")))
                                                     (hygiene guile))
                                                  #{exp2 4804}#))
                                      (list (cons #{id 4800}#
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
                                                         #("i4795"
                                                           "i4796"
                                                           "i4797"
                                                           "i4798"
                                                           "i4799"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4777")))
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
                                                         #("i4795"
                                                           "i4796"
                                                           "i4797"
                                                           "i4798"
                                                           "i4799"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4777")))
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
                                                        #("i4795"
                                                          "i4796"
                                                          "i4797"
                                                          "i4798"
                                                          "i4799"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4777")))
                                                     (hygiene guile))
                                                  (cons #{exp1 4801}#
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
                                                               #("i4795"
                                                                 "i4796"
                                                                 "i4797"
                                                                 "i4798"
                                                                 "i4799"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4777")))
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
                                                               #("i4795"
                                                                 "i4796"
                                                                 "i4797"
                                                                 "i4798"
                                                                 "i4799"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4777")))
                                                            (hygiene
                                                              guile))))))
                                      (list #{id 4800}#
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
                                                        #("i4795"
                                                          "i4796"
                                                          "i4797"
                                                          "i4798"
                                                          "i4799"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4777")))
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
                                                              #("i4795"
                                                                "i4796"
                                                                "i4797"
                                                                "i4798"
                                                                "i4799"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(x)
                                                              #((top))
                                                              #("i4777")))
                                                           (hygiene guile))
                                                        #{id 4800}#))
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
                                                        #("i4795"
                                                          "i4796"
                                                          "i4797"
                                                          "i4798"
                                                          "i4799"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4777")))
                                                     (hygiene guile))
                                                  #{exp1 4801}#))))))
                  #{tmp 4782}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4778}#)))))))))

(define define*
  (make-syntax-transformer
    'define*
    'macro
    (lambda (#{x 4805}#)
      (let ((#{tmp 4807}# #{x 4805}#))
        (let ((#{tmp 4808}#
                ($sc-dispatch
                  #{tmp 4807}#
                  '(_ (any . any) any . each-any))))
          (if #{tmp 4808}#
            (@apply
              (lambda (#{id 4813}#
                       #{args 4814}#
                       #{b0 4815}#
                       #{b1 4816}#)
                (list '#(syntax-object
                         define
                         ((top)
                          #(ribcage
                            #(id args b0 b1)
                            #((top) (top) (top) (top))
                            #("i4809" "i4810" "i4811" "i4812"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4806")))
                         (hygiene guile))
                      #{id 4813}#
                      (cons '#(syntax-object
                               lambda*
                               ((top)
                                #(ribcage
                                  #(id args b0 b1)
                                  #((top) (top) (top) (top))
                                  #("i4809" "i4810" "i4811" "i4812"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4806")))
                               (hygiene guile))
                            (cons #{args 4814}#
                                  (cons #{b0 4815}# #{b1 4816}#)))))
              #{tmp 4808}#)
            (let ((#{tmp 4818}#
                    ($sc-dispatch #{tmp 4807}# '(_ any any))))
              (if (if #{tmp 4818}#
                    (@apply
                      (lambda (#{id 4821}# #{val 4822}#)
                        (identifier?
                          '#(syntax-object
                             x
                             ((top)
                              #(ribcage
                                #(id val)
                                #((top) (top))
                                #("i4819" "i4820"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4806")))
                             (hygiene guile))))
                      #{tmp 4818}#)
                    #f)
                (@apply
                  (lambda (#{id 4825}# #{val 4826}#)
                    (list '#(syntax-object
                             define
                             ((top)
                              #(ribcage
                                #(id val)
                                #((top) (top))
                                #("i4823" "i4824"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4806")))
                             (hygiene guile))
                          #{id 4825}#
                          #{val 4826}#))
                  #{tmp 4818}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4807}#)))))))))

