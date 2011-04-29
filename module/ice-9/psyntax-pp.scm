(eval-when (compile) (set-current-module (resolve-module (quote (guile)))))
(if #f #f)

(letrec*
  ((#{and-map* 39}#
     (lambda (#{f 203}# #{first 204}# . #{rest 205}#)
       (begin
         (let ((#{t 211}# (null? #{first 204}#)))
           (if #{t 211}#
             #{t 211}#
             (if (null? #{rest 205}#)
               (letrec*
                 ((#{andmap 215}#
                    (lambda (#{first 216}#)
                      (begin
                        (let ((#{x 219}# (car #{first 216}#))
                              (#{first 220}# (cdr #{first 216}#)))
                          (if (null? #{first 220}#)
                            (#{f 203}# #{x 219}#)
                            (if (#{f 203}# #{x 219}#)
                              (#{andmap 215}# #{first 220}#)
                              #f)))))))
                 (begin (#{andmap 215}# #{first 204}#)))
               (letrec*
                 ((#{andmap 226}#
                    (lambda (#{first 227}# #{rest 228}#)
                      (begin
                        (let ((#{x 233}# (car #{first 227}#))
                              (#{xr 234}# (map car #{rest 228}#))
                              (#{first 235}# (cdr #{first 227}#))
                              (#{rest 236}# (map cdr #{rest 228}#)))
                          (if (null? #{first 235}#)
                            (@apply #{f 203}# #{x 233}# #{xr 234}#)
                            (if (@apply #{f 203}# #{x 233}# #{xr 234}#)
                              (#{andmap 226}# #{first 235}# #{rest 236}#)
                              #f)))))))
                 (begin
                   (#{andmap 226}# #{first 204}# #{rest 205}#))))))))))
  (begin
    (letrec*
      ((#{make-void 241}#
         (lambda (#{src 799}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 0)
             #{src 799}#)))
       (#{make-const 243}#
         (lambda (#{src 801}# #{exp 802}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 1)
             #{src 801}#
             #{exp 802}#)))
       (#{make-lexical-ref 247}#
         (lambda (#{src 809}# #{name 810}# #{gensym 811}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 3)
             #{src 809}#
             #{name 810}#
             #{gensym 811}#)))
       (#{make-lexical-set 249}#
         (lambda (#{src 815}#
                  #{name 816}#
                  #{gensym 817}#
                  #{exp 818}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 4)
             #{src 815}#
             #{name 816}#
             #{gensym 817}#
             #{exp 818}#)))
       (#{make-module-ref 251}#
         (lambda (#{src 823}#
                  #{mod 824}#
                  #{name 825}#
                  #{public? 826}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 5)
             #{src 823}#
             #{mod 824}#
             #{name 825}#
             #{public? 826}#)))
       (#{make-module-set 253}#
         (lambda (#{src 831}#
                  #{mod 832}#
                  #{name 833}#
                  #{public? 834}#
                  #{exp 835}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 6)
             #{src 831}#
             #{mod 832}#
             #{name 833}#
             #{public? 834}#
             #{exp 835}#)))
       (#{make-toplevel-ref 255}#
         (lambda (#{src 841}# #{name 842}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 7)
             #{src 841}#
             #{name 842}#)))
       (#{make-toplevel-set 257}#
         (lambda (#{src 845}# #{name 846}# #{exp 847}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 8)
             #{src 845}#
             #{name 846}#
             #{exp 847}#)))
       (#{make-toplevel-define 259}#
         (lambda (#{src 851}# #{name 852}# #{exp 853}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 9)
             #{src 851}#
             #{name 852}#
             #{exp 853}#)))
       (#{make-conditional 261}#
         (lambda (#{src 857}#
                  #{test 858}#
                  #{consequent 859}#
                  #{alternate 860}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 10)
             #{src 857}#
             #{test 858}#
             #{consequent 859}#
             #{alternate 860}#)))
       (#{make-application 263}#
         (lambda (#{src 865}# #{proc 866}# #{args 867}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 11)
             #{src 865}#
             #{proc 866}#
             #{args 867}#)))
       (#{make-sequence 265}#
         (lambda (#{src 871}# #{exps 872}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 12)
             #{src 871}#
             #{exps 872}#)))
       (#{make-lambda 267}#
         (lambda (#{src 875}# #{meta 876}# #{body 877}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 13)
             #{src 875}#
             #{meta 876}#
             #{body 877}#)))
       (#{make-lambda-case 269}#
         (lambda (#{src 881}#
                  #{req 882}#
                  #{opt 883}#
                  #{rest 884}#
                  #{kw 885}#
                  #{inits 886}#
                  #{gensyms 887}#
                  #{body 888}#
                  #{alternate 889}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 14)
             #{src 881}#
             #{req 882}#
             #{opt 883}#
             #{rest 884}#
             #{kw 885}#
             #{inits 886}#
             #{gensyms 887}#
             #{body 888}#
             #{alternate 889}#)))
       (#{make-let 271}#
         (lambda (#{src 899}#
                  #{names 900}#
                  #{gensyms 901}#
                  #{vals 902}#
                  #{body 903}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 15)
             #{src 899}#
             #{names 900}#
             #{gensyms 901}#
             #{vals 902}#
             #{body 903}#)))
       (#{make-letrec 273}#
         (lambda (#{src 909}#
                  #{in-order? 910}#
                  #{names 911}#
                  #{gensyms 912}#
                  #{vals 913}#
                  #{body 914}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 16)
             #{src 909}#
             #{in-order? 910}#
             #{names 911}#
             #{gensyms 912}#
             #{vals 913}#
             #{body 914}#)))
       (#{make-dynlet 275}#
         (lambda (#{src 921}#
                  #{fluids 922}#
                  #{vals 923}#
                  #{body 924}#)
           (make-struct/no-tail
             (vector-ref %expanded-vtables 17)
             #{src 921}#
             #{fluids 922}#
             #{vals 923}#
             #{body 924}#)))
       (#{lambda? 278}#
         (lambda (#{x 929}#)
           (if (struct? #{x 929}#)
             (eq? (struct-vtable #{x 929}#)
                  (vector-ref %expanded-vtables 13))
             #f)))
       (#{lambda-meta 280}#
         (lambda (#{x 933}#) (struct-ref #{x 933}# 1)))
       (#{set-lambda-meta! 282}#
         (lambda (#{x 935}# #{v 936}#)
           (struct-set! #{x 935}# 1 #{v 936}#)))
       (#{top-level-eval-hook 288}#
         (lambda (#{x 939}# #{mod 940}#)
           (primitive-eval #{x 939}#)))
       (#{local-eval-hook 290}#
         (lambda (#{x 943}# #{mod 944}#)
           (primitive-eval #{x 943}#)))
       (#{put-global-definition-hook 293}#
         (lambda (#{symbol 947}# #{type 948}# #{val 949}#)
           (module-define!
             (current-module)
             #{symbol 947}#
             (make-syntax-transformer
               #{symbol 947}#
               #{type 948}#
               #{val 949}#))))
       (#{get-global-definition-hook 295}#
         (lambda (#{symbol 953}# #{module 954}#)
           (begin
             (if (if (not #{module 954}#) (current-module) #f)
               (warn "module system is booted, we should have a module"
                     #{symbol 953}#))
             (begin
               (let ((#{v 960}# (module-variable
                                  (if #{module 954}#
                                    (resolve-module (cdr #{module 954}#))
                                    (current-module))
                                  #{symbol 953}#)))
                 (if #{v 960}#
                   (if (variable-bound? #{v 960}#)
                     (begin
                       (let ((#{val 965}# (variable-ref #{v 960}#)))
                         (if (macro? #{val 965}#)
                           (if (macro-type #{val 965}#)
                             (cons (macro-type #{val 965}#)
                                   (macro-binding #{val 965}#))
                             #f)
                           #f)))
                     #f)
                   #f))))))
       (#{decorate-source 297}#
         (lambda (#{e 969}# #{s 970}#)
           (begin
             (if (if (pair? #{e 969}#) #{s 970}# #f)
               (set-source-properties! #{e 969}# #{s 970}#))
             #{e 969}#)))
       (#{maybe-name-value! 299}#
         (lambda (#{name 975}# #{val 976}#)
           (if (#{lambda? 278}# #{val 976}#)
             (begin
               (let ((#{meta 980}# (#{lambda-meta 280}# #{val 976}#)))
                 (if (not (assq 'name #{meta 980}#))
                   (#{set-lambda-meta! 282}#
                     #{val 976}#
                     (cons (cons 'name #{name 975}#) #{meta 980}#))))))))
       (#{build-void 301}#
         (lambda (#{source 981}#)
           (#{make-void 241}# #{source 981}#)))
       (#{build-application 303}#
         (lambda (#{source 983}# #{fun-exp 984}# #{arg-exps 985}#)
           (#{make-application 263}#
             #{source 983}#
             #{fun-exp 984}#
             #{arg-exps 985}#)))
       (#{build-conditional 305}#
         (lambda (#{source 989}#
                  #{test-exp 990}#
                  #{then-exp 991}#
                  #{else-exp 992}#)
           (#{make-conditional 261}#
             #{source 989}#
             #{test-exp 990}#
             #{then-exp 991}#
             #{else-exp 992}#)))
       (#{build-dynlet 307}#
         (lambda (#{source 997}#
                  #{fluids 998}#
                  #{vals 999}#
                  #{body 1000}#)
           (#{make-dynlet 275}#
             #{source 997}#
             #{fluids 998}#
             #{vals 999}#
             #{body 1000}#)))
       (#{build-lexical-reference 309}#
         (lambda (#{type 1005}#
                  #{source 1006}#
                  #{name 1007}#
                  #{var 1008}#)
           (#{make-lexical-ref 247}#
             #{source 1006}#
             #{name 1007}#
             #{var 1008}#)))
       (#{build-lexical-assignment 311}#
         (lambda (#{source 1013}#
                  #{name 1014}#
                  #{var 1015}#
                  #{exp 1016}#)
           (begin
             (#{maybe-name-value! 299}#
               #{name 1014}#
               #{exp 1016}#)
             (#{make-lexical-set 249}#
               #{source 1013}#
               #{name 1014}#
               #{var 1015}#
               #{exp 1016}#))))
       (#{analyze-variable 313}#
         (lambda (#{mod 1021}#
                  #{var 1022}#
                  #{modref-cont 1023}#
                  #{bare-cont 1024}#)
           (if (not #{mod 1021}#)
             (#{bare-cont 1024}# #{var 1022}#)
             (begin
               (let ((#{kind 1031}# (car #{mod 1021}#))
                     (#{mod 1032}# (cdr #{mod 1021}#)))
                 (if (eqv? #{kind 1031}# 'public)
                   (#{modref-cont 1023}#
                     #{mod 1032}#
                     #{var 1022}#
                     #t)
                   (if (eqv? #{kind 1031}# 'private)
                     (if (not (equal?
                                #{mod 1032}#
                                (module-name (current-module))))
                       (#{modref-cont 1023}#
                         #{mod 1032}#
                         #{var 1022}#
                         #f)
                       (#{bare-cont 1024}# #{var 1022}#))
                     (if (eqv? #{kind 1031}# 'bare)
                       (#{bare-cont 1024}# #{var 1022}#)
                       (if (eqv? #{kind 1031}# 'hygiene)
                         (if (if (not (equal?
                                        #{mod 1032}#
                                        (module-name (current-module))))
                               (module-variable
                                 (resolve-module #{mod 1032}#)
                                 #{var 1022}#)
                               #f)
                           (#{modref-cont 1023}#
                             #{mod 1032}#
                             #{var 1022}#
                             #f)
                           (#{bare-cont 1024}# #{var 1022}#))
                         (syntax-violation
                           #f
                           "bad module kind"
                           #{var 1022}#
                           #{mod 1032}#))))))))))
       (#{build-global-reference 315}#
         (lambda (#{source 1040}# #{var 1041}# #{mod 1042}#)
           (#{analyze-variable 313}#
             #{mod 1042}#
             #{var 1041}#
             (lambda (#{mod 1046}# #{var 1047}# #{public? 1048}#)
               (#{make-module-ref 251}#
                 #{source 1040}#
                 #{mod 1046}#
                 #{var 1047}#
                 #{public? 1048}#))
             (lambda (#{var 1052}#)
               (#{make-toplevel-ref 255}#
                 #{source 1040}#
                 #{var 1052}#)))))
       (#{build-global-assignment 317}#
         (lambda (#{source 1054}#
                  #{var 1055}#
                  #{exp 1056}#
                  #{mod 1057}#)
           (begin
             (#{maybe-name-value! 299}#
               #{var 1055}#
               #{exp 1056}#)
             (#{analyze-variable 313}#
               #{mod 1057}#
               #{var 1055}#
               (lambda (#{mod 1062}# #{var 1063}# #{public? 1064}#)
                 (#{make-module-set 253}#
                   #{source 1054}#
                   #{mod 1062}#
                   #{var 1063}#
                   #{public? 1064}#
                   #{exp 1056}#))
               (lambda (#{var 1068}#)
                 (#{make-toplevel-set 257}#
                   #{source 1054}#
                   #{var 1068}#
                   #{exp 1056}#))))))
       (#{build-global-definition 319}#
         (lambda (#{source 1070}# #{var 1071}# #{exp 1072}#)
           (begin
             (#{maybe-name-value! 299}#
               #{var 1071}#
               #{exp 1072}#)
             (#{make-toplevel-define 259}#
               #{source 1070}#
               #{var 1071}#
               #{exp 1072}#))))
       (#{build-simple-lambda 321}#
         (lambda (#{src 1076}#
                  #{req 1077}#
                  #{rest 1078}#
                  #{vars 1079}#
                  #{meta 1080}#
                  #{exp 1081}#)
           (#{make-lambda 267}#
             #{src 1076}#
             #{meta 1080}#
             (#{make-lambda-case 269}#
               #{src 1076}#
               #{req 1077}#
               #f
               #{rest 1078}#
               #f
               '()
               #{vars 1079}#
               #{exp 1081}#
               #f))))
       (#{build-case-lambda 323}#
         (lambda (#{src 1088}# #{meta 1089}# #{body 1090}#)
           (#{make-lambda 267}#
             #{src 1088}#
             #{meta 1089}#
             #{body 1090}#)))
       (#{build-lambda-case 325}#
         (lambda (#{src 1094}#
                  #{req 1095}#
                  #{opt 1096}#
                  #{rest 1097}#
                  #{kw 1098}#
                  #{inits 1099}#
                  #{vars 1100}#
                  #{body 1101}#
                  #{else-case 1102}#)
           (#{make-lambda-case 269}#
             #{src 1094}#
             #{req 1095}#
             #{opt 1096}#
             #{rest 1097}#
             #{kw 1098}#
             #{inits 1099}#
             #{vars 1100}#
             #{body 1101}#
             #{else-case 1102}#)))
       (#{build-primref 327}#
         (lambda (#{src 1112}# #{name 1113}#)
           (if (equal? (module-name (current-module)) '(guile))
             (#{make-toplevel-ref 255}#
               #{src 1112}#
               #{name 1113}#)
             (#{make-module-ref 251}#
               #{src 1112}#
               '(guile)
               #{name 1113}#
               #f))))
       (#{build-data 329}#
         (lambda (#{src 1116}# #{exp 1117}#)
           (#{make-const 243}# #{src 1116}# #{exp 1117}#)))
       (#{build-sequence 331}#
         (lambda (#{src 1120}# #{exps 1121}#)
           (if (null? (cdr #{exps 1121}#))
             (car #{exps 1121}#)
             (#{make-sequence 265}#
               #{src 1120}#
               #{exps 1121}#))))
       (#{build-let 333}#
         (lambda (#{src 1124}#
                  #{ids 1125}#
                  #{vars 1126}#
                  #{val-exps 1127}#
                  #{body-exp 1128}#)
           (begin
             (for-each
               #{maybe-name-value! 299}#
               #{ids 1125}#
               #{val-exps 1127}#)
             (if (null? #{vars 1126}#)
               #{body-exp 1128}#
               (#{make-let 271}#
                 #{src 1124}#
                 #{ids 1125}#
                 #{vars 1126}#
                 #{val-exps 1127}#
                 #{body-exp 1128}#)))))
       (#{build-named-let 335}#
         (lambda (#{src 1134}#
                  #{ids 1135}#
                  #{vars 1136}#
                  #{val-exps 1137}#
                  #{body-exp 1138}#)
           (begin
             (let ((#{f 1148}# (car #{vars 1136}#))
                   (#{f-name 1149}# (car #{ids 1135}#))
                   (#{vars 1150}# (cdr #{vars 1136}#))
                   (#{ids 1151}# (cdr #{ids 1135}#)))
               (begin
                 (let ((#{proc 1153}#
                         (#{build-simple-lambda 321}#
                           #{src 1134}#
                           #{ids 1151}#
                           #f
                           #{vars 1150}#
                           '()
                           #{body-exp 1138}#)))
                   (begin
                     (#{maybe-name-value! 299}#
                       #{f-name 1149}#
                       #{proc 1153}#)
                     (for-each
                       #{maybe-name-value! 299}#
                       #{ids 1151}#
                       #{val-exps 1137}#)
                     (#{make-letrec 273}#
                       #{src 1134}#
                       #f
                       (list #{f-name 1149}#)
                       (list #{f 1148}#)
                       (list #{proc 1153}#)
                       (#{build-application 303}#
                         #{src 1134}#
                         (#{build-lexical-reference 309}#
                           'fun
                           #{src 1134}#
                           #{f-name 1149}#
                           #{f 1148}#)
                         #{val-exps 1137}#)))))))))
       (#{build-letrec 337}#
         (lambda (#{src 1154}#
                  #{in-order? 1155}#
                  #{ids 1156}#
                  #{vars 1157}#
                  #{val-exps 1158}#
                  #{body-exp 1159}#)
           (if (null? #{vars 1157}#)
             #{body-exp 1159}#
             (begin
               (for-each
                 #{maybe-name-value! 299}#
                 #{ids 1156}#
                 #{val-exps 1158}#)
               (#{make-letrec 273}#
                 #{src 1154}#
                 #{in-order? 1155}#
                 #{ids 1156}#
                 #{vars 1157}#
                 #{val-exps 1158}#
                 #{body-exp 1159}#)))))
       (#{make-syntax-object 341}#
         (lambda (#{expression 1166}#
                  #{wrap 1167}#
                  #{module 1168}#)
           (vector
             'syntax-object
             #{expression 1166}#
             #{wrap 1167}#
             #{module 1168}#)))
       (#{syntax-object? 343}#
         (lambda (#{x 1172}#)
           (if (vector? #{x 1172}#)
             (if (= (vector-length #{x 1172}#) 4)
               (eq? (vector-ref #{x 1172}# 0) 'syntax-object)
               #f)
             #f)))
       (#{syntax-object-expression 345}#
         (lambda (#{x 1177}#) (vector-ref #{x 1177}# 1)))
       (#{syntax-object-wrap 347}#
         (lambda (#{x 1179}#) (vector-ref #{x 1179}# 2)))
       (#{syntax-object-module 349}#
         (lambda (#{x 1181}#) (vector-ref #{x 1181}# 3)))
       (#{source-annotation 358}#
         (lambda (#{x 1195}#)
           (if (#{syntax-object? 343}# #{x 1195}#)
             (#{source-annotation 358}#
               (#{syntax-object-expression 345}# #{x 1195}#))
             (if (pair? #{x 1195}#)
               (begin
                 (let ((#{props 1202}# (source-properties #{x 1195}#)))
                   (if (pair? #{props 1202}#) #{props 1202}# #f)))
               #f))))
       (#{extend-env 365}#
         (lambda (#{labels 1204}# #{bindings 1205}# #{r 1206}#)
           (if (null? #{labels 1204}#)
             #{r 1206}#
             (#{extend-env 365}#
               (cdr #{labels 1204}#)
               (cdr #{bindings 1205}#)
               (cons (cons (car #{labels 1204}#)
                           (car #{bindings 1205}#))
                     #{r 1206}#)))))
       (#{extend-var-env 367}#
         (lambda (#{labels 1210}# #{vars 1211}# #{r 1212}#)
           (if (null? #{labels 1210}#)
             #{r 1212}#
             (#{extend-var-env 367}#
               (cdr #{labels 1210}#)
               (cdr #{vars 1211}#)
               (cons (cons (car #{labels 1210}#)
                           (cons 'lexical (car #{vars 1211}#)))
                     #{r 1212}#)))))
       (#{macros-only-env 369}#
         (lambda (#{r 1217}#)
           (if (null? #{r 1217}#)
             '()
             (begin
               (let ((#{a 1220}# (car #{r 1217}#)))
                 (if (eq? (car (cdr #{a 1220}#)) 'macro)
                   (cons #{a 1220}#
                         (#{macros-only-env 369}# (cdr #{r 1217}#)))
                   (#{macros-only-env 369}# (cdr #{r 1217}#))))))))
       (#{lookup 371}#
         (lambda (#{x 1221}# #{r 1222}# #{mod 1223}#)
           (begin
             (let ((#{t 1229}# (assq #{x 1221}# #{r 1222}#)))
               (if #{t 1229}#
                 (cdr #{t 1229}#)
                 (if (symbol? #{x 1221}#)
                   (begin
                     (let ((#{t 1235}#
                             (#{get-global-definition-hook 295}#
                               #{x 1221}#
                               #{mod 1223}#)))
                       (if #{t 1235}# #{t 1235}# '(global))))
                   '(displaced-lexical)))))))
       (#{global-extend 373}#
         (lambda (#{type 1240}# #{sym 1241}# #{val 1242}#)
           (#{put-global-definition-hook 293}#
             #{sym 1241}#
             #{type 1240}#
             #{val 1242}#)))
       (#{nonsymbol-id? 375}#
         (lambda (#{x 1246}#)
           (if (#{syntax-object? 343}# #{x 1246}#)
             (symbol?
               (#{syntax-object-expression 345}# #{x 1246}#))
             #f)))
       (#{id? 377}#
         (lambda (#{x 1250}#)
           (if (symbol? #{x 1250}#)
             #t
             (if (#{syntax-object? 343}# #{x 1250}#)
               (symbol?
                 (#{syntax-object-expression 345}# #{x 1250}#))
               #f))))
       (#{id-sym-name&marks 380}#
         (lambda (#{x 1257}# #{w 1258}#)
           (if (#{syntax-object? 343}# #{x 1257}#)
             (values
               (#{syntax-object-expression 345}# #{x 1257}#)
               (#{join-marks 427}#
                 (car #{w 1258}#)
                 (car (#{syntax-object-wrap 347}# #{x 1257}#))))
             (values #{x 1257}# (car #{w 1258}#)))))
       (#{gen-label 390}#
         (lambda () (symbol->string (gensym "i"))))
       (#{gen-labels 392}#
         (lambda (#{ls 1264}#)
           (if (null? #{ls 1264}#)
             '()
             (cons (#{gen-label 390}#)
                   (#{gen-labels 392}# (cdr #{ls 1264}#))))))
       (#{make-ribcage 395}#
         (lambda (#{symnames 1266}#
                  #{marks 1267}#
                  #{labels 1268}#)
           (vector
             'ribcage
             #{symnames 1266}#
             #{marks 1267}#
             #{labels 1268}#)))
       (#{ribcage-symnames 399}#
         (lambda (#{x 1277}#) (vector-ref #{x 1277}# 1)))
       (#{ribcage-marks 401}#
         (lambda (#{x 1279}#) (vector-ref #{x 1279}# 2)))
       (#{ribcage-labels 403}#
         (lambda (#{x 1281}#) (vector-ref #{x 1281}# 3)))
       (#{set-ribcage-symnames! 405}#
         (lambda (#{x 1283}# #{update 1284}#)
           (vector-set! #{x 1283}# 1 #{update 1284}#)))
       (#{set-ribcage-marks! 407}#
         (lambda (#{x 1287}# #{update 1288}#)
           (vector-set! #{x 1287}# 2 #{update 1288}#)))
       (#{set-ribcage-labels! 409}#
         (lambda (#{x 1291}# #{update 1292}#)
           (vector-set! #{x 1291}# 3 #{update 1292}#)))
       (#{anti-mark 415}#
         (lambda (#{w 1295}#)
           (cons (cons #f (car #{w 1295}#))
                 (cons 'shift (cdr #{w 1295}#)))))
       (#{extend-ribcage! 419}#
         (lambda (#{ribcage 1301}# #{id 1302}# #{label 1303}#)
           (begin
             (#{set-ribcage-symnames! 405}#
               #{ribcage 1301}#
               (cons (#{syntax-object-expression 345}# #{id 1302}#)
                     (#{ribcage-symnames 399}# #{ribcage 1301}#)))
             (#{set-ribcage-marks! 407}#
               #{ribcage 1301}#
               (cons (car (#{syntax-object-wrap 347}# #{id 1302}#))
                     (#{ribcage-marks 401}# #{ribcage 1301}#)))
             (#{set-ribcage-labels! 409}#
               #{ribcage 1301}#
               (cons #{label 1303}#
                     (#{ribcage-labels 403}# #{ribcage 1301}#))))))
       (#{make-binding-wrap 421}#
         (lambda (#{ids 1308}# #{labels 1309}# #{w 1310}#)
           (if (null? #{ids 1308}#)
             #{w 1310}#
             (cons (car #{w 1310}#)
                   (cons (begin
                           (let ((#{labelvec 1317}#
                                   (list->vector #{labels 1309}#)))
                             (begin
                               (let ((#{n 1319}#
                                       (vector-length #{labelvec 1317}#)))
                                 (begin
                                   (let ((#{symnamevec 1322}#
                                           (make-vector #{n 1319}#))
                                         (#{marksvec 1323}#
                                           (make-vector #{n 1319}#)))
                                     (begin
                                       (letrec*
                                         ((#{f 1327}#
                                            (lambda (#{ids 1328}# #{i 1329}#)
                                              (if (not (null? #{ids 1328}#))
                                                (call-with-values
                                                  (lambda ()
                                                    (#{id-sym-name&marks 380}#
                                                      (car #{ids 1328}#)
                                                      #{w 1310}#))
                                                  (lambda (#{symname 1330}#
                                                           #{marks 1331}#)
                                                    (begin
                                                      (vector-set!
                                                        #{symnamevec 1322}#
                                                        #{i 1329}#
                                                        #{symname 1330}#)
                                                      (vector-set!
                                                        #{marksvec 1323}#
                                                        #{i 1329}#
                                                        #{marks 1331}#)
                                                      (#{f 1327}#
                                                        (cdr #{ids 1328}#)
                                                        (#{1+}# #{i 1329}#)))))))))
                                         (begin (#{f 1327}# #{ids 1308}# 0)))
                                       (#{make-ribcage 395}#
                                         #{symnamevec 1322}#
                                         #{marksvec 1323}#
                                         #{labelvec 1317}#))))))))
                         (cdr #{w 1310}#))))))
       (#{smart-append 423}#
         (lambda (#{m1 1336}# #{m2 1337}#)
           (if (null? #{m2 1337}#)
             #{m1 1336}#
             (append #{m1 1336}# #{m2 1337}#))))
       (#{join-wraps 425}#
         (lambda (#{w1 1340}# #{w2 1341}#)
           (begin
             (let ((#{m1 1346}# (car #{w1 1340}#))
                   (#{s1 1347}# (cdr #{w1 1340}#)))
               (if (null? #{m1 1346}#)
                 (if (null? #{s1 1347}#)
                   #{w2 1341}#
                   (cons (car #{w2 1341}#)
                         (#{smart-append 423}#
                           #{s1 1347}#
                           (cdr #{w2 1341}#))))
                 (cons (#{smart-append 423}#
                         #{m1 1346}#
                         (car #{w2 1341}#))
                       (#{smart-append 423}#
                         #{s1 1347}#
                         (cdr #{w2 1341}#))))))))
       (#{join-marks 427}#
         (lambda (#{m1 1356}# #{m2 1357}#)
           (#{smart-append 423}# #{m1 1356}# #{m2 1357}#)))
       (#{same-marks? 429}#
         (lambda (#{x 1360}# #{y 1361}#)
           (begin
             (let ((#{t 1366}# (eq? #{x 1360}# #{y 1361}#)))
               (if #{t 1366}#
                 #{t 1366}#
                 (if (not (null? #{x 1360}#))
                   (if (not (null? #{y 1361}#))
                     (if (eq? (car #{x 1360}#) (car #{y 1361}#))
                       (#{same-marks? 429}#
                         (cdr #{x 1360}#)
                         (cdr #{y 1361}#))
                       #f)
                     #f)
                   #f))))))
       (#{id-var-name 431}#
         (lambda (#{id 1372}# #{w 1373}#)
           (letrec*
             ((#{search 1378}#
                (lambda (#{sym 1394}# #{subst 1395}# #{marks 1396}#)
                  (if (null? #{subst 1395}#)
                    (values #f #{marks 1396}#)
                    (begin
                      (let ((#{fst 1401}# (car #{subst 1395}#)))
                        (if (eq? #{fst 1401}# 'shift)
                          (#{search 1378}#
                            #{sym 1394}#
                            (cdr #{subst 1395}#)
                            (cdr #{marks 1396}#))
                          (begin
                            (let ((#{symnames 1403}#
                                    (#{ribcage-symnames 399}# #{fst 1401}#)))
                              (if (vector? #{symnames 1403}#)
                                (#{search-vector-rib 1382}#
                                  #{sym 1394}#
                                  #{subst 1395}#
                                  #{marks 1396}#
                                  #{symnames 1403}#
                                  #{fst 1401}#)
                                (#{search-list-rib 1380}#
                                  #{sym 1394}#
                                  #{subst 1395}#
                                  #{marks 1396}#
                                  #{symnames 1403}#
                                  #{fst 1401}#))))))))))
              (#{search-list-rib 1380}#
                (lambda (#{sym 1404}#
                         #{subst 1405}#
                         #{marks 1406}#
                         #{symnames 1407}#
                         #{ribcage 1408}#)
                  (letrec*
                    ((#{f 1417}#
                       (lambda (#{symnames 1418}# #{i 1419}#)
                         (if (null? #{symnames 1418}#)
                           (#{search 1378}#
                             #{sym 1404}#
                             (cdr #{subst 1405}#)
                             #{marks 1406}#)
                           (if (if (eq? (car #{symnames 1418}#) #{sym 1404}#)
                                 (#{same-marks? 429}#
                                   #{marks 1406}#
                                   (list-ref
                                     (#{ribcage-marks 401}# #{ribcage 1408}#)
                                     #{i 1419}#))
                                 #f)
                             (values
                               (list-ref
                                 (#{ribcage-labels 403}# #{ribcage 1408}#)
                                 #{i 1419}#)
                               #{marks 1406}#)
                             (#{f 1417}#
                               (cdr #{symnames 1418}#)
                               (#{1+}# #{i 1419}#)))))))
                    (begin (#{f 1417}# #{symnames 1407}# 0)))))
              (#{search-vector-rib 1382}#
                (lambda (#{sym 1428}#
                         #{subst 1429}#
                         #{marks 1430}#
                         #{symnames 1431}#
                         #{ribcage 1432}#)
                  (begin
                    (let ((#{n 1439}# (vector-length #{symnames 1431}#)))
                      (letrec*
                        ((#{f 1442}#
                           (lambda (#{i 1443}#)
                             (if (= #{i 1443}# #{n 1439}#)
                               (#{search 1378}#
                                 #{sym 1428}#
                                 (cdr #{subst 1429}#)
                                 #{marks 1430}#)
                               (if (if (eq? (vector-ref
                                              #{symnames 1431}#
                                              #{i 1443}#)
                                            #{sym 1428}#)
                                     (#{same-marks? 429}#
                                       #{marks 1430}#
                                       (vector-ref
                                         (#{ribcage-marks 401}#
                                           #{ribcage 1432}#)
                                         #{i 1443}#))
                                     #f)
                                 (values
                                   (vector-ref
                                     (#{ribcage-labels 403}# #{ribcage 1432}#)
                                     #{i 1443}#)
                                   #{marks 1430}#)
                                 (#{f 1442}# (#{1+}# #{i 1443}#)))))))
                        (begin (#{f 1442}# 0))))))))
             (begin
               (if (symbol? #{id 1372}#)
                 (begin
                   (let ((#{t 1455}#
                           (#{search 1378}#
                             #{id 1372}#
                             (cdr #{w 1373}#)
                             (car #{w 1373}#))))
                     (if #{t 1455}# #{t 1455}# #{id 1372}#)))
                 (if (#{syntax-object? 343}# #{id 1372}#)
                   (begin
                     (let ((#{id 1464}#
                             (#{syntax-object-expression 345}# #{id 1372}#))
                           (#{w1 1465}#
                             (#{syntax-object-wrap 347}# #{id 1372}#)))
                       (begin
                         (let ((#{marks 1467}#
                                 (#{join-marks 427}#
                                   (car #{w 1373}#)
                                   (car #{w1 1465}#))))
                           (call-with-values
                             (lambda ()
                               (#{search 1378}#
                                 #{id 1464}#
                                 (cdr #{w 1373}#)
                                 #{marks 1467}#))
                             (lambda (#{new-id 1471}# #{marks 1472}#)
                               (begin
                                 (let ((#{t 1477}# #{new-id 1471}#))
                                   (if #{t 1477}#
                                     #{t 1477}#
                                     (begin
                                       (let ((#{t 1480}#
                                               (#{search 1378}#
                                                 #{id 1464}#
                                                 (cdr #{w1 1465}#)
                                                 #{marks 1472}#)))
                                         (if #{t 1480}#
                                           #{t 1480}#
                                           #{id 1464}#))))))))))))
                   (syntax-violation
                     'id-var-name
                     "invalid id"
                     #{id 1372}#)))))))
       (#{free-id=? 433}#
         (lambda (#{i 1485}# #{j 1486}#)
           (begin
             (let ((#{ni 1491}#
                     (#{id-var-name 431}# #{i 1485}# '(())))
                   (#{nj 1492}#
                     (#{id-var-name 431}# #{j 1486}# '(()))))
               (letrec*
                 ((#{id-module-binding 1496}#
                    (lambda (#{id 1497}#)
                      (begin
                        (let ((#{mod 1500}#
                                (if (#{syntax-object? 343}# #{id 1497}#)
                                  (#{syntax-object-module 349}# #{id 1497}#)
                                  #f)))
                          (module-variable
                            (if #{mod 1500}#
                              (resolve-module (cdr #{mod 1500}#))
                              (current-module))
                            (begin
                              (let ((#{x 1505}# #{id 1497}#))
                                (if (#{syntax-object? 343}# #{x 1505}#)
                                  (#{syntax-object-expression 345}# #{x 1505}#)
                                  #{x 1505}#)))))))))
                 (begin
                   (if (eq? #{ni 1491}#
                            (begin
                              (let ((#{x 1508}# #{i 1485}#))
                                (if (#{syntax-object? 343}# #{x 1508}#)
                                  (#{syntax-object-expression 345}# #{x 1508}#)
                                  #{x 1508}#))))
                     (if (eq? #{nj 1492}#
                              (begin
                                (let ((#{x 1512}# #{j 1486}#))
                                  (if (#{syntax-object? 343}# #{x 1512}#)
                                    (#{syntax-object-expression 345}#
                                      #{x 1512}#)
                                    #{x 1512}#))))
                       (if (begin
                             (let ((#{bi 1515}#
                                     (#{id-module-binding 1496}# #{i 1485}#)))
                               (if #{bi 1515}#
                                 (eq? #{bi 1515}#
                                      (#{id-module-binding 1496}# #{j 1486}#))
                                 (if (not (#{id-module-binding 1496}#
                                            #{j 1486}#))
                                   (eq? #{ni 1491}# #{nj 1492}#)
                                   #f))))
                         (eq? (#{id-module-binding 1496}# #{i 1485}#)
                              (#{id-module-binding 1496}# #{j 1486}#))
                         #f)
                       #f)
                     (if (eq? #{ni 1491}# #{nj 1492}#)
                       (not (eq? #{nj 1492}#
                                 (begin
                                   (let ((#{x 1523}# #{j 1486}#))
                                     (if (#{syntax-object? 343}# #{x 1523}#)
                                       (#{syntax-object-expression 345}#
                                         #{x 1523}#)
                                       #{x 1523}#)))))
                       #f))))))))
       (#{bound-id=? 435}#
         (lambda (#{i 1524}# #{j 1525}#)
           (if (if (#{syntax-object? 343}# #{i 1524}#)
                 (#{syntax-object? 343}# #{j 1525}#)
                 #f)
             (if (eq? (#{syntax-object-expression 345}# #{i 1524}#)
                      (#{syntax-object-expression 345}# #{j 1525}#))
               (#{same-marks? 429}#
                 (car (#{syntax-object-wrap 347}# #{i 1524}#))
                 (car (#{syntax-object-wrap 347}# #{j 1525}#)))
               #f)
             (eq? #{i 1524}# #{j 1525}#))))
       (#{valid-bound-ids? 437}#
         (lambda (#{ids 1534}#)
           (if (letrec*
                 ((#{all-ids? 1539}#
                    (lambda (#{ids 1540}#)
                      (begin
                        (let ((#{t 1543}# (null? #{ids 1540}#)))
                          (if #{t 1543}#
                            #{t 1543}#
                            (if (#{id? 377}# (car #{ids 1540}#))
                              (#{all-ids? 1539}# (cdr #{ids 1540}#))
                              #f)))))))
                 (begin (#{all-ids? 1539}# #{ids 1534}#)))
             (#{distinct-bound-ids? 439}# #{ids 1534}#)
             #f)))
       (#{distinct-bound-ids? 439}#
         (lambda (#{ids 1548}#)
           (letrec*
             ((#{distinct? 1552}#
                (lambda (#{ids 1553}#)
                  (begin
                    (let ((#{t 1556}# (null? #{ids 1553}#)))
                      (if #{t 1556}#
                        #{t 1556}#
                        (if (not (#{bound-id-member? 441}#
                                   (car #{ids 1553}#)
                                   (cdr #{ids 1553}#)))
                          (#{distinct? 1552}# (cdr #{ids 1553}#))
                          #f)))))))
             (begin (#{distinct? 1552}# #{ids 1548}#)))))
       (#{bound-id-member? 441}#
         (lambda (#{x 1560}# #{list 1561}#)
           (if (not (null? #{list 1561}#))
             (begin
               (let ((#{t 1568}#
                       (#{bound-id=? 435}#
                         #{x 1560}#
                         (car #{list 1561}#))))
                 (if #{t 1568}#
                   #{t 1568}#
                   (#{bound-id-member? 441}#
                     #{x 1560}#
                     (cdr #{list 1561}#)))))
             #f)))
       (#{wrap 443}#
         (lambda (#{x 1570}# #{w 1571}# #{defmod 1572}#)
           (if (if (null? (car #{w 1571}#))
                 (null? (cdr #{w 1571}#))
                 #f)
             #{x 1570}#
             (if (#{syntax-object? 343}# #{x 1570}#)
               (#{make-syntax-object 341}#
                 (#{syntax-object-expression 345}# #{x 1570}#)
                 (#{join-wraps 425}#
                   #{w 1571}#
                   (#{syntax-object-wrap 347}# #{x 1570}#))
                 (#{syntax-object-module 349}# #{x 1570}#))
               (if (null? #{x 1570}#)
                 #{x 1570}#
                 (#{make-syntax-object 341}#
                   #{x 1570}#
                   #{w 1571}#
                   #{defmod 1572}#))))))
       (#{source-wrap 445}#
         (lambda (#{x 1587}#
                  #{w 1588}#
                  #{s 1589}#
                  #{defmod 1590}#)
           (#{wrap 443}#
             (#{decorate-source 297}# #{x 1587}# #{s 1589}#)
             #{w 1588}#
             #{defmod 1590}#)))
       (#{chi-sequence 447}#
         (lambda (#{body 1595}#
                  #{r 1596}#
                  #{w 1597}#
                  #{s 1598}#
                  #{mod 1599}#)
           (#{build-sequence 331}#
             #{s 1598}#
             (letrec*
               ((#{dobody 1610}#
                  (lambda (#{body 1611}#
                           #{r 1612}#
                           #{w 1613}#
                           #{mod 1614}#)
                    (if (null? #{body 1611}#)
                      '()
                      (begin
                        (let ((#{first 1616}#
                                (#{chi 457}#
                                  (car #{body 1611}#)
                                  #{r 1612}#
                                  #{w 1613}#
                                  #{mod 1614}#)))
                          (cons #{first 1616}#
                                (#{dobody 1610}#
                                  (cdr #{body 1611}#)
                                  #{r 1612}#
                                  #{w 1613}#
                                  #{mod 1614}#))))))))
               (begin
                 (#{dobody 1610}#
                   #{body 1595}#
                   #{r 1596}#
                   #{w 1597}#
                   #{mod 1599}#))))))
       (#{chi-top-sequence 449}#
         (lambda (#{body 1617}#
                  #{r 1618}#
                  #{w 1619}#
                  #{s 1620}#
                  #{m 1621}#
                  #{esew 1622}#
                  #{mod 1623}#)
           (letrec*
             ((#{scan 1632}#
                (lambda (#{body 1633}#
                         #{r 1634}#
                         #{w 1635}#
                         #{s 1636}#
                         #{m 1637}#
                         #{esew 1638}#
                         #{mod 1639}#
                         #{exps 1640}#)
                  (if (null? #{body 1633}#)
                    #{exps 1640}#
                    (call-with-values
                      (lambda ()
                        (call-with-values
                          (lambda ()
                            (begin
                              (let ((#{e 1653}# (car #{body 1633}#)))
                                (#{syntax-type 455}#
                                  #{e 1653}#
                                  #{r 1634}#
                                  #{w 1635}#
                                  (begin
                                    (let ((#{t 1656}#
                                            (#{source-annotation 358}#
                                              #{e 1653}#)))
                                      (if #{t 1656}# #{t 1656}# #{s 1636}#)))
                                  #f
                                  #{mod 1639}#
                                  #f))))
                          (lambda (#{type 1658}#
                                   #{value 1659}#
                                   #{e 1660}#
                                   #{w 1661}#
                                   #{s 1662}#
                                   #{mod 1663}#)
                            (if (eqv? #{type 1658}# 'begin-form)
                              (let ((#{tmp 1671}# #{e 1660}#))
                                (let ((#{tmp 1672}#
                                        ($sc-dispatch #{tmp 1671}# '(_))))
                                  (if #{tmp 1672}#
                                    (@apply
                                      (lambda () #{exps 1640}#)
                                      #{tmp 1672}#)
                                    (let ((#{tmp 1673}#
                                            ($sc-dispatch
                                              #{tmp 1671}#
                                              '(_ any . each-any))))
                                      (if #{tmp 1673}#
                                        (@apply
                                          (lambda (#{e1 1676}# #{e2 1677}#)
                                            (#{scan 1632}#
                                              (cons #{e1 1676}# #{e2 1677}#)
                                              #{r 1634}#
                                              #{w 1661}#
                                              #{s 1662}#
                                              #{m 1637}#
                                              #{esew 1638}#
                                              #{mod 1663}#
                                              #{exps 1640}#))
                                          #{tmp 1673}#)
                                        (syntax-violation
                                          #f
                                          "source expression failed to match any pattern"
                                          #{tmp 1671}#))))))
                              (if (eqv? #{type 1658}# 'local-syntax-form)
                                (#{chi-local-syntax 467}#
                                  #{value 1659}#
                                  #{e 1660}#
                                  #{r 1634}#
                                  #{w 1661}#
                                  #{s 1662}#
                                  #{mod 1663}#
                                  (lambda (#{body 1680}#
                                           #{r 1681}#
                                           #{w 1682}#
                                           #{s 1683}#
                                           #{mod 1684}#)
                                    (#{scan 1632}#
                                      #{body 1680}#
                                      #{r 1681}#
                                      #{w 1682}#
                                      #{s 1683}#
                                      #{m 1637}#
                                      #{esew 1638}#
                                      #{mod 1684}#
                                      #{exps 1640}#)))
                                (if (eqv? #{type 1658}# 'eval-when-form)
                                  (let ((#{tmp 1691}# #{e 1660}#))
                                    (let ((#{tmp 1692}#
                                            ($sc-dispatch
                                              #{tmp 1691}#
                                              '(_ each-any any . each-any))))
                                      (if #{tmp 1692}#
                                        (@apply
                                          (lambda (#{x 1696}#
                                                   #{e1 1697}#
                                                   #{e2 1698}#)
                                            (begin
                                              (let ((#{when-list 1701}#
                                                      (#{chi-when-list 453}#
                                                        #{e 1660}#
                                                        #{x 1696}#
                                                        #{w 1661}#))
                                                    (#{body 1702}#
                                                      (cons #{e1 1697}#
                                                            #{e2 1698}#)))
                                                (if (eq? #{m 1637}# 'e)
                                                  (if (memq 'eval
                                                            #{when-list 1701}#)
                                                    (#{scan 1632}#
                                                      #{body 1702}#
                                                      #{r 1634}#
                                                      #{w 1661}#
                                                      #{s 1662}#
                                                      (if (memq 'expand
                                                                #{when-list 1701}#)
                                                        'c&e
                                                        'e)
                                                      '(eval)
                                                      #{mod 1663}#
                                                      #{exps 1640}#)
                                                    (begin
                                                      (if (memq 'expand
                                                                #{when-list 1701}#)
                                                        (#{top-level-eval-hook 288}#
                                                          (#{chi-top-sequence 449}#
                                                            #{body 1702}#
                                                            #{r 1634}#
                                                            #{w 1661}#
                                                            #{s 1662}#
                                                            'e
                                                            '(eval)
                                                            #{mod 1663}#)
                                                          #{mod 1663}#))
                                                      #{exps 1640}#))
                                                  (if (memq 'load
                                                            #{when-list 1701}#)
                                                    (if (begin
                                                          (let ((#{t 1711}#
                                                                  (memq 'compile
                                                                        #{when-list 1701}#)))
                                                            (if #{t 1711}#
                                                              #{t 1711}#
                                                              (begin
                                                                (let ((#{t 1714}#
                                                                        (memq 'expand
                                                                              #{when-list 1701}#)))
                                                                  (if #{t 1714}#
                                                                    #{t 1714}#
                                                                    (if (eq? #{m 1637}#
                                                                             'c&e)
                                                                      (memq 'eval
                                                                            #{when-list 1701}#)
                                                                      #f)))))))
                                                      (#{scan 1632}#
                                                        #{body 1702}#
                                                        #{r 1634}#
                                                        #{w 1661}#
                                                        #{s 1662}#
                                                        'c&e
                                                        '(compile load)
                                                        #{mod 1663}#
                                                        #{exps 1640}#)
                                                      (if (if (eq? #{m 1637}#
                                                                   'c)
                                                            #t
                                                            (eq? #{m 1637}#
                                                                 'c&e))
                                                        (#{scan 1632}#
                                                          #{body 1702}#
                                                          #{r 1634}#
                                                          #{w 1661}#
                                                          #{s 1662}#
                                                          'c
                                                          '(load)
                                                          #{mod 1663}#
                                                          #{exps 1640}#)
                                                        #{exps 1640}#))
                                                    (if (begin
                                                          (let ((#{t 1722}#
                                                                  (memq 'compile
                                                                        #{when-list 1701}#)))
                                                            (if #{t 1722}#
                                                              #{t 1722}#
                                                              (begin
                                                                (let ((#{t 1725}#
                                                                        (memq 'expand
                                                                              #{when-list 1701}#)))
                                                                  (if #{t 1725}#
                                                                    #{t 1725}#
                                                                    (if (eq? #{m 1637}#
                                                                             'c&e)
                                                                      (memq 'eval
                                                                            #{when-list 1701}#)
                                                                      #f)))))))
                                                      (begin
                                                        (#{top-level-eval-hook 288}#
                                                          (#{chi-top-sequence 449}#
                                                            #{body 1702}#
                                                            #{r 1634}#
                                                            #{w 1661}#
                                                            #{s 1662}#
                                                            'e
                                                            '(eval)
                                                            #{mod 1663}#)
                                                          #{mod 1663}#)
                                                        #{exps 1640}#)
                                                      #{exps 1640}#))))))
                                          #{tmp 1692}#)
                                        (syntax-violation
                                          #f
                                          "source expression failed to match any pattern"
                                          #{tmp 1691}#))))
                                  (if (eqv? #{type 1658}# 'define-syntax-form)
                                    (begin
                                      (let ((#{n 1733}#
                                              (#{id-var-name 431}#
                                                #{value 1659}#
                                                #{w 1661}#))
                                            (#{r 1734}#
                                              (#{macros-only-env 369}#
                                                #{r 1634}#)))
                                        (if (eqv? #{m 1637}# 'c)
                                          (if (memq 'compile #{esew 1638}#)
                                            (begin
                                              (let ((#{e 1737}#
                                                      (#{chi-install-global 451}#
                                                        #{n 1733}#
                                                        (#{chi 457}#
                                                          #{e 1660}#
                                                          #{r 1734}#
                                                          #{w 1661}#
                                                          #{mod 1663}#))))
                                                (begin
                                                  (#{top-level-eval-hook 288}#
                                                    #{e 1737}#
                                                    #{mod 1663}#)
                                                  (if (memq 'load
                                                            #{esew 1638}#)
                                                    (cons #{e 1737}#
                                                          #{exps 1640}#)
                                                    #{exps 1640}#))))
                                            (if (memq 'load #{esew 1638}#)
                                              (cons (#{chi-install-global 451}#
                                                      #{n 1733}#
                                                      (#{chi 457}#
                                                        #{e 1660}#
                                                        #{r 1734}#
                                                        #{w 1661}#
                                                        #{mod 1663}#))
                                                    #{exps 1640}#)
                                              #{exps 1640}#))
                                          (if (eqv? #{m 1637}# 'c&e)
                                            (begin
                                              (let ((#{e 1740}#
                                                      (#{chi-install-global 451}#
                                                        #{n 1733}#
                                                        (#{chi 457}#
                                                          #{e 1660}#
                                                          #{r 1734}#
                                                          #{w 1661}#
                                                          #{mod 1663}#))))
                                                (begin
                                                  (#{top-level-eval-hook 288}#
                                                    #{e 1740}#
                                                    #{mod 1663}#)
                                                  (cons #{e 1740}#
                                                        #{exps 1640}#))))
                                            (begin
                                              (if (memq 'eval #{esew 1638}#)
                                                (#{top-level-eval-hook 288}#
                                                  (#{chi-install-global 451}#
                                                    #{n 1733}#
                                                    (#{chi 457}#
                                                      #{e 1660}#
                                                      #{r 1734}#
                                                      #{w 1661}#
                                                      #{mod 1663}#))
                                                  #{mod 1663}#))
                                              #{exps 1640}#)))))
                                    (if (eqv? #{type 1658}# 'define-form)
                                      (begin
                                        (let ((#{n 1745}#
                                                (#{id-var-name 431}#
                                                  #{value 1659}#
                                                  #{w 1661}#)))
                                          (begin
                                            (let ((#{type 1747}#
                                                    (car (#{lookup 371}#
                                                           #{n 1745}#
                                                           #{r 1634}#
                                                           #{mod 1663}#))))
                                              (if (if (eqv? #{type 1747}#
                                                            'global)
                                                    #t
                                                    (if (eqv? #{type 1747}#
                                                              'core)
                                                      #t
                                                      (if (eqv? #{type 1747}#
                                                                'macro)
                                                        #t
                                                        (eqv? #{type 1747}#
                                                              'module-ref))))
                                                (begin
                                                  (if (if (if (eq? #{m 1637}#
                                                                   'c)
                                                            #t
                                                            (eq? #{m 1637}#
                                                                 'c&e))
                                                        (if (not (module-local-variable
                                                                   (current-module)
                                                                   #{n 1745}#))
                                                          (current-module)
                                                          #f)
                                                        #f)
                                                    (begin
                                                      (let ((#{old 1754}#
                                                              (module-variable
                                                                (current-module)
                                                                #{n 1745}#)))
                                                        (if (if (variable?
                                                                  #{old 1754}#)
                                                              (variable-bound?
                                                                #{old 1754}#)
                                                              #f)
                                                          (module-define!
                                                            (current-module)
                                                            #{n 1745}#
                                                            (variable-ref
                                                              #{old 1754}#))
                                                          (module-add!
                                                            (current-module)
                                                            #{n 1745}#
                                                            (make-undefined-variable))))))
                                                  (cons (if (eq? #{m 1637}#
                                                                 'c&e)
                                                          (begin
                                                            (let ((#{x 1758}#
                                                                    (#{build-global-definition 319}#
                                                                      #{s 1662}#
                                                                      #{n 1745}#
                                                                      (#{chi 457}#
                                                                        #{e 1660}#
                                                                        #{r 1634}#
                                                                        #{w 1661}#
                                                                        #{mod 1663}#))))
                                                              (begin
                                                                (#{top-level-eval-hook 288}#
                                                                  #{x 1758}#
                                                                  #{mod 1663}#)
                                                                #{x 1758}#)))
                                                          (lambda ()
                                                            (#{build-global-definition 319}#
                                                              #{s 1662}#
                                                              #{n 1745}#
                                                              (#{chi 457}#
                                                                #{e 1660}#
                                                                #{r 1634}#
                                                                #{w 1661}#
                                                                #{mod 1663}#))))
                                                        #{exps 1640}#))
                                                (if (eqv? #{type 1747}#
                                                          'displaced-lexical)
                                                  (syntax-violation
                                                    #f
                                                    "identifier out of context"
                                                    #{e 1660}#
                                                    (#{wrap 443}#
                                                      #{value 1659}#
                                                      #{w 1661}#
                                                      #{mod 1663}#))
                                                  (syntax-violation
                                                    #f
                                                    "cannot define keyword at top level"
                                                    #{e 1660}#
                                                    (#{wrap 443}#
                                                      #{value 1659}#
                                                      #{w 1661}#
                                                      #{mod 1663}#))))))))
                                      (cons (if (eq? #{m 1637}# 'c&e)
                                              (begin
                                                (let ((#{x 1763}#
                                                        (#{chi-expr 459}#
                                                          #{type 1658}#
                                                          #{value 1659}#
                                                          #{e 1660}#
                                                          #{r 1634}#
                                                          #{w 1661}#
                                                          #{s 1662}#
                                                          #{mod 1663}#)))
                                                  (begin
                                                    (#{top-level-eval-hook 288}#
                                                      #{x 1763}#
                                                      #{mod 1663}#)
                                                    #{x 1763}#)))
                                              (lambda ()
                                                (#{chi-expr 459}#
                                                  #{type 1658}#
                                                  #{value 1659}#
                                                  #{e 1660}#
                                                  #{r 1634}#
                                                  #{w 1661}#
                                                  #{s 1662}#
                                                  #{mod 1663}#)))
                                            #{exps 1640}#)))))))))
                      (lambda (#{exps 1764}#)
                        (#{scan 1632}#
                          (cdr #{body 1633}#)
                          #{r 1634}#
                          #{w 1635}#
                          #{s 1636}#
                          #{m 1637}#
                          #{esew 1638}#
                          #{mod 1639}#
                          #{exps 1764}#)))))))
             (begin
               (call-with-values
                 (lambda ()
                   (#{scan 1632}#
                     #{body 1617}#
                     #{r 1618}#
                     #{w 1619}#
                     #{s 1620}#
                     #{m 1621}#
                     #{esew 1622}#
                     #{mod 1623}#
                     '()))
                 (lambda (#{exps 1766}#)
                   (if (null? #{exps 1766}#)
                     (#{build-void 301}# #{s 1620}#)
                     (#{build-sequence 331}#
                       #{s 1620}#
                       (letrec*
                         ((#{lp 1771}#
                            (lambda (#{in 1772}# #{out 1773}#)
                              (if (null? #{in 1772}#)
                                #{out 1773}#
                                (begin
                                  (let ((#{e 1775}# (car #{in 1772}#)))
                                    (#{lp 1771}#
                                      (cdr #{in 1772}#)
                                      (cons (if (procedure? #{e 1775}#)
                                              (#{e 1775}#)
                                              #{e 1775}#)
                                            #{out 1773}#))))))))
                         (begin (#{lp 1771}# #{exps 1766}# '())))))))))))
       (#{chi-install-global 451}#
         (lambda (#{name 1776}# #{e 1777}#)
           (#{build-global-definition 319}#
             #f
             #{name 1776}#
             (#{build-application 303}#
               #f
               (#{build-primref 327}#
                 #f
                 'make-syntax-transformer)
               (list (#{build-data 329}# #f #{name 1776}#)
                     (#{build-data 329}# #f 'macro)
                     #{e 1777}#)))))
       (#{chi-when-list 453}#
         (lambda (#{e 1785}# #{when-list 1786}# #{w 1787}#)
           (letrec*
             ((#{f 1794}#
                (lambda (#{when-list 1795}# #{situations 1796}#)
                  (if (null? #{when-list 1795}#)
                    #{situations 1796}#
                    (#{f 1794}#
                      (cdr #{when-list 1795}#)
                      (cons (begin
                              (let ((#{x 1798}#
                                      (syntax->datum
                                        (car #{when-list 1795}#))))
                                (if (if (eq? #{x 1798}# 'compile)
                                      #t
                                      (if (eq? #{x 1798}# 'load)
                                        #t
                                        (if (eq? #{x 1798}# 'eval)
                                          #t
                                          (eq? #{x 1798}# 'expand))))
                                  #{x 1798}#
                                  (syntax-violation
                                    'eval-when
                                    "invalid situation"
                                    #{e 1785}#
                                    (#{wrap 443}#
                                      (car #{when-list 1795}#)
                                      #{w 1787}#
                                      #f)))))
                            #{situations 1796}#))))))
             (begin (#{f 1794}# #{when-list 1786}# '())))))
       (#{syntax-type 455}#
         (lambda (#{e 1799}#
                  #{r 1800}#
                  #{w 1801}#
                  #{s 1802}#
                  #{rib 1803}#
                  #{mod 1804}#
                  #{for-car? 1805}#)
           (if (symbol? #{e 1799}#)
             (begin
               (let ((#{n 1817}#
                       (#{id-var-name 431}# #{e 1799}# #{w 1801}#)))
                 (begin
                   (let ((#{b 1819}#
                           (#{lookup 371}#
                             #{n 1817}#
                             #{r 1800}#
                             #{mod 1804}#)))
                     (begin
                       (let ((#{type 1821}# (car #{b 1819}#)))
                         (if (eqv? #{type 1821}# 'lexical)
                           (values
                             #{type 1821}#
                             (cdr #{b 1819}#)
                             #{e 1799}#
                             #{w 1801}#
                             #{s 1802}#
                             #{mod 1804}#)
                           (if (eqv? #{type 1821}# 'global)
                             (values
                               #{type 1821}#
                               #{n 1817}#
                               #{e 1799}#
                               #{w 1801}#
                               #{s 1802}#
                               #{mod 1804}#)
                             (if (eqv? #{type 1821}# 'macro)
                               (if #{for-car? 1805}#
                                 (values
                                   #{type 1821}#
                                   (cdr #{b 1819}#)
                                   #{e 1799}#
                                   #{w 1801}#
                                   #{s 1802}#
                                   #{mod 1804}#)
                                 (#{syntax-type 455}#
                                   (#{chi-macro 463}#
                                     (cdr #{b 1819}#)
                                     #{e 1799}#
                                     #{r 1800}#
                                     #{w 1801}#
                                     #{s 1802}#
                                     #{rib 1803}#
                                     #{mod 1804}#)
                                   #{r 1800}#
                                   '(())
                                   #{s 1802}#
                                   #{rib 1803}#
                                   #{mod 1804}#
                                   #f))
                               (values
                                 #{type 1821}#
                                 (cdr #{b 1819}#)
                                 #{e 1799}#
                                 #{w 1801}#
                                 #{s 1802}#
                                 #{mod 1804}#))))))))))
             (if (pair? #{e 1799}#)
               (begin
                 (let ((#{first 1835}# (car #{e 1799}#)))
                   (call-with-values
                     (lambda ()
                       (#{syntax-type 455}#
                         #{first 1835}#
                         #{r 1800}#
                         #{w 1801}#
                         #{s 1802}#
                         #{rib 1803}#
                         #{mod 1804}#
                         #t))
                     (lambda (#{ftype 1836}#
                              #{fval 1837}#
                              #{fe 1838}#
                              #{fw 1839}#
                              #{fs 1840}#
                              #{fmod 1841}#)
                       (if (eqv? #{ftype 1836}# 'lexical)
                         (values
                           'lexical-call
                           #{fval 1837}#
                           #{e 1799}#
                           #{w 1801}#
                           #{s 1802}#
                           #{mod 1804}#)
                         (if (eqv? #{ftype 1836}# 'global)
                           (values
                             'global-call
                             (#{make-syntax-object 341}#
                               #{fval 1837}#
                               #{w 1801}#
                               #{fmod 1841}#)
                             #{e 1799}#
                             #{w 1801}#
                             #{s 1802}#
                             #{mod 1804}#)
                           (if (eqv? #{ftype 1836}# 'macro)
                             (#{syntax-type 455}#
                               (#{chi-macro 463}#
                                 #{fval 1837}#
                                 #{e 1799}#
                                 #{r 1800}#
                                 #{w 1801}#
                                 #{s 1802}#
                                 #{rib 1803}#
                                 #{mod 1804}#)
                               #{r 1800}#
                               '(())
                               #{s 1802}#
                               #{rib 1803}#
                               #{mod 1804}#
                               #{for-car? 1805}#)
                             (if (eqv? #{ftype 1836}# 'module-ref)
                               (call-with-values
                                 (lambda ()
                                   (#{fval 1837}#
                                     #{e 1799}#
                                     #{r 1800}#
                                     #{w 1801}#))
                                 (lambda (#{e 1853}#
                                          #{r 1854}#
                                          #{w 1855}#
                                          #{s 1856}#
                                          #{mod 1857}#)
                                   (#{syntax-type 455}#
                                     #{e 1853}#
                                     #{r 1854}#
                                     #{w 1855}#
                                     #{s 1856}#
                                     #{rib 1803}#
                                     #{mod 1857}#
                                     #{for-car? 1805}#)))
                               (if (eqv? #{ftype 1836}# 'core)
                                 (values
                                   'core-form
                                   #{fval 1837}#
                                   #{e 1799}#
                                   #{w 1801}#
                                   #{s 1802}#
                                   #{mod 1804}#)
                                 (if (eqv? #{ftype 1836}# 'local-syntax)
                                   (values
                                     'local-syntax-form
                                     #{fval 1837}#
                                     #{e 1799}#
                                     #{w 1801}#
                                     #{s 1802}#
                                     #{mod 1804}#)
                                   (if (eqv? #{ftype 1836}# 'begin)
                                     (values
                                       'begin-form
                                       #f
                                       #{e 1799}#
                                       #{w 1801}#
                                       #{s 1802}#
                                       #{mod 1804}#)
                                     (if (eqv? #{ftype 1836}# 'eval-when)
                                       (values
                                         'eval-when-form
                                         #f
                                         #{e 1799}#
                                         #{w 1801}#
                                         #{s 1802}#
                                         #{mod 1804}#)
                                       (if (eqv? #{ftype 1836}# 'define)
                                         (let ((#{tmp 1868}# #{e 1799}#))
                                           (let ((#{tmp 1869}#
                                                   ($sc-dispatch
                                                     #{tmp 1868}#
                                                     '(_ any any))))
                                             (if (if #{tmp 1869}#
                                                   (@apply
                                                     (lambda (#{name 1872}#
                                                              #{val 1873}#)
                                                       (#{id? 377}#
                                                         #{name 1872}#))
                                                     #{tmp 1869}#)
                                                   #f)
                                               (@apply
                                                 (lambda (#{name 1876}#
                                                          #{val 1877}#)
                                                   (values
                                                     'define-form
                                                     #{name 1876}#
                                                     #{val 1877}#
                                                     #{w 1801}#
                                                     #{s 1802}#
                                                     #{mod 1804}#))
                                                 #{tmp 1869}#)
                                               (let ((#{tmp 1878}#
                                                       ($sc-dispatch
                                                         #{tmp 1868}#
                                                         '(_ (any . any)
                                                             any
                                                             .
                                                             each-any))))
                                                 (if (if #{tmp 1878}#
                                                       (@apply
                                                         (lambda (#{name 1883}#
                                                                  #{args 1884}#
                                                                  #{e1 1885}#
                                                                  #{e2 1886}#)
                                                           (if (#{id? 377}#
                                                                 #{name 1883}#)
                                                             (#{valid-bound-ids? 437}#
                                                               (#{lambda-var-list 487}#
                                                                 #{args 1884}#))
                                                             #f))
                                                         #{tmp 1878}#)
                                                       #f)
                                                   (@apply
                                                     (lambda (#{name 1893}#
                                                              #{args 1894}#
                                                              #{e1 1895}#
                                                              #{e2 1896}#)
                                                       (values
                                                         'define-form
                                                         (#{wrap 443}#
                                                           #{name 1893}#
                                                           #{w 1801}#
                                                           #{mod 1804}#)
                                                         (#{decorate-source 297}#
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
                                                                       #("i1889"
                                                                         "i1890"
                                                                         "i1891"
                                                                         "i1892"))
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
                                                                       #("i1842"
                                                                         "i1843"
                                                                         "i1844"
                                                                         "i1845"
                                                                         "i1846"
                                                                         "i1847"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(first)
                                                                       #((top))
                                                                       #("i1834"))
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
                                                                       #("i1806"
                                                                         "i1807"
                                                                         "i1808"
                                                                         "i1809"
                                                                         "i1810"
                                                                         "i1811"
                                                                         "i1812"))
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
                                                                         chi-application
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
                                                                         build-application
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
                                                                         make-sequence
                                                                         make-application
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
                                                                        (top))
                                                                       ("i486"
                                                                        "i484"
                                                                        "i482"
                                                                        "i480"
                                                                        "i478"
                                                                        "i476"
                                                                        "i474"
                                                                        "i472"
                                                                        "i470"
                                                                        "i468"
                                                                        "i466"
                                                                        "i464"
                                                                        "i462"
                                                                        "i460"
                                                                        "i458"
                                                                        "i456"
                                                                        "i454"
                                                                        "i452"
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
                                                                        "i417"
                                                                        "i416"
                                                                        "i414"
                                                                        "i413"
                                                                        "i412"
                                                                        "i411"
                                                                        "i410"
                                                                        "i408"
                                                                        "i406"
                                                                        "i404"
                                                                        "i402"
                                                                        "i400"
                                                                        "i398"
                                                                        "i396"
                                                                        "i394"
                                                                        "i391"
                                                                        "i389"
                                                                        "i388"
                                                                        "i387"
                                                                        "i386"
                                                                        "i385"
                                                                        "i384"
                                                                        "i383"
                                                                        "i382"
                                                                        "i381"
                                                                        "i379"
                                                                        "i378"
                                                                        "i376"
                                                                        "i374"
                                                                        "i372"
                                                                        "i370"
                                                                        "i368"
                                                                        "i366"
                                                                        "i364"
                                                                        "i363"
                                                                        "i362"
                                                                        "i361"
                                                                        "i360"
                                                                        "i359"
                                                                        "i357"
                                                                        "i356"
                                                                        "i354"
                                                                        "i352"
                                                                        "i350"
                                                                        "i348"
                                                                        "i346"
                                                                        "i344"
                                                                        "i342"
                                                                        "i340"
                                                                        "i338"
                                                                        "i336"
                                                                        "i334"
                                                                        "i332"
                                                                        "i330"
                                                                        "i328"
                                                                        "i326"
                                                                        "i324"
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
                                                                        "i291"
                                                                        "i289"
                                                                        "i287"
                                                                        "i286"
                                                                        "i285"
                                                                        "i284"
                                                                        "i283"
                                                                        "i281"
                                                                        "i279"
                                                                        "i277"
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
                                                                        "i254"
                                                                        "i252"
                                                                        "i250"
                                                                        "i248"
                                                                        "i246"
                                                                        "i244"
                                                                        "i242"
                                                                        "i240"))
                                                                     #(ribcage
                                                                       (define-structure
                                                                         define-expansion-accessors
                                                                         define-expansion-constructors
                                                                         and-map*)
                                                                       ((top)
                                                                        (top)
                                                                        (top)
                                                                        (top))
                                                                       ("i42"
                                                                        "i41"
                                                                        "i40"
                                                                        "i38")))
                                                                    (hygiene
                                                                      guile))
                                                                 (#{wrap 443}#
                                                                   (cons #{args 1894}#
                                                                         (cons #{e1 1895}#
                                                                               #{e2 1896}#))
                                                                   #{w 1801}#
                                                                   #{mod 1804}#))
                                                           #{s 1802}#)
                                                         '(())
                                                         #{s 1802}#
                                                         #{mod 1804}#))
                                                     #{tmp 1878}#)
                                                   (let ((#{tmp 1899}#
                                                           ($sc-dispatch
                                                             #{tmp 1868}#
                                                             '(_ any))))
                                                     (if (if #{tmp 1899}#
                                                           (@apply
                                                             (lambda (#{name 1901}#)
                                                               (#{id? 377}#
                                                                 #{name 1901}#))
                                                             #{tmp 1899}#)
                                                           #f)
                                                       (@apply
                                                         (lambda (#{name 1903}#)
                                                           (values
                                                             'define-form
                                                             (#{wrap 443}#
                                                               #{name 1903}#
                                                               #{w 1801}#
                                                               #{mod 1804}#)
                                                             '(#(syntax-object
                                                                 if
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(name)
                                                                    #((top))
                                                                    #("i1902"))
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
                                                                    #("i1842"
                                                                      "i1843"
                                                                      "i1844"
                                                                      "i1845"
                                                                      "i1846"
                                                                      "i1847"))
                                                                  #(ribcage
                                                                    ()
                                                                    ()
                                                                    ())
                                                                  #(ribcage
                                                                    #(first)
                                                                    #((top))
                                                                    #("i1834"))
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
                                                                    #("i1806"
                                                                      "i1807"
                                                                      "i1808"
                                                                      "i1809"
                                                                      "i1810"
                                                                      "i1811"
                                                                      "i1812"))
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
                                                                      chi-application
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
                                                                      build-application
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
                                                                      make-sequence
                                                                      make-application
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
                                                                     (top))
                                                                    ("i486"
                                                                     "i484"
                                                                     "i482"
                                                                     "i480"
                                                                     "i478"
                                                                     "i476"
                                                                     "i474"
                                                                     "i472"
                                                                     "i470"
                                                                     "i468"
                                                                     "i466"
                                                                     "i464"
                                                                     "i462"
                                                                     "i460"
                                                                     "i458"
                                                                     "i456"
                                                                     "i454"
                                                                     "i452"
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
                                                                     "i417"
                                                                     "i416"
                                                                     "i414"
                                                                     "i413"
                                                                     "i412"
                                                                     "i411"
                                                                     "i410"
                                                                     "i408"
                                                                     "i406"
                                                                     "i404"
                                                                     "i402"
                                                                     "i400"
                                                                     "i398"
                                                                     "i396"
                                                                     "i394"
                                                                     "i391"
                                                                     "i389"
                                                                     "i388"
                                                                     "i387"
                                                                     "i386"
                                                                     "i385"
                                                                     "i384"
                                                                     "i383"
                                                                     "i382"
                                                                     "i381"
                                                                     "i379"
                                                                     "i378"
                                                                     "i376"
                                                                     "i374"
                                                                     "i372"
                                                                     "i370"
                                                                     "i368"
                                                                     "i366"
                                                                     "i364"
                                                                     "i363"
                                                                     "i362"
                                                                     "i361"
                                                                     "i360"
                                                                     "i359"
                                                                     "i357"
                                                                     "i356"
                                                                     "i354"
                                                                     "i352"
                                                                     "i350"
                                                                     "i348"
                                                                     "i346"
                                                                     "i344"
                                                                     "i342"
                                                                     "i340"
                                                                     "i338"
                                                                     "i336"
                                                                     "i334"
                                                                     "i332"
                                                                     "i330"
                                                                     "i328"
                                                                     "i326"
                                                                     "i324"
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
                                                                     "i291"
                                                                     "i289"
                                                                     "i287"
                                                                     "i286"
                                                                     "i285"
                                                                     "i284"
                                                                     "i283"
                                                                     "i281"
                                                                     "i279"
                                                                     "i277"
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
                                                                     "i254"
                                                                     "i252"
                                                                     "i250"
                                                                     "i248"
                                                                     "i246"
                                                                     "i244"
                                                                     "i242"
                                                                     "i240"))
                                                                  #(ribcage
                                                                    (define-structure
                                                                      define-expansion-accessors
                                                                      define-expansion-constructors
                                                                      and-map*)
                                                                    ((top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                    ("i42"
                                                                     "i41"
                                                                     "i40"
                                                                     "i38")))
                                                                 (hygiene
                                                                   guile))
                                                               #(syntax-object
                                                                 #f
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(name)
                                                                    #((top))
                                                                    #("i1902"))
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
                                                                    #("i1842"
                                                                      "i1843"
                                                                      "i1844"
                                                                      "i1845"
                                                                      "i1846"
                                                                      "i1847"))
                                                                  #(ribcage
                                                                    ()
                                                                    ()
                                                                    ())
                                                                  #(ribcage
                                                                    #(first)
                                                                    #((top))
                                                                    #("i1834"))
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
                                                                    #("i1806"
                                                                      "i1807"
                                                                      "i1808"
                                                                      "i1809"
                                                                      "i1810"
                                                                      "i1811"
                                                                      "i1812"))
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
                                                                      chi-application
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
                                                                      build-application
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
                                                                      make-sequence
                                                                      make-application
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
                                                                     (top))
                                                                    ("i486"
                                                                     "i484"
                                                                     "i482"
                                                                     "i480"
                                                                     "i478"
                                                                     "i476"
                                                                     "i474"
                                                                     "i472"
                                                                     "i470"
                                                                     "i468"
                                                                     "i466"
                                                                     "i464"
                                                                     "i462"
                                                                     "i460"
                                                                     "i458"
                                                                     "i456"
                                                                     "i454"
                                                                     "i452"
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
                                                                     "i417"
                                                                     "i416"
                                                                     "i414"
                                                                     "i413"
                                                                     "i412"
                                                                     "i411"
                                                                     "i410"
                                                                     "i408"
                                                                     "i406"
                                                                     "i404"
                                                                     "i402"
                                                                     "i400"
                                                                     "i398"
                                                                     "i396"
                                                                     "i394"
                                                                     "i391"
                                                                     "i389"
                                                                     "i388"
                                                                     "i387"
                                                                     "i386"
                                                                     "i385"
                                                                     "i384"
                                                                     "i383"
                                                                     "i382"
                                                                     "i381"
                                                                     "i379"
                                                                     "i378"
                                                                     "i376"
                                                                     "i374"
                                                                     "i372"
                                                                     "i370"
                                                                     "i368"
                                                                     "i366"
                                                                     "i364"
                                                                     "i363"
                                                                     "i362"
                                                                     "i361"
                                                                     "i360"
                                                                     "i359"
                                                                     "i357"
                                                                     "i356"
                                                                     "i354"
                                                                     "i352"
                                                                     "i350"
                                                                     "i348"
                                                                     "i346"
                                                                     "i344"
                                                                     "i342"
                                                                     "i340"
                                                                     "i338"
                                                                     "i336"
                                                                     "i334"
                                                                     "i332"
                                                                     "i330"
                                                                     "i328"
                                                                     "i326"
                                                                     "i324"
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
                                                                     "i291"
                                                                     "i289"
                                                                     "i287"
                                                                     "i286"
                                                                     "i285"
                                                                     "i284"
                                                                     "i283"
                                                                     "i281"
                                                                     "i279"
                                                                     "i277"
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
                                                                     "i254"
                                                                     "i252"
                                                                     "i250"
                                                                     "i248"
                                                                     "i246"
                                                                     "i244"
                                                                     "i242"
                                                                     "i240"))
                                                                  #(ribcage
                                                                    (define-structure
                                                                      define-expansion-accessors
                                                                      define-expansion-constructors
                                                                      and-map*)
                                                                    ((top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                    ("i42"
                                                                     "i41"
                                                                     "i40"
                                                                     "i38")))
                                                                 (hygiene
                                                                   guile))
                                                               #(syntax-object
                                                                 #f
                                                                 ((top)
                                                                  #(ribcage
                                                                    #(name)
                                                                    #((top))
                                                                    #("i1902"))
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
                                                                    #("i1842"
                                                                      "i1843"
                                                                      "i1844"
                                                                      "i1845"
                                                                      "i1846"
                                                                      "i1847"))
                                                                  #(ribcage
                                                                    ()
                                                                    ()
                                                                    ())
                                                                  #(ribcage
                                                                    #(first)
                                                                    #((top))
                                                                    #("i1834"))
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
                                                                    #("i1806"
                                                                      "i1807"
                                                                      "i1808"
                                                                      "i1809"
                                                                      "i1810"
                                                                      "i1811"
                                                                      "i1812"))
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
                                                                      chi-application
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
                                                                      build-application
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
                                                                      make-sequence
                                                                      make-application
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
                                                                     (top))
                                                                    ("i486"
                                                                     "i484"
                                                                     "i482"
                                                                     "i480"
                                                                     "i478"
                                                                     "i476"
                                                                     "i474"
                                                                     "i472"
                                                                     "i470"
                                                                     "i468"
                                                                     "i466"
                                                                     "i464"
                                                                     "i462"
                                                                     "i460"
                                                                     "i458"
                                                                     "i456"
                                                                     "i454"
                                                                     "i452"
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
                                                                     "i417"
                                                                     "i416"
                                                                     "i414"
                                                                     "i413"
                                                                     "i412"
                                                                     "i411"
                                                                     "i410"
                                                                     "i408"
                                                                     "i406"
                                                                     "i404"
                                                                     "i402"
                                                                     "i400"
                                                                     "i398"
                                                                     "i396"
                                                                     "i394"
                                                                     "i391"
                                                                     "i389"
                                                                     "i388"
                                                                     "i387"
                                                                     "i386"
                                                                     "i385"
                                                                     "i384"
                                                                     "i383"
                                                                     "i382"
                                                                     "i381"
                                                                     "i379"
                                                                     "i378"
                                                                     "i376"
                                                                     "i374"
                                                                     "i372"
                                                                     "i370"
                                                                     "i368"
                                                                     "i366"
                                                                     "i364"
                                                                     "i363"
                                                                     "i362"
                                                                     "i361"
                                                                     "i360"
                                                                     "i359"
                                                                     "i357"
                                                                     "i356"
                                                                     "i354"
                                                                     "i352"
                                                                     "i350"
                                                                     "i348"
                                                                     "i346"
                                                                     "i344"
                                                                     "i342"
                                                                     "i340"
                                                                     "i338"
                                                                     "i336"
                                                                     "i334"
                                                                     "i332"
                                                                     "i330"
                                                                     "i328"
                                                                     "i326"
                                                                     "i324"
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
                                                                     "i291"
                                                                     "i289"
                                                                     "i287"
                                                                     "i286"
                                                                     "i285"
                                                                     "i284"
                                                                     "i283"
                                                                     "i281"
                                                                     "i279"
                                                                     "i277"
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
                                                                     "i254"
                                                                     "i252"
                                                                     "i250"
                                                                     "i248"
                                                                     "i246"
                                                                     "i244"
                                                                     "i242"
                                                                     "i240"))
                                                                  #(ribcage
                                                                    (define-structure
                                                                      define-expansion-accessors
                                                                      define-expansion-constructors
                                                                      and-map*)
                                                                    ((top)
                                                                     (top)
                                                                     (top)
                                                                     (top))
                                                                    ("i42"
                                                                     "i41"
                                                                     "i40"
                                                                     "i38")))
                                                                 (hygiene
                                                                   guile)))
                                                             '(())
                                                             #{s 1802}#
                                                             #{mod 1804}#))
                                                         #{tmp 1899}#)
                                                       (syntax-violation
                                                         #f
                                                         "source expression failed to match any pattern"
                                                         #{tmp 1868}#))))))))
                                         (if (eqv? #{ftype 1836}#
                                                   'define-syntax)
                                           (let ((#{tmp 1906}# #{e 1799}#))
                                             (let ((#{tmp 1907}#
                                                     ($sc-dispatch
                                                       #{tmp 1906}#
                                                       '(_ any any))))
                                               (if (if #{tmp 1907}#
                                                     (@apply
                                                       (lambda (#{name 1910}#
                                                                #{val 1911}#)
                                                         (#{id? 377}#
                                                           #{name 1910}#))
                                                       #{tmp 1907}#)
                                                     #f)
                                                 (@apply
                                                   (lambda (#{name 1914}#
                                                            #{val 1915}#)
                                                     (values
                                                       'define-syntax-form
                                                       #{name 1914}#
                                                       #{val 1915}#
                                                       #{w 1801}#
                                                       #{s 1802}#
                                                       #{mod 1804}#))
                                                   #{tmp 1907}#)
                                                 (syntax-violation
                                                   #f
                                                   "source expression failed to match any pattern"
                                                   #{tmp 1906}#))))
                                           (values
                                             'call
                                             #f
                                             #{e 1799}#
                                             #{w 1801}#
                                             #{s 1802}#
                                             #{mod 1804}#)))))))))))))))
               (if (#{syntax-object? 343}# #{e 1799}#)
                 (#{syntax-type 455}#
                   (#{syntax-object-expression 345}# #{e 1799}#)
                   #{r 1800}#
                   (#{join-wraps 425}#
                     #{w 1801}#
                     (#{syntax-object-wrap 347}# #{e 1799}#))
                   (begin
                     (let ((#{t 1921}#
                             (#{source-annotation 358}# #{e 1799}#)))
                       (if #{t 1921}# #{t 1921}# #{s 1802}#)))
                   #{rib 1803}#
                   (begin
                     (let ((#{t 1925}#
                             (#{syntax-object-module 349}# #{e 1799}#)))
                       (if #{t 1925}# #{t 1925}# #{mod 1804}#)))
                   #{for-car? 1805}#)
                 (if (self-evaluating? #{e 1799}#)
                   (values
                     'constant
                     #f
                     #{e 1799}#
                     #{w 1801}#
                     #{s 1802}#
                     #{mod 1804}#)
                   (values
                     'other
                     #f
                     #{e 1799}#
                     #{w 1801}#
                     #{s 1802}#
                     #{mod 1804}#)))))))
       (#{chi 457}#
         (lambda (#{e 1930}# #{r 1931}# #{w 1932}# #{mod 1933}#)
           (call-with-values
             (lambda ()
               (#{syntax-type 455}#
                 #{e 1930}#
                 #{r 1931}#
                 #{w 1932}#
                 (#{source-annotation 358}# #{e 1930}#)
                 #f
                 #{mod 1933}#
                 #f))
             (lambda (#{type 1938}#
                      #{value 1939}#
                      #{e 1940}#
                      #{w 1941}#
                      #{s 1942}#
                      #{mod 1943}#)
               (#{chi-expr 459}#
                 #{type 1938}#
                 #{value 1939}#
                 #{e 1940}#
                 #{r 1931}#
                 #{w 1941}#
                 #{s 1942}#
                 #{mod 1943}#)))))
       (#{chi-expr 459}#
         (lambda (#{type 1950}#
                  #{value 1951}#
                  #{e 1952}#
                  #{r 1953}#
                  #{w 1954}#
                  #{s 1955}#
                  #{mod 1956}#)
           (if (eqv? #{type 1950}# 'lexical)
             (#{build-lexical-reference 309}#
               'value
               #{s 1955}#
               #{e 1952}#
               #{value 1951}#)
             (if (if (eqv? #{type 1950}# 'core)
                   #t
                   (eqv? #{type 1950}# 'core-form))
               (#{value 1951}#
                 #{e 1952}#
                 #{r 1953}#
                 #{w 1954}#
                 #{s 1955}#
                 #{mod 1956}#)
               (if (eqv? #{type 1950}# 'module-ref)
                 (call-with-values
                   (lambda ()
                     (#{value 1951}# #{e 1952}# #{r 1953}# #{w 1954}#))
                   (lambda (#{e 1967}#
                            #{r 1968}#
                            #{w 1969}#
                            #{s 1970}#
                            #{mod 1971}#)
                     (#{chi 457}#
                       #{e 1967}#
                       #{r 1968}#
                       #{w 1969}#
                       #{mod 1971}#)))
                 (if (eqv? #{type 1950}# 'lexical-call)
                   (#{chi-application 461}#
                     (begin
                       (let ((#{id 1979}# (car #{e 1952}#)))
                         (#{build-lexical-reference 309}#
                           'fun
                           (#{source-annotation 358}# #{id 1979}#)
                           (if (#{syntax-object? 343}# #{id 1979}#)
                             (syntax->datum #{id 1979}#)
                             #{id 1979}#)
                           #{value 1951}#)))
                     #{e 1952}#
                     #{r 1953}#
                     #{w 1954}#
                     #{s 1955}#
                     #{mod 1956}#)
                   (if (eqv? #{type 1950}# 'global-call)
                     (#{chi-application 461}#
                       (#{build-global-reference 315}#
                         (#{source-annotation 358}# (car #{e 1952}#))
                         (if (#{syntax-object? 343}# #{value 1951}#)
                           (#{syntax-object-expression 345}# #{value 1951}#)
                           #{value 1951}#)
                         (if (#{syntax-object? 343}# #{value 1951}#)
                           (#{syntax-object-module 349}# #{value 1951}#)
                           #{mod 1956}#))
                       #{e 1952}#
                       #{r 1953}#
                       #{w 1954}#
                       #{s 1955}#
                       #{mod 1956}#)
                     (if (eqv? #{type 1950}# 'constant)
                       (#{build-data 329}#
                         #{s 1955}#
                         (#{strip 483}#
                           (#{source-wrap 445}#
                             #{e 1952}#
                             #{w 1954}#
                             #{s 1955}#
                             #{mod 1956}#)
                           '(())))
                       (if (eqv? #{type 1950}# 'global)
                         (#{build-global-reference 315}#
                           #{s 1955}#
                           #{value 1951}#
                           #{mod 1956}#)
                         (if (eqv? #{type 1950}# 'call)
                           (#{chi-application 461}#
                             (#{chi 457}#
                               (car #{e 1952}#)
                               #{r 1953}#
                               #{w 1954}#
                               #{mod 1956}#)
                             #{e 1952}#
                             #{r 1953}#
                             #{w 1954}#
                             #{s 1955}#
                             #{mod 1956}#)
                           (if (eqv? #{type 1950}# 'begin-form)
                             (let ((#{tmp 1986}# #{e 1952}#))
                               (let ((#{tmp 1987}#
                                       ($sc-dispatch
                                         #{tmp 1986}#
                                         '(_ any . each-any))))
                                 (if #{tmp 1987}#
                                   (@apply
                                     (lambda (#{e1 1990}# #{e2 1991}#)
                                       (#{chi-sequence 447}#
                                         (cons #{e1 1990}# #{e2 1991}#)
                                         #{r 1953}#
                                         #{w 1954}#
                                         #{s 1955}#
                                         #{mod 1956}#))
                                     #{tmp 1987}#)
                                   (syntax-violation
                                     #f
                                     "source expression failed to match any pattern"
                                     #{tmp 1986}#))))
                             (if (eqv? #{type 1950}# 'local-syntax-form)
                               (#{chi-local-syntax 467}#
                                 #{value 1951}#
                                 #{e 1952}#
                                 #{r 1953}#
                                 #{w 1954}#
                                 #{s 1955}#
                                 #{mod 1956}#
                                 #{chi-sequence 447}#)
                               (if (eqv? #{type 1950}# 'eval-when-form)
                                 (let ((#{tmp 1995}# #{e 1952}#))
                                   (let ((#{tmp 1996}#
                                           ($sc-dispatch
                                             #{tmp 1995}#
                                             '(_ each-any any . each-any))))
                                     (if #{tmp 1996}#
                                       (@apply
                                         (lambda (#{x 2000}#
                                                  #{e1 2001}#
                                                  #{e2 2002}#)
                                           (begin
                                             (let ((#{when-list 2004}#
                                                     (#{chi-when-list 453}#
                                                       #{e 1952}#
                                                       #{x 2000}#
                                                       #{w 1954}#)))
                                               (if (memq 'eval
                                                         #{when-list 2004}#)
                                                 (#{chi-sequence 447}#
                                                   (cons #{e1 2001}#
                                                         #{e2 2002}#)
                                                   #{r 1953}#
                                                   #{w 1954}#
                                                   #{s 1955}#
                                                   #{mod 1956}#)
                                                 (#{chi-void 471}#)))))
                                         #{tmp 1996}#)
                                       (syntax-violation
                                         #f
                                         "source expression failed to match any pattern"
                                         #{tmp 1995}#))))
                                 (if (if (eqv? #{type 1950}# 'define-form)
                                       #t
                                       (eqv? #{type 1950}#
                                             'define-syntax-form))
                                   (syntax-violation
                                     #f
                                     "definition in expression context"
                                     #{e 1952}#
                                     (#{wrap 443}#
                                       #{value 1951}#
                                       #{w 1954}#
                                       #{mod 1956}#))
                                   (if (eqv? #{type 1950}# 'syntax)
                                     (syntax-violation
                                       #f
                                       "reference to pattern variable outside syntax form"
                                       (#{source-wrap 445}#
                                         #{e 1952}#
                                         #{w 1954}#
                                         #{s 1955}#
                                         #{mod 1956}#))
                                     (if (eqv? #{type 1950}#
                                               'displaced-lexical)
                                       (syntax-violation
                                         #f
                                         "reference to identifier outside its scope"
                                         (#{source-wrap 445}#
                                           #{e 1952}#
                                           #{w 1954}#
                                           #{s 1955}#
                                           #{mod 1956}#))
                                       (syntax-violation
                                         #f
                                         "unexpected syntax"
                                         (#{source-wrap 445}#
                                           #{e 1952}#
                                           #{w 1954}#
                                           #{s 1955}#
                                           #{mod 1956}#))))))))))))))))))
       (#{chi-application 461}#
         (lambda (#{x 2011}#
                  #{e 2012}#
                  #{r 2013}#
                  #{w 2014}#
                  #{s 2015}#
                  #{mod 2016}#)
           (let ((#{tmp 2023}# #{e 2012}#))
             (let ((#{tmp 2024}#
                     ($sc-dispatch #{tmp 2023}# '(any . each-any))))
               (if #{tmp 2024}#
                 (@apply
                   (lambda (#{e0 2027}# #{e1 2028}#)
                     (#{build-application 303}#
                       #{s 2015}#
                       #{x 2011}#
                       (map (lambda (#{e 2029}#)
                              (#{chi 457}#
                                #{e 2029}#
                                #{r 2013}#
                                #{w 2014}#
                                #{mod 2016}#))
                            #{e1 2028}#)))
                   #{tmp 2024}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp 2023}#))))))
       (#{chi-macro 463}#
         (lambda (#{p 2032}#
                  #{e 2033}#
                  #{r 2034}#
                  #{w 2035}#
                  #{s 2036}#
                  #{rib 2037}#
                  #{mod 2038}#)
           (letrec*
             ((#{rebuild-macro-output 2047}#
                (lambda (#{x 2048}# #{m 2049}#)
                  (if (pair? #{x 2048}#)
                    (#{decorate-source 297}#
                      (cons (#{rebuild-macro-output 2047}#
                              (car #{x 2048}#)
                              #{m 2049}#)
                            (#{rebuild-macro-output 2047}#
                              (cdr #{x 2048}#)
                              #{m 2049}#))
                      #{s 2036}#)
                    (if (#{syntax-object? 343}# #{x 2048}#)
                      (begin
                        (let ((#{w 2057}#
                                (#{syntax-object-wrap 347}# #{x 2048}#)))
                          (begin
                            (let ((#{ms 2060}# (car #{w 2057}#))
                                  (#{s 2061}# (cdr #{w 2057}#)))
                              (if (if (pair? #{ms 2060}#)
                                    (eq? (car #{ms 2060}#) #f)
                                    #f)
                                (#{make-syntax-object 341}#
                                  (#{syntax-object-expression 345}# #{x 2048}#)
                                  (cons (cdr #{ms 2060}#)
                                        (if #{rib 2037}#
                                          (cons #{rib 2037}# (cdr #{s 2061}#))
                                          (cdr #{s 2061}#)))
                                  (#{syntax-object-module 349}# #{x 2048}#))
                                (#{make-syntax-object 341}#
                                  (#{decorate-source 297}#
                                    (#{syntax-object-expression 345}#
                                      #{x 2048}#)
                                    #{s 2061}#)
                                  (cons (cons #{m 2049}# #{ms 2060}#)
                                        (if #{rib 2037}#
                                          (cons #{rib 2037}#
                                                (cons 'shift #{s 2061}#))
                                          (cons 'shift #{s 2061}#)))
                                  (#{syntax-object-module 349}#
                                    #{x 2048}#)))))))
                      (if (vector? #{x 2048}#)
                        (begin
                          (let ((#{n 2073}# (vector-length #{x 2048}#)))
                            (begin
                              (let ((#{v 2075}#
                                      (#{decorate-source 297}#
                                        (make-vector #{n 2073}#)
                                        #{x 2048}#)))
                                (letrec*
                                  ((#{loop 2078}#
                                     (lambda (#{i 2079}#)
                                       (if (= #{i 2079}# #{n 2073}#)
                                         (begin (if #f #f) #{v 2075}#)
                                         (begin
                                           (vector-set!
                                             #{v 2075}#
                                             #{i 2079}#
                                             (#{rebuild-macro-output 2047}#
                                               (vector-ref
                                                 #{x 2048}#
                                                 #{i 2079}#)
                                               #{m 2049}#))
                                           (#{loop 2078}#
                                             (#{1+}# #{i 2079}#)))))))
                                  (begin (#{loop 2078}# 0)))))))
                        (if (symbol? #{x 2048}#)
                          (syntax-violation
                            #f
                            "encountered raw symbol in macro output"
                            (#{source-wrap 445}#
                              #{e 2033}#
                              #{w 2035}#
                              (cdr #{w 2035}#)
                              #{mod 2038}#)
                            #{x 2048}#)
                          (#{decorate-source 297}#
                            #{x 2048}#
                            #{s 2036}#))))))))
             (begin
               (#{rebuild-macro-output 2047}#
                 (#{p 2032}#
                   (#{source-wrap 445}#
                     #{e 2033}#
                     (#{anti-mark 415}# #{w 2035}#)
                     #{s 2036}#
                     #{mod 2038}#))
                 (gensym "m"))))))
       (#{chi-body 465}#
         (lambda (#{body 2089}#
                  #{outer-form 2090}#
                  #{r 2091}#
                  #{w 2092}#
                  #{mod 2093}#)
           (begin
             (let ((#{r 2101}#
                     (cons '("placeholder" placeholder) #{r 2091}#)))
               (begin
                 (let ((#{ribcage 2103}#
                         (#{make-ribcage 395}# '() '() '())))
                   (begin
                     (let ((#{w 2106}#
                             (cons (car #{w 2092}#)
                                   (cons #{ribcage 2103}# (cdr #{w 2092}#)))))
                       (letrec*
                         ((#{parse 2118}#
                            (lambda (#{body 2119}#
                                     #{ids 2120}#
                                     #{labels 2121}#
                                     #{var-ids 2122}#
                                     #{vars 2123}#
                                     #{vals 2124}#
                                     #{bindings 2125}#)
                              (if (null? #{body 2119}#)
                                (syntax-violation
                                  #f
                                  "no expressions in body"
                                  #{outer-form 2090}#)
                                (begin
                                  (let ((#{e 2130}# (cdr (car #{body 2119}#)))
                                        (#{er 2131}#
                                          (car (car #{body 2119}#))))
                                    (call-with-values
                                      (lambda ()
                                        (#{syntax-type 455}#
                                          #{e 2130}#
                                          #{er 2131}#
                                          '(())
                                          (#{source-annotation 358}#
                                            #{er 2131}#)
                                          #{ribcage 2103}#
                                          #{mod 2093}#
                                          #f))
                                      (lambda (#{type 2133}#
                                               #{value 2134}#
                                               #{e 2135}#
                                               #{w 2136}#
                                               #{s 2137}#
                                               #{mod 2138}#)
                                        (if (eqv? #{type 2133}# 'define-form)
                                          (begin
                                            (let ((#{id 2148}#
                                                    (#{wrap 443}#
                                                      #{value 2134}#
                                                      #{w 2136}#
                                                      #{mod 2138}#))
                                                  (#{label 2149}#
                                                    (#{gen-label 390}#)))
                                              (begin
                                                (let ((#{var 2151}#
                                                        (#{gen-var 485}#
                                                          #{id 2148}#)))
                                                  (begin
                                                    (#{extend-ribcage! 419}#
                                                      #{ribcage 2103}#
                                                      #{id 2148}#
                                                      #{label 2149}#)
                                                    (#{parse 2118}#
                                                      (cdr #{body 2119}#)
                                                      (cons #{id 2148}#
                                                            #{ids 2120}#)
                                                      (cons #{label 2149}#
                                                            #{labels 2121}#)
                                                      (cons #{id 2148}#
                                                            #{var-ids 2122}#)
                                                      (cons #{var 2151}#
                                                            #{vars 2123}#)
                                                      (cons (cons #{er 2131}#
                                                                  (#{wrap 443}#
                                                                    #{e 2135}#
                                                                    #{w 2136}#
                                                                    #{mod 2138}#))
                                                            #{vals 2124}#)
                                                      (cons (cons 'lexical
                                                                  #{var 2151}#)
                                                            #{bindings 2125}#)))))))
                                          (if (eqv? #{type 2133}#
                                                    'define-syntax-form)
                                            (begin
                                              (let ((#{id 2156}#
                                                      (#{wrap 443}#
                                                        #{value 2134}#
                                                        #{w 2136}#
                                                        #{mod 2138}#))
                                                    (#{label 2157}#
                                                      (#{gen-label 390}#)))
                                                (begin
                                                  (#{extend-ribcage! 419}#
                                                    #{ribcage 2103}#
                                                    #{id 2156}#
                                                    #{label 2157}#)
                                                  (#{parse 2118}#
                                                    (cdr #{body 2119}#)
                                                    (cons #{id 2156}#
                                                          #{ids 2120}#)
                                                    (cons #{label 2157}#
                                                          #{labels 2121}#)
                                                    #{var-ids 2122}#
                                                    #{vars 2123}#
                                                    #{vals 2124}#
                                                    (cons (cons 'macro
                                                                (cons #{er 2131}#
                                                                      (#{wrap 443}#
                                                                        #{e 2135}#
                                                                        #{w 2136}#
                                                                        #{mod 2138}#)))
                                                          #{bindings 2125}#)))))
                                            (if (eqv? #{type 2133}#
                                                      'begin-form)
                                              (let ((#{tmp 2160}# #{e 2135}#))
                                                (let ((#{tmp 2161}#
                                                        ($sc-dispatch
                                                          #{tmp 2160}#
                                                          '(_ . each-any))))
                                                  (if #{tmp 2161}#
                                                    (@apply
                                                      (lambda (#{e1 2163}#)
                                                        (#{parse 2118}#
                                                          (letrec*
                                                            ((#{f 2166}#
                                                               (lambda (#{forms 2167}#)
                                                                 (if (null? #{forms 2167}#)
                                                                   (cdr #{body 2119}#)
                                                                   (cons (cons #{er 2131}#
                                                                               (#{wrap 443}#
                                                                                 (car #{forms 2167}#)
                                                                                 #{w 2136}#
                                                                                 #{mod 2138}#))
                                                                         (#{f 2166}#
                                                                           (cdr #{forms 2167}#)))))))
                                                            (begin
                                                              (#{f 2166}#
                                                                #{e1 2163}#)))
                                                          #{ids 2120}#
                                                          #{labels 2121}#
                                                          #{var-ids 2122}#
                                                          #{vars 2123}#
                                                          #{vals 2124}#
                                                          #{bindings 2125}#))
                                                      #{tmp 2161}#)
                                                    (syntax-violation
                                                      #f
                                                      "source expression failed to match any pattern"
                                                      #{tmp 2160}#))))
                                              (if (eqv? #{type 2133}#
                                                        'local-syntax-form)
                                                (#{chi-local-syntax 467}#
                                                  #{value 2134}#
                                                  #{e 2135}#
                                                  #{er 2131}#
                                                  #{w 2136}#
                                                  #{s 2137}#
                                                  #{mod 2138}#
                                                  (lambda (#{forms 2170}#
                                                           #{er 2171}#
                                                           #{w 2172}#
                                                           #{s 2173}#
                                                           #{mod 2174}#)
                                                    (#{parse 2118}#
                                                      (letrec*
                                                        ((#{f 2182}#
                                                           (lambda (#{forms 2183}#)
                                                             (if (null? #{forms 2183}#)
                                                               (cdr #{body 2119}#)
                                                               (cons (cons #{er 2171}#
                                                                           (#{wrap 443}#
                                                                             (car #{forms 2183}#)
                                                                             #{w 2172}#
                                                                             #{mod 2174}#))
                                                                     (#{f 2182}#
                                                                       (cdr #{forms 2183}#)))))))
                                                        (begin
                                                          (#{f 2182}#
                                                            #{forms 2170}#)))
                                                      #{ids 2120}#
                                                      #{labels 2121}#
                                                      #{var-ids 2122}#
                                                      #{vars 2123}#
                                                      #{vals 2124}#
                                                      #{bindings 2125}#)))
                                                (if (null? #{ids 2120}#)
                                                  (#{build-sequence 331}#
                                                    #f
                                                    (map (lambda (#{x 2186}#)
                                                           (#{chi 457}#
                                                             (cdr #{x 2186}#)
                                                             (car #{x 2186}#)
                                                             '(())
                                                             #{mod 2138}#))
                                                         (cons (cons #{er 2131}#
                                                                     (#{source-wrap 445}#
                                                                       #{e 2135}#
                                                                       #{w 2136}#
                                                                       #{s 2137}#
                                                                       #{mod 2138}#))
                                                               (cdr #{body 2119}#))))
                                                  (begin
                                                    (if (not (#{valid-bound-ids? 437}#
                                                               #{ids 2120}#))
                                                      (syntax-violation
                                                        #f
                                                        "invalid or duplicate identifier in definition"
                                                        #{outer-form 2090}#))
                                                    (letrec*
                                                      ((#{loop 2193}#
                                                         (lambda (#{bs 2194}#
                                                                  #{er-cache 2195}#
                                                                  #{r-cache 2196}#)
                                                           (if (not (null? #{bs 2194}#))
                                                             (begin
                                                               (let ((#{b 2199}#
                                                                       (car #{bs 2194}#)))
                                                                 (if (eq? (car #{b 2199}#)
                                                                          'macro)
                                                                   (begin
                                                                     (let ((#{er 2202}#
                                                                             (car (cdr #{b 2199}#))))
                                                                       (begin
                                                                         (let ((#{r-cache 2204}#
                                                                                 (if (eq? #{er 2202}#
                                                                                          #{er-cache 2195}#)
                                                                                   #{r-cache 2196}#
                                                                                   (#{macros-only-env 369}#
                                                                                     #{er 2202}#))))
                                                                           (begin
                                                                             (set-cdr!
                                                                               #{b 2199}#
                                                                               (#{eval-local-transformer 469}#
                                                                                 (#{chi 457}#
                                                                                   (cdr (cdr #{b 2199}#))
                                                                                   #{r-cache 2204}#
                                                                                   '(())
                                                                                   #{mod 2138}#)
                                                                                 #{mod 2138}#))
                                                                             (#{loop 2193}#
                                                                               (cdr #{bs 2194}#)
                                                                               #{er 2202}#
                                                                               #{r-cache 2204}#))))))
                                                                   (#{loop 2193}#
                                                                     (cdr #{bs 2194}#)
                                                                     #{er-cache 2195}#
                                                                     #{r-cache 2196}#))))))))
                                                      (begin
                                                        (#{loop 2193}#
                                                          #{bindings 2125}#
                                                          #f
                                                          #f)))
                                                    (set-cdr!
                                                      #{r 2101}#
                                                      (#{extend-env 365}#
                                                        #{labels 2121}#
                                                        #{bindings 2125}#
                                                        (cdr #{r 2101}#)))
                                                    (#{build-letrec 337}#
                                                      #f
                                                      #t
                                                      (reverse
                                                        (map syntax->datum
                                                             #{var-ids 2122}#))
                                                      (reverse #{vars 2123}#)
                                                      (map (lambda (#{x 2207}#)
                                                             (#{chi 457}#
                                                               (cdr #{x 2207}#)
                                                               (car #{x 2207}#)
                                                               '(())
                                                               #{mod 2138}#))
                                                           (reverse
                                                             #{vals 2124}#))
                                                      (#{build-sequence 331}#
                                                        #f
                                                        (map (lambda (#{x 2211}#)
                                                               (#{chi 457}#
                                                                 (cdr #{x 2211}#)
                                                                 (car #{x 2211}#)
                                                                 '(())
                                                                 #{mod 2138}#))
                                                             (cons (cons #{er 2131}#
                                                                         (#{source-wrap 445}#
                                                                           #{e 2135}#
                                                                           #{w 2136}#
                                                                           #{s 2137}#
                                                                           #{mod 2138}#))
                                                                   (cdr #{body 2119}#)))))))))))))))))))
                         (begin
                           (#{parse 2118}#
                             (map (lambda (#{x 2126}#)
                                    (cons #{r 2101}#
                                          (#{wrap 443}#
                                            #{x 2126}#
                                            #{w 2106}#
                                            #{mod 2093}#)))
                                  #{body 2089}#)
                             '()
                             '()
                             '()
                             '()
                             '()
                             '())))))))))))
       (#{chi-local-syntax 467}#
         (lambda (#{rec? 2214}#
                  #{e 2215}#
                  #{r 2216}#
                  #{w 2217}#
                  #{s 2218}#
                  #{mod 2219}#
                  #{k 2220}#)
           (let ((#{tmp 2228}# #{e 2215}#))
             (let ((#{tmp 2229}#
                     ($sc-dispatch
                       #{tmp 2228}#
                       '(_ #(each (any any)) any . each-any))))
               (if #{tmp 2229}#
                 (@apply
                   (lambda (#{id 2234}#
                            #{val 2235}#
                            #{e1 2236}#
                            #{e2 2237}#)
                     (begin
                       (let ((#{ids 2239}# #{id 2234}#))
                         (if (not (#{valid-bound-ids? 437}# #{ids 2239}#))
                           (syntax-violation
                             #f
                             "duplicate bound keyword"
                             #{e 2215}#)
                           (begin
                             (let ((#{labels 2242}#
                                     (#{gen-labels 392}# #{ids 2239}#)))
                               (begin
                                 (let ((#{new-w 2244}#
                                         (#{make-binding-wrap 421}#
                                           #{ids 2239}#
                                           #{labels 2242}#
                                           #{w 2217}#)))
                                   (#{k 2220}#
                                     (cons #{e1 2236}# #{e2 2237}#)
                                     (#{extend-env 365}#
                                       #{labels 2242}#
                                       (begin
                                         (let ((#{w 2248}#
                                                 (if #{rec? 2214}#
                                                   #{new-w 2244}#
                                                   #{w 2217}#))
                                               (#{trans-r 2249}#
                                                 (#{macros-only-env 369}#
                                                   #{r 2216}#)))
                                           (map (lambda (#{x 2250}#)
                                                  (cons 'macro
                                                        (#{eval-local-transformer 469}#
                                                          (#{chi 457}#
                                                            #{x 2250}#
                                                            #{trans-r 2249}#
                                                            #{w 2248}#
                                                            #{mod 2219}#)
                                                          #{mod 2219}#)))
                                                #{val 2235}#)))
                                       #{r 2216}#)
                                     #{new-w 2244}#
                                     #{s 2218}#
                                     #{mod 2219}#)))))))))
                   #{tmp 2229}#)
                 (let ((#{_ 2255}# #{tmp 2228}#))
                   (syntax-violation
                     #f
                     "bad local syntax definition"
                     (#{source-wrap 445}#
                       #{e 2215}#
                       #{w 2217}#
                       #{s 2218}#
                       #{mod 2219}#))))))))
       (#{eval-local-transformer 469}#
         (lambda (#{expanded 2256}# #{mod 2257}#)
           (begin
             (let ((#{p 2261}#
                     (#{local-eval-hook 290}#
                       #{expanded 2256}#
                       #{mod 2257}#)))
               (if (procedure? #{p 2261}#)
                 #{p 2261}#
                 (syntax-violation
                   #f
                   "nonprocedure transformer"
                   #{p 2261}#))))))
       (#{chi-void 471}#
         (lambda () (#{build-void 301}# #f)))
       (#{ellipsis? 473}#
         (lambda (#{x 2263}#)
           (if (#{nonsymbol-id? 375}# #{x 2263}#)
             (#{free-id=? 433}#
               #{x 2263}#
               '#(syntax-object
                  ...
                  ((top)
                   #(ribcage () () ())
                   #(ribcage () () ())
                   #(ribcage #(x) #((top)) #("i2264"))
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
                       chi-application
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
                       build-application
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
                       make-sequence
                       make-application
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
                      (top))
                     ("i486"
                      "i484"
                      "i482"
                      "i480"
                      "i478"
                      "i476"
                      "i474"
                      "i472"
                      "i470"
                      "i468"
                      "i466"
                      "i464"
                      "i462"
                      "i460"
                      "i458"
                      "i456"
                      "i454"
                      "i452"
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
                      "i417"
                      "i416"
                      "i414"
                      "i413"
                      "i412"
                      "i411"
                      "i410"
                      "i408"
                      "i406"
                      "i404"
                      "i402"
                      "i400"
                      "i398"
                      "i396"
                      "i394"
                      "i391"
                      "i389"
                      "i388"
                      "i387"
                      "i386"
                      "i385"
                      "i384"
                      "i383"
                      "i382"
                      "i381"
                      "i379"
                      "i378"
                      "i376"
                      "i374"
                      "i372"
                      "i370"
                      "i368"
                      "i366"
                      "i364"
                      "i363"
                      "i362"
                      "i361"
                      "i360"
                      "i359"
                      "i357"
                      "i356"
                      "i354"
                      "i352"
                      "i350"
                      "i348"
                      "i346"
                      "i344"
                      "i342"
                      "i340"
                      "i338"
                      "i336"
                      "i334"
                      "i332"
                      "i330"
                      "i328"
                      "i326"
                      "i324"
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
                      "i291"
                      "i289"
                      "i287"
                      "i286"
                      "i285"
                      "i284"
                      "i283"
                      "i281"
                      "i279"
                      "i277"
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
                      "i254"
                      "i252"
                      "i250"
                      "i248"
                      "i246"
                      "i244"
                      "i242"
                      "i240"))
                   #(ribcage
                     (define-structure
                       define-expansion-accessors
                       define-expansion-constructors
                       and-map*)
                     ((top) (top) (top) (top))
                     ("i42" "i41" "i40" "i38")))
                  (hygiene guile)))
             #f)))
       (#{lambda-formals 475}#
         (lambda (#{orig-args 2267}#)
           (letrec*
             ((#{req 2270}#
                (lambda (#{args 2273}# #{rreq 2274}#)
                  (let ((#{tmp 2277}# #{args 2273}#))
                    (let ((#{tmp 2278}# ($sc-dispatch #{tmp 2277}# '())))
                      (if #{tmp 2278}#
                        (@apply
                          (lambda ()
                            (#{check 2272}# (reverse #{rreq 2274}#) #f))
                          #{tmp 2278}#)
                        (let ((#{tmp 2279}#
                                ($sc-dispatch #{tmp 2277}# '(any . any))))
                          (if (if #{tmp 2279}#
                                (@apply
                                  (lambda (#{a 2282}# #{b 2283}#)
                                    (#{id? 377}# #{a 2282}#))
                                  #{tmp 2279}#)
                                #f)
                            (@apply
                              (lambda (#{a 2286}# #{b 2287}#)
                                (#{req 2270}#
                                  #{b 2287}#
                                  (cons #{a 2286}# #{rreq 2274}#)))
                              #{tmp 2279}#)
                            (let ((#{tmp 2288}# (list #{tmp 2277}#)))
                              (if (if #{tmp 2288}#
                                    (@apply
                                      (lambda (#{r 2290}#)
                                        (#{id? 377}# #{r 2290}#))
                                      #{tmp 2288}#)
                                    #f)
                                (@apply
                                  (lambda (#{r 2292}#)
                                    (#{check 2272}#
                                      (reverse #{rreq 2274}#)
                                      #{r 2292}#))
                                  #{tmp 2288}#)
                                (let ((#{else 2294}# #{tmp 2277}#))
                                  (syntax-violation
                                    'lambda
                                    "invalid argument list"
                                    #{orig-args 2267}#
                                    #{args 2273}#)))))))))))
              (#{check 2272}#
                (lambda (#{req 2295}# #{rest 2296}#)
                  (if (#{distinct-bound-ids? 439}#
                        (if #{rest 2296}#
                          (cons #{rest 2296}# #{req 2295}#)
                          #{req 2295}#))
                    (values #{req 2295}# #f #{rest 2296}# #f)
                    (syntax-violation
                      'lambda
                      "duplicate identifier in argument list"
                      #{orig-args 2267}#)))))
             (begin (#{req 2270}# #{orig-args 2267}# '())))))
       (#{chi-simple-lambda 477}#
         (lambda (#{e 2302}#
                  #{r 2303}#
                  #{w 2304}#
                  #{s 2305}#
                  #{mod 2306}#
                  #{req 2307}#
                  #{rest 2308}#
                  #{meta 2309}#
                  #{body 2310}#)
           (begin
             (let ((#{ids 2322}#
                     (if #{rest 2308}#
                       (append #{req 2307}# (list #{rest 2308}#))
                       #{req 2307}#)))
               (begin
                 (let ((#{vars 2324}#
                         (map #{gen-var 485}# #{ids 2322}#)))
                   (begin
                     (let ((#{labels 2326}#
                             (#{gen-labels 392}# #{ids 2322}#)))
                       (#{build-simple-lambda 321}#
                         #{s 2305}#
                         (map syntax->datum #{req 2307}#)
                         (if #{rest 2308}#
                           (syntax->datum #{rest 2308}#)
                           #f)
                         #{vars 2324}#
                         #{meta 2309}#
                         (#{chi-body 465}#
                           #{body 2310}#
                           (#{source-wrap 445}#
                             #{e 2302}#
                             #{w 2304}#
                             #{s 2305}#
                             #{mod 2306}#)
                           (#{extend-var-env 367}#
                             #{labels 2326}#
                             #{vars 2324}#
                             #{r 2303}#)
                           (#{make-binding-wrap 421}#
                             #{ids 2322}#
                             #{labels 2326}#
                             #{w 2304}#)
                           #{mod 2306}#))))))))))
       (#{lambda*-formals 479}#
         (lambda (#{orig-args 2329}#)
           (letrec*
             ((#{req 2332}#
                (lambda (#{args 2341}# #{rreq 2342}#)
                  (let ((#{tmp 2345}# #{args 2341}#))
                    (let ((#{tmp 2346}# ($sc-dispatch #{tmp 2345}# '())))
                      (if #{tmp 2346}#
                        (@apply
                          (lambda ()
                            (#{check 2340}#
                              (reverse #{rreq 2342}#)
                              '()
                              #f
                              '()))
                          #{tmp 2346}#)
                        (let ((#{tmp 2347}#
                                ($sc-dispatch #{tmp 2345}# '(any . any))))
                          (if (if #{tmp 2347}#
                                (@apply
                                  (lambda (#{a 2350}# #{b 2351}#)
                                    (#{id? 377}# #{a 2350}#))
                                  #{tmp 2347}#)
                                #f)
                            (@apply
                              (lambda (#{a 2354}# #{b 2355}#)
                                (#{req 2332}#
                                  #{b 2355}#
                                  (cons #{a 2354}# #{rreq 2342}#)))
                              #{tmp 2347}#)
                            (let ((#{tmp 2356}#
                                    ($sc-dispatch #{tmp 2345}# '(any . any))))
                              (if (if #{tmp 2356}#
                                    (@apply
                                      (lambda (#{a 2359}# #{b 2360}#)
                                        (eq? (syntax->datum #{a 2359}#)
                                             #:optional))
                                      #{tmp 2356}#)
                                    #f)
                                (@apply
                                  (lambda (#{a 2363}# #{b 2364}#)
                                    (#{opt 2334}#
                                      #{b 2364}#
                                      (reverse #{rreq 2342}#)
                                      '()))
                                  #{tmp 2356}#)
                                (let ((#{tmp 2365}#
                                        ($sc-dispatch
                                          #{tmp 2345}#
                                          '(any . any))))
                                  (if (if #{tmp 2365}#
                                        (@apply
                                          (lambda (#{a 2368}# #{b 2369}#)
                                            (eq? (syntax->datum #{a 2368}#)
                                                 #:key))
                                          #{tmp 2365}#)
                                        #f)
                                    (@apply
                                      (lambda (#{a 2372}# #{b 2373}#)
                                        (#{key 2336}#
                                          #{b 2373}#
                                          (reverse #{rreq 2342}#)
                                          '()
                                          '()))
                                      #{tmp 2365}#)
                                    (let ((#{tmp 2374}#
                                            ($sc-dispatch
                                              #{tmp 2345}#
                                              '(any any))))
                                      (if (if #{tmp 2374}#
                                            (@apply
                                              (lambda (#{a 2377}# #{b 2378}#)
                                                (eq? (syntax->datum #{a 2377}#)
                                                     #:rest))
                                              #{tmp 2374}#)
                                            #f)
                                        (@apply
                                          (lambda (#{a 2381}# #{b 2382}#)
                                            (#{rest 2338}#
                                              #{b 2382}#
                                              (reverse #{rreq 2342}#)
                                              '()
                                              '()))
                                          #{tmp 2374}#)
                                        (let ((#{tmp 2383}#
                                                (list #{tmp 2345}#)))
                                          (if (if #{tmp 2383}#
                                                (@apply
                                                  (lambda (#{r 2385}#)
                                                    (#{id? 377}# #{r 2385}#))
                                                  #{tmp 2383}#)
                                                #f)
                                            (@apply
                                              (lambda (#{r 2387}#)
                                                (#{rest 2338}#
                                                  #{r 2387}#
                                                  (reverse #{rreq 2342}#)
                                                  '()
                                                  '()))
                                              #{tmp 2383}#)
                                            (let ((#{else 2389}# #{tmp 2345}#))
                                              (syntax-violation
                                                'lambda*
                                                "invalid argument list"
                                                #{orig-args 2329}#
                                                #{args 2341}#)))))))))))))))))
              (#{opt 2334}#
                (lambda (#{args 2390}# #{req 2391}# #{ropt 2392}#)
                  (let ((#{tmp 2396}# #{args 2390}#))
                    (let ((#{tmp 2397}# ($sc-dispatch #{tmp 2396}# '())))
                      (if #{tmp 2397}#
                        (@apply
                          (lambda ()
                            (#{check 2340}#
                              #{req 2391}#
                              (reverse #{ropt 2392}#)
                              #f
                              '()))
                          #{tmp 2397}#)
                        (let ((#{tmp 2398}#
                                ($sc-dispatch #{tmp 2396}# '(any . any))))
                          (if (if #{tmp 2398}#
                                (@apply
                                  (lambda (#{a 2401}# #{b 2402}#)
                                    (#{id? 377}# #{a 2401}#))
                                  #{tmp 2398}#)
                                #f)
                            (@apply
                              (lambda (#{a 2405}# #{b 2406}#)
                                (#{opt 2334}#
                                  #{b 2406}#
                                  #{req 2391}#
                                  (cons (cons #{a 2405}#
                                              '(#(syntax-object
                                                  #f
                                                  ((top)
                                                   #(ribcage
                                                     #(a b)
                                                     #((top) (top))
                                                     #("i2403" "i2404"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(args req ropt)
                                                     #((top) (top) (top))
                                                     #("i2393"
                                                       "i2394"
                                                       "i2395"))
                                                   #(ribcage
                                                     (check rest key opt req)
                                                     ((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                     ("i2339"
                                                      "i2337"
                                                      "i2335"
                                                      "i2333"
                                                      "i2331"))
                                                   #(ribcage
                                                     #(orig-args)
                                                     #((top))
                                                     #("i2330"))
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
                                                       chi-application
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
                                                       build-application
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
                                                       make-sequence
                                                       make-application
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
                                                      (top))
                                                     ("i486"
                                                      "i484"
                                                      "i482"
                                                      "i480"
                                                      "i478"
                                                      "i476"
                                                      "i474"
                                                      "i472"
                                                      "i470"
                                                      "i468"
                                                      "i466"
                                                      "i464"
                                                      "i462"
                                                      "i460"
                                                      "i458"
                                                      "i456"
                                                      "i454"
                                                      "i452"
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
                                                      "i417"
                                                      "i416"
                                                      "i414"
                                                      "i413"
                                                      "i412"
                                                      "i411"
                                                      "i410"
                                                      "i408"
                                                      "i406"
                                                      "i404"
                                                      "i402"
                                                      "i400"
                                                      "i398"
                                                      "i396"
                                                      "i394"
                                                      "i391"
                                                      "i389"
                                                      "i388"
                                                      "i387"
                                                      "i386"
                                                      "i385"
                                                      "i384"
                                                      "i383"
                                                      "i382"
                                                      "i381"
                                                      "i379"
                                                      "i378"
                                                      "i376"
                                                      "i374"
                                                      "i372"
                                                      "i370"
                                                      "i368"
                                                      "i366"
                                                      "i364"
                                                      "i363"
                                                      "i362"
                                                      "i361"
                                                      "i360"
                                                      "i359"
                                                      "i357"
                                                      "i356"
                                                      "i354"
                                                      "i352"
                                                      "i350"
                                                      "i348"
                                                      "i346"
                                                      "i344"
                                                      "i342"
                                                      "i340"
                                                      "i338"
                                                      "i336"
                                                      "i334"
                                                      "i332"
                                                      "i330"
                                                      "i328"
                                                      "i326"
                                                      "i324"
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
                                                      "i291"
                                                      "i289"
                                                      "i287"
                                                      "i286"
                                                      "i285"
                                                      "i284"
                                                      "i283"
                                                      "i281"
                                                      "i279"
                                                      "i277"
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
                                                      "i254"
                                                      "i252"
                                                      "i250"
                                                      "i248"
                                                      "i246"
                                                      "i244"
                                                      "i242"
                                                      "i240"))
                                                   #(ribcage
                                                     (define-structure
                                                       define-expansion-accessors
                                                       define-expansion-constructors
                                                       and-map*)
                                                     ((top) (top) (top) (top))
                                                     ("i42"
                                                      "i41"
                                                      "i40"
                                                      "i38")))
                                                  (hygiene guile))))
                                        #{ropt 2392}#)))
                              #{tmp 2398}#)
                            (let ((#{tmp 2407}#
                                    ($sc-dispatch
                                      #{tmp 2396}#
                                      '((any any) . any))))
                              (if (if #{tmp 2407}#
                                    (@apply
                                      (lambda (#{a 2411}#
                                               #{init 2412}#
                                               #{b 2413}#)
                                        (#{id? 377}# #{a 2411}#))
                                      #{tmp 2407}#)
                                    #f)
                                (@apply
                                  (lambda (#{a 2417}# #{init 2418}# #{b 2419}#)
                                    (#{opt 2334}#
                                      #{b 2419}#
                                      #{req 2391}#
                                      (cons (list #{a 2417}# #{init 2418}#)
                                            #{ropt 2392}#)))
                                  #{tmp 2407}#)
                                (let ((#{tmp 2420}#
                                        ($sc-dispatch
                                          #{tmp 2396}#
                                          '(any . any))))
                                  (if (if #{tmp 2420}#
                                        (@apply
                                          (lambda (#{a 2423}# #{b 2424}#)
                                            (eq? (syntax->datum #{a 2423}#)
                                                 #:key))
                                          #{tmp 2420}#)
                                        #f)
                                    (@apply
                                      (lambda (#{a 2427}# #{b 2428}#)
                                        (#{key 2336}#
                                          #{b 2428}#
                                          #{req 2391}#
                                          (reverse #{ropt 2392}#)
                                          '()))
                                      #{tmp 2420}#)
                                    (let ((#{tmp 2429}#
                                            ($sc-dispatch
                                              #{tmp 2396}#
                                              '(any any))))
                                      (if (if #{tmp 2429}#
                                            (@apply
                                              (lambda (#{a 2432}# #{b 2433}#)
                                                (eq? (syntax->datum #{a 2432}#)
                                                     #:rest))
                                              #{tmp 2429}#)
                                            #f)
                                        (@apply
                                          (lambda (#{a 2436}# #{b 2437}#)
                                            (#{rest 2338}#
                                              #{b 2437}#
                                              #{req 2391}#
                                              (reverse #{ropt 2392}#)
                                              '()))
                                          #{tmp 2429}#)
                                        (let ((#{tmp 2438}#
                                                (list #{tmp 2396}#)))
                                          (if (if #{tmp 2438}#
                                                (@apply
                                                  (lambda (#{r 2440}#)
                                                    (#{id? 377}# #{r 2440}#))
                                                  #{tmp 2438}#)
                                                #f)
                                            (@apply
                                              (lambda (#{r 2442}#)
                                                (#{rest 2338}#
                                                  #{r 2442}#
                                                  #{req 2391}#
                                                  (reverse #{ropt 2392}#)
                                                  '()))
                                              #{tmp 2438}#)
                                            (let ((#{else 2444}# #{tmp 2396}#))
                                              (syntax-violation
                                                'lambda*
                                                "invalid optional argument list"
                                                #{orig-args 2329}#
                                                #{args 2390}#)))))))))))))))))
              (#{key 2336}#
                (lambda (#{args 2445}#
                         #{req 2446}#
                         #{opt 2447}#
                         #{rkey 2448}#)
                  (let ((#{tmp 2453}# #{args 2445}#))
                    (let ((#{tmp 2454}# ($sc-dispatch #{tmp 2453}# '())))
                      (if #{tmp 2454}#
                        (@apply
                          (lambda ()
                            (#{check 2340}#
                              #{req 2446}#
                              #{opt 2447}#
                              #f
                              (cons #f (reverse #{rkey 2448}#))))
                          #{tmp 2454}#)
                        (let ((#{tmp 2455}#
                                ($sc-dispatch #{tmp 2453}# '(any . any))))
                          (if (if #{tmp 2455}#
                                (@apply
                                  (lambda (#{a 2458}# #{b 2459}#)
                                    (#{id? 377}# #{a 2458}#))
                                  #{tmp 2455}#)
                                #f)
                            (@apply
                              (lambda (#{a 2462}# #{b 2463}#)
                                (let ((#{tmp 2465}#
                                        (symbol->keyword
                                          (syntax->datum #{a 2462}#))))
                                  (let ((#{k 2467}# #{tmp 2465}#))
                                    (#{key 2336}#
                                      #{b 2463}#
                                      #{req 2446}#
                                      #{opt 2447}#
                                      (cons (cons #{k 2467}#
                                                  (cons #{a 2462}#
                                                        '(#(syntax-object
                                                            #f
                                                            ((top)
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(k)
                                                               #((top))
                                                               #("i2466"))
                                                             #(ribcage
                                                               #(a b)
                                                               #((top) (top))
                                                               #("i2460"
                                                                 "i2461"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(args
                                                                 req
                                                                 opt
                                                                 rkey)
                                                               #((top)
                                                                 (top)
                                                                 (top)
                                                                 (top))
                                                               #("i2449"
                                                                 "i2450"
                                                                 "i2451"
                                                                 "i2452"))
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
                                                               ("i2339"
                                                                "i2337"
                                                                "i2335"
                                                                "i2333"
                                                                "i2331"))
                                                             #(ribcage
                                                               #(orig-args)
                                                               #((top))
                                                               #("i2330"))
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
                                                                 chi-application
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
                                                                 build-application
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
                                                                 make-sequence
                                                                 make-application
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
                                                                (top))
                                                               ("i486"
                                                                "i484"
                                                                "i482"
                                                                "i480"
                                                                "i478"
                                                                "i476"
                                                                "i474"
                                                                "i472"
                                                                "i470"
                                                                "i468"
                                                                "i466"
                                                                "i464"
                                                                "i462"
                                                                "i460"
                                                                "i458"
                                                                "i456"
                                                                "i454"
                                                                "i452"
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
                                                                "i417"
                                                                "i416"
                                                                "i414"
                                                                "i413"
                                                                "i412"
                                                                "i411"
                                                                "i410"
                                                                "i408"
                                                                "i406"
                                                                "i404"
                                                                "i402"
                                                                "i400"
                                                                "i398"
                                                                "i396"
                                                                "i394"
                                                                "i391"
                                                                "i389"
                                                                "i388"
                                                                "i387"
                                                                "i386"
                                                                "i385"
                                                                "i384"
                                                                "i383"
                                                                "i382"
                                                                "i381"
                                                                "i379"
                                                                "i378"
                                                                "i376"
                                                                "i374"
                                                                "i372"
                                                                "i370"
                                                                "i368"
                                                                "i366"
                                                                "i364"
                                                                "i363"
                                                                "i362"
                                                                "i361"
                                                                "i360"
                                                                "i359"
                                                                "i357"
                                                                "i356"
                                                                "i354"
                                                                "i352"
                                                                "i350"
                                                                "i348"
                                                                "i346"
                                                                "i344"
                                                                "i342"
                                                                "i340"
                                                                "i338"
                                                                "i336"
                                                                "i334"
                                                                "i332"
                                                                "i330"
                                                                "i328"
                                                                "i326"
                                                                "i324"
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
                                                                "i291"
                                                                "i289"
                                                                "i287"
                                                                "i286"
                                                                "i285"
                                                                "i284"
                                                                "i283"
                                                                "i281"
                                                                "i279"
                                                                "i277"
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
                                                                "i254"
                                                                "i252"
                                                                "i250"
                                                                "i248"
                                                                "i246"
                                                                "i244"
                                                                "i242"
                                                                "i240"))
                                                             #(ribcage
                                                               (define-structure
                                                                 define-expansion-accessors
                                                                 define-expansion-constructors
                                                                 and-map*)
                                                               ((top)
                                                                (top)
                                                                (top)
                                                                (top))
                                                               ("i42"
                                                                "i41"
                                                                "i40"
                                                                "i38")))
                                                            (hygiene guile)))))
                                            #{rkey 2448}#)))))
                              #{tmp 2455}#)
                            (let ((#{tmp 2468}#
                                    ($sc-dispatch
                                      #{tmp 2453}#
                                      '((any any) . any))))
                              (if (if #{tmp 2468}#
                                    (@apply
                                      (lambda (#{a 2472}#
                                               #{init 2473}#
                                               #{b 2474}#)
                                        (#{id? 377}# #{a 2472}#))
                                      #{tmp 2468}#)
                                    #f)
                                (@apply
                                  (lambda (#{a 2478}# #{init 2479}# #{b 2480}#)
                                    (let ((#{tmp 2482}#
                                            (symbol->keyword
                                              (syntax->datum #{a 2478}#))))
                                      (let ((#{k 2484}# #{tmp 2482}#))
                                        (#{key 2336}#
                                          #{b 2480}#
                                          #{req 2446}#
                                          #{opt 2447}#
                                          (cons (list #{k 2484}#
                                                      #{a 2478}#
                                                      #{init 2479}#)
                                                #{rkey 2448}#)))))
                                  #{tmp 2468}#)
                                (let ((#{tmp 2485}#
                                        ($sc-dispatch
                                          #{tmp 2453}#
                                          '((any any any) . any))))
                                  (if (if #{tmp 2485}#
                                        (@apply
                                          (lambda (#{a 2490}#
                                                   #{init 2491}#
                                                   #{k 2492}#
                                                   #{b 2493}#)
                                            (if (#{id? 377}# #{a 2490}#)
                                              (keyword?
                                                (syntax->datum #{k 2492}#))
                                              #f))
                                          #{tmp 2485}#)
                                        #f)
                                    (@apply
                                      (lambda (#{a 2500}#
                                               #{init 2501}#
                                               #{k 2502}#
                                               #{b 2503}#)
                                        (#{key 2336}#
                                          #{b 2503}#
                                          #{req 2446}#
                                          #{opt 2447}#
                                          (cons (list #{k 2502}#
                                                      #{a 2500}#
                                                      #{init 2501}#)
                                                #{rkey 2448}#)))
                                      #{tmp 2485}#)
                                    (let ((#{tmp 2504}#
                                            ($sc-dispatch
                                              #{tmp 2453}#
                                              '(any))))
                                      (if (if #{tmp 2504}#
                                            (@apply
                                              (lambda (#{aok 2506}#)
                                                (eq? (syntax->datum
                                                       #{aok 2506}#)
                                                     #:allow-other-keys))
                                              #{tmp 2504}#)
                                            #f)
                                        (@apply
                                          (lambda (#{aok 2508}#)
                                            (#{check 2340}#
                                              #{req 2446}#
                                              #{opt 2447}#
                                              #f
                                              (cons #t
                                                    (reverse #{rkey 2448}#))))
                                          #{tmp 2504}#)
                                        (let ((#{tmp 2509}#
                                                ($sc-dispatch
                                                  #{tmp 2453}#
                                                  '(any any any))))
                                          (if (if #{tmp 2509}#
                                                (@apply
                                                  (lambda (#{aok 2513}#
                                                           #{a 2514}#
                                                           #{b 2515}#)
                                                    (if (eq? (syntax->datum
                                                               #{aok 2513}#)
                                                             #:allow-other-keys)
                                                      (eq? (syntax->datum
                                                             #{a 2514}#)
                                                           #:rest)
                                                      #f))
                                                  #{tmp 2509}#)
                                                #f)
                                            (@apply
                                              (lambda (#{aok 2521}#
                                                       #{a 2522}#
                                                       #{b 2523}#)
                                                (#{rest 2338}#
                                                  #{b 2523}#
                                                  #{req 2446}#
                                                  #{opt 2447}#
                                                  (cons #t
                                                        (reverse
                                                          #{rkey 2448}#))))
                                              #{tmp 2509}#)
                                            (let ((#{tmp 2524}#
                                                    ($sc-dispatch
                                                      #{tmp 2453}#
                                                      '(any . any))))
                                              (if (if #{tmp 2524}#
                                                    (@apply
                                                      (lambda (#{aok 2527}#
                                                               #{r 2528}#)
                                                        (if (eq? (syntax->datum
                                                                   #{aok 2527}#)
                                                                 #:allow-other-keys)
                                                          (#{id? 377}#
                                                            #{r 2528}#)
                                                          #f))
                                                      #{tmp 2524}#)
                                                    #f)
                                                (@apply
                                                  (lambda (#{aok 2533}#
                                                           #{r 2534}#)
                                                    (#{rest 2338}#
                                                      #{r 2534}#
                                                      #{req 2446}#
                                                      #{opt 2447}#
                                                      (cons #t
                                                            (reverse
                                                              #{rkey 2448}#))))
                                                  #{tmp 2524}#)
                                                (let ((#{tmp 2535}#
                                                        ($sc-dispatch
                                                          #{tmp 2453}#
                                                          '(any any))))
                                                  (if (if #{tmp 2535}#
                                                        (@apply
                                                          (lambda (#{a 2538}#
                                                                   #{b 2539}#)
                                                            (eq? (syntax->datum
                                                                   #{a 2538}#)
                                                                 #:rest))
                                                          #{tmp 2535}#)
                                                        #f)
                                                    (@apply
                                                      (lambda (#{a 2542}#
                                                               #{b 2543}#)
                                                        (#{rest 2338}#
                                                          #{b 2543}#
                                                          #{req 2446}#
                                                          #{opt 2447}#
                                                          (cons #f
                                                                (reverse
                                                                  #{rkey 2448}#))))
                                                      #{tmp 2535}#)
                                                    (let ((#{tmp 2544}#
                                                            (list #{tmp 2453}#)))
                                                      (if (if #{tmp 2544}#
                                                            (@apply
                                                              (lambda (#{r 2546}#)
                                                                (#{id? 377}#
                                                                  #{r 2546}#))
                                                              #{tmp 2544}#)
                                                            #f)
                                                        (@apply
                                                          (lambda (#{r 2548}#)
                                                            (#{rest 2338}#
                                                              #{r 2548}#
                                                              #{req 2446}#
                                                              #{opt 2447}#
                                                              (cons #f
                                                                    (reverse
                                                                      #{rkey 2448}#))))
                                                          #{tmp 2544}#)
                                                        (let ((#{else 2550}#
                                                                #{tmp 2453}#))
                                                          (syntax-violation
                                                            'lambda*
                                                            "invalid keyword argument list"
                                                            #{orig-args 2329}#
                                                            #{args 2445}#)))))))))))))))))))))))
              (#{rest 2338}#
                (lambda (#{args 2551}#
                         #{req 2552}#
                         #{opt 2553}#
                         #{kw 2554}#)
                  (let ((#{tmp 2559}# #{args 2551}#))
                    (let ((#{tmp 2560}# (list #{tmp 2559}#)))
                      (if (if #{tmp 2560}#
                            (@apply
                              (lambda (#{r 2562}#) (#{id? 377}# #{r 2562}#))
                              #{tmp 2560}#)
                            #f)
                        (@apply
                          (lambda (#{r 2564}#)
                            (#{check 2340}#
                              #{req 2552}#
                              #{opt 2553}#
                              #{r 2564}#
                              #{kw 2554}#))
                          #{tmp 2560}#)
                        (let ((#{else 2566}# #{tmp 2559}#))
                          (syntax-violation
                            'lambda*
                            "invalid rest argument"
                            #{orig-args 2329}#
                            #{args 2551}#)))))))
              (#{check 2340}#
                (lambda (#{req 2567}#
                         #{opt 2568}#
                         #{rest 2569}#
                         #{kw 2570}#)
                  (if (#{distinct-bound-ids? 439}#
                        (append
                          #{req 2567}#
                          (map car #{opt 2568}#)
                          (if #{rest 2569}# (list #{rest 2569}#) '())
                          (if (pair? #{kw 2570}#)
                            (map cadr (cdr #{kw 2570}#))
                            '())))
                    (values
                      #{req 2567}#
                      #{opt 2568}#
                      #{rest 2569}#
                      #{kw 2570}#)
                    (syntax-violation
                      'lambda*
                      "duplicate identifier in argument list"
                      #{orig-args 2329}#)))))
             (begin (#{req 2332}# #{orig-args 2329}# '())))))
       (#{chi-lambda-case 481}#
         (lambda (#{e 2578}#
                  #{r 2579}#
                  #{w 2580}#
                  #{s 2581}#
                  #{mod 2582}#
                  #{get-formals 2583}#
                  #{clauses 2584}#)
           (letrec*
             ((#{expand-req 2593}#
                (lambda (#{req 2600}#
                         #{opt 2601}#
                         #{rest 2602}#
                         #{kw 2603}#
                         #{body 2604}#)
                  (begin
                    (let ((#{vars 2612}#
                            (map #{gen-var 485}# #{req 2600}#))
                          (#{labels 2613}#
                            (#{gen-labels 392}# #{req 2600}#)))
                      (begin
                        (let ((#{r* 2616}#
                                (#{extend-var-env 367}#
                                  #{labels 2613}#
                                  #{vars 2612}#
                                  #{r 2579}#))
                              (#{w* 2617}#
                                (#{make-binding-wrap 421}#
                                  #{req 2600}#
                                  #{labels 2613}#
                                  #{w 2580}#)))
                          (#{expand-opt 2595}#
                            (map syntax->datum #{req 2600}#)
                            #{opt 2601}#
                            #{rest 2602}#
                            #{kw 2603}#
                            #{body 2604}#
                            (reverse #{vars 2612}#)
                            #{r* 2616}#
                            #{w* 2617}#
                            '()
                            '())))))))
              (#{expand-opt 2595}#
                (lambda (#{req 2618}#
                         #{opt 2619}#
                         #{rest 2620}#
                         #{kw 2621}#
                         #{body 2622}#
                         #{vars 2623}#
                         #{r* 2624}#
                         #{w* 2625}#
                         #{out 2626}#
                         #{inits 2627}#)
                  (if (pair? #{opt 2619}#)
                    (let ((#{tmp 2640}# (car #{opt 2619}#)))
                      (let ((#{tmp 2641}#
                              ($sc-dispatch #{tmp 2640}# '(any any))))
                        (if #{tmp 2641}#
                          (@apply
                            (lambda (#{id 2644}# #{i 2645}#)
                              (begin
                                (let ((#{v 2648}#
                                        (#{gen-var 485}# #{id 2644}#)))
                                  (begin
                                    (let ((#{l 2650}#
                                            (#{gen-labels 392}#
                                              (list #{v 2648}#))))
                                      (begin
                                        (let ((#{r** 2652}#
                                                (#{extend-var-env 367}#
                                                  #{l 2650}#
                                                  (list #{v 2648}#)
                                                  #{r* 2624}#)))
                                          (begin
                                            (let ((#{w** 2654}#
                                                    (#{make-binding-wrap 421}#
                                                      (list #{id 2644}#)
                                                      #{l 2650}#
                                                      #{w* 2625}#)))
                                              (#{expand-opt 2595}#
                                                #{req 2618}#
                                                (cdr #{opt 2619}#)
                                                #{rest 2620}#
                                                #{kw 2621}#
                                                #{body 2622}#
                                                (cons #{v 2648}# #{vars 2623}#)
                                                #{r** 2652}#
                                                #{w** 2654}#
                                                (cons (syntax->datum
                                                        #{id 2644}#)
                                                      #{out 2626}#)
                                                (cons (#{chi 457}#
                                                        #{i 2645}#
                                                        #{r* 2624}#
                                                        #{w* 2625}#
                                                        #{mod 2582}#)
                                                      #{inits 2627}#)))))))))))
                            #{tmp 2641}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp 2640}#))))
                    (if #{rest 2620}#
                      (begin
                        (let ((#{v 2659}# (#{gen-var 485}# #{rest 2620}#)))
                          (begin
                            (let ((#{l 2661}#
                                    (#{gen-labels 392}# (list #{v 2659}#))))
                              (begin
                                (let ((#{r* 2663}#
                                        (#{extend-var-env 367}#
                                          #{l 2661}#
                                          (list #{v 2659}#)
                                          #{r* 2624}#)))
                                  (begin
                                    (let ((#{w* 2665}#
                                            (#{make-binding-wrap 421}#
                                              (list #{rest 2620}#)
                                              #{l 2661}#
                                              #{w* 2625}#)))
                                      (#{expand-kw 2597}#
                                        #{req 2618}#
                                        (if (pair? #{out 2626}#)
                                          (reverse #{out 2626}#)
                                          #f)
                                        (syntax->datum #{rest 2620}#)
                                        (if (pair? #{kw 2621}#)
                                          (cdr #{kw 2621}#)
                                          #{kw 2621}#)
                                        #{body 2622}#
                                        (cons #{v 2659}# #{vars 2623}#)
                                        #{r* 2663}#
                                        #{w* 2665}#
                                        (if (pair? #{kw 2621}#)
                                          (car #{kw 2621}#)
                                          #f)
                                        '()
                                        #{inits 2627}#)))))))))
                      (#{expand-kw 2597}#
                        #{req 2618}#
                        (if (pair? #{out 2626}#)
                          (reverse #{out 2626}#)
                          #f)
                        #f
                        (if (pair? #{kw 2621}#)
                          (cdr #{kw 2621}#)
                          #{kw 2621}#)
                        #{body 2622}#
                        #{vars 2623}#
                        #{r* 2624}#
                        #{w* 2625}#
                        (if (pair? #{kw 2621}#) (car #{kw 2621}#) #f)
                        '()
                        #{inits 2627}#)))))
              (#{expand-kw 2597}#
                (lambda (#{req 2667}#
                         #{opt 2668}#
                         #{rest 2669}#
                         #{kw 2670}#
                         #{body 2671}#
                         #{vars 2672}#
                         #{r* 2673}#
                         #{w* 2674}#
                         #{aok 2675}#
                         #{out 2676}#
                         #{inits 2677}#)
                  (if (pair? #{kw 2670}#)
                    (let ((#{tmp 2691}# (car #{kw 2670}#)))
                      (let ((#{tmp 2692}#
                              ($sc-dispatch #{tmp 2691}# '(any any any))))
                        (if #{tmp 2692}#
                          (@apply
                            (lambda (#{k 2696}# #{id 2697}# #{i 2698}#)
                              (begin
                                (let ((#{v 2701}#
                                        (#{gen-var 485}# #{id 2697}#)))
                                  (begin
                                    (let ((#{l 2703}#
                                            (#{gen-labels 392}#
                                              (list #{v 2701}#))))
                                      (begin
                                        (let ((#{r** 2705}#
                                                (#{extend-var-env 367}#
                                                  #{l 2703}#
                                                  (list #{v 2701}#)
                                                  #{r* 2673}#)))
                                          (begin
                                            (let ((#{w** 2707}#
                                                    (#{make-binding-wrap 421}#
                                                      (list #{id 2697}#)
                                                      #{l 2703}#
                                                      #{w* 2674}#)))
                                              (#{expand-kw 2597}#
                                                #{req 2667}#
                                                #{opt 2668}#
                                                #{rest 2669}#
                                                (cdr #{kw 2670}#)
                                                #{body 2671}#
                                                (cons #{v 2701}# #{vars 2672}#)
                                                #{r** 2705}#
                                                #{w** 2707}#
                                                #{aok 2675}#
                                                (cons (list (syntax->datum
                                                              #{k 2696}#)
                                                            (syntax->datum
                                                              #{id 2697}#)
                                                            #{v 2701}#)
                                                      #{out 2676}#)
                                                (cons (#{chi 457}#
                                                        #{i 2698}#
                                                        #{r* 2673}#
                                                        #{w* 2674}#
                                                        #{mod 2582}#)
                                                      #{inits 2677}#)))))))))))
                            #{tmp 2692}#)
                          (syntax-violation
                            #f
                            "source expression failed to match any pattern"
                            #{tmp 2691}#))))
                    (#{expand-body 2599}#
                      #{req 2667}#
                      #{opt 2668}#
                      #{rest 2669}#
                      (if (begin
                            (let ((#{t 2711}# #{aok 2675}#))
                              (if #{t 2711}# #{t 2711}# (pair? #{out 2676}#))))
                        (cons #{aok 2675}# (reverse #{out 2676}#))
                        #f)
                      #{body 2671}#
                      (reverse #{vars 2672}#)
                      #{r* 2673}#
                      #{w* 2674}#
                      (reverse #{inits 2677}#)
                      '()))))
              (#{expand-body 2599}#
                (lambda (#{req 2713}#
                         #{opt 2714}#
                         #{rest 2715}#
                         #{kw 2716}#
                         #{body 2717}#
                         #{vars 2718}#
                         #{r* 2719}#
                         #{w* 2720}#
                         #{inits 2721}#
                         #{meta 2722}#)
                  (let ((#{tmp 2733}# #{body 2717}#))
                    (let ((#{tmp 2734}#
                            ($sc-dispatch #{tmp 2733}# '(any any . each-any))))
                      (if (if #{tmp 2734}#
                            (@apply
                              (lambda (#{docstring 2738}#
                                       #{e1 2739}#
                                       #{e2 2740}#)
                                (string? (syntax->datum #{docstring 2738}#)))
                              #{tmp 2734}#)
                            #f)
                        (@apply
                          (lambda (#{docstring 2744}# #{e1 2745}# #{e2 2746}#)
                            (#{expand-body 2599}#
                              #{req 2713}#
                              #{opt 2714}#
                              #{rest 2715}#
                              #{kw 2716}#
                              (cons #{e1 2745}# #{e2 2746}#)
                              #{vars 2718}#
                              #{r* 2719}#
                              #{w* 2720}#
                              #{inits 2721}#
                              (append
                                #{meta 2722}#
                                (list (cons 'documentation
                                            (syntax->datum
                                              #{docstring 2744}#))))))
                          #{tmp 2734}#)
                        (let ((#{tmp 2749}#
                                ($sc-dispatch
                                  #{tmp 2733}#
                                  '(#(vector #(each (any . any)))
                                    any
                                    .
                                    each-any))))
                          (if #{tmp 2749}#
                            (@apply
                              (lambda (#{k 2754}#
                                       #{v 2755}#
                                       #{e1 2756}#
                                       #{e2 2757}#)
                                (#{expand-body 2599}#
                                  #{req 2713}#
                                  #{opt 2714}#
                                  #{rest 2715}#
                                  #{kw 2716}#
                                  (cons #{e1 2756}# #{e2 2757}#)
                                  #{vars 2718}#
                                  #{r* 2719}#
                                  #{w* 2720}#
                                  #{inits 2721}#
                                  (append
                                    #{meta 2722}#
                                    (syntax->datum
                                      (map cons #{k 2754}# #{v 2755}#)))))
                              #{tmp 2749}#)
                            (let ((#{tmp 2761}#
                                    ($sc-dispatch
                                      #{tmp 2733}#
                                      '(any . each-any))))
                              (if #{tmp 2761}#
                                (@apply
                                  (lambda (#{e1 2764}# #{e2 2765}#)
                                    (values
                                      #{meta 2722}#
                                      #{req 2713}#
                                      #{opt 2714}#
                                      #{rest 2715}#
                                      #{kw 2716}#
                                      #{inits 2721}#
                                      #{vars 2718}#
                                      (#{chi-body 465}#
                                        (cons #{e1 2764}# #{e2 2765}#)
                                        (#{source-wrap 445}#
                                          #{e 2578}#
                                          #{w 2580}#
                                          #{s 2581}#
                                          #{mod 2582}#)
                                        #{r* 2719}#
                                        #{w* 2720}#
                                        #{mod 2582}#)))
                                  #{tmp 2761}#)
                                (syntax-violation
                                  #f
                                  "source expression failed to match any pattern"
                                  #{tmp 2733}#)))))))))))
             (begin
               (let ((#{tmp 2767}# #{clauses 2584}#))
                 (let ((#{tmp 2768}# ($sc-dispatch #{tmp 2767}# '())))
                   (if #{tmp 2768}#
                     (@apply (lambda () (values '() #f)) #{tmp 2768}#)
                     (let ((#{tmp 2769}#
                             ($sc-dispatch
                               #{tmp 2767}#
                               '((any any . each-any)
                                 .
                                 #(each (any any . each-any))))))
                       (if #{tmp 2769}#
                         (@apply
                           (lambda (#{args 2776}#
                                    #{e1 2777}#
                                    #{e2 2778}#
                                    #{args* 2779}#
                                    #{e1* 2780}#
                                    #{e2* 2781}#)
                             (call-with-values
                               (lambda () (#{get-formals 2583}# #{args 2776}#))
                               (lambda (#{req 2782}#
                                        #{opt 2783}#
                                        #{rest 2784}#
                                        #{kw 2785}#)
                                 (call-with-values
                                   (lambda ()
                                     (#{expand-req 2593}#
                                       #{req 2782}#
                                       #{opt 2783}#
                                       #{rest 2784}#
                                       #{kw 2785}#
                                       (cons #{e1 2777}# #{e2 2778}#)))
                                   (lambda (#{meta 2791}#
                                            #{req 2792}#
                                            #{opt 2793}#
                                            #{rest 2794}#
                                            #{kw 2795}#
                                            #{inits 2796}#
                                            #{vars 2797}#
                                            #{body 2798}#)
                                     (call-with-values
                                       (lambda ()
                                         (#{chi-lambda-case 481}#
                                           #{e 2578}#
                                           #{r 2579}#
                                           #{w 2580}#
                                           #{s 2581}#
                                           #{mod 2582}#
                                           #{get-formals 2583}#
                                           (map (lambda (#{tmp 2809}#
                                                         #{tmp 2808}#
                                                         #{tmp 2807}#)
                                                  (cons #{tmp 2807}#
                                                        (cons #{tmp 2808}#
                                                              #{tmp 2809}#)))
                                                #{e2* 2781}#
                                                #{e1* 2780}#
                                                #{args* 2779}#)))
                                       (lambda (#{meta* 2811}# #{else* 2812}#)
                                         (values
                                           (append
                                             #{meta 2791}#
                                             #{meta* 2811}#)
                                           (#{build-lambda-case 325}#
                                             #{s 2581}#
                                             #{req 2792}#
                                             #{opt 2793}#
                                             #{rest 2794}#
                                             #{kw 2795}#
                                             #{inits 2796}#
                                             #{vars 2797}#
                                             #{body 2798}#
                                             #{else* 2812}#)))))))))
                           #{tmp 2769}#)
                         (syntax-violation
                           #f
                           "source expression failed to match any pattern"
                           #{tmp 2767}#))))))))))
       (#{strip 483}#
         (lambda (#{x 2815}# #{w 2816}#)
           (if (memq 'top (car #{w 2816}#))
             #{x 2815}#
             (letrec*
               ((#{f 2823}#
                  (lambda (#{x 2824}#)
                    (if (#{syntax-object? 343}# #{x 2824}#)
                      (#{strip 483}#
                        (#{syntax-object-expression 345}# #{x 2824}#)
                        (#{syntax-object-wrap 347}# #{x 2824}#))
                      (if (pair? #{x 2824}#)
                        (begin
                          (let ((#{a 2831}# (#{f 2823}# (car #{x 2824}#)))
                                (#{d 2832}# (#{f 2823}# (cdr #{x 2824}#))))
                            (if (if (eq? #{a 2831}# (car #{x 2824}#))
                                  (eq? #{d 2832}# (cdr #{x 2824}#))
                                  #f)
                              #{x 2824}#
                              (cons #{a 2831}# #{d 2832}#))))
                        (if (vector? #{x 2824}#)
                          (begin
                            (let ((#{old 2838}# (vector->list #{x 2824}#)))
                              (begin
                                (let ((#{new 2840}#
                                        (map #{f 2823}# #{old 2838}#)))
                                  (if (#{and-map* 39}#
                                        eq?
                                        #{old 2838}#
                                        #{new 2840}#)
                                    #{x 2824}#
                                    (list->vector #{new 2840}#))))))
                          #{x 2824}#))))))
               (begin (#{f 2823}# #{x 2815}#))))))
       (#{gen-var 485}#
         (lambda (#{id 2842}#)
           (begin
             (let ((#{id 2845}#
                     (if (#{syntax-object? 343}# #{id 2842}#)
                       (#{syntax-object-expression 345}# #{id 2842}#)
                       #{id 2842}#)))
               (gensym
                 (string-append (symbol->string #{id 2845}#) " "))))))
       (#{lambda-var-list 487}#
         (lambda (#{vars 2847}#)
           (letrec*
             ((#{lvl 2853}#
                (lambda (#{vars 2854}# #{ls 2855}# #{w 2856}#)
                  (if (pair? #{vars 2854}#)
                    (#{lvl 2853}#
                      (cdr #{vars 2854}#)
                      (cons (#{wrap 443}# (car #{vars 2854}#) #{w 2856}# #f)
                            #{ls 2855}#)
                      #{w 2856}#)
                    (if (#{id? 377}# #{vars 2854}#)
                      (cons (#{wrap 443}# #{vars 2854}# #{w 2856}# #f)
                            #{ls 2855}#)
                      (if (null? #{vars 2854}#)
                        #{ls 2855}#
                        (if (#{syntax-object? 343}# #{vars 2854}#)
                          (#{lvl 2853}#
                            (#{syntax-object-expression 345}# #{vars 2854}#)
                            #{ls 2855}#
                            (#{join-wraps 425}#
                              #{w 2856}#
                              (#{syntax-object-wrap 347}# #{vars 2854}#)))
                          (cons #{vars 2854}# #{ls 2855}#))))))))
             (begin (#{lvl 2853}# #{vars 2847}# '() '(())))))))
      (begin
        (lambda (#{src 805}# #{name 806}#)
          (make-struct/no-tail
            (vector-ref %expanded-vtables 2)
            #{src 805}#
            #{name 806}#))
        (lambda (#{x 1183}# #{update 1184}#)
          (vector-set! #{x 1183}# 1 #{update 1184}#))
        (lambda (#{x 1187}# #{update 1188}#)
          (vector-set! #{x 1187}# 2 #{update 1188}#))
        (lambda (#{x 1191}# #{update 1192}#)
          (vector-set! #{x 1191}# 3 #{update 1192}#))
        (lambda (#{x 1272}#)
          (if (vector? #{x 1272}#)
            (if (= (vector-length #{x 1272}#) 4)
              (eq? (vector-ref #{x 1272}# 0) 'ribcage)
              #f)
            #f))
        (begin
          (#{global-extend 373}#
            'local-syntax
            'letrec-syntax
            #t)
          (#{global-extend 373}#
            'local-syntax
            'let-syntax
            #f)
          (#{global-extend 373}#
            'core
            'fluid-let-syntax
            (lambda (#{e 2867}#
                     #{r 2868}#
                     #{w 2869}#
                     #{s 2870}#
                     #{mod 2871}#)
              (let ((#{tmp 2877}# #{e 2867}#))
                (let ((#{tmp 2878}#
                        ($sc-dispatch
                          #{tmp 2877}#
                          '(_ #(each (any any)) any . each-any))))
                  (if (if #{tmp 2878}#
                        (@apply
                          (lambda (#{var 2883}#
                                   #{val 2884}#
                                   #{e1 2885}#
                                   #{e2 2886}#)
                            (#{valid-bound-ids? 437}# #{var 2883}#))
                          #{tmp 2878}#)
                        #f)
                    (@apply
                      (lambda (#{var 2892}#
                               #{val 2893}#
                               #{e1 2894}#
                               #{e2 2895}#)
                        (begin
                          (let ((#{names 2897}#
                                  (map (lambda (#{x 2898}#)
                                         (#{id-var-name 431}#
                                           #{x 2898}#
                                           #{w 2869}#))
                                       #{var 2892}#)))
                            (begin
                              (for-each
                                (lambda (#{id 2901}# #{n 2902}#)
                                  (begin
                                    (let ((#{atom-key 2907}#
                                            (car (#{lookup 371}#
                                                   #{n 2902}#
                                                   #{r 2868}#
                                                   #{mod 2871}#))))
                                      (if (eqv? #{atom-key 2907}#
                                                'displaced-lexical)
                                        (syntax-violation
                                          'fluid-let-syntax
                                          "identifier out of context"
                                          #{e 2867}#
                                          (#{source-wrap 445}#
                                            #{id 2901}#
                                            #{w 2869}#
                                            #{s 2870}#
                                            #{mod 2871}#))))))
                                #{var 2892}#
                                #{names 2897}#)
                              (#{chi-body 465}#
                                (cons #{e1 2894}# #{e2 2895}#)
                                (#{source-wrap 445}#
                                  #{e 2867}#
                                  #{w 2869}#
                                  #{s 2870}#
                                  #{mod 2871}#)
                                (#{extend-env 365}#
                                  #{names 2897}#
                                  (begin
                                    (let ((#{trans-r 2913}#
                                            (#{macros-only-env 369}#
                                              #{r 2868}#)))
                                      (map (lambda (#{x 2914}#)
                                             (cons 'macro
                                                   (#{eval-local-transformer 469}#
                                                     (#{chi 457}#
                                                       #{x 2914}#
                                                       #{trans-r 2913}#
                                                       #{w 2869}#
                                                       #{mod 2871}#)
                                                     #{mod 2871}#)))
                                           #{val 2893}#)))
                                  #{r 2868}#)
                                #{w 2869}#
                                #{mod 2871}#)))))
                      #{tmp 2878}#)
                    (let ((#{_ 2919}# #{tmp 2877}#))
                      (syntax-violation
                        'fluid-let-syntax
                        "bad syntax"
                        (#{source-wrap 445}#
                          #{e 2867}#
                          #{w 2869}#
                          #{s 2870}#
                          #{mod 2871}#))))))))
          (#{global-extend 373}#
            'core
            'quote
            (lambda (#{e 2920}#
                     #{r 2921}#
                     #{w 2922}#
                     #{s 2923}#
                     #{mod 2924}#)
              (let ((#{tmp 2930}# #{e 2920}#))
                (let ((#{tmp 2931}#
                        ($sc-dispatch #{tmp 2930}# '(_ any))))
                  (if #{tmp 2931}#
                    (@apply
                      (lambda (#{e 2933}#)
                        (#{build-data 329}#
                          #{s 2923}#
                          (#{strip 483}# #{e 2933}# #{w 2922}#)))
                      #{tmp 2931}#)
                    (let ((#{_ 2935}# #{tmp 2930}#))
                      (syntax-violation
                        'quote
                        "bad syntax"
                        (#{source-wrap 445}#
                          #{e 2920}#
                          #{w 2922}#
                          #{s 2923}#
                          #{mod 2924}#))))))))
          (#{global-extend 373}#
            'core
            'syntax
            (letrec*
              ((#{gen-syntax 2937}#
                 (lambda (#{src 2952}#
                          #{e 2953}#
                          #{r 2954}#
                          #{maps 2955}#
                          #{ellipsis? 2956}#
                          #{mod 2957}#)
                   (if (#{id? 377}# #{e 2953}#)
                     (begin
                       (let ((#{label 2965}#
                               (#{id-var-name 431}# #{e 2953}# '(()))))
                         (begin
                           (let ((#{b 2968}#
                                   (#{lookup 371}#
                                     #{label 2965}#
                                     #{r 2954}#
                                     #{mod 2957}#)))
                             (if (eq? (car #{b 2968}#) 'syntax)
                               (call-with-values
                                 (lambda ()
                                   (begin
                                     (let ((#{var.lev 2971}# (cdr #{b 2968}#)))
                                       (#{gen-ref 2939}#
                                         #{src 2952}#
                                         (car #{var.lev 2971}#)
                                         (cdr #{var.lev 2971}#)
                                         #{maps 2955}#))))
                                 (lambda (#{var 2973}# #{maps 2974}#)
                                   (values
                                     (list 'ref #{var 2973}#)
                                     #{maps 2974}#)))
                               (if (#{ellipsis? 2956}# #{e 2953}#)
                                 (syntax-violation
                                   'syntax
                                   "misplaced ellipsis"
                                   #{src 2952}#)
                                 (values
                                   (list 'quote #{e 2953}#)
                                   #{maps 2955}#)))))))
                     (let ((#{tmp 2979}# #{e 2953}#))
                       (let ((#{tmp 2980}#
                               ($sc-dispatch #{tmp 2979}# '(any any))))
                         (if (if #{tmp 2980}#
                               (@apply
                                 (lambda (#{dots 2983}# #{e 2984}#)
                                   (#{ellipsis? 2956}# #{dots 2983}#))
                                 #{tmp 2980}#)
                               #f)
                           (@apply
                             (lambda (#{dots 2987}# #{e 2988}#)
                               (#{gen-syntax 2937}#
                                 #{src 2952}#
                                 #{e 2988}#
                                 #{r 2954}#
                                 #{maps 2955}#
                                 (lambda (#{x 2989}#) #f)
                                 #{mod 2957}#))
                             #{tmp 2980}#)
                           (let ((#{tmp 2991}#
                                   ($sc-dispatch
                                     #{tmp 2979}#
                                     '(any any . any))))
                             (if (if #{tmp 2991}#
                                   (@apply
                                     (lambda (#{x 2995}#
                                              #{dots 2996}#
                                              #{y 2997}#)
                                       (#{ellipsis? 2956}# #{dots 2996}#))
                                     #{tmp 2991}#)
                                   #f)
                               (@apply
                                 (lambda (#{x 3001}# #{dots 3002}# #{y 3003}#)
                                   (letrec*
                                     ((#{f 3007}#
                                        (lambda (#{y 3008}# #{k 3009}#)
                                          (let ((#{tmp 3016}# #{y 3008}#))
                                            (let ((#{tmp 3017}#
                                                    ($sc-dispatch
                                                      #{tmp 3016}#
                                                      '(any . any))))
                                              (if (if #{tmp 3017}#
                                                    (@apply
                                                      (lambda (#{dots 3020}#
                                                               #{y 3021}#)
                                                        (#{ellipsis? 2956}#
                                                          #{dots 3020}#))
                                                      #{tmp 3017}#)
                                                    #f)
                                                (@apply
                                                  (lambda (#{dots 3024}#
                                                           #{y 3025}#)
                                                    (#{f 3007}#
                                                      #{y 3025}#
                                                      (lambda (#{maps 3026}#)
                                                        (call-with-values
                                                          (lambda ()
                                                            (#{k 3009}#
                                                              (cons '()
                                                                    #{maps 3026}#)))
                                                          (lambda (#{x 3028}#
                                                                   #{maps 3029}#)
                                                            (if (null? (car #{maps 3029}#))
                                                              (syntax-violation
                                                                'syntax
                                                                "extra ellipsis"
                                                                #{src 2952}#)
                                                              (values
                                                                (#{gen-mappend 2941}#
                                                                  #{x 3028}#
                                                                  (car #{maps 3029}#))
                                                                (cdr #{maps 3029}#))))))))
                                                  #{tmp 3017}#)
                                                (let ((#{_ 3033}#
                                                        #{tmp 3016}#))
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{gen-syntax 2937}#
                                                        #{src 2952}#
                                                        #{y 3008}#
                                                        #{r 2954}#
                                                        #{maps 2955}#
                                                        #{ellipsis? 2956}#
                                                        #{mod 2957}#))
                                                    (lambda (#{y 3034}#
                                                             #{maps 3035}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{k 3009}#
                                                            #{maps 3035}#))
                                                        (lambda (#{x 3038}#
                                                                 #{maps 3039}#)
                                                          (values
                                                            (#{gen-append 2947}#
                                                              #{x 3038}#
                                                              #{y 3034}#)
                                                            #{maps 3039}#))))))))))))
                                     (begin
                                       (#{f 3007}#
                                         #{y 3003}#
                                         (lambda (#{maps 3010}#)
                                           (call-with-values
                                             (lambda ()
                                               (#{gen-syntax 2937}#
                                                 #{src 2952}#
                                                 #{x 3001}#
                                                 #{r 2954}#
                                                 (cons '() #{maps 3010}#)
                                                 #{ellipsis? 2956}#
                                                 #{mod 2957}#))
                                             (lambda (#{x 3012}# #{maps 3013}#)
                                               (if (null? (car #{maps 3013}#))
                                                 (syntax-violation
                                                   'syntax
                                                   "extra ellipsis"
                                                   #{src 2952}#)
                                                 (values
                                                   (#{gen-map 2943}#
                                                     #{x 3012}#
                                                     (car #{maps 3013}#))
                                                   (cdr #{maps 3013}#))))))))))
                                 #{tmp 2991}#)
                               (let ((#{tmp 3042}#
                                       ($sc-dispatch
                                         #{tmp 2979}#
                                         '(any . any))))
                                 (if #{tmp 3042}#
                                   (@apply
                                     (lambda (#{x 3045}# #{y 3046}#)
                                       (call-with-values
                                         (lambda ()
                                           (#{gen-syntax 2937}#
                                             #{src 2952}#
                                             #{x 3045}#
                                             #{r 2954}#
                                             #{maps 2955}#
                                             #{ellipsis? 2956}#
                                             #{mod 2957}#))
                                         (lambda (#{x 3047}# #{maps 3048}#)
                                           (call-with-values
                                             (lambda ()
                                               (#{gen-syntax 2937}#
                                                 #{src 2952}#
                                                 #{y 3046}#
                                                 #{r 2954}#
                                                 #{maps 3048}#
                                                 #{ellipsis? 2956}#
                                                 #{mod 2957}#))
                                             (lambda (#{y 3051}# #{maps 3052}#)
                                               (values
                                                 (#{gen-cons 2945}#
                                                   #{x 3047}#
                                                   #{y 3051}#)
                                                 #{maps 3052}#))))))
                                     #{tmp 3042}#)
                                   (let ((#{tmp 3055}#
                                           ($sc-dispatch
                                             #{tmp 2979}#
                                             '#(vector (any . each-any)))))
                                     (if #{tmp 3055}#
                                       (@apply
                                         (lambda (#{e1 3058}# #{e2 3059}#)
                                           (call-with-values
                                             (lambda ()
                                               (#{gen-syntax 2937}#
                                                 #{src 2952}#
                                                 (cons #{e1 3058}# #{e2 3059}#)
                                                 #{r 2954}#
                                                 #{maps 2955}#
                                                 #{ellipsis? 2956}#
                                                 #{mod 2957}#))
                                             (lambda (#{e 3061}# #{maps 3062}#)
                                               (values
                                                 (#{gen-vector 2949}#
                                                   #{e 3061}#)
                                                 #{maps 3062}#))))
                                         #{tmp 3055}#)
                                       (let ((#{_ 3066}# #{tmp 2979}#))
                                         (values
                                           (list 'quote #{e 2953}#)
                                           #{maps 2955}#))))))))))))))
               (#{gen-ref 2939}#
                 (lambda (#{src 3068}#
                          #{var 3069}#
                          #{level 3070}#
                          #{maps 3071}#)
                   (if (= #{level 3070}# 0)
                     (values #{var 3069}# #{maps 3071}#)
                     (if (null? #{maps 3071}#)
                       (syntax-violation
                         'syntax
                         "missing ellipsis"
                         #{src 3068}#)
                       (call-with-values
                         (lambda ()
                           (#{gen-ref 2939}#
                             #{src 3068}#
                             #{var 3069}#
                             (#{1-}# #{level 3070}#)
                             (cdr #{maps 3071}#)))
                         (lambda (#{outer-var 3078}# #{outer-maps 3079}#)
                           (begin
                             (let ((#{b 3083}#
                                     (assq #{outer-var 3078}#
                                           (car #{maps 3071}#))))
                               (if #{b 3083}#
                                 (values (cdr #{b 3083}#) #{maps 3071}#)
                                 (begin
                                   (let ((#{inner-var 3085}#
                                           (#{gen-var 485}# 'tmp)))
                                     (values
                                       #{inner-var 3085}#
                                       (cons (cons (cons #{outer-var 3078}#
                                                         #{inner-var 3085}#)
                                                   (car #{maps 3071}#))
                                             #{outer-maps 3079}#)))))))))))))
               (#{gen-mappend 2941}#
                 (lambda (#{e 3086}# #{map-env 3087}#)
                   (list 'apply
                         '(primitive append)
                         (#{gen-map 2943}# #{e 3086}# #{map-env 3087}#))))
               (#{gen-map 2943}#
                 (lambda (#{e 3091}# #{map-env 3092}#)
                   (begin
                     (let ((#{formals 3097}# (map cdr #{map-env 3092}#))
                           (#{actuals 3098}#
                             (map (lambda (#{x 3099}#)
                                    (list 'ref (car #{x 3099}#)))
                                  #{map-env 3092}#)))
                       (if (eq? (car #{e 3091}#) 'ref)
                         (car #{actuals 3098}#)
                         (if (and-map
                               (lambda (#{x 3106}#)
                                 (if (eq? (car #{x 3106}#) 'ref)
                                   (memq (car (cdr #{x 3106}#))
                                         #{formals 3097}#)
                                   #f))
                               (cdr #{e 3091}#))
                           (cons 'map
                                 (cons (list 'primitive (car #{e 3091}#))
                                       (map (begin
                                              (let ((#{r 3112}#
                                                      (map cons
                                                           #{formals 3097}#
                                                           #{actuals 3098}#)))
                                                (lambda (#{x 3113}#)
                                                  (cdr (assq (car (cdr #{x 3113}#))
                                                             #{r 3112}#)))))
                                            (cdr #{e 3091}#))))
                           (cons 'map
                                 (cons (list 'lambda
                                             #{formals 3097}#
                                             #{e 3091}#)
                                       #{actuals 3098}#))))))))
               (#{gen-cons 2945}#
                 (lambda (#{x 3117}# #{y 3118}#)
                   (begin
                     (let ((#{atom-key 3123}# (car #{y 3118}#)))
                       (if (eqv? #{atom-key 3123}# 'quote)
                         (if (eq? (car #{x 3117}#) 'quote)
                           (list 'quote
                                 (cons (car (cdr #{x 3117}#))
                                       (car (cdr #{y 3118}#))))
                           (if (eq? (car (cdr #{y 3118}#)) '())
                             (list 'list #{x 3117}#)
                             (list 'cons #{x 3117}# #{y 3118}#)))
                         (if (eqv? #{atom-key 3123}# 'list)
                           (cons 'list (cons #{x 3117}# (cdr #{y 3118}#)))
                           (list 'cons #{x 3117}# #{y 3118}#)))))))
               (#{gen-append 2947}#
                 (lambda (#{x 3132}# #{y 3133}#)
                   (if (equal? #{y 3133}# ''())
                     #{x 3132}#
                     (list 'append #{x 3132}# #{y 3133}#))))
               (#{gen-vector 2949}#
                 (lambda (#{x 3137}#)
                   (if (eq? (car #{x 3137}#) 'list)
                     (cons 'vector (cdr #{x 3137}#))
                     (if (eq? (car #{x 3137}#) 'quote)
                       (list 'quote
                             (list->vector (car (cdr #{x 3137}#))))
                       (list 'list->vector #{x 3137}#)))))
               (#{regen 2951}#
                 (lambda (#{x 3147}#)
                   (begin
                     (let ((#{atom-key 3151}# (car #{x 3147}#)))
                       (if (eqv? #{atom-key 3151}# 'ref)
                         (#{build-lexical-reference 309}#
                           'value
                           #f
                           (car (cdr #{x 3147}#))
                           (car (cdr #{x 3147}#)))
                         (if (eqv? #{atom-key 3151}# 'primitive)
                           (#{build-primref 327}# #f (car (cdr #{x 3147}#)))
                           (if (eqv? #{atom-key 3151}# 'quote)
                             (#{build-data 329}# #f (car (cdr #{x 3147}#)))
                             (if (eqv? #{atom-key 3151}# 'lambda)
                               (if (list? (car (cdr #{x 3147}#)))
                                 (#{build-simple-lambda 321}#
                                   #f
                                   (car (cdr #{x 3147}#))
                                   #f
                                   (car (cdr #{x 3147}#))
                                   '()
                                   (#{regen 2951}#
                                     (car (cdr (cdr #{x 3147}#)))))
                                 (error "how did we get here" #{x 3147}#))
                               (#{build-application 303}#
                                 #f
                                 (#{build-primref 327}# #f (car #{x 3147}#))
                                 (map #{regen 2951}#
                                      (cdr #{x 3147}#))))))))))))
              (begin
                (lambda (#{e 3163}#
                         #{r 3164}#
                         #{w 3165}#
                         #{s 3166}#
                         #{mod 3167}#)
                  (begin
                    (let ((#{e 3174}#
                            (#{source-wrap 445}#
                              #{e 3163}#
                              #{w 3165}#
                              #{s 3166}#
                              #{mod 3167}#)))
                      (let ((#{tmp 3175}# #{e 3174}#))
                        (let ((#{tmp 3176}#
                                ($sc-dispatch #{tmp 3175}# '(_ any))))
                          (if #{tmp 3176}#
                            (@apply
                              (lambda (#{x 3178}#)
                                (call-with-values
                                  (lambda ()
                                    (#{gen-syntax 2937}#
                                      #{e 3174}#
                                      #{x 3178}#
                                      #{r 3164}#
                                      '()
                                      #{ellipsis? 473}#
                                      #{mod 3167}#))
                                  (lambda (#{e 3179}# #{maps 3180}#)
                                    (#{regen 2951}# #{e 3179}#))))
                              #{tmp 3176}#)
                            (let ((#{_ 3184}# #{tmp 3175}#))
                              (syntax-violation
                                'syntax
                                "bad `syntax' form"
                                #{e 3174}#)))))))))))
          (#{global-extend 373}#
            'core
            'lambda
            (lambda (#{e 3185}#
                     #{r 3186}#
                     #{w 3187}#
                     #{s 3188}#
                     #{mod 3189}#)
              (let ((#{tmp 3195}# #{e 3185}#))
                (let ((#{tmp 3196}#
                        ($sc-dispatch
                          #{tmp 3195}#
                          '(_ any any . each-any))))
                  (if #{tmp 3196}#
                    (@apply
                      (lambda (#{args 3200}# #{e1 3201}# #{e2 3202}#)
                        (call-with-values
                          (lambda ()
                            (#{lambda-formals 475}# #{args 3200}#))
                          (lambda (#{req 3203}#
                                   #{opt 3204}#
                                   #{rest 3205}#
                                   #{kw 3206}#)
                            (letrec*
                              ((#{lp 3214}#
                                 (lambda (#{body 3215}# #{meta 3216}#)
                                   (let ((#{tmp 3218}# #{body 3215}#))
                                     (let ((#{tmp 3219}#
                                             ($sc-dispatch
                                               #{tmp 3218}#
                                               '(any any . each-any))))
                                       (if (if #{tmp 3219}#
                                             (@apply
                                               (lambda (#{docstring 3223}#
                                                        #{e1 3224}#
                                                        #{e2 3225}#)
                                                 (string?
                                                   (syntax->datum
                                                     #{docstring 3223}#)))
                                               #{tmp 3219}#)
                                             #f)
                                         (@apply
                                           (lambda (#{docstring 3229}#
                                                    #{e1 3230}#
                                                    #{e2 3231}#)
                                             (#{lp 3214}#
                                               (cons #{e1 3230}# #{e2 3231}#)
                                               (append
                                                 #{meta 3216}#
                                                 (list (cons 'documentation
                                                             (syntax->datum
                                                               #{docstring 3229}#))))))
                                           #{tmp 3219}#)
                                         (let ((#{tmp 3234}#
                                                 ($sc-dispatch
                                                   #{tmp 3218}#
                                                   '(#(vector
                                                       #(each (any . any)))
                                                     any
                                                     .
                                                     each-any))))
                                           (if #{tmp 3234}#
                                             (@apply
                                               (lambda (#{k 3239}#
                                                        #{v 3240}#
                                                        #{e1 3241}#
                                                        #{e2 3242}#)
                                                 (#{lp 3214}#
                                                   (cons #{e1 3241}#
                                                         #{e2 3242}#)
                                                   (append
                                                     #{meta 3216}#
                                                     (syntax->datum
                                                       (map cons
                                                            #{k 3239}#
                                                            #{v 3240}#)))))
                                               #{tmp 3234}#)
                                             (let ((#{_ 3247}# #{tmp 3218}#))
                                               (#{chi-simple-lambda 477}#
                                                 #{e 3185}#
                                                 #{r 3186}#
                                                 #{w 3187}#
                                                 #{s 3188}#
                                                 #{mod 3189}#
                                                 #{req 3203}#
                                                 #{rest 3205}#
                                                 #{meta 3216}#
                                                 #{body 3215}#))))))))))
                              (begin
                                (#{lp 3214}#
                                  (cons #{e1 3201}# #{e2 3202}#)
                                  '()))))))
                      #{tmp 3196}#)
                    (let ((#{_ 3249}# #{tmp 3195}#))
                      (syntax-violation
                        'lambda
                        "bad lambda"
                        #{e 3185}#)))))))
          (#{global-extend 373}#
            'core
            'lambda*
            (lambda (#{e 3250}#
                     #{r 3251}#
                     #{w 3252}#
                     #{s 3253}#
                     #{mod 3254}#)
              (let ((#{tmp 3260}# #{e 3250}#))
                (let ((#{tmp 3261}#
                        ($sc-dispatch
                          #{tmp 3260}#
                          '(_ any any . each-any))))
                  (if #{tmp 3261}#
                    (@apply
                      (lambda (#{args 3265}# #{e1 3266}# #{e2 3267}#)
                        (call-with-values
                          (lambda ()
                            (#{chi-lambda-case 481}#
                              #{e 3250}#
                              #{r 3251}#
                              #{w 3252}#
                              #{s 3253}#
                              #{mod 3254}#
                              #{lambda*-formals 479}#
                              (list (cons #{args 3265}#
                                          (cons #{e1 3266}# #{e2 3267}#)))))
                          (lambda (#{meta 3269}# #{lcase 3270}#)
                            (#{build-case-lambda 323}#
                              #{s 3253}#
                              #{meta 3269}#
                              #{lcase 3270}#))))
                      #{tmp 3261}#)
                    (let ((#{_ 3274}# #{tmp 3260}#))
                      (syntax-violation
                        'lambda
                        "bad lambda*"
                        #{e 3250}#)))))))
          (#{global-extend 373}#
            'core
            'case-lambda
            (lambda (#{e 3275}#
                     #{r 3276}#
                     #{w 3277}#
                     #{s 3278}#
                     #{mod 3279}#)
              (let ((#{tmp 3285}# #{e 3275}#))
                (let ((#{tmp 3286}#
                        ($sc-dispatch
                          #{tmp 3285}#
                          '(_ (any any . each-any)
                              .
                              #(each (any any . each-any))))))
                  (if #{tmp 3286}#
                    (@apply
                      (lambda (#{args 3293}#
                               #{e1 3294}#
                               #{e2 3295}#
                               #{args* 3296}#
                               #{e1* 3297}#
                               #{e2* 3298}#)
                        (call-with-values
                          (lambda ()
                            (#{chi-lambda-case 481}#
                              #{e 3275}#
                              #{r 3276}#
                              #{w 3277}#
                              #{s 3278}#
                              #{mod 3279}#
                              #{lambda-formals 475}#
                              (cons (cons #{args 3293}#
                                          (cons #{e1 3294}# #{e2 3295}#))
                                    (map (lambda (#{tmp 3302}#
                                                  #{tmp 3301}#
                                                  #{tmp 3300}#)
                                           (cons #{tmp 3300}#
                                                 (cons #{tmp 3301}#
                                                       #{tmp 3302}#)))
                                         #{e2* 3298}#
                                         #{e1* 3297}#
                                         #{args* 3296}#))))
                          (lambda (#{meta 3304}# #{lcase 3305}#)
                            (#{build-case-lambda 323}#
                              #{s 3278}#
                              #{meta 3304}#
                              #{lcase 3305}#))))
                      #{tmp 3286}#)
                    (let ((#{_ 3309}# #{tmp 3285}#))
                      (syntax-violation
                        'case-lambda
                        "bad case-lambda"
                        #{e 3275}#)))))))
          (#{global-extend 373}#
            'core
            'case-lambda*
            (lambda (#{e 3310}#
                     #{r 3311}#
                     #{w 3312}#
                     #{s 3313}#
                     #{mod 3314}#)
              (let ((#{tmp 3320}# #{e 3310}#))
                (let ((#{tmp 3321}#
                        ($sc-dispatch
                          #{tmp 3320}#
                          '(_ (any any . each-any)
                              .
                              #(each (any any . each-any))))))
                  (if #{tmp 3321}#
                    (@apply
                      (lambda (#{args 3328}#
                               #{e1 3329}#
                               #{e2 3330}#
                               #{args* 3331}#
                               #{e1* 3332}#
                               #{e2* 3333}#)
                        (call-with-values
                          (lambda ()
                            (#{chi-lambda-case 481}#
                              #{e 3310}#
                              #{r 3311}#
                              #{w 3312}#
                              #{s 3313}#
                              #{mod 3314}#
                              #{lambda*-formals 479}#
                              (cons (cons #{args 3328}#
                                          (cons #{e1 3329}# #{e2 3330}#))
                                    (map (lambda (#{tmp 3337}#
                                                  #{tmp 3336}#
                                                  #{tmp 3335}#)
                                           (cons #{tmp 3335}#
                                                 (cons #{tmp 3336}#
                                                       #{tmp 3337}#)))
                                         #{e2* 3333}#
                                         #{e1* 3332}#
                                         #{args* 3331}#))))
                          (lambda (#{meta 3339}# #{lcase 3340}#)
                            (#{build-case-lambda 323}#
                              #{s 3313}#
                              #{meta 3339}#
                              #{lcase 3340}#))))
                      #{tmp 3321}#)
                    (let ((#{_ 3344}# #{tmp 3320}#))
                      (syntax-violation
                        'case-lambda
                        "bad case-lambda*"
                        #{e 3310}#)))))))
          (#{global-extend 373}#
            'core
            'let
            (letrec*
              ((#{chi-let 3346}#
                 (lambda (#{e 3347}#
                          #{r 3348}#
                          #{w 3349}#
                          #{s 3350}#
                          #{mod 3351}#
                          #{constructor 3352}#
                          #{ids 3353}#
                          #{vals 3354}#
                          #{exps 3355}#)
                   (if (not (#{valid-bound-ids? 437}# #{ids 3353}#))
                     (syntax-violation
                       'let
                       "duplicate bound variable"
                       #{e 3347}#)
                     (begin
                       (let ((#{labels 3367}#
                               (#{gen-labels 392}# #{ids 3353}#))
                             (#{new-vars 3368}#
                               (map #{gen-var 485}# #{ids 3353}#)))
                         (begin
                           (let ((#{nw 3371}#
                                   (#{make-binding-wrap 421}#
                                     #{ids 3353}#
                                     #{labels 3367}#
                                     #{w 3349}#))
                                 (#{nr 3372}#
                                   (#{extend-var-env 367}#
                                     #{labels 3367}#
                                     #{new-vars 3368}#
                                     #{r 3348}#)))
                             (#{constructor 3352}#
                               #{s 3350}#
                               (map syntax->datum #{ids 3353}#)
                               #{new-vars 3368}#
                               (map (lambda (#{x 3373}#)
                                      (#{chi 457}#
                                        #{x 3373}#
                                        #{r 3348}#
                                        #{w 3349}#
                                        #{mod 3351}#))
                                    #{vals 3354}#)
                               (#{chi-body 465}#
                                 #{exps 3355}#
                                 (#{source-wrap 445}#
                                   #{e 3347}#
                                   #{nw 3371}#
                                   #{s 3350}#
                                   #{mod 3351}#)
                                 #{nr 3372}#
                                 #{nw 3371}#
                                 #{mod 3351}#))))))))))
              (begin
                (lambda (#{e 3375}#
                         #{r 3376}#
                         #{w 3377}#
                         #{s 3378}#
                         #{mod 3379}#)
                  (let ((#{tmp 3385}# #{e 3375}#))
                    (let ((#{tmp 3386}#
                            ($sc-dispatch
                              #{tmp 3385}#
                              '(_ #(each (any any)) any . each-any))))
                      (if (if #{tmp 3386}#
                            (@apply
                              (lambda (#{id 3391}#
                                       #{val 3392}#
                                       #{e1 3393}#
                                       #{e2 3394}#)
                                (and-map #{id? 377}# #{id 3391}#))
                              #{tmp 3386}#)
                            #f)
                        (@apply
                          (lambda (#{id 3400}#
                                   #{val 3401}#
                                   #{e1 3402}#
                                   #{e2 3403}#)
                            (#{chi-let 3346}#
                              #{e 3375}#
                              #{r 3376}#
                              #{w 3377}#
                              #{s 3378}#
                              #{mod 3379}#
                              #{build-let 333}#
                              #{id 3400}#
                              #{val 3401}#
                              (cons #{e1 3402}# #{e2 3403}#)))
                          #{tmp 3386}#)
                        (let ((#{tmp 3407}#
                                ($sc-dispatch
                                  #{tmp 3385}#
                                  '(_ any #(each (any any)) any . each-any))))
                          (if (if #{tmp 3407}#
                                (@apply
                                  (lambda (#{f 3413}#
                                           #{id 3414}#
                                           #{val 3415}#
                                           #{e1 3416}#
                                           #{e2 3417}#)
                                    (if (#{id? 377}# #{f 3413}#)
                                      (and-map #{id? 377}# #{id 3414}#)
                                      #f))
                                  #{tmp 3407}#)
                                #f)
                            (@apply
                              (lambda (#{f 3426}#
                                       #{id 3427}#
                                       #{val 3428}#
                                       #{e1 3429}#
                                       #{e2 3430}#)
                                (#{chi-let 3346}#
                                  #{e 3375}#
                                  #{r 3376}#
                                  #{w 3377}#
                                  #{s 3378}#
                                  #{mod 3379}#
                                  #{build-named-let 335}#
                                  (cons #{f 3426}# #{id 3427}#)
                                  #{val 3428}#
                                  (cons #{e1 3429}# #{e2 3430}#)))
                              #{tmp 3407}#)
                            (let ((#{_ 3435}# #{tmp 3385}#))
                              (syntax-violation
                                'let
                                "bad let"
                                (#{source-wrap 445}#
                                  #{e 3375}#
                                  #{w 3377}#
                                  #{s 3378}#
                                  #{mod 3379}#))))))))))))
          (#{global-extend 373}#
            'core
            'letrec
            (lambda (#{e 3436}#
                     #{r 3437}#
                     #{w 3438}#
                     #{s 3439}#
                     #{mod 3440}#)
              (let ((#{tmp 3446}# #{e 3436}#))
                (let ((#{tmp 3447}#
                        ($sc-dispatch
                          #{tmp 3446}#
                          '(_ #(each (any any)) any . each-any))))
                  (if (if #{tmp 3447}#
                        (@apply
                          (lambda (#{id 3452}#
                                   #{val 3453}#
                                   #{e1 3454}#
                                   #{e2 3455}#)
                            (and-map #{id? 377}# #{id 3452}#))
                          #{tmp 3447}#)
                        #f)
                    (@apply
                      (lambda (#{id 3461}#
                               #{val 3462}#
                               #{e1 3463}#
                               #{e2 3464}#)
                        (begin
                          (let ((#{ids 3466}# #{id 3461}#))
                            (if (not (#{valid-bound-ids? 437}# #{ids 3466}#))
                              (syntax-violation
                                'letrec
                                "duplicate bound variable"
                                #{e 3436}#)
                              (begin
                                (let ((#{labels 3470}#
                                        (#{gen-labels 392}# #{ids 3466}#))
                                      (#{new-vars 3471}#
                                        (map #{gen-var 485}# #{ids 3466}#)))
                                  (begin
                                    (let ((#{w 3474}#
                                            (#{make-binding-wrap 421}#
                                              #{ids 3466}#
                                              #{labels 3470}#
                                              #{w 3438}#))
                                          (#{r 3475}#
                                            (#{extend-var-env 367}#
                                              #{labels 3470}#
                                              #{new-vars 3471}#
                                              #{r 3437}#)))
                                      (#{build-letrec 337}#
                                        #{s 3439}#
                                        #f
                                        (map syntax->datum #{ids 3466}#)
                                        #{new-vars 3471}#
                                        (map (lambda (#{x 3476}#)
                                               (#{chi 457}#
                                                 #{x 3476}#
                                                 #{r 3475}#
                                                 #{w 3474}#
                                                 #{mod 3440}#))
                                             #{val 3462}#)
                                        (#{chi-body 465}#
                                          (cons #{e1 3463}# #{e2 3464}#)
                                          (#{source-wrap 445}#
                                            #{e 3436}#
                                            #{w 3474}#
                                            #{s 3439}#
                                            #{mod 3440}#)
                                          #{r 3475}#
                                          #{w 3474}#
                                          #{mod 3440}#))))))))))
                      #{tmp 3447}#)
                    (let ((#{_ 3481}# #{tmp 3446}#))
                      (syntax-violation
                        'letrec
                        "bad letrec"
                        (#{source-wrap 445}#
                          #{e 3436}#
                          #{w 3438}#
                          #{s 3439}#
                          #{mod 3440}#))))))))
          (#{global-extend 373}#
            'core
            'letrec*
            (lambda (#{e 3482}#
                     #{r 3483}#
                     #{w 3484}#
                     #{s 3485}#
                     #{mod 3486}#)
              (let ((#{tmp 3492}# #{e 3482}#))
                (let ((#{tmp 3493}#
                        ($sc-dispatch
                          #{tmp 3492}#
                          '(_ #(each (any any)) any . each-any))))
                  (if (if #{tmp 3493}#
                        (@apply
                          (lambda (#{id 3498}#
                                   #{val 3499}#
                                   #{e1 3500}#
                                   #{e2 3501}#)
                            (and-map #{id? 377}# #{id 3498}#))
                          #{tmp 3493}#)
                        #f)
                    (@apply
                      (lambda (#{id 3507}#
                               #{val 3508}#
                               #{e1 3509}#
                               #{e2 3510}#)
                        (begin
                          (let ((#{ids 3512}# #{id 3507}#))
                            (if (not (#{valid-bound-ids? 437}# #{ids 3512}#))
                              (syntax-violation
                                'letrec*
                                "duplicate bound variable"
                                #{e 3482}#)
                              (begin
                                (let ((#{labels 3516}#
                                        (#{gen-labels 392}# #{ids 3512}#))
                                      (#{new-vars 3517}#
                                        (map #{gen-var 485}# #{ids 3512}#)))
                                  (begin
                                    (let ((#{w 3520}#
                                            (#{make-binding-wrap 421}#
                                              #{ids 3512}#
                                              #{labels 3516}#
                                              #{w 3484}#))
                                          (#{r 3521}#
                                            (#{extend-var-env 367}#
                                              #{labels 3516}#
                                              #{new-vars 3517}#
                                              #{r 3483}#)))
                                      (#{build-letrec 337}#
                                        #{s 3485}#
                                        #t
                                        (map syntax->datum #{ids 3512}#)
                                        #{new-vars 3517}#
                                        (map (lambda (#{x 3522}#)
                                               (#{chi 457}#
                                                 #{x 3522}#
                                                 #{r 3521}#
                                                 #{w 3520}#
                                                 #{mod 3486}#))
                                             #{val 3508}#)
                                        (#{chi-body 465}#
                                          (cons #{e1 3509}# #{e2 3510}#)
                                          (#{source-wrap 445}#
                                            #{e 3482}#
                                            #{w 3520}#
                                            #{s 3485}#
                                            #{mod 3486}#)
                                          #{r 3521}#
                                          #{w 3520}#
                                          #{mod 3486}#))))))))))
                      #{tmp 3493}#)
                    (let ((#{_ 3527}# #{tmp 3492}#))
                      (syntax-violation
                        'letrec*
                        "bad letrec*"
                        (#{source-wrap 445}#
                          #{e 3482}#
                          #{w 3484}#
                          #{s 3485}#
                          #{mod 3486}#))))))))
          (#{global-extend 373}#
            'core
            'set!
            (lambda (#{e 3528}#
                     #{r 3529}#
                     #{w 3530}#
                     #{s 3531}#
                     #{mod 3532}#)
              (let ((#{tmp 3538}# #{e 3528}#))
                (let ((#{tmp 3539}#
                        ($sc-dispatch #{tmp 3538}# '(_ any any))))
                  (if (if #{tmp 3539}#
                        (@apply
                          (lambda (#{id 3542}# #{val 3543}#)
                            (#{id? 377}# #{id 3542}#))
                          #{tmp 3539}#)
                        #f)
                    (@apply
                      (lambda (#{id 3546}# #{val 3547}#)
                        (begin
                          (let ((#{n 3550}#
                                  (#{id-var-name 431}# #{id 3546}# #{w 3530}#))
                                (#{id-mod 3551}#
                                  (if (#{syntax-object? 343}# #{id 3546}#)
                                    (#{syntax-object-module 349}# #{id 3546}#)
                                    #{mod 3532}#)))
                            (begin
                              (let ((#{b 3553}#
                                      (#{lookup 371}#
                                        #{n 3550}#
                                        #{r 3529}#
                                        #{id-mod 3551}#)))
                                (begin
                                  (let ((#{atom-key 3556}# (car #{b 3553}#)))
                                    (if (eqv? #{atom-key 3556}# 'lexical)
                                      (#{build-lexical-assignment 311}#
                                        #{s 3531}#
                                        (syntax->datum #{id 3546}#)
                                        (cdr #{b 3553}#)
                                        (#{chi 457}#
                                          #{val 3547}#
                                          #{r 3529}#
                                          #{w 3530}#
                                          #{mod 3532}#))
                                      (if (eqv? #{atom-key 3556}# 'global)
                                        (#{build-global-assignment 317}#
                                          #{s 3531}#
                                          #{n 3550}#
                                          (#{chi 457}#
                                            #{val 3547}#
                                            #{r 3529}#
                                            #{w 3530}#
                                            #{mod 3532}#)
                                          #{id-mod 3551}#)
                                        (if (eqv? #{atom-key 3556}# 'macro)
                                          (begin
                                            (let ((#{p 3563}#
                                                    (cdr #{b 3553}#)))
                                              (if (procedure-property
                                                    #{p 3563}#
                                                    'variable-transformer)
                                                (#{chi 457}#
                                                  (#{chi-macro 463}#
                                                    #{p 3563}#
                                                    #{e 3528}#
                                                    #{r 3529}#
                                                    #{w 3530}#
                                                    #{s 3531}#
                                                    #f
                                                    #{mod 3532}#)
                                                  #{r 3529}#
                                                  '(())
                                                  #{mod 3532}#)
                                                (syntax-violation
                                                  'set!
                                                  "not a variable transformer"
                                                  (#{wrap 443}#
                                                    #{e 3528}#
                                                    #{w 3530}#
                                                    #{mod 3532}#)
                                                  (#{wrap 443}#
                                                    #{id 3546}#
                                                    #{w 3530}#
                                                    #{id-mod 3551}#)))))
                                          (if (eqv? #{atom-key 3556}#
                                                    'displaced-lexical)
                                            (syntax-violation
                                              'set!
                                              "identifier out of context"
                                              (#{wrap 443}#
                                                #{id 3546}#
                                                #{w 3530}#
                                                #{mod 3532}#))
                                            (syntax-violation
                                              'set!
                                              "bad set!"
                                              (#{source-wrap 445}#
                                                #{e 3528}#
                                                #{w 3530}#
                                                #{s 3531}#
                                                #{mod 3532}#)))))))))))))
                      #{tmp 3539}#)
                    (let ((#{tmp 3568}#
                            ($sc-dispatch
                              #{tmp 3538}#
                              '(_ (any . each-any) any))))
                      (if #{tmp 3568}#
                        (@apply
                          (lambda (#{head 3572}# #{tail 3573}# #{val 3574}#)
                            (call-with-values
                              (lambda ()
                                (#{syntax-type 455}#
                                  #{head 3572}#
                                  #{r 3529}#
                                  '(())
                                  #f
                                  #f
                                  #{mod 3532}#
                                  #t))
                              (lambda (#{type 3577}#
                                       #{value 3578}#
                                       #{ee 3579}#
                                       #{ww 3580}#
                                       #{ss 3581}#
                                       #{modmod 3582}#)
                                (if (eqv? #{type 3577}# 'module-ref)
                                  (begin
                                    (let ((#{val 3591}#
                                            (#{chi 457}#
                                              #{val 3574}#
                                              #{r 3529}#
                                              #{w 3530}#
                                              #{mod 3532}#)))
                                      (call-with-values
                                        (lambda ()
                                          (#{value 3578}#
                                            (cons #{head 3572}# #{tail 3573}#)
                                            #{r 3529}#
                                            #{w 3530}#))
                                        (lambda (#{e 3593}#
                                                 #{r 3594}#
                                                 #{w 3595}#
                                                 #{s* 3596}#
                                                 #{mod 3597}#)
                                          (let ((#{tmp 3603}# #{e 3593}#))
                                            (let ((#{tmp 3604}#
                                                    (list #{tmp 3603}#)))
                                              (if (if #{tmp 3604}#
                                                    (@apply
                                                      (lambda (#{e 3606}#)
                                                        (#{id? 377}#
                                                          #{e 3606}#))
                                                      #{tmp 3604}#)
                                                    #f)
                                                (@apply
                                                  (lambda (#{e 3608}#)
                                                    (#{build-global-assignment 317}#
                                                      #{s 3531}#
                                                      (syntax->datum
                                                        #{e 3608}#)
                                                      #{val 3591}#
                                                      #{mod 3597}#))
                                                  #{tmp 3604}#)
                                                (syntax-violation
                                                  #f
                                                  "source expression failed to match any pattern"
                                                  #{tmp 3603}#))))))))
                                  (#{build-application 303}#
                                    #{s 3531}#
                                    (#{chi 457}#
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
                                                  #("i3583"
                                                    "i3584"
                                                    "i3585"
                                                    "i3586"
                                                    "i3587"
                                                    "i3588"))
                                                #(ribcage
                                                  #(head tail val)
                                                  #((top) (top) (top))
                                                  #("i3569" "i3570" "i3571"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(e r w s mod)
                                                  #((top)
                                                    (top)
                                                    (top)
                                                    (top)
                                                    (top))
                                                  #("i3533"
                                                    "i3534"
                                                    "i3535"
                                                    "i3536"
                                                    "i3537"))
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
                                                    chi-application
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
                                                    build-application
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
                                                    make-sequence
                                                    make-application
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
                                                   (top))
                                                  ("i486"
                                                   "i484"
                                                   "i482"
                                                   "i480"
                                                   "i478"
                                                   "i476"
                                                   "i474"
                                                   "i472"
                                                   "i470"
                                                   "i468"
                                                   "i466"
                                                   "i464"
                                                   "i462"
                                                   "i460"
                                                   "i458"
                                                   "i456"
                                                   "i454"
                                                   "i452"
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
                                                   "i417"
                                                   "i416"
                                                   "i414"
                                                   "i413"
                                                   "i412"
                                                   "i411"
                                                   "i410"
                                                   "i408"
                                                   "i406"
                                                   "i404"
                                                   "i402"
                                                   "i400"
                                                   "i398"
                                                   "i396"
                                                   "i394"
                                                   "i391"
                                                   "i389"
                                                   "i388"
                                                   "i387"
                                                   "i386"
                                                   "i385"
                                                   "i384"
                                                   "i383"
                                                   "i382"
                                                   "i381"
                                                   "i379"
                                                   "i378"
                                                   "i376"
                                                   "i374"
                                                   "i372"
                                                   "i370"
                                                   "i368"
                                                   "i366"
                                                   "i364"
                                                   "i363"
                                                   "i362"
                                                   "i361"
                                                   "i360"
                                                   "i359"
                                                   "i357"
                                                   "i356"
                                                   "i354"
                                                   "i352"
                                                   "i350"
                                                   "i348"
                                                   "i346"
                                                   "i344"
                                                   "i342"
                                                   "i340"
                                                   "i338"
                                                   "i336"
                                                   "i334"
                                                   "i332"
                                                   "i330"
                                                   "i328"
                                                   "i326"
                                                   "i324"
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
                                                   "i291"
                                                   "i289"
                                                   "i287"
                                                   "i286"
                                                   "i285"
                                                   "i284"
                                                   "i283"
                                                   "i281"
                                                   "i279"
                                                   "i277"
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
                                                   "i254"
                                                   "i252"
                                                   "i250"
                                                   "i248"
                                                   "i246"
                                                   "i244"
                                                   "i242"
                                                   "i240"))
                                                #(ribcage
                                                  (define-structure
                                                    define-expansion-accessors
                                                    define-expansion-constructors
                                                    and-map*)
                                                  ((top) (top) (top) (top))
                                                  ("i42" "i41" "i40" "i38")))
                                               (hygiene guile))
                                            #{head 3572}#)
                                      #{r 3529}#
                                      #{w 3530}#
                                      #{mod 3532}#)
                                    (map (lambda (#{e 3610}#)
                                           (#{chi 457}#
                                             #{e 3610}#
                                             #{r 3529}#
                                             #{w 3530}#
                                             #{mod 3532}#))
                                         (append
                                           #{tail 3573}#
                                           (list #{val 3574}#))))))))
                          #{tmp 3568}#)
                        (let ((#{_ 3614}# #{tmp 3538}#))
                          (syntax-violation
                            'set!
                            "bad set!"
                            (#{source-wrap 445}#
                              #{e 3528}#
                              #{w 3530}#
                              #{s 3531}#
                              #{mod 3532}#))))))))))
          (#{global-extend 373}#
            'module-ref
            '@
            (lambda (#{e 3615}# #{r 3616}# #{w 3617}#)
              (let ((#{tmp 3621}# #{e 3615}#))
                (let ((#{tmp 3622}#
                        ($sc-dispatch #{tmp 3621}# '(_ each-any any))))
                  (if (if #{tmp 3622}#
                        (@apply
                          (lambda (#{mod 3625}# #{id 3626}#)
                            (if (and-map #{id? 377}# #{mod 3625}#)
                              (#{id? 377}# #{id 3626}#)
                              #f))
                          #{tmp 3622}#)
                        #f)
                    (@apply
                      (lambda (#{mod 3632}# #{id 3633}#)
                        (values
                          (syntax->datum #{id 3633}#)
                          #{r 3616}#
                          #{w 3617}#
                          #f
                          (syntax->datum
                            (cons '#(syntax-object
                                     public
                                     ((top)
                                      #(ribcage
                                        #(mod id)
                                        #((top) (top))
                                        #("i3630" "i3631"))
                                      #(ribcage () () ())
                                      #(ribcage
                                        #(e r w)
                                        #((top) (top) (top))
                                        #("i3618" "i3619" "i3620"))
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
                                          chi-application
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
                                          build-application
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
                                          make-sequence
                                          make-application
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
                                         (top))
                                        ("i486"
                                         "i484"
                                         "i482"
                                         "i480"
                                         "i478"
                                         "i476"
                                         "i474"
                                         "i472"
                                         "i470"
                                         "i468"
                                         "i466"
                                         "i464"
                                         "i462"
                                         "i460"
                                         "i458"
                                         "i456"
                                         "i454"
                                         "i452"
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
                                         "i417"
                                         "i416"
                                         "i414"
                                         "i413"
                                         "i412"
                                         "i411"
                                         "i410"
                                         "i408"
                                         "i406"
                                         "i404"
                                         "i402"
                                         "i400"
                                         "i398"
                                         "i396"
                                         "i394"
                                         "i391"
                                         "i389"
                                         "i388"
                                         "i387"
                                         "i386"
                                         "i385"
                                         "i384"
                                         "i383"
                                         "i382"
                                         "i381"
                                         "i379"
                                         "i378"
                                         "i376"
                                         "i374"
                                         "i372"
                                         "i370"
                                         "i368"
                                         "i366"
                                         "i364"
                                         "i363"
                                         "i362"
                                         "i361"
                                         "i360"
                                         "i359"
                                         "i357"
                                         "i356"
                                         "i354"
                                         "i352"
                                         "i350"
                                         "i348"
                                         "i346"
                                         "i344"
                                         "i342"
                                         "i340"
                                         "i338"
                                         "i336"
                                         "i334"
                                         "i332"
                                         "i330"
                                         "i328"
                                         "i326"
                                         "i324"
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
                                         "i291"
                                         "i289"
                                         "i287"
                                         "i286"
                                         "i285"
                                         "i284"
                                         "i283"
                                         "i281"
                                         "i279"
                                         "i277"
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
                                         "i254"
                                         "i252"
                                         "i250"
                                         "i248"
                                         "i246"
                                         "i244"
                                         "i242"
                                         "i240"))
                                      #(ribcage
                                        (define-structure
                                          define-expansion-accessors
                                          define-expansion-constructors
                                          and-map*)
                                        ((top) (top) (top) (top))
                                        ("i42" "i41" "i40" "i38")))
                                     (hygiene guile))
                                  #{mod 3632}#))))
                      #{tmp 3622}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp 3621}#))))))
          (#{global-extend 373}#
            'module-ref
            '@@
            (lambda (#{e 3635}# #{r 3636}# #{w 3637}#)
              (letrec*
                ((#{remodulate 3642}#
                   (lambda (#{x 3643}# #{mod 3644}#)
                     (if (pair? #{x 3643}#)
                       (cons (#{remodulate 3642}#
                               (car #{x 3643}#)
                               #{mod 3644}#)
                             (#{remodulate 3642}#
                               (cdr #{x 3643}#)
                               #{mod 3644}#))
                       (if (#{syntax-object? 343}# #{x 3643}#)
                         (#{make-syntax-object 341}#
                           (#{remodulate 3642}#
                             (#{syntax-object-expression 345}# #{x 3643}#)
                             #{mod 3644}#)
                           (#{syntax-object-wrap 347}# #{x 3643}#)
                           #{mod 3644}#)
                         (if (vector? #{x 3643}#)
                           (begin
                             (let ((#{n 3655}# (vector-length #{x 3643}#)))
                               (begin
                                 (let ((#{v 3657}# (make-vector #{n 3655}#)))
                                   (letrec*
                                     ((#{loop 3660}#
                                        (lambda (#{i 3661}#)
                                          (if (= #{i 3661}# #{n 3655}#)
                                            (begin (if #f #f) #{v 3657}#)
                                            (begin
                                              (vector-set!
                                                #{v 3657}#
                                                #{i 3661}#
                                                (#{remodulate 3642}#
                                                  (vector-ref
                                                    #{x 3643}#
                                                    #{i 3661}#)
                                                  #{mod 3644}#))
                                              (#{loop 3660}#
                                                (#{1+}# #{i 3661}#)))))))
                                     (begin (#{loop 3660}# 0)))))))
                           #{x 3643}#))))))
                (begin
                  (let ((#{tmp 3667}# #{e 3635}#))
                    (let ((#{tmp 3668}#
                            ($sc-dispatch #{tmp 3667}# '(_ each-any any))))
                      (if (if #{tmp 3668}#
                            (@apply
                              (lambda (#{mod 3671}# #{exp 3672}#)
                                (and-map #{id? 377}# #{mod 3671}#))
                              #{tmp 3668}#)
                            #f)
                        (@apply
                          (lambda (#{mod 3676}# #{exp 3677}#)
                            (begin
                              (let ((#{mod 3679}#
                                      (syntax->datum
                                        (cons '#(syntax-object
                                                 private
                                                 ((top)
                                                  #(ribcage
                                                    #(mod exp)
                                                    #((top) (top))
                                                    #("i3674" "i3675"))
                                                  #(ribcage
                                                    (remodulate)
                                                    ((top))
                                                    ("i3641"))
                                                  #(ribcage
                                                    #(e r w)
                                                    #((top) (top) (top))
                                                    #("i3638" "i3639" "i3640"))
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
                                                      chi-application
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
                                                      build-application
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
                                                      make-sequence
                                                      make-application
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
                                                     (top))
                                                    ("i486"
                                                     "i484"
                                                     "i482"
                                                     "i480"
                                                     "i478"
                                                     "i476"
                                                     "i474"
                                                     "i472"
                                                     "i470"
                                                     "i468"
                                                     "i466"
                                                     "i464"
                                                     "i462"
                                                     "i460"
                                                     "i458"
                                                     "i456"
                                                     "i454"
                                                     "i452"
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
                                                     "i417"
                                                     "i416"
                                                     "i414"
                                                     "i413"
                                                     "i412"
                                                     "i411"
                                                     "i410"
                                                     "i408"
                                                     "i406"
                                                     "i404"
                                                     "i402"
                                                     "i400"
                                                     "i398"
                                                     "i396"
                                                     "i394"
                                                     "i391"
                                                     "i389"
                                                     "i388"
                                                     "i387"
                                                     "i386"
                                                     "i385"
                                                     "i384"
                                                     "i383"
                                                     "i382"
                                                     "i381"
                                                     "i379"
                                                     "i378"
                                                     "i376"
                                                     "i374"
                                                     "i372"
                                                     "i370"
                                                     "i368"
                                                     "i366"
                                                     "i364"
                                                     "i363"
                                                     "i362"
                                                     "i361"
                                                     "i360"
                                                     "i359"
                                                     "i357"
                                                     "i356"
                                                     "i354"
                                                     "i352"
                                                     "i350"
                                                     "i348"
                                                     "i346"
                                                     "i344"
                                                     "i342"
                                                     "i340"
                                                     "i338"
                                                     "i336"
                                                     "i334"
                                                     "i332"
                                                     "i330"
                                                     "i328"
                                                     "i326"
                                                     "i324"
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
                                                     "i291"
                                                     "i289"
                                                     "i287"
                                                     "i286"
                                                     "i285"
                                                     "i284"
                                                     "i283"
                                                     "i281"
                                                     "i279"
                                                     "i277"
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
                                                     "i254"
                                                     "i252"
                                                     "i250"
                                                     "i248"
                                                     "i246"
                                                     "i244"
                                                     "i242"
                                                     "i240"))
                                                  #(ribcage
                                                    (define-structure
                                                      define-expansion-accessors
                                                      define-expansion-constructors
                                                      and-map*)
                                                    ((top) (top) (top) (top))
                                                    ("i42" "i41" "i40" "i38")))
                                                 (hygiene guile))
                                              #{mod 3676}#))))
                                (values
                                  (#{remodulate 3642}#
                                    #{exp 3677}#
                                    #{mod 3679}#)
                                  #{r 3636}#
                                  #{w 3637}#
                                  (#{source-annotation 358}# #{exp 3677}#)
                                  #{mod 3679}#))))
                          #{tmp 3668}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp 3667}#))))))))
          (#{global-extend 373}#
            'core
            'if
            (lambda (#{e 3681}#
                     #{r 3682}#
                     #{w 3683}#
                     #{s 3684}#
                     #{mod 3685}#)
              (let ((#{tmp 3691}# #{e 3681}#))
                (let ((#{tmp 3692}#
                        ($sc-dispatch #{tmp 3691}# '(_ any any))))
                  (if #{tmp 3692}#
                    (@apply
                      (lambda (#{test 3695}# #{then 3696}#)
                        (#{build-conditional 305}#
                          #{s 3684}#
                          (#{chi 457}#
                            #{test 3695}#
                            #{r 3682}#
                            #{w 3683}#
                            #{mod 3685}#)
                          (#{chi 457}#
                            #{then 3696}#
                            #{r 3682}#
                            #{w 3683}#
                            #{mod 3685}#)
                          (#{build-void 301}# #f)))
                      #{tmp 3692}#)
                    (let ((#{tmp 3698}#
                            ($sc-dispatch #{tmp 3691}# '(_ any any any))))
                      (if #{tmp 3698}#
                        (@apply
                          (lambda (#{test 3702}# #{then 3703}# #{else 3704}#)
                            (#{build-conditional 305}#
                              #{s 3684}#
                              (#{chi 457}#
                                #{test 3702}#
                                #{r 3682}#
                                #{w 3683}#
                                #{mod 3685}#)
                              (#{chi 457}#
                                #{then 3703}#
                                #{r 3682}#
                                #{w 3683}#
                                #{mod 3685}#)
                              (#{chi 457}#
                                #{else 3704}#
                                #{r 3682}#
                                #{w 3683}#
                                #{mod 3685}#)))
                          #{tmp 3698}#)
                        (syntax-violation
                          #f
                          "source expression failed to match any pattern"
                          #{tmp 3691}#))))))))
          (#{global-extend 373}#
            'core
            'with-fluids
            (lambda (#{e 3705}#
                     #{r 3706}#
                     #{w 3707}#
                     #{s 3708}#
                     #{mod 3709}#)
              (let ((#{tmp 3715}# #{e 3705}#))
                (let ((#{tmp 3716}#
                        ($sc-dispatch
                          #{tmp 3715}#
                          '(_ #(each (any any)) any . each-any))))
                  (if #{tmp 3716}#
                    (@apply
                      (lambda (#{fluid 3721}#
                               #{val 3722}#
                               #{b 3723}#
                               #{b* 3724}#)
                        (#{build-dynlet 307}#
                          #{s 3708}#
                          (map (lambda (#{x 3725}#)
                                 (#{chi 457}#
                                   #{x 3725}#
                                   #{r 3706}#
                                   #{w 3707}#
                                   #{mod 3709}#))
                               #{fluid 3721}#)
                          (map (lambda (#{x 3728}#)
                                 (#{chi 457}#
                                   #{x 3728}#
                                   #{r 3706}#
                                   #{w 3707}#
                                   #{mod 3709}#))
                               #{val 3722}#)
                          (#{chi-body 465}#
                            (cons #{b 3723}# #{b* 3724}#)
                            (#{source-wrap 445}#
                              #{e 3705}#
                              #{w 3707}#
                              #{s 3708}#
                              #{mod 3709}#)
                            #{r 3706}#
                            #{w 3707}#
                            #{mod 3709}#)))
                      #{tmp 3716}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp 3715}#))))))
          (#{global-extend 373}# 'begin 'begin '())
          (#{global-extend 373}# 'define 'define '())
          (#{global-extend 373}#
            'define-syntax
            'define-syntax
            '())
          (#{global-extend 373}# 'eval-when 'eval-when '())
          (#{global-extend 373}#
            'core
            'syntax-case
            (letrec*
              ((#{convert-pattern 3733}#
                 (lambda (#{pattern 3740}# #{keys 3741}#)
                   (letrec*
                     ((#{cvt* 3745}#
                        (lambda (#{p* 3748}# #{n 3749}# #{ids 3750}#)
                          (if (null? #{p* 3748}#)
                            (values '() #{ids 3750}#)
                            (call-with-values
                              (lambda ()
                                (#{cvt* 3745}#
                                  (cdr #{p* 3748}#)
                                  #{n 3749}#
                                  #{ids 3750}#))
                              (lambda (#{y 3754}# #{ids 3755}#)
                                (call-with-values
                                  (lambda ()
                                    (#{cvt 3747}#
                                      (car #{p* 3748}#)
                                      #{n 3749}#
                                      #{ids 3755}#))
                                  (lambda (#{x 3758}# #{ids 3759}#)
                                    (values
                                      (cons #{x 3758}# #{y 3754}#)
                                      #{ids 3759}#))))))))
                      (#{cvt 3747}#
                        (lambda (#{p 3762}# #{n 3763}# #{ids 3764}#)
                          (if (#{id? 377}# #{p 3762}#)
                            (if (#{bound-id-member? 441}#
                                  #{p 3762}#
                                  #{keys 3741}#)
                              (values
                                (vector 'free-id #{p 3762}#)
                                #{ids 3764}#)
                              (if (#{free-id=? 433}#
                                    #{p 3762}#
                                    '#(syntax-object
                                       _
                                       ((top)
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p n ids)
                                          #((top) (top) (top))
                                          #("i3765" "i3766" "i3767"))
                                        #(ribcage
                                          (cvt cvt*)
                                          ((top) (top))
                                          ("i3746" "i3744"))
                                        #(ribcage
                                          #(pattern keys)
                                          #((top) (top))
                                          #("i3742" "i3743"))
                                        #(ribcage
                                          (gen-syntax-case
                                            gen-clause
                                            build-dispatch-call
                                            convert-pattern)
                                          ((top) (top) (top) (top))
                                          ("i3738" "i3736" "i3734" "i3732"))
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
                                            chi-application
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
                                            build-application
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
                                            make-sequence
                                            make-application
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
                                           (top))
                                          ("i486"
                                           "i484"
                                           "i482"
                                           "i480"
                                           "i478"
                                           "i476"
                                           "i474"
                                           "i472"
                                           "i470"
                                           "i468"
                                           "i466"
                                           "i464"
                                           "i462"
                                           "i460"
                                           "i458"
                                           "i456"
                                           "i454"
                                           "i452"
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
                                           "i417"
                                           "i416"
                                           "i414"
                                           "i413"
                                           "i412"
                                           "i411"
                                           "i410"
                                           "i408"
                                           "i406"
                                           "i404"
                                           "i402"
                                           "i400"
                                           "i398"
                                           "i396"
                                           "i394"
                                           "i391"
                                           "i389"
                                           "i388"
                                           "i387"
                                           "i386"
                                           "i385"
                                           "i384"
                                           "i383"
                                           "i382"
                                           "i381"
                                           "i379"
                                           "i378"
                                           "i376"
                                           "i374"
                                           "i372"
                                           "i370"
                                           "i368"
                                           "i366"
                                           "i364"
                                           "i363"
                                           "i362"
                                           "i361"
                                           "i360"
                                           "i359"
                                           "i357"
                                           "i356"
                                           "i354"
                                           "i352"
                                           "i350"
                                           "i348"
                                           "i346"
                                           "i344"
                                           "i342"
                                           "i340"
                                           "i338"
                                           "i336"
                                           "i334"
                                           "i332"
                                           "i330"
                                           "i328"
                                           "i326"
                                           "i324"
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
                                           "i291"
                                           "i289"
                                           "i287"
                                           "i286"
                                           "i285"
                                           "i284"
                                           "i283"
                                           "i281"
                                           "i279"
                                           "i277"
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
                                           "i254"
                                           "i252"
                                           "i250"
                                           "i248"
                                           "i246"
                                           "i244"
                                           "i242"
                                           "i240"))
                                        #(ribcage
                                          (define-structure
                                            define-expansion-accessors
                                            define-expansion-constructors
                                            and-map*)
                                          ((top) (top) (top) (top))
                                          ("i42" "i41" "i40" "i38")))
                                       (hygiene guile)))
                                (values '_ #{ids 3764}#)
                                (values
                                  'any
                                  (cons (cons #{p 3762}# #{n 3763}#)
                                        #{ids 3764}#))))
                            (let ((#{tmp 3773}# #{p 3762}#))
                              (let ((#{tmp 3774}#
                                      ($sc-dispatch #{tmp 3773}# '(any any))))
                                (if (if #{tmp 3774}#
                                      (@apply
                                        (lambda (#{x 3777}# #{dots 3778}#)
                                          (#{ellipsis? 473}# #{dots 3778}#))
                                        #{tmp 3774}#)
                                      #f)
                                  (@apply
                                    (lambda (#{x 3781}# #{dots 3782}#)
                                      (call-with-values
                                        (lambda ()
                                          (#{cvt 3747}#
                                            #{x 3781}#
                                            (#{1+}# #{n 3763}#)
                                            #{ids 3764}#))
                                        (lambda (#{p 3784}# #{ids 3785}#)
                                          (values
                                            (if (eq? #{p 3784}# 'any)
                                              'each-any
                                              (vector 'each #{p 3784}#))
                                            #{ids 3785}#))))
                                    #{tmp 3774}#)
                                  (let ((#{tmp 3788}#
                                          ($sc-dispatch
                                            #{tmp 3773}#
                                            '(any any . each-any))))
                                    (if (if #{tmp 3788}#
                                          (@apply
                                            (lambda (#{x 3792}#
                                                     #{dots 3793}#
                                                     #{ys 3794}#)
                                              (#{ellipsis? 473}#
                                                #{dots 3793}#))
                                            #{tmp 3788}#)
                                          #f)
                                      (@apply
                                        (lambda (#{x 3798}#
                                                 #{dots 3799}#
                                                 #{ys 3800}#)
                                          (call-with-values
                                            (lambda ()
                                              (#{cvt* 3745}#
                                                #{ys 3800}#
                                                #{n 3763}#
                                                #{ids 3764}#))
                                            (lambda (#{ys 3802}# #{ids 3803}#)
                                              (call-with-values
                                                (lambda ()
                                                  (#{cvt 3747}#
                                                    #{x 3798}#
                                                    (#{1+}# #{n 3763}#)
                                                    #{ids 3803}#))
                                                (lambda (#{x 3806}#
                                                         #{ids 3807}#)
                                                  (values
                                                    (vector
                                                      'each+
                                                      #{x 3806}#
                                                      (reverse #{ys 3802}#)
                                                      '())
                                                    #{ids 3807}#))))))
                                        #{tmp 3788}#)
                                      (let ((#{tmp 3811}#
                                              ($sc-dispatch
                                                #{tmp 3773}#
                                                '(any . any))))
                                        (if #{tmp 3811}#
                                          (@apply
                                            (lambda (#{x 3814}# #{y 3815}#)
                                              (call-with-values
                                                (lambda ()
                                                  (#{cvt 3747}#
                                                    #{y 3815}#
                                                    #{n 3763}#
                                                    #{ids 3764}#))
                                                (lambda (#{y 3816}#
                                                         #{ids 3817}#)
                                                  (call-with-values
                                                    (lambda ()
                                                      (#{cvt 3747}#
                                                        #{x 3814}#
                                                        #{n 3763}#
                                                        #{ids 3817}#))
                                                    (lambda (#{x 3820}#
                                                             #{ids 3821}#)
                                                      (values
                                                        (cons #{x 3820}#
                                                              #{y 3816}#)
                                                        #{ids 3821}#))))))
                                            #{tmp 3811}#)
                                          (let ((#{tmp 3824}#
                                                  ($sc-dispatch
                                                    #{tmp 3773}#
                                                    '())))
                                            (if #{tmp 3824}#
                                              (@apply
                                                (lambda ()
                                                  (values '() #{ids 3764}#))
                                                #{tmp 3824}#)
                                              (let ((#{tmp 3825}#
                                                      ($sc-dispatch
                                                        #{tmp 3773}#
                                                        '#(vector each-any))))
                                                (if #{tmp 3825}#
                                                  (@apply
                                                    (lambda (#{x 3827}#)
                                                      (call-with-values
                                                        (lambda ()
                                                          (#{cvt 3747}#
                                                            #{x 3827}#
                                                            #{n 3763}#
                                                            #{ids 3764}#))
                                                        (lambda (#{p 3829}#
                                                                 #{ids 3830}#)
                                                          (values
                                                            (vector
                                                              'vector
                                                              #{p 3829}#)
                                                            #{ids 3830}#))))
                                                    #{tmp 3825}#)
                                                  (let ((#{x 3834}#
                                                          #{tmp 3773}#))
                                                    (values
                                                      (vector
                                                        'atom
                                                        (#{strip 483}#
                                                          #{p 3762}#
                                                          '(())))
                                                      #{ids 3764}#)))))))))))))))))
                     (begin (#{cvt 3747}# #{pattern 3740}# 0 '())))))
               (#{build-dispatch-call 3735}#
                 (lambda (#{pvars 3836}#
                          #{exp 3837}#
                          #{y 3838}#
                          #{r 3839}#
                          #{mod 3840}#)
                   (begin
                     (map cdr #{pvars 3836}#)
                     (let ((#{ids 3848}# (map car #{pvars 3836}#)))
                       (begin
                         (let ((#{labels 3852}#
                                 (#{gen-labels 392}# #{ids 3848}#))
                               (#{new-vars 3853}#
                                 (map #{gen-var 485}# #{ids 3848}#)))
                           (#{build-application 303}#
                             #f
                             (#{build-primref 327}# #f 'apply)
                             (list (#{build-simple-lambda 321}#
                                     #f
                                     (map syntax->datum #{ids 3848}#)
                                     #f
                                     #{new-vars 3853}#
                                     '()
                                     (#{chi 457}#
                                       #{exp 3837}#
                                       (#{extend-env 365}#
                                         #{labels 3852}#
                                         (map (lambda (#{var 3857}#
                                                       #{level 3858}#)
                                                (cons 'syntax
                                                      (cons #{var 3857}#
                                                            #{level 3858}#)))
                                              #{new-vars 3853}#
                                              (map cdr #{pvars 3836}#))
                                         #{r 3839}#)
                                       (#{make-binding-wrap 421}#
                                         #{ids 3848}#
                                         #{labels 3852}#
                                         '(()))
                                       #{mod 3840}#))
                                   #{y 3838}#))))))))
               (#{gen-clause 3737}#
                 (lambda (#{x 3864}#
                          #{keys 3865}#
                          #{clauses 3866}#
                          #{r 3867}#
                          #{pat 3868}#
                          #{fender 3869}#
                          #{exp 3870}#
                          #{mod 3871}#)
                   (call-with-values
                     (lambda ()
                       (#{convert-pattern 3733}#
                         #{pat 3868}#
                         #{keys 3865}#))
                     (lambda (#{p 3880}# #{pvars 3881}#)
                       (if (not (#{distinct-bound-ids? 439}#
                                  (map car #{pvars 3881}#)))
                         (syntax-violation
                           'syntax-case
                           "duplicate pattern variable"
                           #{pat 3868}#)
                         (if (not (and-map
                                    (lambda (#{x 3888}#)
                                      (not (#{ellipsis? 473}#
                                             (car #{x 3888}#))))
                                    #{pvars 3881}#))
                           (syntax-violation
                             'syntax-case
                             "misplaced ellipsis"
                             #{pat 3868}#)
                           (begin
                             (let ((#{y 3892}# (#{gen-var 485}# 'tmp)))
                               (#{build-application 303}#
                                 #f
                                 (#{build-simple-lambda 321}#
                                   #f
                                   (list 'tmp)
                                   #f
                                   (list #{y 3892}#)
                                   '()
                                   (begin
                                     (let ((#{y 3896}#
                                             (#{build-lexical-reference 309}#
                                               'value
                                               #f
                                               'tmp
                                               #{y 3892}#)))
                                       (#{build-conditional 305}#
                                         #f
                                         (let ((#{tmp 3899}# #{fender 3869}#))
                                           (let ((#{tmp 3900}#
                                                   ($sc-dispatch
                                                     #{tmp 3899}#
                                                     '#(atom #t))))
                                             (if #{tmp 3900}#
                                               (@apply
                                                 (lambda () #{y 3896}#)
                                                 #{tmp 3900}#)
                                               (let ((#{_ 3902}# #{tmp 3899}#))
                                                 (#{build-conditional 305}#
                                                   #f
                                                   #{y 3896}#
                                                   (#{build-dispatch-call 3735}#
                                                     #{pvars 3881}#
                                                     #{fender 3869}#
                                                     #{y 3896}#
                                                     #{r 3867}#
                                                     #{mod 3871}#)
                                                   (#{build-data 329}#
                                                     #f
                                                     #f))))))
                                         (#{build-dispatch-call 3735}#
                                           #{pvars 3881}#
                                           #{exp 3870}#
                                           #{y 3896}#
                                           #{r 3867}#
                                           #{mod 3871}#)
                                         (#{gen-syntax-case 3739}#
                                           #{x 3864}#
                                           #{keys 3865}#
                                           #{clauses 3866}#
                                           #{r 3867}#
                                           #{mod 3871}#)))))
                                 (list (if (eq? #{p 3880}# 'any)
                                         (#{build-application 303}#
                                           #f
                                           (#{build-primref 327}# #f 'list)
                                           (list #{x 3864}#))
                                         (#{build-application 303}#
                                           #f
                                           (#{build-primref 327}#
                                             #f
                                             '$sc-dispatch)
                                           (list #{x 3864}#
                                                 (#{build-data 329}#
                                                   #f
                                                   #{p 3880}#))))))))))))))
               (#{gen-syntax-case 3739}#
                 (lambda (#{x 3910}#
                          #{keys 3911}#
                          #{clauses 3912}#
                          #{r 3913}#
                          #{mod 3914}#)
                   (if (null? #{clauses 3912}#)
                     (#{build-application 303}#
                       #f
                       (#{build-primref 327}# #f 'syntax-violation)
                       (list (#{build-data 329}# #f #f)
                             (#{build-data 329}#
                               #f
                               "source expression failed to match any pattern")
                             #{x 3910}#))
                     (let ((#{tmp 3924}# (car #{clauses 3912}#)))
                       (let ((#{tmp 3925}#
                               ($sc-dispatch #{tmp 3924}# '(any any))))
                         (if #{tmp 3925}#
                           (@apply
                             (lambda (#{pat 3928}# #{exp 3929}#)
                               (if (if (#{id? 377}# #{pat 3928}#)
                                     (and-map
                                       (lambda (#{x 3932}#)
                                         (not (#{free-id=? 433}#
                                                #{pat 3928}#
                                                #{x 3932}#)))
                                       (cons '#(syntax-object
                                                ...
                                                ((top)
                                                 #(ribcage
                                                   #(pat exp)
                                                   #((top) (top))
                                                   #("i3926" "i3927"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x keys clauses r mod)
                                                   #((top)
                                                     (top)
                                                     (top)
                                                     (top)
                                                     (top))
                                                   #("i3915"
                                                     "i3916"
                                                     "i3917"
                                                     "i3918"
                                                     "i3919"))
                                                 #(ribcage
                                                   (gen-syntax-case
                                                     gen-clause
                                                     build-dispatch-call
                                                     convert-pattern)
                                                   ((top) (top) (top) (top))
                                                   ("i3738"
                                                    "i3736"
                                                    "i3734"
                                                    "i3732"))
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
                                                     chi-application
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
                                                     build-application
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
                                                     make-sequence
                                                     make-application
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
                                                    (top))
                                                   ("i486"
                                                    "i484"
                                                    "i482"
                                                    "i480"
                                                    "i478"
                                                    "i476"
                                                    "i474"
                                                    "i472"
                                                    "i470"
                                                    "i468"
                                                    "i466"
                                                    "i464"
                                                    "i462"
                                                    "i460"
                                                    "i458"
                                                    "i456"
                                                    "i454"
                                                    "i452"
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
                                                    "i417"
                                                    "i416"
                                                    "i414"
                                                    "i413"
                                                    "i412"
                                                    "i411"
                                                    "i410"
                                                    "i408"
                                                    "i406"
                                                    "i404"
                                                    "i402"
                                                    "i400"
                                                    "i398"
                                                    "i396"
                                                    "i394"
                                                    "i391"
                                                    "i389"
                                                    "i388"
                                                    "i387"
                                                    "i386"
                                                    "i385"
                                                    "i384"
                                                    "i383"
                                                    "i382"
                                                    "i381"
                                                    "i379"
                                                    "i378"
                                                    "i376"
                                                    "i374"
                                                    "i372"
                                                    "i370"
                                                    "i368"
                                                    "i366"
                                                    "i364"
                                                    "i363"
                                                    "i362"
                                                    "i361"
                                                    "i360"
                                                    "i359"
                                                    "i357"
                                                    "i356"
                                                    "i354"
                                                    "i352"
                                                    "i350"
                                                    "i348"
                                                    "i346"
                                                    "i344"
                                                    "i342"
                                                    "i340"
                                                    "i338"
                                                    "i336"
                                                    "i334"
                                                    "i332"
                                                    "i330"
                                                    "i328"
                                                    "i326"
                                                    "i324"
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
                                                    "i291"
                                                    "i289"
                                                    "i287"
                                                    "i286"
                                                    "i285"
                                                    "i284"
                                                    "i283"
                                                    "i281"
                                                    "i279"
                                                    "i277"
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
                                                    "i254"
                                                    "i252"
                                                    "i250"
                                                    "i248"
                                                    "i246"
                                                    "i244"
                                                    "i242"
                                                    "i240"))
                                                 #(ribcage
                                                   (define-structure
                                                     define-expansion-accessors
                                                     define-expansion-constructors
                                                     and-map*)
                                                   ((top) (top) (top) (top))
                                                   ("i42" "i41" "i40" "i38")))
                                                (hygiene guile))
                                             #{keys 3911}#))
                                     #f)
                                 (if (#{free-id=? 433}#
                                       '#(syntax-object
                                          pad
                                          ((top)
                                           #(ribcage
                                             #(pat exp)
                                             #((top) (top))
                                             #("i3926" "i3927"))
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(x keys clauses r mod)
                                             #((top) (top) (top) (top) (top))
                                             #("i3915"
                                               "i3916"
                                               "i3917"
                                               "i3918"
                                               "i3919"))
                                           #(ribcage
                                             (gen-syntax-case
                                               gen-clause
                                               build-dispatch-call
                                               convert-pattern)
                                             ((top) (top) (top) (top))
                                             ("i3738" "i3736" "i3734" "i3732"))
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
                                               chi-application
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
                                               build-application
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
                                               make-sequence
                                               make-application
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
                                              (top))
                                             ("i486"
                                              "i484"
                                              "i482"
                                              "i480"
                                              "i478"
                                              "i476"
                                              "i474"
                                              "i472"
                                              "i470"
                                              "i468"
                                              "i466"
                                              "i464"
                                              "i462"
                                              "i460"
                                              "i458"
                                              "i456"
                                              "i454"
                                              "i452"
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
                                              "i417"
                                              "i416"
                                              "i414"
                                              "i413"
                                              "i412"
                                              "i411"
                                              "i410"
                                              "i408"
                                              "i406"
                                              "i404"
                                              "i402"
                                              "i400"
                                              "i398"
                                              "i396"
                                              "i394"
                                              "i391"
                                              "i389"
                                              "i388"
                                              "i387"
                                              "i386"
                                              "i385"
                                              "i384"
                                              "i383"
                                              "i382"
                                              "i381"
                                              "i379"
                                              "i378"
                                              "i376"
                                              "i374"
                                              "i372"
                                              "i370"
                                              "i368"
                                              "i366"
                                              "i364"
                                              "i363"
                                              "i362"
                                              "i361"
                                              "i360"
                                              "i359"
                                              "i357"
                                              "i356"
                                              "i354"
                                              "i352"
                                              "i350"
                                              "i348"
                                              "i346"
                                              "i344"
                                              "i342"
                                              "i340"
                                              "i338"
                                              "i336"
                                              "i334"
                                              "i332"
                                              "i330"
                                              "i328"
                                              "i326"
                                              "i324"
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
                                              "i291"
                                              "i289"
                                              "i287"
                                              "i286"
                                              "i285"
                                              "i284"
                                              "i283"
                                              "i281"
                                              "i279"
                                              "i277"
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
                                              "i254"
                                              "i252"
                                              "i250"
                                              "i248"
                                              "i246"
                                              "i244"
                                              "i242"
                                              "i240"))
                                           #(ribcage
                                             (define-structure
                                               define-expansion-accessors
                                               define-expansion-constructors
                                               and-map*)
                                             ((top) (top) (top) (top))
                                             ("i42" "i41" "i40" "i38")))
                                          (hygiene guile))
                                       '#(syntax-object
                                          _
                                          ((top)
                                           #(ribcage
                                             #(pat exp)
                                             #((top) (top))
                                             #("i3926" "i3927"))
                                           #(ribcage () () ())
                                           #(ribcage
                                             #(x keys clauses r mod)
                                             #((top) (top) (top) (top) (top))
                                             #("i3915"
                                               "i3916"
                                               "i3917"
                                               "i3918"
                                               "i3919"))
                                           #(ribcage
                                             (gen-syntax-case
                                               gen-clause
                                               build-dispatch-call
                                               convert-pattern)
                                             ((top) (top) (top) (top))
                                             ("i3738" "i3736" "i3734" "i3732"))
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
                                               chi-application
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
                                               build-application
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
                                               make-sequence
                                               make-application
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
                                              (top))
                                             ("i486"
                                              "i484"
                                              "i482"
                                              "i480"
                                              "i478"
                                              "i476"
                                              "i474"
                                              "i472"
                                              "i470"
                                              "i468"
                                              "i466"
                                              "i464"
                                              "i462"
                                              "i460"
                                              "i458"
                                              "i456"
                                              "i454"
                                              "i452"
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
                                              "i417"
                                              "i416"
                                              "i414"
                                              "i413"
                                              "i412"
                                              "i411"
                                              "i410"
                                              "i408"
                                              "i406"
                                              "i404"
                                              "i402"
                                              "i400"
                                              "i398"
                                              "i396"
                                              "i394"
                                              "i391"
                                              "i389"
                                              "i388"
                                              "i387"
                                              "i386"
                                              "i385"
                                              "i384"
                                              "i383"
                                              "i382"
                                              "i381"
                                              "i379"
                                              "i378"
                                              "i376"
                                              "i374"
                                              "i372"
                                              "i370"
                                              "i368"
                                              "i366"
                                              "i364"
                                              "i363"
                                              "i362"
                                              "i361"
                                              "i360"
                                              "i359"
                                              "i357"
                                              "i356"
                                              "i354"
                                              "i352"
                                              "i350"
                                              "i348"
                                              "i346"
                                              "i344"
                                              "i342"
                                              "i340"
                                              "i338"
                                              "i336"
                                              "i334"
                                              "i332"
                                              "i330"
                                              "i328"
                                              "i326"
                                              "i324"
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
                                              "i291"
                                              "i289"
                                              "i287"
                                              "i286"
                                              "i285"
                                              "i284"
                                              "i283"
                                              "i281"
                                              "i279"
                                              "i277"
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
                                              "i254"
                                              "i252"
                                              "i250"
                                              "i248"
                                              "i246"
                                              "i244"
                                              "i242"
                                              "i240"))
                                           #(ribcage
                                             (define-structure
                                               define-expansion-accessors
                                               define-expansion-constructors
                                               and-map*)
                                             ((top) (top) (top) (top))
                                             ("i42" "i41" "i40" "i38")))
                                          (hygiene guile)))
                                   (#{chi 457}#
                                     #{exp 3929}#
                                     #{r 3913}#
                                     '(())
                                     #{mod 3914}#)
                                   (begin
                                     (let ((#{labels 3937}#
                                             (list (#{gen-label 390}#)))
                                           (#{var 3938}#
                                             (#{gen-var 485}# #{pat 3928}#)))
                                       (#{build-application 303}#
                                         #f
                                         (#{build-simple-lambda 321}#
                                           #f
                                           (list (syntax->datum #{pat 3928}#))
                                           #f
                                           (list #{var 3938}#)
                                           '()
                                           (#{chi 457}#
                                             #{exp 3929}#
                                             (#{extend-env 365}#
                                               #{labels 3937}#
                                               (list (cons 'syntax
                                                           (cons #{var 3938}#
                                                                 0)))
                                               #{r 3913}#)
                                             (#{make-binding-wrap 421}#
                                               (list #{pat 3928}#)
                                               #{labels 3937}#
                                               '(()))
                                             #{mod 3914}#))
                                         (list #{x 3910}#)))))
                                 (#{gen-clause 3737}#
                                   #{x 3910}#
                                   #{keys 3911}#
                                   (cdr #{clauses 3912}#)
                                   #{r 3913}#
                                   #{pat 3928}#
                                   #t
                                   #{exp 3929}#
                                   #{mod 3914}#)))
                             #{tmp 3925}#)
                           (let ((#{tmp 3944}#
                                   ($sc-dispatch #{tmp 3924}# '(any any any))))
                             (if #{tmp 3944}#
                               (@apply
                                 (lambda (#{pat 3948}#
                                          #{fender 3949}#
                                          #{exp 3950}#)
                                   (#{gen-clause 3737}#
                                     #{x 3910}#
                                     #{keys 3911}#
                                     (cdr #{clauses 3912}#)
                                     #{r 3913}#
                                     #{pat 3948}#
                                     #{fender 3949}#
                                     #{exp 3950}#
                                     #{mod 3914}#))
                                 #{tmp 3944}#)
                               (let ((#{_ 3952}# #{tmp 3924}#))
                                 (syntax-violation
                                   'syntax-case
                                   "invalid clause"
                                   (car #{clauses 3912}#))))))))))))
              (begin
                (lambda (#{e 3953}#
                         #{r 3954}#
                         #{w 3955}#
                         #{s 3956}#
                         #{mod 3957}#)
                  (begin
                    (let ((#{e 3964}#
                            (#{source-wrap 445}#
                              #{e 3953}#
                              #{w 3955}#
                              #{s 3956}#
                              #{mod 3957}#)))
                      (let ((#{tmp 3965}# #{e 3964}#))
                        (let ((#{tmp 3966}#
                                ($sc-dispatch
                                  #{tmp 3965}#
                                  '(_ any each-any . each-any))))
                          (if #{tmp 3966}#
                            (@apply
                              (lambda (#{val 3970}# #{key 3971}# #{m 3972}#)
                                (if (and-map
                                      (lambda (#{x 3973}#)
                                        (if (#{id? 377}# #{x 3973}#)
                                          (not (#{ellipsis? 473}# #{x 3973}#))
                                          #f))
                                      #{key 3971}#)
                                  (begin
                                    (let ((#{x 3979}# (#{gen-var 485}# 'tmp)))
                                      (#{build-application 303}#
                                        #{s 3956}#
                                        (#{build-simple-lambda 321}#
                                          #f
                                          (list 'tmp)
                                          #f
                                          (list #{x 3979}#)
                                          '()
                                          (#{gen-syntax-case 3739}#
                                            (#{build-lexical-reference 309}#
                                              'value
                                              #f
                                              'tmp
                                              #{x 3979}#)
                                            #{key 3971}#
                                            #{m 3972}#
                                            #{r 3954}#
                                            #{mod 3957}#))
                                        (list (#{chi 457}#
                                                #{val 3970}#
                                                #{r 3954}#
                                                '(())
                                                #{mod 3957}#)))))
                                  (syntax-violation
                                    'syntax-case
                                    "invalid literals list"
                                    #{e 3964}#)))
                              #{tmp 3966}#)
                            (syntax-violation
                              #f
                              "source expression failed to match any pattern"
                              #{tmp 3965}#))))))))))
          (set! macroexpand
            (lambda*
              (#{x 3985}#
                #:optional
                (#{m 3987}# 'e)
                (#{esew 3989}# '(eval)))
              (#{chi-top-sequence 449}#
                (list #{x 3985}#)
                '()
                '((top))
                #f
                #{m 3987}#
                #{esew 3989}#
                (cons 'hygiene (module-name (current-module))))))
          (set! identifier?
            (lambda (#{x 3993}#)
              (#{nonsymbol-id? 375}# #{x 3993}#)))
          (set! datum->syntax
            (lambda (#{id 3995}# #{datum 3996}#)
              (#{make-syntax-object 341}#
                #{datum 3996}#
                (#{syntax-object-wrap 347}# #{id 3995}#)
                (#{syntax-object-module 349}# #{id 3995}#))))
          (set! syntax->datum
            (lambda (#{x 3999}#)
              (#{strip 483}# #{x 3999}# '(()))))
          (set! syntax-source
            (lambda (#{x 4002}#)
              (#{source-annotation 358}# #{x 4002}#)))
          (set! generate-temporaries
            (lambda (#{ls 4004}#)
              (begin
                (begin
                  (let ((#{x 4008}# #{ls 4004}#))
                    (if (not (list? #{x 4008}#))
                      (syntax-violation
                        'generate-temporaries
                        "invalid argument"
                        #{x 4008}#))))
                (map (lambda (#{x 4009}#)
                       (#{wrap 443}# (gensym) '((top)) #f))
                     #{ls 4004}#))))
          (set! free-identifier=?
            (lambda (#{x 4013}# #{y 4014}#)
              (begin
                (begin
                  (let ((#{x 4019}# #{x 4013}#))
                    (if (not (#{nonsymbol-id? 375}# #{x 4019}#))
                      (syntax-violation
                        'free-identifier=?
                        "invalid argument"
                        #{x 4019}#))))
                (begin
                  (let ((#{x 4022}# #{y 4014}#))
                    (if (not (#{nonsymbol-id? 375}# #{x 4022}#))
                      (syntax-violation
                        'free-identifier=?
                        "invalid argument"
                        #{x 4022}#))))
                (#{free-id=? 433}# #{x 4013}# #{y 4014}#))))
          (set! bound-identifier=?
            (lambda (#{x 4023}# #{y 4024}#)
              (begin
                (begin
                  (let ((#{x 4029}# #{x 4023}#))
                    (if (not (#{nonsymbol-id? 375}# #{x 4029}#))
                      (syntax-violation
                        'bound-identifier=?
                        "invalid argument"
                        #{x 4029}#))))
                (begin
                  (let ((#{x 4032}# #{y 4024}#))
                    (if (not (#{nonsymbol-id? 375}# #{x 4032}#))
                      (syntax-violation
                        'bound-identifier=?
                        "invalid argument"
                        #{x 4032}#))))
                (#{bound-id=? 435}# #{x 4023}# #{y 4024}#))))
          (set! syntax-violation
            (lambda*
              (#{who 4033}#
                #{message 4034}#
                #{form 4035}#
                #:optional
                (#{subform 4039}# #f))
              (begin
                (begin
                  (let ((#{x 4043}# #{who 4033}#))
                    (if (not (let ((#{x 4044}# #{x 4043}#))
                               (begin
                                 (let ((#{t 4048}# (not #{x 4044}#)))
                                   (if #{t 4048}#
                                     #{t 4048}#
                                     (begin
                                       (let ((#{t 4051}# (string? #{x 4044}#)))
                                         (if #{t 4051}#
                                           #{t 4051}#
                                           (symbol? #{x 4044}#)))))))))
                      (syntax-violation
                        'syntax-violation
                        "invalid argument"
                        #{x 4043}#))))
                (begin
                  (let ((#{x 4055}# #{message 4034}#))
                    (if (not (string? #{x 4055}#))
                      (syntax-violation
                        'syntax-violation
                        "invalid argument"
                        #{x 4055}#))))
                (throw 'syntax-error
                       #{who 4033}#
                       #{message 4034}#
                       (#{source-annotation 358}#
                         (begin
                           (let ((#{t 4058}# #{form 4035}#))
                             (if #{t 4058}# #{t 4058}# #{subform 4039}#))))
                       (#{strip 483}# #{form 4035}# '(()))
                       (if #{subform 4039}#
                         (#{strip 483}# #{subform 4039}# '(()))
                         #f)))))
          (letrec*
            ((#{match-each 4065}#
               (lambda (#{e 4078}# #{p 4079}# #{w 4080}# #{mod 4081}#)
                 (if (pair? #{e 4078}#)
                   (begin
                     (let ((#{first 4089}#
                             (#{match 4077}#
                               (car #{e 4078}#)
                               #{p 4079}#
                               #{w 4080}#
                               '()
                               #{mod 4081}#)))
                       (if #{first 4089}#
                         (begin
                           (let ((#{rest 4093}#
                                   (#{match-each 4065}#
                                     (cdr #{e 4078}#)
                                     #{p 4079}#
                                     #{w 4080}#
                                     #{mod 4081}#)))
                             (if #{rest 4093}#
                               (cons #{first 4089}# #{rest 4093}#)
                               #f)))
                         #f)))
                   (if (null? #{e 4078}#)
                     '()
                     (if (#{syntax-object? 343}# #{e 4078}#)
                       (#{match-each 4065}#
                         (#{syntax-object-expression 345}# #{e 4078}#)
                         #{p 4079}#
                         (#{join-wraps 425}#
                           #{w 4080}#
                           (#{syntax-object-wrap 347}# #{e 4078}#))
                         (#{syntax-object-module 349}# #{e 4078}#))
                       #f)))))
             (#{match-each+ 4067}#
               (lambda (#{e 4101}#
                        #{x-pat 4102}#
                        #{y-pat 4103}#
                        #{z-pat 4104}#
                        #{w 4105}#
                        #{r 4106}#
                        #{mod 4107}#)
                 (letrec*
                   ((#{f 4118}#
                      (lambda (#{e 4119}# #{w 4120}#)
                        (if (pair? #{e 4119}#)
                          (call-with-values
                            (lambda ()
                              (#{f 4118}# (cdr #{e 4119}#) #{w 4120}#))
                            (lambda (#{xr* 4123}# #{y-pat 4124}# #{r 4125}#)
                              (if #{r 4125}#
                                (if (null? #{y-pat 4124}#)
                                  (begin
                                    (let ((#{xr 4130}#
                                            (#{match 4077}#
                                              (car #{e 4119}#)
                                              #{x-pat 4102}#
                                              #{w 4120}#
                                              '()
                                              #{mod 4107}#)))
                                      (if #{xr 4130}#
                                        (values
                                          (cons #{xr 4130}# #{xr* 4123}#)
                                          #{y-pat 4124}#
                                          #{r 4125}#)
                                        (values #f #f #f))))
                                  (values
                                    '()
                                    (cdr #{y-pat 4124}#)
                                    (#{match 4077}#
                                      (car #{e 4119}#)
                                      (car #{y-pat 4124}#)
                                      #{w 4120}#
                                      #{r 4125}#
                                      #{mod 4107}#)))
                                (values #f #f #f))))
                          (if (#{syntax-object? 343}# #{e 4119}#)
                            (#{f 4118}#
                              (#{syntax-object-expression 345}# #{e 4119}#)
                              (#{join-wraps 425}# #{w 4120}# #{e 4119}#))
                            (values
                              '()
                              #{y-pat 4103}#
                              (#{match 4077}#
                                #{e 4119}#
                                #{z-pat 4104}#
                                #{w 4120}#
                                #{r 4106}#
                                #{mod 4107}#)))))))
                   (begin (#{f 4118}# #{e 4101}# #{w 4105}#)))))
             (#{match-each-any 4069}#
               (lambda (#{e 4134}# #{w 4135}# #{mod 4136}#)
                 (if (pair? #{e 4134}#)
                   (begin
                     (let ((#{l 4143}#
                             (#{match-each-any 4069}#
                               (cdr #{e 4134}#)
                               #{w 4135}#
                               #{mod 4136}#)))
                       (if #{l 4143}#
                         (cons (#{wrap 443}#
                                 (car #{e 4134}#)
                                 #{w 4135}#
                                 #{mod 4136}#)
                               #{l 4143}#)
                         #f)))
                   (if (null? #{e 4134}#)
                     '()
                     (if (#{syntax-object? 343}# #{e 4134}#)
                       (#{match-each-any 4069}#
                         (#{syntax-object-expression 345}# #{e 4134}#)
                         (#{join-wraps 425}#
                           #{w 4135}#
                           (#{syntax-object-wrap 347}# #{e 4134}#))
                         #{mod 4136}#)
                       #f)))))
             (#{match-empty 4071}#
               (lambda (#{p 4151}# #{r 4152}#)
                 (if (null? #{p 4151}#)
                   #{r 4152}#
                   (if (eq? #{p 4151}# '_)
                     #{r 4152}#
                     (if (eq? #{p 4151}# 'any)
                       (cons '() #{r 4152}#)
                       (if (pair? #{p 4151}#)
                         (#{match-empty 4071}#
                           (car #{p 4151}#)
                           (#{match-empty 4071}#
                             (cdr #{p 4151}#)
                             #{r 4152}#))
                         (if (eq? #{p 4151}# 'each-any)
                           (cons '() #{r 4152}#)
                           (begin
                             (let ((#{atom-key 4168}#
                                     (vector-ref #{p 4151}# 0)))
                               (if (eqv? #{atom-key 4168}# 'each)
                                 (#{match-empty 4071}#
                                   (vector-ref #{p 4151}# 1)
                                   #{r 4152}#)
                                 (if (eqv? #{atom-key 4168}# 'each+)
                                   (#{match-empty 4071}#
                                     (vector-ref #{p 4151}# 1)
                                     (#{match-empty 4071}#
                                       (reverse (vector-ref #{p 4151}# 2))
                                       (#{match-empty 4071}#
                                         (vector-ref #{p 4151}# 3)
                                         #{r 4152}#)))
                                   (if (if (eqv? #{atom-key 4168}# 'free-id)
                                         #t
                                         (eqv? #{atom-key 4168}# 'atom))
                                     #{r 4152}#
                                     (if (eqv? #{atom-key 4168}# 'vector)
                                       (#{match-empty 4071}#
                                         (vector-ref #{p 4151}# 1)
                                         #{r 4152}#))))))))))))))
             (#{combine 4073}#
               (lambda (#{r* 4173}# #{r 4174}#)
                 (if (null? (car #{r* 4173}#))
                   #{r 4174}#
                   (cons (map car #{r* 4173}#)
                         (#{combine 4073}#
                           (map cdr #{r* 4173}#)
                           #{r 4174}#)))))
             (#{match* 4075}#
               (lambda (#{e 4177}#
                        #{p 4178}#
                        #{w 4179}#
                        #{r 4180}#
                        #{mod 4181}#)
                 (if (null? #{p 4178}#)
                   (if (null? #{e 4177}#) #{r 4180}# #f)
                   (if (pair? #{p 4178}#)
                     (if (pair? #{e 4177}#)
                       (#{match 4077}#
                         (car #{e 4177}#)
                         (car #{p 4178}#)
                         #{w 4179}#
                         (#{match 4077}#
                           (cdr #{e 4177}#)
                           (cdr #{p 4178}#)
                           #{w 4179}#
                           #{r 4180}#
                           #{mod 4181}#)
                         #{mod 4181}#)
                       #f)
                     (if (eq? #{p 4178}# 'each-any)
                       (begin
                         (let ((#{l 4198}#
                                 (#{match-each-any 4069}#
                                   #{e 4177}#
                                   #{w 4179}#
                                   #{mod 4181}#)))
                           (if #{l 4198}# (cons #{l 4198}# #{r 4180}#) #f)))
                       (begin
                         (let ((#{atom-key 4204}# (vector-ref #{p 4178}# 0)))
                           (if (eqv? #{atom-key 4204}# 'each)
                             (if (null? #{e 4177}#)
                               (#{match-empty 4071}#
                                 (vector-ref #{p 4178}# 1)
                                 #{r 4180}#)
                               (begin
                                 (let ((#{l 4207}#
                                         (#{match-each 4065}#
                                           #{e 4177}#
                                           (vector-ref #{p 4178}# 1)
                                           #{w 4179}#
                                           #{mod 4181}#)))
                                   (if #{l 4207}#
                                     (letrec*
                                       ((#{collect 4212}#
                                          (lambda (#{l 4213}#)
                                            (if (null? (car #{l 4213}#))
                                              #{r 4180}#
                                              (cons (map car #{l 4213}#)
                                                    (#{collect 4212}#
                                                      (map cdr
                                                           #{l 4213}#)))))))
                                       (begin (#{collect 4212}# #{l 4207}#)))
                                     #f))))
                             (if (eqv? #{atom-key 4204}# 'each+)
                               (call-with-values
                                 (lambda ()
                                   (#{match-each+ 4067}#
                                     #{e 4177}#
                                     (vector-ref #{p 4178}# 1)
                                     (vector-ref #{p 4178}# 2)
                                     (vector-ref #{p 4178}# 3)
                                     #{w 4179}#
                                     #{r 4180}#
                                     #{mod 4181}#))
                                 (lambda (#{xr* 4215}#
                                          #{y-pat 4216}#
                                          #{r 4217}#)
                                   (if #{r 4217}#
                                     (if (null? #{y-pat 4216}#)
                                       (if (null? #{xr* 4215}#)
                                         (#{match-empty 4071}#
                                           (vector-ref #{p 4178}# 1)
                                           #{r 4217}#)
                                         (#{combine 4073}#
                                           #{xr* 4215}#
                                           #{r 4217}#))
                                       #f)
                                     #f)))
                               (if (eqv? #{atom-key 4204}# 'free-id)
                                 (if (#{id? 377}# #{e 4177}#)
                                   (if (#{free-id=? 433}#
                                         (#{wrap 443}#
                                           #{e 4177}#
                                           #{w 4179}#
                                           #{mod 4181}#)
                                         (vector-ref #{p 4178}# 1))
                                     #{r 4180}#
                                     #f)
                                   #f)
                                 (if (eqv? #{atom-key 4204}# 'atom)
                                   (if (equal?
                                         (vector-ref #{p 4178}# 1)
                                         (#{strip 483}# #{e 4177}# #{w 4179}#))
                                     #{r 4180}#
                                     #f)
                                   (if (eqv? #{atom-key 4204}# 'vector)
                                     (if (vector? #{e 4177}#)
                                       (#{match 4077}#
                                         (vector->list #{e 4177}#)
                                         (vector-ref #{p 4178}# 1)
                                         #{w 4179}#
                                         #{r 4180}#
                                         #{mod 4181}#)
                                       #f)))))))))))))
             (#{match 4077}#
               (lambda (#{e 4234}#
                        #{p 4235}#
                        #{w 4236}#
                        #{r 4237}#
                        #{mod 4238}#)
                 (if (not #{r 4237}#)
                   #f
                   (if (eq? #{p 4235}# '_)
                     #{r 4237}#
                     (if (eq? #{p 4235}# 'any)
                       (cons (#{wrap 443}# #{e 4234}# #{w 4236}# #{mod 4238}#)
                             #{r 4237}#)
                       (if (#{syntax-object? 343}# #{e 4234}#)
                         (#{match* 4075}#
                           (#{syntax-object-expression 345}# #{e 4234}#)
                           #{p 4235}#
                           (#{join-wraps 425}#
                             #{w 4236}#
                             (#{syntax-object-wrap 347}# #{e 4234}#))
                           #{r 4237}#
                           (#{syntax-object-module 349}# #{e 4234}#))
                         (#{match* 4075}#
                           #{e 4234}#
                           #{p 4235}#
                           #{w 4236}#
                           #{r 4237}#
                           #{mod 4238}#))))))))
            (begin
              (set! $sc-dispatch
                (lambda (#{e 4253}# #{p 4254}#)
                  (if (eq? #{p 4254}# 'any)
                    (list #{e 4253}#)
                    (if (eq? #{p 4254}# '_)
                      '()
                      (if (#{syntax-object? 343}# #{e 4253}#)
                        (#{match* 4075}#
                          (#{syntax-object-expression 345}# #{e 4253}#)
                          #{p 4254}#
                          (#{syntax-object-wrap 347}# #{e 4253}#)
                          '()
                          (#{syntax-object-module 349}# #{e 4253}#))
                        (#{match* 4075}#
                          #{e 4253}#
                          #{p 4254}#
                          '(())
                          '()
                          #f)))))))))))))

(define with-syntax
  (make-syntax-transformer
    'with-syntax
    'macro
    (lambda (#{x 4265}#)
      (let ((#{tmp 4267}# #{x 4265}#))
        (let ((#{tmp 4268}#
                ($sc-dispatch
                  #{tmp 4267}#
                  '(_ () any . each-any))))
          (if #{tmp 4268}#
            (@apply
              (lambda (#{e1 4271}# #{e2 4272}#)
                (cons '#(syntax-object
                         let
                         ((top)
                          #(ribcage
                            #(e1 e2)
                            #((top) (top))
                            #("i4269" "i4270"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4266")))
                         (hygiene guile))
                      (cons '() (cons #{e1 4271}# #{e2 4272}#))))
              #{tmp 4268}#)
            (let ((#{tmp 4274}#
                    ($sc-dispatch
                      #{tmp 4267}#
                      '(_ ((any any)) any . each-any))))
              (if #{tmp 4274}#
                (@apply
                  (lambda (#{out 4279}#
                           #{in 4280}#
                           #{e1 4281}#
                           #{e2 4282}#)
                    (list '#(syntax-object
                             syntax-case
                             ((top)
                              #(ribcage
                                #(out in e1 e2)
                                #((top) (top) (top) (top))
                                #("i4275" "i4276" "i4277" "i4278"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4266")))
                             (hygiene guile))
                          #{in 4280}#
                          '()
                          (list #{out 4279}#
                                (cons '#(syntax-object
                                         let
                                         ((top)
                                          #(ribcage
                                            #(out in e1 e2)
                                            #((top) (top) (top) (top))
                                            #("i4275" "i4276" "i4277" "i4278"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4266")))
                                         (hygiene guile))
                                      (cons '()
                                            (cons #{e1 4281}# #{e2 4282}#))))))
                  #{tmp 4274}#)
                (let ((#{tmp 4284}#
                        ($sc-dispatch
                          #{tmp 4267}#
                          '(_ #(each (any any)) any . each-any))))
                  (if #{tmp 4284}#
                    (@apply
                      (lambda (#{out 4289}#
                               #{in 4290}#
                               #{e1 4291}#
                               #{e2 4292}#)
                        (list '#(syntax-object
                                 syntax-case
                                 ((top)
                                  #(ribcage
                                    #(out in e1 e2)
                                    #((top) (top) (top) (top))
                                    #("i4285" "i4286" "i4287" "i4288"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4266")))
                                 (hygiene guile))
                              (cons '#(syntax-object
                                       list
                                       ((top)
                                        #(ribcage
                                          #(out in e1 e2)
                                          #((top) (top) (top) (top))
                                          #("i4285" "i4286" "i4287" "i4288"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4266")))
                                       (hygiene guile))
                                    #{in 4290}#)
                              '()
                              (list #{out 4289}#
                                    (cons '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage
                                                #(out in e1 e2)
                                                #((top) (top) (top) (top))
                                                #("i4285"
                                                  "i4286"
                                                  "i4287"
                                                  "i4288"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(x)
                                                #((top))
                                                #("i4266")))
                                             (hygiene guile))
                                          (cons '()
                                                (cons #{e1 4291}#
                                                      #{e2 4292}#))))))
                      #{tmp 4284}#)
                    (syntax-violation
                      #f
                      "source expression failed to match any pattern"
                      #{tmp 4267}#)))))))))))

(define syntax-rules
  (make-syntax-transformer
    'syntax-rules
    'macro
    (lambda (#{x 4296}#)
      (let ((#{tmp 4298}# #{x 4296}#))
        (let ((#{tmp 4299}#
                ($sc-dispatch
                  #{tmp 4298}#
                  '(_ each-any . #(each ((any . any) any))))))
          (if #{tmp 4299}#
            (@apply
              (lambda (#{k 4304}#
                       #{keyword 4305}#
                       #{pattern 4306}#
                       #{template 4307}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage
                            #(k keyword pattern template)
                            #((top) (top) (top) (top))
                            #("i4300" "i4301" "i4302" "i4303"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4297")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage
                             #(k keyword pattern template)
                             #((top) (top) (top) (top))
                             #("i4300" "i4301" "i4302" "i4303"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4297")))
                          (hygiene guile)))
                      (vector
                        '(#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage
                               #(k keyword pattern template)
                               #((top) (top) (top) (top))
                               #("i4300" "i4301" "i4302" "i4303"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4297")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            syntax-rules
                            ((top)
                             #(ribcage
                               #(k keyword pattern template)
                               #((top) (top) (top) (top))
                               #("i4300" "i4301" "i4302" "i4303"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4297")))
                            (hygiene guile)))
                        (cons '#(syntax-object
                                 patterns
                                 ((top)
                                  #(ribcage
                                    #(k keyword pattern template)
                                    #((top) (top) (top) (top))
                                    #("i4300" "i4301" "i4302" "i4303"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4297")))
                                 (hygiene guile))
                              #{pattern 4306}#))
                      (cons '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage
                                  #(k keyword pattern template)
                                  #((top) (top) (top) (top))
                                  #("i4300" "i4301" "i4302" "i4303"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4297")))
                               (hygiene guile))
                            (cons '#(syntax-object
                                     x
                                     ((top)
                                      #(ribcage
                                        #(k keyword pattern template)
                                        #((top) (top) (top) (top))
                                        #("i4300" "i4301" "i4302" "i4303"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4297")))
                                     (hygiene guile))
                                  (cons #{k 4304}#
                                        (map (lambda (#{tmp 4311}#
                                                      #{tmp 4310}#)
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
                                                                 #("i4300"
                                                                   "i4301"
                                                                   "i4302"
                                                                   "i4303"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4297")))
                                                              (hygiene guile))
                                                           #{tmp 4310}#)
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
                                                                 #("i4300"
                                                                   "i4301"
                                                                   "i4302"
                                                                   "i4303"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4297")))
                                                              (hygiene guile))
                                                           #{tmp 4311}#)))
                                             #{template 4307}#
                                             #{pattern 4306}#))))))
              #{tmp 4299}#)
            (let ((#{tmp 4312}#
                    ($sc-dispatch
                      #{tmp 4298}#
                      '(_ each-any any . #(each ((any . any) any))))))
              (if (if #{tmp 4312}#
                    (@apply
                      (lambda (#{k 4318}#
                               #{docstring 4319}#
                               #{keyword 4320}#
                               #{pattern 4321}#
                               #{template 4322}#)
                        (string? (syntax->datum #{docstring 4319}#)))
                      #{tmp 4312}#)
                    #f)
                (@apply
                  (lambda (#{k 4328}#
                           #{docstring 4329}#
                           #{keyword 4330}#
                           #{pattern 4331}#
                           #{template 4332}#)
                    (list '#(syntax-object
                             lambda
                             ((top)
                              #(ribcage
                                #(k docstring keyword pattern template)
                                #((top) (top) (top) (top) (top))
                                #("i4323" "i4324" "i4325" "i4326" "i4327"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4297")))
                             (hygiene guile))
                          '(#(syntax-object
                              x
                              ((top)
                               #(ribcage
                                 #(k docstring keyword pattern template)
                                 #((top) (top) (top) (top) (top))
                                 #("i4323" "i4324" "i4325" "i4326" "i4327"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4297")))
                              (hygiene guile)))
                          #{docstring 4329}#
                          (vector
                            '(#(syntax-object
                                macro-type
                                ((top)
                                 #(ribcage
                                   #(k docstring keyword pattern template)
                                   #((top) (top) (top) (top) (top))
                                   #("i4323" "i4324" "i4325" "i4326" "i4327"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4297")))
                                (hygiene guile))
                              .
                              #(syntax-object
                                syntax-rules
                                ((top)
                                 #(ribcage
                                   #(k docstring keyword pattern template)
                                   #((top) (top) (top) (top) (top))
                                   #("i4323" "i4324" "i4325" "i4326" "i4327"))
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4297")))
                                (hygiene guile)))
                            (cons '#(syntax-object
                                     patterns
                                     ((top)
                                      #(ribcage
                                        #(k docstring keyword pattern template)
                                        #((top) (top) (top) (top) (top))
                                        #("i4323"
                                          "i4324"
                                          "i4325"
                                          "i4326"
                                          "i4327"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4297")))
                                     (hygiene guile))
                                  #{pattern 4331}#))
                          (cons '#(syntax-object
                                   syntax-case
                                   ((top)
                                    #(ribcage
                                      #(k docstring keyword pattern template)
                                      #((top) (top) (top) (top) (top))
                                      #("i4323"
                                        "i4324"
                                        "i4325"
                                        "i4326"
                                        "i4327"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i4297")))
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
                                            #("i4323"
                                              "i4324"
                                              "i4325"
                                              "i4326"
                                              "i4327"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4297")))
                                         (hygiene guile))
                                      (cons #{k 4328}#
                                            (map (lambda (#{tmp 4336}#
                                                          #{tmp 4335}#)
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
                                                                     #("i4323"
                                                                       "i4324"
                                                                       "i4325"
                                                                       "i4326"
                                                                       "i4327"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(x)
                                                                     #((top))
                                                                     #("i4297")))
                                                                  (hygiene
                                                                    guile))
                                                               #{tmp 4335}#)
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
                                                                     #("i4323"
                                                                       "i4324"
                                                                       "i4325"
                                                                       "i4326"
                                                                       "i4327"))
                                                                   #(ribcage
                                                                     ()
                                                                     ()
                                                                     ())
                                                                   #(ribcage
                                                                     #(x)
                                                                     #((top))
                                                                     #("i4297")))
                                                                  (hygiene
                                                                    guile))
                                                               #{tmp 4336}#)))
                                                 #{template 4332}#
                                                 #{pattern 4331}#))))))
                  #{tmp 4312}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4298}#)))))))))

(define let*
  (make-syntax-transformer
    'let*
    'macro
    (lambda (#{x 4337}#)
      (let ((#{tmp 4339}# #{x 4337}#))
        (let ((#{tmp 4340}#
                ($sc-dispatch
                  #{tmp 4339}#
                  '(any #(each (any any)) any . each-any))))
          (if (if #{tmp 4340}#
                (@apply
                  (lambda (#{let* 4346}#
                           #{x 4347}#
                           #{v 4348}#
                           #{e1 4349}#
                           #{e2 4350}#)
                    (and-map identifier? #{x 4347}#))
                  #{tmp 4340}#)
                #f)
            (@apply
              (lambda (#{let* 4357}#
                       #{x 4358}#
                       #{v 4359}#
                       #{e1 4360}#
                       #{e2 4361}#)
                (letrec*
                  ((#{f 4364}#
                     (lambda (#{bindings 4365}#)
                       (if (null? #{bindings 4365}#)
                         (cons '#(syntax-object
                                  let
                                  ((top)
                                   #(ribcage () () ())
                                   #(ribcage
                                     #(f bindings)
                                     #((top) (top))
                                     #("i4362" "i4363"))
                                   #(ribcage
                                     #(let* x v e1 e2)
                                     #((top) (top) (top) (top) (top))
                                     #("i4352"
                                       "i4353"
                                       "i4354"
                                       "i4355"
                                       "i4356"))
                                   #(ribcage () () ())
                                   #(ribcage #(x) #((top)) #("i4338")))
                                  (hygiene guile))
                               (cons '() (cons #{e1 4360}# #{e2 4361}#)))
                         (let ((#{tmp 4370}#
                                 (list (#{f 4364}# (cdr #{bindings 4365}#))
                                       (car #{bindings 4365}#))))
                           (let ((#{tmp 4371}#
                                   ($sc-dispatch #{tmp 4370}# '(any any))))
                             (if #{tmp 4371}#
                               (@apply
                                 (lambda (#{body 4374}# #{binding 4375}#)
                                   (list '#(syntax-object
                                            let
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(body binding)
                                               #((top) (top))
                                               #("i4372" "i4373"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(f bindings)
                                               #((top) (top))
                                               #("i4362" "i4363"))
                                             #(ribcage
                                               #(let* x v e1 e2)
                                               #((top) (top) (top) (top) (top))
                                               #("i4352"
                                                 "i4353"
                                                 "i4354"
                                                 "i4355"
                                                 "i4356"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4338")))
                                            (hygiene guile))
                                         (list #{binding 4375}#)
                                         #{body 4374}#))
                                 #{tmp 4371}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4370}#))))))))
                  (begin
                    (#{f 4364}# (map list #{x 4358}# #{v 4359}#)))))
              #{tmp 4340}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4339}#)))))))

(define do
  (make-syntax-transformer
    'do
    'macro
    (lambda (#{orig-x 4376}#)
      (let ((#{tmp 4378}# #{orig-x 4376}#))
        (let ((#{tmp 4379}#
                ($sc-dispatch
                  #{tmp 4378}#
                  '(_ #(each (any any . any))
                      (any . each-any)
                      .
                      each-any))))
          (if #{tmp 4379}#
            (@apply
              (lambda (#{var 4386}#
                       #{init 4387}#
                       #{step 4388}#
                       #{e0 4389}#
                       #{e1 4390}#
                       #{c 4391}#)
                (let ((#{tmp 4393}#
                        (map (lambda (#{v 4414}# #{s 4415}#)
                               (let ((#{tmp 4418}# #{s 4415}#))
                                 (let ((#{tmp 4419}#
                                         ($sc-dispatch #{tmp 4418}# '())))
                                   (if #{tmp 4419}#
                                     (@apply
                                       (lambda () #{v 4414}#)
                                       #{tmp 4419}#)
                                     (let ((#{tmp 4420}#
                                             ($sc-dispatch
                                               #{tmp 4418}#
                                               '(any))))
                                       (if #{tmp 4420}#
                                         (@apply
                                           (lambda (#{e 4422}#) #{e 4422}#)
                                           #{tmp 4420}#)
                                         (let ((#{_ 4424}# #{tmp 4418}#))
                                           (syntax-violation
                                             'do
                                             "bad step expression"
                                             #{orig-x 4376}#
                                             #{s 4415}#))))))))
                             #{var 4386}#
                             #{step 4388}#)))
                  (let ((#{tmp 4394}#
                          ($sc-dispatch #{tmp 4393}# 'each-any)))
                    (if #{tmp 4394}#
                      (@apply
                        (lambda (#{step 4396}#)
                          (let ((#{tmp 4397}# #{e1 4390}#))
                            (let ((#{tmp 4398}#
                                    ($sc-dispatch #{tmp 4397}# '())))
                              (if #{tmp 4398}#
                                (@apply
                                  (lambda ()
                                    (list '#(syntax-object
                                             let
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4395"))
                                              #(ribcage
                                                #(var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4380"
                                                  "i4381"
                                                  "i4382"
                                                  "i4383"
                                                  "i4384"
                                                  "i4385"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4377")))
                                             (hygiene guile))
                                          '#(syntax-object
                                             doloop
                                             ((top)
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(step)
                                                #((top))
                                                #("i4395"))
                                              #(ribcage
                                                #(var init step e0 e1 c)
                                                #((top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top)
                                                  (top))
                                                #("i4380"
                                                  "i4381"
                                                  "i4382"
                                                  "i4383"
                                                  "i4384"
                                                  "i4385"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(orig-x)
                                                #((top))
                                                #("i4377")))
                                             (hygiene guile))
                                          (map list #{var 4386}# #{init 4387}#)
                                          (list '#(syntax-object
                                                   if
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(step)
                                                      #((top))
                                                      #("i4395"))
                                                    #(ribcage
                                                      #(var init step e0 e1 c)
                                                      #((top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top)
                                                        (top))
                                                      #("i4380"
                                                        "i4381"
                                                        "i4382"
                                                        "i4383"
                                                        "i4384"
                                                        "i4385"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(orig-x)
                                                      #((top))
                                                      #("i4377")))
                                                   (hygiene guile))
                                                (list '#(syntax-object
                                                         not
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4395"))
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
                                                            #("i4380"
                                                              "i4381"
                                                              "i4382"
                                                              "i4383"
                                                              "i4384"
                                                              "i4385"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4377")))
                                                         (hygiene guile))
                                                      #{e0 4389}#)
                                                (cons '#(syntax-object
                                                         begin
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(step)
                                                            #((top))
                                                            #("i4395"))
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
                                                            #("i4380"
                                                              "i4381"
                                                              "i4382"
                                                              "i4383"
                                                              "i4384"
                                                              "i4385"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(orig-x)
                                                            #((top))
                                                            #("i4377")))
                                                         (hygiene guile))
                                                      (append
                                                        #{c 4391}#
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
                                                                          #("i4395"))
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
                                                                          #("i4380"
                                                                            "i4381"
                                                                            "i4382"
                                                                            "i4383"
                                                                            "i4384"
                                                                            "i4385"))
                                                                        #(ribcage
                                                                          ()
                                                                          ()
                                                                          ())
                                                                        #(ribcage
                                                                          #(orig-x)
                                                                          #((top))
                                                                          #("i4377")))
                                                                       (hygiene
                                                                         guile))
                                                                    #{step 4396}#)))))))
                                  #{tmp 4398}#)
                                (let ((#{tmp 4403}#
                                        ($sc-dispatch
                                          #{tmp 4397}#
                                          '(any . each-any))))
                                  (if #{tmp 4403}#
                                    (@apply
                                      (lambda (#{e1 4406}# #{e2 4407}#)
                                        (list '#(syntax-object
                                                 let
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4404" "i4405"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4395"))
                                                  #(ribcage
                                                    #(var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4380"
                                                      "i4381"
                                                      "i4382"
                                                      "i4383"
                                                      "i4384"
                                                      "i4385"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4377")))
                                                 (hygiene guile))
                                              '#(syntax-object
                                                 doloop
                                                 ((top)
                                                  #(ribcage
                                                    #(e1 e2)
                                                    #((top) (top))
                                                    #("i4404" "i4405"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(step)
                                                    #((top))
                                                    #("i4395"))
                                                  #(ribcage
                                                    #(var init step e0 e1 c)
                                                    #((top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top)
                                                      (top))
                                                    #("i4380"
                                                      "i4381"
                                                      "i4382"
                                                      "i4383"
                                                      "i4384"
                                                      "i4385"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(orig-x)
                                                    #((top))
                                                    #("i4377")))
                                                 (hygiene guile))
                                              (map list
                                                   #{var 4386}#
                                                   #{init 4387}#)
                                              (list '#(syntax-object
                                                       if
                                                       ((top)
                                                        #(ribcage
                                                          #(e1 e2)
                                                          #((top) (top))
                                                          #("i4404" "i4405"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(step)
                                                          #((top))
                                                          #("i4395"))
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
                                                          #("i4380"
                                                            "i4381"
                                                            "i4382"
                                                            "i4383"
                                                            "i4384"
                                                            "i4385"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(orig-x)
                                                          #((top))
                                                          #("i4377")))
                                                       (hygiene guile))
                                                    #{e0 4389}#
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4404"
                                                                  "i4405"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4395"))
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
                                                                #("i4380"
                                                                  "i4381"
                                                                  "i4382"
                                                                  "i4383"
                                                                  "i4384"
                                                                  "i4385"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4377")))
                                                             (hygiene guile))
                                                          (cons #{e1 4406}#
                                                                #{e2 4407}#))
                                                    (cons '#(syntax-object
                                                             begin
                                                             ((top)
                                                              #(ribcage
                                                                #(e1 e2)
                                                                #((top) (top))
                                                                #("i4404"
                                                                  "i4405"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(step)
                                                                #((top))
                                                                #("i4395"))
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
                                                                #("i4380"
                                                                  "i4381"
                                                                  "i4382"
                                                                  "i4383"
                                                                  "i4384"
                                                                  "i4385"))
                                                              #(ribcage
                                                                ()
                                                                ()
                                                                ())
                                                              #(ribcage
                                                                #(orig-x)
                                                                #((top))
                                                                #("i4377")))
                                                             (hygiene guile))
                                                          (append
                                                            #{c 4391}#
                                                            (list (cons '#(syntax-object
                                                                           doloop
                                                                           ((top)
                                                                            #(ribcage
                                                                              #(e1
                                                                                e2)
                                                                              #((top)
                                                                                (top))
                                                                              #("i4404"
                                                                                "i4405"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(step)
                                                                              #((top))
                                                                              #("i4395"))
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
                                                                              #("i4380"
                                                                                "i4381"
                                                                                "i4382"
                                                                                "i4383"
                                                                                "i4384"
                                                                                "i4385"))
                                                                            #(ribcage
                                                                              ()
                                                                              ()
                                                                              ())
                                                                            #(ribcage
                                                                              #(orig-x)
                                                                              #((top))
                                                                              #("i4377")))
                                                                           (hygiene
                                                                             guile))
                                                                        #{step 4396}#)))))))
                                      #{tmp 4403}#)
                                    (syntax-violation
                                      #f
                                      "source expression failed to match any pattern"
                                      #{tmp 4397}#)))))))
                        #{tmp 4394}#)
                      (syntax-violation
                        #f
                        "source expression failed to match any pattern"
                        #{tmp 4393}#)))))
              #{tmp 4379}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4378}#)))))))

(define quasiquote
  (make-syntax-transformer
    'quasiquote
    'macro
    (letrec*
      ((#{quasi 4428}#
         (lambda (#{p 4441}# #{lev 4442}#)
           (let ((#{tmp 4445}# #{p 4441}#))
             (let ((#{tmp 4446}#
                     ($sc-dispatch
                       #{tmp 4445}#
                       '(#(free-id
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4443" "i4444"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4439"
                                 "i4437"
                                 "i4435"
                                 "i4433"
                                 "i4431"
                                 "i4429"
                                 "i4427")))
                             (hygiene guile)))
                         any))))
               (if #{tmp 4446}#
                 (@apply
                   (lambda (#{p 4448}#)
                     (if (= #{lev 4442}# 0)
                       (list '#(syntax-object
                                "value"
                                ((top)
                                 #(ribcage #(p) #((top)) #("i4447"))
                                 #(ribcage () () ())
                                 #(ribcage
                                   #(p lev)
                                   #((top) (top))
                                   #("i4443" "i4444"))
                                 #(ribcage
                                   (emit quasivector
                                         quasilist*
                                         quasiappend
                                         quasicons
                                         vquasi
                                         quasi)
                                   ((top) (top) (top) (top) (top) (top) (top))
                                   ("i4439"
                                    "i4437"
                                    "i4435"
                                    "i4433"
                                    "i4431"
                                    "i4429"
                                    "i4427")))
                                (hygiene guile))
                             #{p 4448}#)
                       (#{quasicons 4432}#
                         '(#(syntax-object
                             "quote"
                             ((top)
                              #(ribcage #(p) #((top)) #("i4447"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4443" "i4444"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4439"
                                 "i4437"
                                 "i4435"
                                 "i4433"
                                 "i4431"
                                 "i4429"
                                 "i4427")))
                             (hygiene guile))
                           #(syntax-object
                             unquote
                             ((top)
                              #(ribcage #(p) #((top)) #("i4447"))
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4443" "i4444"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4439"
                                 "i4437"
                                 "i4435"
                                 "i4433"
                                 "i4431"
                                 "i4429"
                                 "i4427")))
                             (hygiene guile)))
                         (#{quasi 4428}#
                           (list #{p 4448}#)
                           (#{1-}# #{lev 4442}#)))))
                   #{tmp 4446}#)
                 (let ((#{tmp 4449}#
                         ($sc-dispatch
                           #{tmp 4445}#
                           '(#(free-id
                               #(syntax-object
                                 quasiquote
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage
                                    #(p lev)
                                    #((top) (top))
                                    #("i4443" "i4444"))
                                  #(ribcage
                                    (emit quasivector
                                          quasilist*
                                          quasiappend
                                          quasicons
                                          vquasi
                                          quasi)
                                    ((top) (top) (top) (top) (top) (top) (top))
                                    ("i4439"
                                     "i4437"
                                     "i4435"
                                     "i4433"
                                     "i4431"
                                     "i4429"
                                     "i4427")))
                                 (hygiene guile)))
                             any))))
                   (if #{tmp 4449}#
                     (@apply
                       (lambda (#{p 4451}#)
                         (#{quasicons 4432}#
                           '(#(syntax-object
                               "quote"
                               ((top)
                                #(ribcage #(p) #((top)) #("i4450"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(p lev)
                                  #((top) (top))
                                  #("i4443" "i4444"))
                                #(ribcage
                                  (emit quasivector
                                        quasilist*
                                        quasiappend
                                        quasicons
                                        vquasi
                                        quasi)
                                  ((top) (top) (top) (top) (top) (top) (top))
                                  ("i4439"
                                   "i4437"
                                   "i4435"
                                   "i4433"
                                   "i4431"
                                   "i4429"
                                   "i4427")))
                               (hygiene guile))
                             #(syntax-object
                               quasiquote
                               ((top)
                                #(ribcage #(p) #((top)) #("i4450"))
                                #(ribcage () () ())
                                #(ribcage
                                  #(p lev)
                                  #((top) (top))
                                  #("i4443" "i4444"))
                                #(ribcage
                                  (emit quasivector
                                        quasilist*
                                        quasiappend
                                        quasicons
                                        vquasi
                                        quasi)
                                  ((top) (top) (top) (top) (top) (top) (top))
                                  ("i4439"
                                   "i4437"
                                   "i4435"
                                   "i4433"
                                   "i4431"
                                   "i4429"
                                   "i4427")))
                               (hygiene guile)))
                           (#{quasi 4428}#
                             (list #{p 4451}#)
                             (#{1+}# #{lev 4442}#))))
                       #{tmp 4449}#)
                     (let ((#{tmp 4452}#
                             ($sc-dispatch #{tmp 4445}# '(any . any))))
                       (if #{tmp 4452}#
                         (@apply
                           (lambda (#{p 4455}# #{q 4456}#)
                             (let ((#{tmp 4457}# #{p 4455}#))
                               (let ((#{tmp 4458}#
                                       ($sc-dispatch
                                         #{tmp 4457}#
                                         '(#(free-id
                                             #(syntax-object
                                               unquote
                                               ((top)
                                                #(ribcage
                                                  #(p q)
                                                  #((top) (top))
                                                  #("i4453" "i4454"))
                                                #(ribcage () () ())
                                                #(ribcage
                                                  #(p lev)
                                                  #((top) (top))
                                                  #("i4443" "i4444"))
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
                                                  ("i4439"
                                                   "i4437"
                                                   "i4435"
                                                   "i4433"
                                                   "i4431"
                                                   "i4429"
                                                   "i4427")))
                                               (hygiene guile)))
                                           .
                                           each-any))))
                                 (if #{tmp 4458}#
                                   (@apply
                                     (lambda (#{p 4460}#)
                                       (if (= #{lev 4442}# 0)
                                         (#{quasilist* 4436}#
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
                                                              #("i4453"
                                                                "i4454"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(p lev)
                                                              #((top) (top))
                                                              #("i4443"
                                                                "i4444"))
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
                                                              ("i4439"
                                                               "i4437"
                                                               "i4435"
                                                               "i4433"
                                                               "i4431"
                                                               "i4429"
                                                               "i4427")))
                                                           (hygiene guile))
                                                        #{tmp 4461}#))
                                                #{p 4460}#)
                                           (#{quasi 4428}#
                                             #{q 4456}#
                                             #{lev 4442}#))
                                         (#{quasicons 4432}#
                                           (#{quasicons 4432}#
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
                                                    #("i4453" "i4454"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(p lev)
                                                    #((top) (top))
                                                    #("i4443" "i4444"))
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
                                                    ("i4439"
                                                     "i4437"
                                                     "i4435"
                                                     "i4433"
                                                     "i4431"
                                                     "i4429"
                                                     "i4427")))
                                                 (hygiene guile))
                                               #(syntax-object
                                                 unquote
                                                 ((top)
                                                  #(ribcage
                                                    #(p)
                                                    #((top))
                                                    #("i4459"))
                                                  #(ribcage
                                                    #(p q)
                                                    #((top) (top))
                                                    #("i4453" "i4454"))
                                                  #(ribcage () () ())
                                                  #(ribcage
                                                    #(p lev)
                                                    #((top) (top))
                                                    #("i4443" "i4444"))
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
                                                    ("i4439"
                                                     "i4437"
                                                     "i4435"
                                                     "i4433"
                                                     "i4431"
                                                     "i4429"
                                                     "i4427")))
                                                 (hygiene guile)))
                                             (#{quasi 4428}#
                                               #{p 4460}#
                                               (#{1-}# #{lev 4442}#)))
                                           (#{quasi 4428}#
                                             #{q 4456}#
                                             #{lev 4442}#))))
                                     #{tmp 4458}#)
                                   (let ((#{tmp 4463}#
                                           ($sc-dispatch
                                             #{tmp 4457}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   unquote-splicing
                                                   ((top)
                                                    #(ribcage
                                                      #(p q)
                                                      #((top) (top))
                                                      #("i4453" "i4454"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(p lev)
                                                      #((top) (top))
                                                      #("i4443" "i4444"))
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
                                                      ("i4439"
                                                       "i4437"
                                                       "i4435"
                                                       "i4433"
                                                       "i4431"
                                                       "i4429"
                                                       "i4427")))
                                                   (hygiene guile)))
                                               .
                                               each-any))))
                                     (if #{tmp 4463}#
                                       (@apply
                                         (lambda (#{p 4465}#)
                                           (if (= #{lev 4442}# 0)
                                             (#{quasiappend 4434}#
                                               (map (lambda (#{tmp 4466}#)
                                                      (list '#(syntax-object
                                                               "value"
                                                               ((top)
                                                                #(ribcage
                                                                  #(p)
                                                                  #((top))
                                                                  #("i4464"))
                                                                #(ribcage
                                                                  #(p q)
                                                                  #((top)
                                                                    (top))
                                                                  #("i4453"
                                                                    "i4454"))
                                                                #(ribcage
                                                                  ()
                                                                  ()
                                                                  ())
                                                                #(ribcage
                                                                  #(p lev)
                                                                  #((top)
                                                                    (top))
                                                                  #("i4443"
                                                                    "i4444"))
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
                                                                  ("i4439"
                                                                   "i4437"
                                                                   "i4435"
                                                                   "i4433"
                                                                   "i4431"
                                                                   "i4429"
                                                                   "i4427")))
                                                               (hygiene guile))
                                                            #{tmp 4466}#))
                                                    #{p 4465}#)
                                               (#{quasi 4428}#
                                                 #{q 4456}#
                                                 #{lev 4442}#))
                                             (#{quasicons 4432}#
                                               (#{quasicons 4432}#
                                                 '(#(syntax-object
                                                     "quote"
                                                     ((top)
                                                      #(ribcage
                                                        #(p)
                                                        #((top))
                                                        #("i4464"))
                                                      #(ribcage
                                                        #(p q)
                                                        #((top) (top))
                                                        #("i4453" "i4454"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(p lev)
                                                        #((top) (top))
                                                        #("i4443" "i4444"))
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
                                                        ("i4439"
                                                         "i4437"
                                                         "i4435"
                                                         "i4433"
                                                         "i4431"
                                                         "i4429"
                                                         "i4427")))
                                                     (hygiene guile))
                                                   #(syntax-object
                                                     unquote-splicing
                                                     ((top)
                                                      #(ribcage
                                                        #(p)
                                                        #((top))
                                                        #("i4464"))
                                                      #(ribcage
                                                        #(p q)
                                                        #((top) (top))
                                                        #("i4453" "i4454"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(p lev)
                                                        #((top) (top))
                                                        #("i4443" "i4444"))
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
                                                        ("i4439"
                                                         "i4437"
                                                         "i4435"
                                                         "i4433"
                                                         "i4431"
                                                         "i4429"
                                                         "i4427")))
                                                     (hygiene guile)))
                                                 (#{quasi 4428}#
                                                   #{p 4465}#
                                                   (#{1-}# #{lev 4442}#)))
                                               (#{quasi 4428}#
                                                 #{q 4456}#
                                                 #{lev 4442}#))))
                                         #{tmp 4463}#)
                                       (let ((#{_ 4469}# #{tmp 4457}#))
                                         (#{quasicons 4432}#
                                           (#{quasi 4428}#
                                             #{p 4455}#
                                             #{lev 4442}#)
                                           (#{quasi 4428}#
                                             #{q 4456}#
                                             #{lev 4442}#)))))))))
                           #{tmp 4452}#)
                         (let ((#{tmp 4470}#
                                 ($sc-dispatch
                                   #{tmp 4445}#
                                   '#(vector each-any))))
                           (if #{tmp 4470}#
                             (@apply
                               (lambda (#{x 4472}#)
                                 (#{quasivector 4438}#
                                   (#{vquasi 4430}# #{x 4472}# #{lev 4442}#)))
                               #{tmp 4470}#)
                             (let ((#{p 4475}# #{tmp 4445}#))
                               (list '#(syntax-object
                                        "quote"
                                        ((top)
                                         #(ribcage #(p) #((top)) #("i4474"))
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p lev)
                                           #((top) (top))
                                           #("i4443" "i4444"))
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
                                           ("i4439"
                                            "i4437"
                                            "i4435"
                                            "i4433"
                                            "i4431"
                                            "i4429"
                                            "i4427")))
                                        (hygiene guile))
                                     #{p 4475}#)))))))))))))
       (#{vquasi 4430}#
         (lambda (#{p 4476}# #{lev 4477}#)
           (let ((#{tmp 4480}# #{p 4476}#))
             (let ((#{tmp 4481}#
                     ($sc-dispatch #{tmp 4480}# '(any . any))))
               (if #{tmp 4481}#
                 (@apply
                   (lambda (#{p 4484}# #{q 4485}#)
                     (let ((#{tmp 4486}# #{p 4484}#))
                       (let ((#{tmp 4487}#
                               ($sc-dispatch
                                 #{tmp 4486}#
                                 '(#(free-id
                                     #(syntax-object
                                       unquote
                                       ((top)
                                        #(ribcage
                                          #(p q)
                                          #((top) (top))
                                          #("i4482" "i4483"))
                                        #(ribcage () () ())
                                        #(ribcage
                                          #(p lev)
                                          #((top) (top))
                                          #("i4478" "i4479"))
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
                                          ("i4439"
                                           "i4437"
                                           "i4435"
                                           "i4433"
                                           "i4431"
                                           "i4429"
                                           "i4427")))
                                       (hygiene guile)))
                                   .
                                   each-any))))
                         (if #{tmp 4487}#
                           (@apply
                             (lambda (#{p 4489}#)
                               (if (= #{lev 4477}# 0)
                                 (#{quasilist* 4436}#
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
                                                      #("i4482" "i4483"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(p lev)
                                                      #((top) (top))
                                                      #("i4478" "i4479"))
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
                                                      ("i4439"
                                                       "i4437"
                                                       "i4435"
                                                       "i4433"
                                                       "i4431"
                                                       "i4429"
                                                       "i4427")))
                                                   (hygiene guile))
                                                #{tmp 4490}#))
                                        #{p 4489}#)
                                   (#{vquasi 4430}# #{q 4485}# #{lev 4477}#))
                                 (#{quasicons 4432}#
                                   (#{quasicons 4432}#
                                     '(#(syntax-object
                                         "quote"
                                         ((top)
                                          #(ribcage #(p) #((top)) #("i4488"))
                                          #(ribcage
                                            #(p q)
                                            #((top) (top))
                                            #("i4482" "i4483"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(p lev)
                                            #((top) (top))
                                            #("i4478" "i4479"))
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
                                            ("i4439"
                                             "i4437"
                                             "i4435"
                                             "i4433"
                                             "i4431"
                                             "i4429"
                                             "i4427")))
                                         (hygiene guile))
                                       #(syntax-object
                                         unquote
                                         ((top)
                                          #(ribcage #(p) #((top)) #("i4488"))
                                          #(ribcage
                                            #(p q)
                                            #((top) (top))
                                            #("i4482" "i4483"))
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(p lev)
                                            #((top) (top))
                                            #("i4478" "i4479"))
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
                                            ("i4439"
                                             "i4437"
                                             "i4435"
                                             "i4433"
                                             "i4431"
                                             "i4429"
                                             "i4427")))
                                         (hygiene guile)))
                                     (#{quasi 4428}#
                                       #{p 4489}#
                                       (#{1-}# #{lev 4477}#)))
                                   (#{vquasi 4430}# #{q 4485}# #{lev 4477}#))))
                             #{tmp 4487}#)
                           (let ((#{tmp 4492}#
                                   ($sc-dispatch
                                     #{tmp 4486}#
                                     '(#(free-id
                                         #(syntax-object
                                           unquote-splicing
                                           ((top)
                                            #(ribcage
                                              #(p q)
                                              #((top) (top))
                                              #("i4482" "i4483"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(p lev)
                                              #((top) (top))
                                              #("i4478" "i4479"))
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
                                              ("i4439"
                                               "i4437"
                                               "i4435"
                                               "i4433"
                                               "i4431"
                                               "i4429"
                                               "i4427")))
                                           (hygiene guile)))
                                       .
                                       each-any))))
                             (if #{tmp 4492}#
                               (@apply
                                 (lambda (#{p 4494}#)
                                   (if (= #{lev 4477}# 0)
                                     (#{quasiappend 4434}#
                                       (map (lambda (#{tmp 4495}#)
                                              (list '#(syntax-object
                                                       "value"
                                                       ((top)
                                                        #(ribcage
                                                          #(p)
                                                          #((top))
                                                          #("i4493"))
                                                        #(ribcage
                                                          #(p q)
                                                          #((top) (top))
                                                          #("i4482" "i4483"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(p lev)
                                                          #((top) (top))
                                                          #("i4478" "i4479"))
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
                                                          ("i4439"
                                                           "i4437"
                                                           "i4435"
                                                           "i4433"
                                                           "i4431"
                                                           "i4429"
                                                           "i4427")))
                                                       (hygiene guile))
                                                    #{tmp 4495}#))
                                            #{p 4494}#)
                                       (#{vquasi 4430}#
                                         #{q 4485}#
                                         #{lev 4477}#))
                                     (#{quasicons 4432}#
                                       (#{quasicons 4432}#
                                         '(#(syntax-object
                                             "quote"
                                             ((top)
                                              #(ribcage
                                                #(p)
                                                #((top))
                                                #("i4493"))
                                              #(ribcage
                                                #(p q)
                                                #((top) (top))
                                                #("i4482" "i4483"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(p lev)
                                                #((top) (top))
                                                #("i4478" "i4479"))
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
                                                ("i4439"
                                                 "i4437"
                                                 "i4435"
                                                 "i4433"
                                                 "i4431"
                                                 "i4429"
                                                 "i4427")))
                                             (hygiene guile))
                                           #(syntax-object
                                             unquote-splicing
                                             ((top)
                                              #(ribcage
                                                #(p)
                                                #((top))
                                                #("i4493"))
                                              #(ribcage
                                                #(p q)
                                                #((top) (top))
                                                #("i4482" "i4483"))
                                              #(ribcage () () ())
                                              #(ribcage
                                                #(p lev)
                                                #((top) (top))
                                                #("i4478" "i4479"))
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
                                                ("i4439"
                                                 "i4437"
                                                 "i4435"
                                                 "i4433"
                                                 "i4431"
                                                 "i4429"
                                                 "i4427")))
                                             (hygiene guile)))
                                         (#{quasi 4428}#
                                           #{p 4494}#
                                           (#{1-}# #{lev 4477}#)))
                                       (#{vquasi 4430}#
                                         #{q 4485}#
                                         #{lev 4477}#))))
                                 #{tmp 4492}#)
                               (let ((#{_ 4498}# #{tmp 4486}#))
                                 (#{quasicons 4432}#
                                   (#{quasi 4428}# #{p 4484}# #{lev 4477}#)
                                   (#{vquasi 4430}#
                                     #{q 4485}#
                                     #{lev 4477}#)))))))))
                   #{tmp 4481}#)
                 (let ((#{tmp 4499}# ($sc-dispatch #{tmp 4480}# '())))
                   (if #{tmp 4499}#
                     (@apply
                       (lambda ()
                         '(#(syntax-object
                             "quote"
                             ((top)
                              #(ribcage () () ())
                              #(ribcage
                                #(p lev)
                                #((top) (top))
                                #("i4478" "i4479"))
                              #(ribcage
                                (emit quasivector
                                      quasilist*
                                      quasiappend
                                      quasicons
                                      vquasi
                                      quasi)
                                ((top) (top) (top) (top) (top) (top) (top))
                                ("i4439"
                                 "i4437"
                                 "i4435"
                                 "i4433"
                                 "i4431"
                                 "i4429"
                                 "i4427")))
                             (hygiene guile))
                           ()))
                       #{tmp 4499}#)
                     (syntax-violation
                       #f
                       "source expression failed to match any pattern"
                       #{tmp 4480}#))))))))
       (#{quasicons 4432}#
         (lambda (#{x 4500}# #{y 4501}#)
           (let ((#{tmp 4505}# (list #{x 4500}# #{y 4501}#)))
             (let ((#{tmp 4506}#
                     ($sc-dispatch #{tmp 4505}# '(any any))))
               (if #{tmp 4506}#
                 (@apply
                   (lambda (#{x 4509}# #{y 4510}#)
                     (let ((#{tmp 4511}# #{y 4510}#))
                       (let ((#{tmp 4512}#
                               ($sc-dispatch
                                 #{tmp 4511}#
                                 '(#(atom "quote") any))))
                         (if #{tmp 4512}#
                           (@apply
                             (lambda (#{dy 4514}#)
                               (let ((#{tmp 4515}# #{x 4509}#))
                                 (let ((#{tmp 4516}#
                                         ($sc-dispatch
                                           #{tmp 4515}#
                                           '(#(atom "quote") any))))
                                   (if #{tmp 4516}#
                                     (@apply
                                       (lambda (#{dx 4518}#)
                                         (list '#(syntax-object
                                                  "quote"
                                                  ((top)
                                                   #(ribcage
                                                     #(dx)
                                                     #((top))
                                                     #("i4517"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4513"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4507" "i4508"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4502" "i4503"))
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
                                                     ("i4439"
                                                      "i4437"
                                                      "i4435"
                                                      "i4433"
                                                      "i4431"
                                                      "i4429"
                                                      "i4427")))
                                                  (hygiene guile))
                                               (cons #{dx 4518}# #{dy 4514}#)))
                                       #{tmp 4516}#)
                                     (let ((#{_ 4520}# #{tmp 4515}#))
                                       (if (null? #{dy 4514}#)
                                         (list '#(syntax-object
                                                  "list"
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4519"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4513"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4507" "i4508"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4502" "i4503"))
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
                                                     ("i4439"
                                                      "i4437"
                                                      "i4435"
                                                      "i4433"
                                                      "i4431"
                                                      "i4429"
                                                      "i4427")))
                                                  (hygiene guile))
                                               #{x 4509}#)
                                         (list '#(syntax-object
                                                  "list*"
                                                  ((top)
                                                   #(ribcage
                                                     #(_)
                                                     #((top))
                                                     #("i4519"))
                                                   #(ribcage
                                                     #(dy)
                                                     #((top))
                                                     #("i4513"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4507" "i4508"))
                                                   #(ribcage () () ())
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x y)
                                                     #((top) (top))
                                                     #("i4502" "i4503"))
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
                                                     ("i4439"
                                                      "i4437"
                                                      "i4435"
                                                      "i4433"
                                                      "i4431"
                                                      "i4429"
                                                      "i4427")))
                                                  (hygiene guile))
                                               #{x 4509}#
                                               #{y 4510}#)))))))
                             #{tmp 4512}#)
                           (let ((#{tmp 4521}#
                                   ($sc-dispatch
                                     #{tmp 4511}#
                                     '(#(atom "list") . any))))
                             (if #{tmp 4521}#
                               (@apply
                                 (lambda (#{stuff 4523}#)
                                   (cons '#(syntax-object
                                            "list"
                                            ((top)
                                             #(ribcage
                                               #(stuff)
                                               #((top))
                                               #("i4522"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4507" "i4508"))
                                             #(ribcage () () ())
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4502" "i4503"))
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
                                               ("i4439"
                                                "i4437"
                                                "i4435"
                                                "i4433"
                                                "i4431"
                                                "i4429"
                                                "i4427")))
                                            (hygiene guile))
                                         (cons #{x 4509}# #{stuff 4523}#)))
                                 #{tmp 4521}#)
                               (let ((#{tmp 4524}#
                                       ($sc-dispatch
                                         #{tmp 4511}#
                                         '(#(atom "list*") . any))))
                                 (if #{tmp 4524}#
                                   (@apply
                                     (lambda (#{stuff 4526}#)
                                       (cons '#(syntax-object
                                                "list*"
                                                ((top)
                                                 #(ribcage
                                                   #(stuff)
                                                   #((top))
                                                   #("i4525"))
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x y)
                                                   #((top) (top))
                                                   #("i4507" "i4508"))
                                                 #(ribcage () () ())
                                                 #(ribcage () () ())
                                                 #(ribcage
                                                   #(x y)
                                                   #((top) (top))
                                                   #("i4502" "i4503"))
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
                                                   ("i4439"
                                                    "i4437"
                                                    "i4435"
                                                    "i4433"
                                                    "i4431"
                                                    "i4429"
                                                    "i4427")))
                                                (hygiene guile))
                                             (cons #{x 4509}# #{stuff 4526}#)))
                                     #{tmp 4524}#)
                                   (let ((#{_ 4528}# #{tmp 4511}#))
                                     (list '#(syntax-object
                                              "list*"
                                              ((top)
                                               #(ribcage
                                                 #(_)
                                                 #((top))
                                                 #("i4527"))
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i4507" "i4508"))
                                               #(ribcage () () ())
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x y)
                                                 #((top) (top))
                                                 #("i4502" "i4503"))
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
                                                 ("i4439"
                                                  "i4437"
                                                  "i4435"
                                                  "i4433"
                                                  "i4431"
                                                  "i4429"
                                                  "i4427")))
                                              (hygiene guile))
                                           #{x 4509}#
                                           #{y 4510}#))))))))))
                   #{tmp 4506}#)
                 (syntax-violation
                   #f
                   "source expression failed to match any pattern"
                   #{tmp 4505}#))))))
       (#{quasiappend 4434}#
         (lambda (#{x 4529}# #{y 4530}#)
           (let ((#{tmp 4533}# #{y 4530}#))
             (let ((#{tmp 4534}#
                     ($sc-dispatch #{tmp 4533}# '(#(atom "quote") ()))))
               (if #{tmp 4534}#
                 (@apply
                   (lambda ()
                     (if (null? #{x 4529}#)
                       '(#(syntax-object
                           "quote"
                           ((top)
                            #(ribcage () () ())
                            #(ribcage
                              #(x y)
                              #((top) (top))
                              #("i4531" "i4532"))
                            #(ribcage
                              (emit quasivector
                                    quasilist*
                                    quasiappend
                                    quasicons
                                    vquasi
                                    quasi)
                              ((top) (top) (top) (top) (top) (top) (top))
                              ("i4439"
                               "i4437"
                               "i4435"
                               "i4433"
                               "i4431"
                               "i4429"
                               "i4427")))
                           (hygiene guile))
                         ())
                       (if (null? (cdr #{x 4529}#))
                         (car #{x 4529}#)
                         (let ((#{tmp 4541}# #{x 4529}#))
                           (let ((#{tmp 4542}#
                                   ($sc-dispatch #{tmp 4541}# 'each-any)))
                             (if #{tmp 4542}#
                               (@apply
                                 (lambda (#{p 4544}#)
                                   (cons '#(syntax-object
                                            "append"
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(p)
                                               #((top))
                                               #("i4543"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x y)
                                               #((top) (top))
                                               #("i4531" "i4532"))
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
                                               ("i4439"
                                                "i4437"
                                                "i4435"
                                                "i4433"
                                                "i4431"
                                                "i4429"
                                                "i4427")))
                                            (hygiene guile))
                                         #{p 4544}#))
                                 #{tmp 4542}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4541}#)))))))
                   #{tmp 4534}#)
                 (let ((#{_ 4547}# #{tmp 4533}#))
                   (if (null? #{x 4529}#)
                     #{y 4530}#
                     (let ((#{tmp 4552}# (list #{x 4529}# #{y 4530}#)))
                       (let ((#{tmp 4553}#
                               ($sc-dispatch #{tmp 4552}# '(each-any any))))
                         (if #{tmp 4553}#
                           (@apply
                             (lambda (#{p 4556}# #{y 4557}#)
                               (cons '#(syntax-object
                                        "append"
                                        ((top)
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(p y)
                                           #((top) (top))
                                           #("i4554" "i4555"))
                                         #(ribcage #(_) #((top)) #("i4546"))
                                         #(ribcage () () ())
                                         #(ribcage
                                           #(x y)
                                           #((top) (top))
                                           #("i4531" "i4532"))
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
                                           ("i4439"
                                            "i4437"
                                            "i4435"
                                            "i4433"
                                            "i4431"
                                            "i4429"
                                            "i4427")))
                                        (hygiene guile))
                                     (append #{p 4556}# (list #{y 4557}#))))
                             #{tmp 4553}#)
                           (syntax-violation
                             #f
                             "source expression failed to match any pattern"
                             #{tmp 4552}#)))))))))))
       (#{quasilist* 4436}#
         (lambda (#{x 4559}# #{y 4560}#)
           (letrec*
             ((#{f 4565}#
                (lambda (#{x 4566}#)
                  (if (null? #{x 4566}#)
                    #{y 4560}#
                    (#{quasicons 4432}#
                      (car #{x 4566}#)
                      (#{f 4565}# (cdr #{x 4566}#)))))))
             (begin (#{f 4565}# #{x 4559}#)))))
       (#{quasivector 4438}#
         (lambda (#{x 4567}#)
           (let ((#{tmp 4569}# #{x 4567}#))
             (let ((#{tmp 4570}#
                     ($sc-dispatch
                       #{tmp 4569}#
                       '(#(atom "quote") each-any))))
               (if #{tmp 4570}#
                 (@apply
                   (lambda (#{x 4572}#)
                     (list '#(syntax-object
                              "quote"
                              ((top)
                               #(ribcage #(x) #((top)) #("i4571"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4568"))
                               #(ribcage
                                 (emit quasivector
                                       quasilist*
                                       quasiappend
                                       quasicons
                                       vquasi
                                       quasi)
                                 ((top) (top) (top) (top) (top) (top) (top))
                                 ("i4439"
                                  "i4437"
                                  "i4435"
                                  "i4433"
                                  "i4431"
                                  "i4429"
                                  "i4427")))
                              (hygiene guile))
                           (list->vector #{x 4572}#)))
                   #{tmp 4570}#)
                 (let ((#{_ 4575}# #{tmp 4569}#))
                   (letrec*
                     ((#{f 4579}#
                        (lambda (#{y 4580}# #{k 4581}#)
                          (let ((#{tmp 4592}# #{y 4580}#))
                            (let ((#{tmp 4593}#
                                    ($sc-dispatch
                                      #{tmp 4592}#
                                      '(#(atom "quote") each-any))))
                              (if #{tmp 4593}#
                                (@apply
                                  (lambda (#{y 4595}#)
                                    (#{k 4581}#
                                      (map (lambda (#{tmp 4596}#)
                                             (list '#(syntax-object
                                                      "quote"
                                                      ((top)
                                                       #(ribcage
                                                         #(y)
                                                         #((top))
                                                         #("i4594"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(f y k)
                                                         #((top) (top) (top))
                                                         #("i4576"
                                                           "i4577"
                                                           "i4578"))
                                                       #(ribcage
                                                         #(_)
                                                         #((top))
                                                         #("i4574"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4568"))
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
                                                         ("i4439"
                                                          "i4437"
                                                          "i4435"
                                                          "i4433"
                                                          "i4431"
                                                          "i4429"
                                                          "i4427")))
                                                      (hygiene guile))
                                                   #{tmp 4596}#))
                                           #{y 4595}#)))
                                  #{tmp 4593}#)
                                (let ((#{tmp 4597}#
                                        ($sc-dispatch
                                          #{tmp 4592}#
                                          '(#(atom "list") . each-any))))
                                  (if #{tmp 4597}#
                                    (@apply
                                      (lambda (#{y 4599}#)
                                        (#{k 4581}# #{y 4599}#))
                                      #{tmp 4597}#)
                                    (let ((#{tmp 4601}#
                                            ($sc-dispatch
                                              #{tmp 4592}#
                                              '(#(atom "list*")
                                                .
                                                #(each+ any (any) ())))))
                                      (if #{tmp 4601}#
                                        (@apply
                                          (lambda (#{y 4604}# #{z 4605}#)
                                            (#{f 4579}#
                                              #{z 4605}#
                                              (lambda (#{ls 4606}#)
                                                (#{k 4581}#
                                                  (append
                                                    #{y 4604}#
                                                    #{ls 4606}#)))))
                                          #{tmp 4601}#)
                                        (let ((#{else 4610}# #{tmp 4592}#))
                                          (let ((#{tmp 4614}# #{x 4567}#))
                                            (let ((#{ g4611 4616}#
                                                    #{tmp 4614}#))
                                              (list '#(syntax-object
                                                       "list->vector"
                                                       ((top)
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(#{ g4611}#)
                                                          #((m4612 top))
                                                          #("i4615"))
                                                        #(ribcage
                                                          #(else)
                                                          #((top))
                                                          #("i4609"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(f y k)
                                                          #((top) (top) (top))
                                                          #("i4576"
                                                            "i4577"
                                                            "i4578"))
                                                        #(ribcage
                                                          #(_)
                                                          #((top))
                                                          #("i4574"))
                                                        #(ribcage () () ())
                                                        #(ribcage
                                                          #(x)
                                                          #((top))
                                                          #("i4568"))
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
                                                          ("i4439"
                                                           "i4437"
                                                           "i4435"
                                                           "i4433"
                                                           "i4431"
                                                           "i4429"
                                                           "i4427")))
                                                       (hygiene guile))
                                                    #{ g4611 4616}#))))))))))))))
                     (begin
                       (#{f 4579}#
                         #{x 4567}#
                         (lambda (#{ls 4582}#)
                           (let ((#{tmp 4587}# #{ls 4582}#))
                             (let ((#{tmp 4588}#
                                     ($sc-dispatch #{tmp 4587}# 'each-any)))
                               (if #{tmp 4588}#
                                 (@apply
                                   (lambda (#{ g4584 4590}#)
                                     (cons '#(syntax-object
                                              "vector"
                                              ((top)
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(#{ g4584}#)
                                                 #((m4585 top))
                                                 #("i4589"))
                                               #(ribcage () () ())
                                               #(ribcage () () ())
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(ls)
                                                 #((top))
                                                 #("i4583"))
                                               #(ribcage
                                                 #(_)
                                                 #((top))
                                                 #("i4574"))
                                               #(ribcage () () ())
                                               #(ribcage
                                                 #(x)
                                                 #((top))
                                                 #("i4568"))
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
                                                 ("i4439"
                                                  "i4437"
                                                  "i4435"
                                                  "i4433"
                                                  "i4431"
                                                  "i4429"
                                                  "i4427")))
                                              (hygiene guile))
                                           #{ g4584 4590}#))
                                   #{tmp 4588}#)
                                 (syntax-violation
                                   #f
                                   "source expression failed to match any pattern"
                                   #{tmp 4587}#))))))))))))))
       (#{emit 4440}#
         (lambda (#{x 4617}#)
           (let ((#{tmp 4619}# #{x 4617}#))
             (let ((#{tmp 4620}#
                     ($sc-dispatch
                       #{tmp 4619}#
                       '(#(atom "quote") any))))
               (if #{tmp 4620}#
                 (@apply
                   (lambda (#{x 4622}#)
                     (list '#(syntax-object
                              quote
                              ((top)
                               #(ribcage #(x) #((top)) #("i4621"))
                               #(ribcage () () ())
                               #(ribcage #(x) #((top)) #("i4618"))
                               #(ribcage
                                 (emit quasivector
                                       quasilist*
                                       quasiappend
                                       quasicons
                                       vquasi
                                       quasi)
                                 ((top) (top) (top) (top) (top) (top) (top))
                                 ("i4439"
                                  "i4437"
                                  "i4435"
                                  "i4433"
                                  "i4431"
                                  "i4429"
                                  "i4427")))
                              (hygiene guile))
                           #{x 4622}#))
                   #{tmp 4620}#)
                 (let ((#{tmp 4623}#
                         ($sc-dispatch
                           #{tmp 4619}#
                           '(#(atom "list") . each-any))))
                   (if #{tmp 4623}#
                     (@apply
                       (lambda (#{x 4625}#)
                         (let ((#{tmp 4629}# (map #{emit 4440}# #{x 4625}#)))
                           (let ((#{tmp 4630}#
                                   ($sc-dispatch #{tmp 4629}# 'each-any)))
                             (if #{tmp 4630}#
                               (@apply
                                 (lambda (#{ g4626 4632}#)
                                   (cons '#(syntax-object
                                            list
                                            ((top)
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(#{ g4626}#)
                                               #((m4627 top))
                                               #("i4631"))
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4624"))
                                             #(ribcage () () ())
                                             #(ribcage
                                               #(x)
                                               #((top))
                                               #("i4618"))
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
                                               ("i4439"
                                                "i4437"
                                                "i4435"
                                                "i4433"
                                                "i4431"
                                                "i4429"
                                                "i4427")))
                                            (hygiene guile))
                                         #{ g4626 4632}#))
                                 #{tmp 4630}#)
                               (syntax-violation
                                 #f
                                 "source expression failed to match any pattern"
                                 #{tmp 4629}#)))))
                       #{tmp 4623}#)
                     (let ((#{tmp 4635}#
                             ($sc-dispatch
                               #{tmp 4619}#
                               '(#(atom "list*") . #(each+ any (any) ())))))
                       (if #{tmp 4635}#
                         (@apply
                           (lambda (#{x 4638}# #{y 4639}#)
                             (letrec*
                               ((#{f 4642}#
                                  (lambda (#{x* 4643}#)
                                    (if (null? #{x* 4643}#)
                                      (#{emit 4440}# #{y 4639}#)
                                      (let ((#{tmp 4649}#
                                              (list (#{emit 4440}#
                                                      (car #{x* 4643}#))
                                                    (#{f 4642}#
                                                      (cdr #{x* 4643}#)))))
                                        (let ((#{tmp 4650}#
                                                ($sc-dispatch
                                                  #{tmp 4649}#
                                                  '(any any))))
                                          (if #{tmp 4650}#
                                            (@apply
                                              (lambda (#{ g4646 4653}#
                                                       #{ g4645 4654}#)
                                                (list '#(syntax-object
                                                         cons
                                                         ((top)
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(#{ g4646}#
                                                              #{ g4645}#)
                                                            #((m4647 top)
                                                              (m4647 top))
                                                            #("i4651" "i4652"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(f x*)
                                                            #((top) (top))
                                                            #("i4640" "i4641"))
                                                          #(ribcage
                                                            #(x y)
                                                            #((top) (top))
                                                            #("i4636" "i4637"))
                                                          #(ribcage () () ())
                                                          #(ribcage
                                                            #(x)
                                                            #((top))
                                                            #("i4618"))
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
                                                            ("i4439"
                                                             "i4437"
                                                             "i4435"
                                                             "i4433"
                                                             "i4431"
                                                             "i4429"
                                                             "i4427")))
                                                         (hygiene guile))
                                                      #{ g4646 4653}#
                                                      #{ g4645 4654}#))
                                              #{tmp 4650}#)
                                            (syntax-violation
                                              #f
                                              "source expression failed to match any pattern"
                                              #{tmp 4649}#))))))))
                               (begin (#{f 4642}# #{x 4638}#))))
                           #{tmp 4635}#)
                         (let ((#{tmp 4655}#
                                 ($sc-dispatch
                                   #{tmp 4619}#
                                   '(#(atom "append") . each-any))))
                           (if #{tmp 4655}#
                             (@apply
                               (lambda (#{x 4657}#)
                                 (let ((#{tmp 4661}#
                                         (map #{emit 4440}# #{x 4657}#)))
                                   (let ((#{tmp 4662}#
                                           ($sc-dispatch
                                             #{tmp 4661}#
                                             'each-any)))
                                     (if #{tmp 4662}#
                                       (@apply
                                         (lambda (#{ g4658 4664}#)
                                           (cons '#(syntax-object
                                                    append
                                                    ((top)
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(#{ g4658}#)
                                                       #((m4659 top))
                                                       #("i4663"))
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4656"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4618"))
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
                                                       ("i4439"
                                                        "i4437"
                                                        "i4435"
                                                        "i4433"
                                                        "i4431"
                                                        "i4429"
                                                        "i4427")))
                                                    (hygiene guile))
                                                 #{ g4658 4664}#))
                                         #{tmp 4662}#)
                                       (syntax-violation
                                         #f
                                         "source expression failed to match any pattern"
                                         #{tmp 4661}#)))))
                               #{tmp 4655}#)
                             (let ((#{tmp 4667}#
                                     ($sc-dispatch
                                       #{tmp 4619}#
                                       '(#(atom "vector") . each-any))))
                               (if #{tmp 4667}#
                                 (@apply
                                   (lambda (#{x 4669}#)
                                     (let ((#{tmp 4673}#
                                             (map #{emit 4440}# #{x 4669}#)))
                                       (let ((#{tmp 4674}#
                                               ($sc-dispatch
                                                 #{tmp 4673}#
                                                 'each-any)))
                                         (if #{tmp 4674}#
                                           (@apply
                                             (lambda (#{ g4670 4676}#)
                                               (cons '#(syntax-object
                                                        vector
                                                        ((top)
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(#{ g4670}#)
                                                           #((m4671 top))
                                                           #("i4675"))
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4668"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4618"))
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
                                                           ("i4439"
                                                            "i4437"
                                                            "i4435"
                                                            "i4433"
                                                            "i4431"
                                                            "i4429"
                                                            "i4427")))
                                                        (hygiene guile))
                                                     #{ g4670 4676}#))
                                             #{tmp 4674}#)
                                           (syntax-violation
                                             #f
                                             "source expression failed to match any pattern"
                                             #{tmp 4673}#)))))
                                   #{tmp 4667}#)
                                 (let ((#{tmp 4679}#
                                         ($sc-dispatch
                                           #{tmp 4619}#
                                           '(#(atom "list->vector") any))))
                                   (if #{tmp 4679}#
                                     (@apply
                                       (lambda (#{x 4681}#)
                                         (let ((#{tmp 4685}#
                                                 (#{emit 4440}# #{x 4681}#)))
                                           (let ((#{ g4682 4687}#
                                                   #{tmp 4685}#))
                                             (list '#(syntax-object
                                                      list->vector
                                                      ((top)
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(#{ g4682}#)
                                                         #((m4683 top))
                                                         #("i4686"))
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4680"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4618"))
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
                                                         ("i4439"
                                                          "i4437"
                                                          "i4435"
                                                          "i4433"
                                                          "i4431"
                                                          "i4429"
                                                          "i4427")))
                                                      (hygiene guile))
                                                   #{ g4682 4687}#))))
                                       #{tmp 4679}#)
                                     (let ((#{tmp 4688}#
                                             ($sc-dispatch
                                               #{tmp 4619}#
                                               '(#(atom "value") any))))
                                       (if #{tmp 4688}#
                                         (@apply
                                           (lambda (#{x 4690}#) #{x 4690}#)
                                           #{tmp 4688}#)
                                         (syntax-violation
                                           #f
                                           "source expression failed to match any pattern"
                                           #{tmp 4619}#)))))))))))))))))))
      (begin
        (lambda (#{x 4691}#)
          (let ((#{tmp 4693}# #{x 4691}#))
            (let ((#{tmp 4694}#
                    ($sc-dispatch #{tmp 4693}# '(_ any))))
              (if #{tmp 4694}#
                (@apply
                  (lambda (#{e 4696}#)
                    (#{emit 4440}# (#{quasi 4428}# #{e 4696}# 0)))
                  #{tmp 4694}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4693}#)))))))))

(define include
  (make-syntax-transformer
    'include
    'macro
    (lambda (#{x 4697}#)
      (letrec*
        ((#{read-file 4700}#
           (lambda (#{fn 4701}# #{k 4702}#)
             (begin
               (let ((#{p 4706}# (open-input-file #{fn 4701}#)))
                 (letrec*
                   ((#{f 4710}#
                      (lambda (#{x 4711}# #{result 4712}#)
                        (if (eof-object? #{x 4711}#)
                          (begin
                            (close-input-port #{p 4706}#)
                            (reverse #{result 4712}#))
                          (#{f 4710}#
                            (read #{p 4706}#)
                            (cons (datum->syntax #{k 4702}# #{x 4711}#)
                                  #{result 4712}#))))))
                   (begin (#{f 4710}# (read #{p 4706}#) '()))))))))
        (begin
          (let ((#{tmp 4713}# #{x 4697}#))
            (let ((#{tmp 4714}#
                    ($sc-dispatch #{tmp 4713}# '(any any))))
              (if #{tmp 4714}#
                (@apply
                  (lambda (#{k 4717}# #{filename 4718}#)
                    (begin
                      (let ((#{fn 4720}# (syntax->datum #{filename 4718}#)))
                        (let ((#{tmp 4722}#
                                (#{read-file 4700}#
                                  #{fn 4720}#
                                  #{filename 4718}#)))
                          (let ((#{tmp 4723}#
                                  ($sc-dispatch #{tmp 4722}# 'each-any)))
                            (if #{tmp 4723}#
                              (@apply
                                (lambda (#{exp 4725}#)
                                  (cons '#(syntax-object
                                           begin
                                           ((top)
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(exp)
                                              #((top))
                                              #("i4724"))
                                            #(ribcage () () ())
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(fn)
                                              #((top))
                                              #("i4719"))
                                            #(ribcage
                                              #(k filename)
                                              #((top) (top))
                                              #("i4715" "i4716"))
                                            #(ribcage
                                              (read-file)
                                              ((top))
                                              ("i4699"))
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4698")))
                                           (hygiene guile))
                                        #{exp 4725}#))
                                #{tmp 4723}#)
                              (syntax-violation
                                #f
                                "source expression failed to match any pattern"
                                #{tmp 4722}#)))))))
                  #{tmp 4714}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4713}#)))))))))

(define include-from-path
  (make-syntax-transformer
    'include-from-path
    'macro
    (lambda (#{x 4727}#)
      (let ((#{tmp 4729}# #{x 4727}#))
        (let ((#{tmp 4730}#
                ($sc-dispatch #{tmp 4729}# '(any any))))
          (if #{tmp 4730}#
            (@apply
              (lambda (#{k 4733}# #{filename 4734}#)
                (begin
                  (let ((#{fn 4736}# (syntax->datum #{filename 4734}#)))
                    (let ((#{tmp 4738}#
                            (datum->syntax
                              #{filename 4734}#
                              (begin
                                (let ((#{t 4743}#
                                        (%search-load-path #{fn 4736}#)))
                                  (if #{t 4743}#
                                    #{t 4743}#
                                    (syntax-violation
                                      'include-from-path
                                      "file not found in path"
                                      #{x 4727}#
                                      #{filename 4734}#)))))))
                      (let ((#{fn 4740}# #{tmp 4738}#))
                        (list '#(syntax-object
                                 include
                                 ((top)
                                  #(ribcage () () ())
                                  #(ribcage #(fn) #((top)) #("i4739"))
                                  #(ribcage () () ())
                                  #(ribcage () () ())
                                  #(ribcage #(fn) #((top)) #("i4735"))
                                  #(ribcage
                                    #(k filename)
                                    #((top) (top))
                                    #("i4731" "i4732"))
                                  #(ribcage () () ())
                                  #(ribcage #(x) #((top)) #("i4728")))
                                 (hygiene guile))
                              #{fn 4740}#))))))
              #{tmp 4730}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4729}#)))))))

(define unquote
  (make-syntax-transformer
    'unquote
    'macro
    (lambda (#{x 4745}#)
      (syntax-violation
        'unquote
        "expression not valid outside of quasiquote"
        #{x 4745}#))))

(define unquote-splicing
  (make-syntax-transformer
    'unquote-splicing
    'macro
    (lambda (#{x 4747}#)
      (syntax-violation
        'unquote-splicing
        "expression not valid outside of quasiquote"
        #{x 4747}#))))

(define case
  (make-syntax-transformer
    'case
    'macro
    (lambda (#{x 4749}#)
      (let ((#{tmp 4751}# #{x 4749}#))
        (let ((#{tmp 4752}#
                ($sc-dispatch
                  #{tmp 4751}#
                  '(_ any any . each-any))))
          (if #{tmp 4752}#
            (@apply
              (lambda (#{e 4756}# #{m1 4757}# #{m2 4758}#)
                (let ((#{tmp 4760}#
                        (letrec*
                          ((#{f 4766}#
                             (lambda (#{clause 4767}# #{clauses 4768}#)
                               (if (null? #{clauses 4768}#)
                                 (let ((#{tmp 4770}# #{clause 4767}#))
                                   (let ((#{tmp 4771}#
                                           ($sc-dispatch
                                             #{tmp 4770}#
                                             '(#(free-id
                                                 #(syntax-object
                                                   else
                                                   ((top)
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(f clause clauses)
                                                      #((top) (top) (top))
                                                      #("i4763"
                                                        "i4764"
                                                        "i4765"))
                                                    #(ribcage
                                                      #(e m1 m2)
                                                      #((top) (top) (top))
                                                      #("i4753"
                                                        "i4754"
                                                        "i4755"))
                                                    #(ribcage () () ())
                                                    #(ribcage
                                                      #(x)
                                                      #((top))
                                                      #("i4750")))
                                                   (hygiene guile)))
                                               any
                                               .
                                               each-any))))
                                     (if #{tmp 4771}#
                                       (@apply
                                         (lambda (#{e1 4774}# #{e2 4775}#)
                                           (cons '#(syntax-object
                                                    begin
                                                    ((top)
                                                     #(ribcage
                                                       #(e1 e2)
                                                       #((top) (top))
                                                       #("i4772" "i4773"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(f clause clauses)
                                                       #((top) (top) (top))
                                                       #("i4763"
                                                         "i4764"
                                                         "i4765"))
                                                     #(ribcage
                                                       #(e m1 m2)
                                                       #((top) (top) (top))
                                                       #("i4753"
                                                         "i4754"
                                                         "i4755"))
                                                     #(ribcage () () ())
                                                     #(ribcage
                                                       #(x)
                                                       #((top))
                                                       #("i4750")))
                                                    (hygiene guile))
                                                 (cons #{e1 4774}#
                                                       #{e2 4775}#)))
                                         #{tmp 4771}#)
                                       (let ((#{tmp 4777}#
                                               ($sc-dispatch
                                                 #{tmp 4770}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp 4777}#
                                           (@apply
                                             (lambda (#{k 4781}#
                                                      #{e1 4782}#
                                                      #{e2 4783}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4778"
                                                             "i4779"
                                                             "i4780"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4763"
                                                             "i4764"
                                                             "i4765"))
                                                         #(ribcage
                                                           #(e m1 m2)
                                                           #((top) (top) (top))
                                                           #("i4753"
                                                             "i4754"
                                                             "i4755"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4750")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4778"
                                                                   "i4779"
                                                                   "i4780"))
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
                                                                 #("i4763"
                                                                   "i4764"
                                                                   "i4765"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4753"
                                                                   "i4754"
                                                                   "i4755"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4750")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4778"
                                                                   "i4779"
                                                                   "i4780"))
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
                                                                 #("i4763"
                                                                   "i4764"
                                                                   "i4765"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4753"
                                                                   "i4754"
                                                                   "i4755"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4750")))
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
                                                                       #("i4778"
                                                                         "i4779"
                                                                         "i4780"))
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
                                                                       #("i4763"
                                                                         "i4764"
                                                                         "i4765"))
                                                                     #(ribcage
                                                                       #(e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4753"
                                                                         "i4754"
                                                                         "i4755"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4750")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k 4781}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4778"
                                                                   "i4779"
                                                                   "i4780"))
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
                                                                 #("i4763"
                                                                   "i4764"
                                                                   "i4765"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4753"
                                                                   "i4754"
                                                                   "i4755"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4750")))
                                                              (hygiene guile))
                                                           (cons #{e1 4782}#
                                                                 #{e2 4783}#))))
                                             #{tmp 4777}#)
                                           (let ((#{_ 4787}# #{tmp 4770}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x 4749}#
                                               #{clause 4767}#)))))))
                                 (let ((#{tmp 4789}#
                                         (#{f 4766}#
                                           (car #{clauses 4768}#)
                                           (cdr #{clauses 4768}#))))
                                   (let ((#{rest 4791}# #{tmp 4789}#))
                                     (let ((#{tmp 4792}# #{clause 4767}#))
                                       (let ((#{tmp 4793}#
                                               ($sc-dispatch
                                                 #{tmp 4792}#
                                                 '(each-any any . each-any))))
                                         (if #{tmp 4793}#
                                           (@apply
                                             (lambda (#{k 4797}#
                                                      #{e1 4798}#
                                                      #{e2 4799}#)
                                               (list '#(syntax-object
                                                        if
                                                        ((top)
                                                         #(ribcage
                                                           #(k e1 e2)
                                                           #((top) (top) (top))
                                                           #("i4794"
                                                             "i4795"
                                                             "i4796"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(rest)
                                                           #((top))
                                                           #("i4790"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(f clause clauses)
                                                           #((top) (top) (top))
                                                           #("i4763"
                                                             "i4764"
                                                             "i4765"))
                                                         #(ribcage
                                                           #(e m1 m2)
                                                           #((top) (top) (top))
                                                           #("i4753"
                                                             "i4754"
                                                             "i4755"))
                                                         #(ribcage () () ())
                                                         #(ribcage
                                                           #(x)
                                                           #((top))
                                                           #("i4750")))
                                                        (hygiene guile))
                                                     (list '#(syntax-object
                                                              memv
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4794"
                                                                   "i4795"
                                                                   "i4796"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4790"))
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
                                                                 #("i4763"
                                                                   "i4764"
                                                                   "i4765"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4753"
                                                                   "i4754"
                                                                   "i4755"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4750")))
                                                              (hygiene guile))
                                                           '#(syntax-object
                                                              t
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4794"
                                                                   "i4795"
                                                                   "i4796"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4790"))
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
                                                                 #("i4763"
                                                                   "i4764"
                                                                   "i4765"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4753"
                                                                   "i4754"
                                                                   "i4755"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4750")))
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
                                                                       #("i4794"
                                                                         "i4795"
                                                                         "i4796"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(rest)
                                                                       #((top))
                                                                       #("i4790"))
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
                                                                       #("i4763"
                                                                         "i4764"
                                                                         "i4765"))
                                                                     #(ribcage
                                                                       #(e
                                                                         m1
                                                                         m2)
                                                                       #((top)
                                                                         (top)
                                                                         (top))
                                                                       #("i4753"
                                                                         "i4754"
                                                                         "i4755"))
                                                                     #(ribcage
                                                                       ()
                                                                       ()
                                                                       ())
                                                                     #(ribcage
                                                                       #(x)
                                                                       #((top))
                                                                       #("i4750")))
                                                                    (hygiene
                                                                      guile))
                                                                 #{k 4797}#))
                                                     (cons '#(syntax-object
                                                              begin
                                                              ((top)
                                                               #(ribcage
                                                                 #(k e1 e2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4794"
                                                                   "i4795"
                                                                   "i4796"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(rest)
                                                                 #((top))
                                                                 #("i4790"))
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
                                                                 #("i4763"
                                                                   "i4764"
                                                                   "i4765"))
                                                               #(ribcage
                                                                 #(e m1 m2)
                                                                 #((top)
                                                                   (top)
                                                                   (top))
                                                                 #("i4753"
                                                                   "i4754"
                                                                   "i4755"))
                                                               #(ribcage
                                                                 ()
                                                                 ()
                                                                 ())
                                                               #(ribcage
                                                                 #(x)
                                                                 #((top))
                                                                 #("i4750")))
                                                              (hygiene guile))
                                                           (cons #{e1 4798}#
                                                                 #{e2 4799}#))
                                                     #{rest 4791}#))
                                             #{tmp 4793}#)
                                           (let ((#{_ 4803}# #{tmp 4792}#))
                                             (syntax-violation
                                               'case
                                               "bad clause"
                                               #{x 4749}#
                                               #{clause 4767}#)))))))))))
                          (begin (#{f 4766}# #{m1 4757}# #{m2 4758}#)))))
                  (let ((#{body 4762}# #{tmp 4760}#))
                    (list '#(syntax-object
                             let
                             ((top)
                              #(ribcage () () ())
                              #(ribcage #(body) #((top)) #("i4761"))
                              #(ribcage
                                #(e m1 m2)
                                #((top) (top) (top))
                                #("i4753" "i4754" "i4755"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4750")))
                             (hygiene guile))
                          (list (list '#(syntax-object
                                         t
                                         ((top)
                                          #(ribcage () () ())
                                          #(ribcage
                                            #(body)
                                            #((top))
                                            #("i4761"))
                                          #(ribcage
                                            #(e m1 m2)
                                            #((top) (top) (top))
                                            #("i4753" "i4754" "i4755"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4750")))
                                         (hygiene guile))
                                      #{e 4756}#))
                          #{body 4762}#))))
              #{tmp 4752}#)
            (syntax-violation
              #f
              "source expression failed to match any pattern"
              #{tmp 4751}#)))))))

(define make-variable-transformer
  (lambda (#{proc 4804}#)
    (if (procedure? #{proc 4804}#)
      (begin
        (letrec*
          ((#{trans 4807}#
             (lambda (#{x 4808}#) (#{proc 4804}# #{x 4808}#))))
          (begin
            (set-procedure-property!
              #{trans 4807}#
              'variable-transformer
              #t)
            #{trans 4807}#)))
      (error "variable transformer not a procedure"
             #{proc 4804}#))))

(define identifier-syntax
  (make-syntax-transformer
    'identifier-syntax
    'macro
    (lambda (#{x 4810}#)
      (let ((#{tmp 4812}# #{x 4810}#))
        (let ((#{tmp 4813}#
                ($sc-dispatch #{tmp 4812}# '(_ any))))
          (if #{tmp 4813}#
            (@apply
              (lambda (#{e 4815}#)
                (list '#(syntax-object
                         lambda
                         ((top)
                          #(ribcage #(e) #((top)) #("i4814"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4811")))
                         (hygiene guile))
                      '(#(syntax-object
                          x
                          ((top)
                           #(ribcage #(e) #((top)) #("i4814"))
                           #(ribcage () () ())
                           #(ribcage #(x) #((top)) #("i4811")))
                          (hygiene guile)))
                      '#((#(syntax-object
                            macro-type
                            ((top)
                             #(ribcage #(e) #((top)) #("i4814"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4811")))
                            (hygiene guile))
                          .
                          #(syntax-object
                            identifier-syntax
                            ((top)
                             #(ribcage #(e) #((top)) #("i4814"))
                             #(ribcage () () ())
                             #(ribcage #(x) #((top)) #("i4811")))
                            (hygiene guile))))
                      (list '#(syntax-object
                               syntax-case
                               ((top)
                                #(ribcage #(e) #((top)) #("i4814"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4811")))
                               (hygiene guile))
                            '#(syntax-object
                               x
                               ((top)
                                #(ribcage #(e) #((top)) #("i4814"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4811")))
                               (hygiene guile))
                            '()
                            (list '#(syntax-object
                                     id
                                     ((top)
                                      #(ribcage #(e) #((top)) #("i4814"))
                                      #(ribcage () () ())
                                      #(ribcage #(x) #((top)) #("i4811")))
                                     (hygiene guile))
                                  '(#(syntax-object
                                      identifier?
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4814"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4811")))
                                      (hygiene guile))
                                    (#(syntax-object
                                       syntax
                                       ((top)
                                        #(ribcage #(e) #((top)) #("i4814"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4811")))
                                       (hygiene guile))
                                     #(syntax-object
                                       id
                                       ((top)
                                        #(ribcage #(e) #((top)) #("i4814"))
                                        #(ribcage () () ())
                                        #(ribcage #(x) #((top)) #("i4811")))
                                       (hygiene guile))))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage #(e) #((top)) #("i4814"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4811")))
                                           (hygiene guile))
                                        #{e 4815}#))
                            (list '(#(syntax-object
                                      _
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4814"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4811")))
                                      (hygiene guile))
                                    #(syntax-object
                                      x
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4814"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4811")))
                                      (hygiene guile))
                                    #(syntax-object
                                      ...
                                      ((top)
                                       #(ribcage #(e) #((top)) #("i4814"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4811")))
                                      (hygiene guile)))
                                  (list '#(syntax-object
                                           syntax
                                           ((top)
                                            #(ribcage #(e) #((top)) #("i4814"))
                                            #(ribcage () () ())
                                            #(ribcage
                                              #(x)
                                              #((top))
                                              #("i4811")))
                                           (hygiene guile))
                                        (cons #{e 4815}#
                                              '(#(syntax-object
                                                  x
                                                  ((top)
                                                   #(ribcage
                                                     #(e)
                                                     #((top))
                                                     #("i4814"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4811")))
                                                  (hygiene guile))
                                                #(syntax-object
                                                  ...
                                                  ((top)
                                                   #(ribcage
                                                     #(e)
                                                     #((top))
                                                     #("i4814"))
                                                   #(ribcage () () ())
                                                   #(ribcage
                                                     #(x)
                                                     #((top))
                                                     #("i4811")))
                                                  (hygiene guile)))))))))
              #{tmp 4813}#)
            (let ((#{tmp 4816}#
                    ($sc-dispatch
                      #{tmp 4812}#
                      '(_ (any any)
                          ((#(free-id
                              #(syntax-object
                                set!
                                ((top)
                                 #(ribcage () () ())
                                 #(ribcage #(x) #((top)) #("i4811")))
                                (hygiene guile)))
                            any
                            any)
                           any)))))
              (if (if #{tmp 4816}#
                    (@apply
                      (lambda (#{id 4822}#
                               #{exp1 4823}#
                               #{var 4824}#
                               #{val 4825}#
                               #{exp2 4826}#)
                        (if (identifier? #{id 4822}#)
                          (identifier? #{var 4824}#)
                          #f))
                      #{tmp 4816}#)
                    #f)
                (@apply
                  (lambda (#{id 4834}#
                           #{exp1 4835}#
                           #{var 4836}#
                           #{val 4837}#
                           #{exp2 4838}#)
                    (list '#(syntax-object
                             make-variable-transformer
                             ((top)
                              #(ribcage
                                #(id exp1 var val exp2)
                                #((top) (top) (top) (top) (top))
                                #("i4829" "i4830" "i4831" "i4832" "i4833"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4811")))
                             (hygiene guile))
                          (list '#(syntax-object
                                   lambda
                                   ((top)
                                    #(ribcage
                                      #(id exp1 var val exp2)
                                      #((top) (top) (top) (top) (top))
                                      #("i4829"
                                        "i4830"
                                        "i4831"
                                        "i4832"
                                        "i4833"))
                                    #(ribcage () () ())
                                    #(ribcage #(x) #((top)) #("i4811")))
                                   (hygiene guile))
                                '(#(syntax-object
                                    x
                                    ((top)
                                     #(ribcage
                                       #(id exp1 var val exp2)
                                       #((top) (top) (top) (top) (top))
                                       #("i4829"
                                         "i4830"
                                         "i4831"
                                         "i4832"
                                         "i4833"))
                                     #(ribcage () () ())
                                     #(ribcage #(x) #((top)) #("i4811")))
                                    (hygiene guile)))
                                '#((#(syntax-object
                                      macro-type
                                      ((top)
                                       #(ribcage
                                         #(id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top))
                                         #("i4829"
                                           "i4830"
                                           "i4831"
                                           "i4832"
                                           "i4833"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4811")))
                                      (hygiene guile))
                                    .
                                    #(syntax-object
                                      variable-transformer
                                      ((top)
                                       #(ribcage
                                         #(id exp1 var val exp2)
                                         #((top) (top) (top) (top) (top))
                                         #("i4829"
                                           "i4830"
                                           "i4831"
                                           "i4832"
                                           "i4833"))
                                       #(ribcage () () ())
                                       #(ribcage #(x) #((top)) #("i4811")))
                                      (hygiene guile))))
                                (list '#(syntax-object
                                         syntax-case
                                         ((top)
                                          #(ribcage
                                            #(id exp1 var val exp2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4829"
                                              "i4830"
                                              "i4831"
                                              "i4832"
                                              "i4833"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4811")))
                                         (hygiene guile))
                                      '#(syntax-object
                                         x
                                         ((top)
                                          #(ribcage
                                            #(id exp1 var val exp2)
                                            #((top) (top) (top) (top) (top))
                                            #("i4829"
                                              "i4830"
                                              "i4831"
                                              "i4832"
                                              "i4833"))
                                          #(ribcage () () ())
                                          #(ribcage #(x) #((top)) #("i4811")))
                                         (hygiene guile))
                                      '(#(syntax-object
                                          set!
                                          ((top)
                                           #(ribcage
                                             #(id exp1 var val exp2)
                                             #((top) (top) (top) (top) (top))
                                             #("i4829"
                                               "i4830"
                                               "i4831"
                                               "i4832"
                                               "i4833"))
                                           #(ribcage () () ())
                                           #(ribcage #(x) #((top)) #("i4811")))
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
                                                        #("i4829"
                                                          "i4830"
                                                          "i4831"
                                                          "i4832"
                                                          "i4833"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4811")))
                                                     (hygiene guile))
                                                  #{var 4836}#
                                                  #{val 4837}#)
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
                                                        #("i4829"
                                                          "i4830"
                                                          "i4831"
                                                          "i4832"
                                                          "i4833"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4811")))
                                                     (hygiene guile))
                                                  #{exp2 4838}#))
                                      (list (cons #{id 4834}#
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
                                                         #("i4829"
                                                           "i4830"
                                                           "i4831"
                                                           "i4832"
                                                           "i4833"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4811")))
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
                                                         #("i4829"
                                                           "i4830"
                                                           "i4831"
                                                           "i4832"
                                                           "i4833"))
                                                       #(ribcage () () ())
                                                       #(ribcage
                                                         #(x)
                                                         #((top))
                                                         #("i4811")))
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
                                                        #("i4829"
                                                          "i4830"
                                                          "i4831"
                                                          "i4832"
                                                          "i4833"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4811")))
                                                     (hygiene guile))
                                                  (cons #{exp1 4835}#
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
                                                               #("i4829"
                                                                 "i4830"
                                                                 "i4831"
                                                                 "i4832"
                                                                 "i4833"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4811")))
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
                                                               #("i4829"
                                                                 "i4830"
                                                                 "i4831"
                                                                 "i4832"
                                                                 "i4833"))
                                                             #(ribcage
                                                               ()
                                                               ()
                                                               ())
                                                             #(ribcage
                                                               #(x)
                                                               #((top))
                                                               #("i4811")))
                                                            (hygiene
                                                              guile))))))
                                      (list #{id 4834}#
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
                                                        #("i4829"
                                                          "i4830"
                                                          "i4831"
                                                          "i4832"
                                                          "i4833"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4811")))
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
                                                              #("i4829"
                                                                "i4830"
                                                                "i4831"
                                                                "i4832"
                                                                "i4833"))
                                                            #(ribcage () () ())
                                                            #(ribcage
                                                              #(x)
                                                              #((top))
                                                              #("i4811")))
                                                           (hygiene guile))
                                                        #{id 4834}#))
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
                                                        #("i4829"
                                                          "i4830"
                                                          "i4831"
                                                          "i4832"
                                                          "i4833"))
                                                      #(ribcage () () ())
                                                      #(ribcage
                                                        #(x)
                                                        #((top))
                                                        #("i4811")))
                                                     (hygiene guile))
                                                  #{exp1 4835}#))))))
                  #{tmp 4816}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4812}#)))))))))

(define define*
  (make-syntax-transformer
    'define*
    'macro
    (lambda (#{x 4839}#)
      (let ((#{tmp 4841}# #{x 4839}#))
        (let ((#{tmp 4842}#
                ($sc-dispatch
                  #{tmp 4841}#
                  '(_ (any . any) any . each-any))))
          (if #{tmp 4842}#
            (@apply
              (lambda (#{id 4847}#
                       #{args 4848}#
                       #{b0 4849}#
                       #{b1 4850}#)
                (list '#(syntax-object
                         define
                         ((top)
                          #(ribcage
                            #(id args b0 b1)
                            #((top) (top) (top) (top))
                            #("i4843" "i4844" "i4845" "i4846"))
                          #(ribcage () () ())
                          #(ribcage #(x) #((top)) #("i4840")))
                         (hygiene guile))
                      #{id 4847}#
                      (cons '#(syntax-object
                               lambda*
                               ((top)
                                #(ribcage
                                  #(id args b0 b1)
                                  #((top) (top) (top) (top))
                                  #("i4843" "i4844" "i4845" "i4846"))
                                #(ribcage () () ())
                                #(ribcage #(x) #((top)) #("i4840")))
                               (hygiene guile))
                            (cons #{args 4848}#
                                  (cons #{b0 4849}# #{b1 4850}#)))))
              #{tmp 4842}#)
            (let ((#{tmp 4852}#
                    ($sc-dispatch #{tmp 4841}# '(_ any any))))
              (if (if #{tmp 4852}#
                    (@apply
                      (lambda (#{id 4855}# #{val 4856}#)
                        (identifier?
                          '#(syntax-object
                             x
                             ((top)
                              #(ribcage
                                #(id val)
                                #((top) (top))
                                #("i4853" "i4854"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4840")))
                             (hygiene guile))))
                      #{tmp 4852}#)
                    #f)
                (@apply
                  (lambda (#{id 4859}# #{val 4860}#)
                    (list '#(syntax-object
                             define
                             ((top)
                              #(ribcage
                                #(id val)
                                #((top) (top))
                                #("i4857" "i4858"))
                              #(ribcage () () ())
                              #(ribcage #(x) #((top)) #("i4840")))
                             (hygiene guile))
                          #{id 4859}#
                          #{val 4860}#))
                  #{tmp 4852}#)
                (syntax-violation
                  #f
                  "source expression failed to match any pattern"
                  #{tmp 4841}#)))))))))

