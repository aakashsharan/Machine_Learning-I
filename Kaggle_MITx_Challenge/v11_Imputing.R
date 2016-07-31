library(mice)
set.seed(121)


train <- read.csv('train2016.csv', na.strings = c('', ' ', 'NA'))
test <- read.csv('test2016.csv', na.strings = c('', ' ', 'NA'))

#summary(train)
#summary(test)

simple_tr = train[c("YOB","Gender","Income","HouseholdStatus","EducationLevel","Q124742","Q124122","Q123464","Q123621","Q122769","Q122770","Q122771","Q122120","Q121699","Q121700","Q120978","Q121011","Q120379","Q120650","Q120472","Q120194","Q120012","Q120014","Q119334","Q119851","Q119650","Q118892","Q118117","Q118232","Q118233","Q118237","Q117186","Q117193","Q116797","Q116881","Q116953","Q116601","Q116441","Q116448","Q116197","Q115602","Q115777","Q115610","Q115611","Q115899","Q115390","Q114961","Q114748","Q115195","Q114517","Q114386","Q113992","Q114152","Q113583","Q113584","Q113181","Q112478","Q112512","Q112270","Q111848","Q111580","Q111220","Q110740","Q109367","Q108950","Q109244","Q108855","Q108617","Q108856","Q108754","Q108342","Q108343","Q107869","Q107491","Q106993","Q106997","Q106272","Q106388","Q106389","Q106042","Q105840","Q105655","Q104996","Q103293","Q102906","Q102674","Q102687","Q102289","Q102089","Q101162","Q101163","Q101596","Q100689","Q100680","Q100562","Q99982","Q100010","Q99716", "Q99581", "Q99480", "Q98869", "Q98578", "Q98059","Q98078", "Q98197", "Q96024")]
simple_te = test[c("YOB","Gender","Income","HouseholdStatus","EducationLevel","Q124742","Q124122","Q123464","Q123621","Q122769","Q122770","Q122771","Q122120","Q121699","Q121700","Q120978","Q121011","Q120379","Q120650","Q120472","Q120194","Q120012","Q120014","Q119334","Q119851","Q119650","Q118892","Q118117","Q118232","Q118233","Q118237","Q117186","Q117193","Q116797","Q116881","Q116953","Q116601","Q116441","Q116448","Q116197","Q115602","Q115777","Q115610","Q115611","Q115899","Q115390","Q114961","Q114748","Q115195","Q114517","Q114386","Q113992","Q114152","Q113583","Q113584","Q113181","Q112478","Q112512","Q112270","Q111848","Q111580","Q111220","Q110740","Q109367","Q108950","Q109244","Q108855","Q108617","Q108856","Q108754","Q108342","Q108343","Q107869","Q107491","Q106993","Q106997","Q106272","Q106388","Q106389","Q106042","Q105840","Q105655","Q104996","Q103293","Q102906","Q102674","Q102687","Q102289","Q102089","Q101162","Q101163","Q101596","Q100689","Q100680","Q100562","Q99982","Q100010","Q99716", "Q99581", "Q99480", "Q98869", "Q98578", "Q98059","Q98078", "Q98197", "Q96024")]
set.seed(121)
imputed_tr = complete(mice(simple_tr))
imputed_te = complete(mice(simple_te))

train$YOB <- imputed_tr$YOB
train$Gender <- imputed_tr$Gender
train$Income <- imputed_tr$Income
train$HouseholdStatus <- imputed_tr$HouseholdStatus
train$EducationLevel <- imputed_tr$EducationLevel
train$Q124742 <- imputed_tr$Q124742
train$Q124122 <- imputed_tr$Q124122
train$Q123464 <- imputed_tr$Q123464
train$Q123621 <- imputed_tr$Q123621
train$Q122769 <- imputed_tr$Q122769
train$Q122770 <- imputed_tr$Q122770
train$Q122771 <- imputed_tr$Q122771
train$Q122120 <- imputed_tr$Q122120
train$Q121699 <- imputed_tr$Q121699
train$Q121700 <- imputed_tr$Q121700
train$Q120978 <- imputed_tr$Q120978
train$Q121011 <- imputed_tr$Q121011
train$Q120379 <- imputed_tr$Q120379
train$Q120650 <- imputed_tr$Q120650
train$Q120472 <- imputed_tr$Q120472
train$Q120194 <- imputed_tr$Q120194
train$Q120012 <- imputed_tr$Q120012
train$Q120014 <- imputed_tr$Q120014
train$Q119334 <- imputed_tr$Q119334
train$Q119851 <- imputed_tr$Q119851
train$Q119650 <- imputed_tr$Q119650
train$Q118892 <- imputed_tr$Q118892
train$Q118117 <- imputed_tr$Q118117
train$Q118232 <- imputed_tr$Q118232
train$Q118233 <- imputed_tr$Q118233
train$Q118237 <- imputed_tr$Q118237
train$Q117186 <- imputed_tr$Q117186
train$Q117193 <- imputed_tr$Q117193
train$Q116797 <- imputed_tr$Q116797
train$Q116881 <- imputed_tr$Q116881
train$Q116953 <- imputed_tr$Q116953
train$Q116601 <- imputed_tr$Q116601
train$Q116441 <- imputed_tr$Q116441
train$Q116448 <- imputed_tr$Q116448
train$Q116197 <- imputed_tr$Q116197
train$Q115602 <- imputed_tr$Q115602
train$Q115777 <- imputed_tr$Q115777
train$Q115610 <- imputed_tr$Q115610
train$Q115611 <- imputed_tr$Q115611
train$Q115899 <- imputed_tr$Q115899
train$Q115390 <- imputed_tr$Q115390
train$Q114961 <- imputed_tr$Q114961
train$Q114748 <- imputed_tr$Q114748
train$Q115195 <- imputed_tr$Q115195
train$Q114517 <- imputed_tr$Q114517
train$Q114386 <- imputed_tr$Q114386
train$Q113992 <- imputed_tr$Q113992
train$Q114152 <- imputed_tr$Q114152
train$Q113583 <- imputed_tr$Q113583
train$Q113584 <- imputed_tr$Q113584
train$Q113181 <- imputed_tr$Q113181
train$Q112478 <- imputed_tr$Q112478
train$Q112512 <- imputed_tr$Q112512
train$Q112270 <- imputed_tr$Q112270
train$Q111848 <- imputed_tr$Q111848
train$Q111580 <- imputed_tr$Q111580
train$Q111220 <- imputed_tr$Q111220
train$Q110740 <- imputed_tr$Q110740
train$Q109367 <- imputed_tr$Q109367
train$Q108950 <- imputed_tr$Q108950
train$Q109244 <- imputed_tr$Q109244
train$Q108855 <- imputed_tr$Q108855
train$Q108617 <- imputed_tr$Q108617
train$Q108856 <- imputed_tr$Q108856
train$Q108754 <- imputed_tr$Q108754
train$Q108342 <- imputed_tr$Q108342
train$Q108343 <- imputed_tr$Q108343
train$Q107869 <- imputed_tr$Q107869
train$Q107491 <- imputed_tr$Q107491
train$Q106993 <- imputed_tr$Q106993
train$Q106997 <- imputed_tr$Q106997
train$Q106272 <- imputed_tr$Q106272
train$Q106388 <- imputed_tr$Q106388
train$Q106389 <- imputed_tr$Q106389
train$Q106042 <- imputed_tr$Q106042
train$Q105840 <- imputed_tr$Q105840
train$Q105655 <- imputed_tr$Q105655
train$Q104996 <- imputed_tr$Q104996
train$Q103293 <- imputed_tr$Q103293
train$Q102906 <- imputed_tr$Q102906
train$Q102674 <- imputed_tr$Q102674
train$Q102687 <- imputed_tr$Q102687
train$Q102289 <- imputed_tr$Q102289
train$Q102089 <- imputed_tr$Q102089
train$Q101162 <- imputed_tr$Q101162
train$Q101163 <- imputed_tr$Q101163
train$Q101596 <- imputed_tr$Q101596
train$Q100689 <- imputed_tr$Q100689
train$Q100680 <- imputed_tr$Q100680
train$Q100562 <- imputed_tr$Q100562
train$Q99982 <- imputed_tr$Q99982
train$Q100010 <- imputed_tr$Q100010
train$Q99716 <- imputed_tr$Q99716
train$Q99581 <- imputed_tr$Q99581
train$Q99480 <- imputed_tr$Q99480
train$Q98869 <- imputed_tr$Q98869
train$Q98578 <- imputed_tr$Q98578
train$Q98059 <- imputed_tr$Q98059
train$Q98078 <- imputed_tr$Q98078
train$Q98197 <- imputed_tr$Q98197
train$Q96024 <- imputed_tr$Q96024


test$YOB <- imputed_te$YOB
test$Gender <- imputed_te$Gender
test$Income <- imputed_te$Income
test$HouseholdStatus <- imputed_te$HouseholdStatus
test$EducationLevel <- imputed_te$EducationLevel
test$Q124742 <- imputed_te$Q124742
test$Q124122 <- imputed_te$Q124122
test$Q123464 <- imputed_te$Q123464
test$Q123621 <- imputed_te$Q123621
test$Q122769 <- imputed_te$Q122769
test$Q122770 <- imputed_te$Q122770
test$Q122771 <- imputed_te$Q122771
test$Q122120 <- imputed_te$Q122120
test$Q121699 <- imputed_te$Q121699
test$Q121700 <- imputed_te$Q121700
test$Q120978 <- imputed_te$Q120978
test$Q121011 <- imputed_te$Q121011
test$Q120379 <- imputed_te$Q120379
test$Q120650 <- imputed_te$Q120650
test$Q120472 <- imputed_te$Q120472
test$Q120194 <- imputed_te$Q120194
test$Q120012 <- imputed_te$Q120012
test$Q120014 <- imputed_te$Q120014
test$Q119334 <- imputed_te$Q119334
test$Q119851 <- imputed_te$Q119851
test$Q119650 <- imputed_te$Q119650
test$Q118892 <- imputed_te$Q118892
test$Q118117 <- imputed_te$Q118117
test$Q118232 <- imputed_te$Q118232
test$Q118233 <- imputed_te$Q118233
test$Q118237 <- imputed_te$Q118237
test$Q117186 <- imputed_te$Q117186
test$Q117193 <- imputed_te$Q117193
test$Q116797 <- imputed_te$Q116797
test$Q116881 <- imputed_te$Q116881
test$Q116953 <- imputed_te$Q116953
test$Q116601 <- imputed_te$Q116601
test$Q116441 <- imputed_te$Q116441
test$Q116448 <- imputed_te$Q116448
test$Q116197 <- imputed_te$Q116197
test$Q115602 <- imputed_te$Q115602
test$Q115777 <- imputed_te$Q115777
test$Q115610 <- imputed_te$Q115610
test$Q115611 <- imputed_te$Q115611
test$Q115899 <- imputed_te$Q115899
test$Q115390 <- imputed_te$Q115390
test$Q114961 <- imputed_te$Q114961
test$Q114748 <- imputed_te$Q114748
test$Q115195 <- imputed_te$Q115195
test$Q114517 <- imputed_te$Q114517
test$Q114386 <- imputed_te$Q114386
test$Q113992 <- imputed_te$Q113992
test$Q114152 <- imputed_te$Q114152
test$Q113583 <- imputed_te$Q113583
test$Q113584 <- imputed_te$Q113584
test$Q113181 <- imputed_te$Q113181
test$Q112478 <- imputed_te$Q112478
test$Q112512 <- imputed_te$Q112512
test$Q112270 <- imputed_te$Q112270
test$Q111848 <- imputed_te$Q111848
test$Q111580 <- imputed_te$Q111580
test$Q111220 <- imputed_te$Q111220
test$Q110740 <- imputed_te$Q110740
test$Q109367 <- imputed_te$Q109367
test$Q108950 <- imputed_te$Q108950
test$Q109244 <- imputed_te$Q109244
test$Q108855 <- imputed_te$Q108855
test$Q108617 <- imputed_te$Q108617
test$Q108856 <- imputed_te$Q108856
test$Q108754 <- imputed_te$Q108754
test$Q108342 <- imputed_te$Q108342
test$Q108343 <- imputed_te$Q108343
test$Q107869 <- imputed_te$Q107869
test$Q107491 <- imputed_te$Q107491
test$Q106993 <- imputed_te$Q106993
test$Q106997 <- imputed_te$Q106997
test$Q106272 <- imputed_te$Q106272
test$Q106388 <- imputed_te$Q106388
test$Q106389 <- imputed_te$Q106389
test$Q106042 <- imputed_te$Q106042
test$Q105840 <- imputed_te$Q105840
test$Q105655 <- imputed_te$Q105655
test$Q104996 <- imputed_te$Q104996
test$Q103293 <- imputed_te$Q103293
test$Q102906 <- imputed_te$Q102906
test$Q102674 <- imputed_te$Q102674
test$Q102687 <- imputed_te$Q102687
test$Q102289 <- imputed_te$Q102289
test$Q102089 <- imputed_te$Q102089
test$Q101162 <- imputed_te$Q101162
test$Q101163 <- imputed_te$Q101163
test$Q101596 <- imputed_te$Q101596
test$Q100689 <- imputed_te$Q100689
test$Q100680 <- imputed_te$Q100680
test$Q100562 <- imputed_te$Q100562
test$Q99982 <- imputed_te$Q99982
test$Q100010 <- imputed_te$Q100010
test$Q99716 <- imputed_te$Q99716
test$Q99581 <- imputed_te$Q99581
test$Q99480 <- imputed_te$Q99480
test$Q98869 <- imputed_te$Q98869
test$Q98578 <- imputed_te$Q98578
test$Q98059 <- imputed_te$Q98059
test$Q98078 <- imputed_te$Q98078
test$Q98197 <- imputed_te$Q98197
test$Q96024 <- imputed_te$Q96024

write.csv(test, 'imputed_test.csv', row.names = FALSE)
write.csv(train, 'imputed_train.csv', row.names = FALSE)




