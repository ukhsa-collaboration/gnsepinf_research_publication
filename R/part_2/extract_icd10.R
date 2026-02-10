##selecting ICD10 codes for my data dictionary
source("paths/icd_10.R")

neoplasm <- icd10 %>% filter(str_detect(Code, "^(C|D0|D1|D2|D3|D4)"))

endocrine <- icd10 %>%
  filter(
    str_detect(Code, "^(E0|E1|E2|E3|E4|E5|E6|E7|E8|P70|P71|P72|P74)")
  )

neuro <- icd10 %>%
  filter(str_detect(Code, "^(F0|F1|F2|F3|F4|F5|F6|F7|F8|F9|G1|G2|G3|G4|G5|G6|G7|G8|G9)")
  )


infectious <- icd10 %>%
  filter(str_detect(Code, "^(A01|A02|A03|A04|A05|A06|A07|A08|A09|A1|A2|A3|A40|A410|A411|A412|A413|A414|A42|A43|A44|A46|A48|A490|A491|A492|A943|A5|A6|A7|A8|A9|B0|B1|B2|B3|B4|B5|B6|B7|B8|B90|B91|B92|B94|B95|B960|B963|B964|B966|B967|B97|P35|P360|P361|P362|P363|P365|P37)")
  )


eye <- icd10 %>%
  filter(str_detect(Code, "^(H0|H1|H2|H3|H4|H5)")
  )

ear <- icd10 %>%
  filter(
    str_detect(Code, "^(H6|H7|H8|H9)")
  )

circulatory <- icd10 %>%
  filter(
    str_detect(Code, "^I")
  )

skin <- icd10 %>%
  filter(
    str_detect(Code, "^L")
  )

digestive <- icd10 %>%
  filter(
    str_detect(Code, "^(K|P76|P77|P78)")
  )
musk<- icd10 %>%
  filter(
    str_detect(Code, "^M")
  )

genitourinary <- icd10 %>%
  filter(
    str_detect(Code, "^N")
  )

pregnancy <- icd10 %>%
  filter(
    str_detect(Code, "^(P01|P02|P03|P04|P10|P11|P12|P13|P14|P15)")
  )

perinatal <- icd10 %>%
  filter(
    str_detect(Code, "^(P19|P23|P24|P25|P26|P27|P28|P29)")
  )

perinatal_other <- icd10 %>% 
  filter(
    str_detect(Code, "^P9")
  )

congenital <- icd10 %>%
  filter(
    str_detect(Code, "^Q")
  )

accident <- icd10 %>%
  filter(
    str_detect(Code, "^(S|T|W|X|Y)")
  )
temp<- icd10 %>% 
  filter(
str_detect(Code, "^(P80|P81|P83)")
)


##save files
export(temp, "temp.xlsx")
export(accident, "accident.xlsx")
export(congenital, "congenital.xlsx")
export(perinatal, "perinatal.xlsx")
export(pregnancy, "pregnancy.xlsx")
export(genitourinary, "genitourinary.xlsx")
export(musk, "musk.xlsx")
export(digestive, "digestive.xlsx")
export(skin, "skin.xlsx")
export(circulatory, "circulatory.xlsx")
export(ear, "ear.xlsx")
export(eye, "eye.xlsx")
export(infectious, "infectious.xlsx")
export(neuro, "neuro.xlsx")
export(endocrine, "endocrine.xlsx")
export(neoplasm, "neoplasm.xlsx")
export(perinatal_other, "perinatal_other.xlsx")

write.csv(neuro, file= "neuro", row.names = FALSE)
write.csv(endocrine, file= "endocrine", row.names = FALSE)
write.csv(neoplasm, file= "neoplasm", row.names = FALSE)
