rm(list=ls())
#국민여가활동조사 데이터 불러오기
all=read_spss('C:/Users/82102/Desktop/깐부 project/국민여가활동조사/6.통합데이터/국민여가활동조사_통합데이터_2014-2021.sav')

df_2021=read_spss('C:/Users/82102/Desktop/깐부 project/국민여가활동조사/3.데이터/2021 국민여가활동조사_데이터.sav')


#변수명 정리
cname_all=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/국민여가활동조사 사용할 변수 정리.csv')
cname_2021=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/국민여가활동조사 사용할 변수 정리(2021).csv')

#all
all=all %>% 
  filter(year==2019 | year==2020 | year==2021) %>% 
  select(cname_all[,2])

colnames(all)=cname_all[,1]

#2021
colnames(df_2021)=toupper(colnames(df_2021))
df_2021=df_2021  %>% 
  select(cname_2021[,2])

table(df_2021$DM11_1, useNA = 'ifany')
colnames(df_2021)=cname_2021[,1]

write.csv(all, file='국민여가활동조사변수정리(all).csv', row.names=FALSE)
write.csv(df_2021, file='국민여가활동조사변수정리(2021).csv', row.names=FALSE)

#거주지코드 통합
df_2021=read.csv('국민여가활동조사변수정리(2021).csv')
area=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/지역 코드 정리/시군구(.분리필요).csv')
view(area)

area=area %>% 
  mutate(DM11_1=str_split(거주지코드,'. ',simplify=TRUE)[,1],
         시군구명=str_trim(str_split(거주지코드,'. ',simplify=TRUE)[,2])) %>% 
  select(-거주지코드)

write.csv(area, 'C:/Users/82102/Desktop/깐부 project/여가활동정리/지역 코드 정리/시군구코드.csv',row.names=FALSE)

si=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/지역 코드 정리/시군구코드.csv')

do=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/지역 코드 정리/시도명코드.csv')

df_2021=df_2021 %>% 
  left_join(si,by='DM11_1') %>% 
  select(-DM11_1)

df_2021=df_2021 %>% 
  left_join(do, by='DM11') %>% 
  select(-DM11)

df_2021=df_2021 %>% 
  mutate(거주지=paste(시도명,시군구명))

df_2021=df_2021 %>% 
  mutate(거주지=ifelse(거주지 %in% c('경기 권선구', '경기 영통구', '경기 장안구', '경기 팔달구'), '경기 수원시', 
                       ifelse(거주지 %in% c('경기 기흥구', '경기 수지구', '경기 처인구'), '경기 용인시', 
                                 ifelse(거주지=='경기 단원구', '경기 안산시',
                                           ifelse(거주지 %in% c('경기 덕양구', '경기 일산동구', '경기 일산서구'), '경기 고양시',
                                                     ifelse(거주지 %in% c('경기 동안구', '경기 만안구'),'경기 안양시',
                                                               ifelse(거주지 %in% c('경기 분당구', '경기 수정구', '경기 중원구'),'경기 성남시',
                                                                         ifelse(거주지=='경기 상록구', '경기 안산시',
                                                                                   ifelse(거주지 %in% c('경남 마산합포구', '경남 마산회원구', '경남 성산구', '경남 의창구', '경남 진해구'), '경남 창원시',
                                                                                             ifelse(거주지 %in% c('경북 남구', '경북 북구'), '경북 포항시',
                                                                                                       ifelse(거주지=='세종.', '세종 .',
                                                                                                                 ifelse(거주지 %in% c('전북 덕진구', '전북 완산구'), '전북 전주시',
                                                                                                                           ifelse(거주지 %in% c('충남 동남구', '충남 서북구'), '충남 천안시', ifelse(거주지 %in% c('충북 상당구', '충북 서원구', '충북 청원구', '충북 흥덕구'), '충북 청주시', 거주지))))))))))))))


arch=arch %>% 
  mutate(거주지=지역명) %>% 
  select(-지역명)

df_2021=read.csv('국민여가활동조사거주지코드정리(2021).csv')
df_2021[df_2021==99]=NA

view(head(df_2021))
view(head(all))
view(arch)


df_2021=df_2021 %>% 
  left_join(arch, by='거주지')

table(df_2021$cluster, useNA = 'ifany')




write.csv(df_2021, file='국민여가활동조사거주지코드정리_cl(2021).csv', row.names=FALSE)

df_2021=read.csv('국민여가활동조사거주지코드정리_cl(2021).csv')

df_2021 %>% 
  mutate(종합정책만족도=Q27_1+Q27_2+Q27_3+Q27_4+Q27_5+Q27_6+Q27_7) %>% 
  group_by(cluster) %>% 
  summarise(전반적만족도=mean(Q30, na.rm=TRUE),
               정책만족도=mean(종합정책만족도, na.rm=TRUE)/7)


all=all %>% 
  mutate(QA=rowSums(across(Q1_A1:Q1_A8),na.rm=TRUE)) %>% 
  mutate(문화예술활동관람참여여부=ifelse(QA >= 1, 1,0)) %>% 
  mutate(QB=rowSums(across(Q1_B1:Q1_B7),na.rm=TRUE)) %>% 
  mutate(문화예술참여활동여부=ifelse(QB >= 1, 1,0)) %>% 
  mutate(QC=rowSums(across(Q1_C1:Q1_C4),na.rm=TRUE)) %>% 
  mutate(스포츠관람활동여부=ifelse(QC >= 1, 1,0)) %>% 
  mutate(QD=rowSums(across(Q1_D1:Q1_D18),na.rm=TRUE)) %>% 
  mutate(스포츠참여활동여부=ifelse(QD >= 1, 1,0)) %>% 
  mutate(QE=rowSums(across(Q1_E1:Q1_E11),na.rm=TRUE)) %>% 
  mutate(관광활동여부=ifelse(QE >= 1, 1,0)) %>% 
  mutate(QF=rowSums(across(Q1_F1:Q1_F22),na.rm=TRUE)) %>% 
  mutate(취미오락활동여부=ifelse(QF >= 1, 1,0)) %>% 
  mutate(QG=rowSums(across(Q1_G1:Q1_G9),na.rm=TRUE)) %>% 
  mutate(휴식활동여부=ifelse(QG >= 1, 1,0)) %>% 
  mutate(QH=rowSums(across(Q1_H1:Q1_H9),na.rm=TRUE)) %>% 
  mutate(사회및기타활동여부=ifelse(QH >= 1, 1,0)) %>%
  mutate(한번이상해본여가활동횟수=QA+QB+QC+QD+QE+QF+QG+QH)

df_2021=df_2021 %>% 
  mutate(across(Q1_A1:Q1_H9,~ifelse(. >= 1,1,0), names='{col}'))

df_2021=df_2021 %>% 
  mutate(QA=rowSums(across(Q1_A1:Q1_A8),na.rm=TRUE)) %>% 
  mutate(문화예술활동관람참여여부=ifelse(QA >= 1, 1,0)) %>% 
  mutate(QB=rowSums(across(Q1_B1:Q1_B7),na.rm=TRUE)) %>% 
  mutate(문화예술참여활동여부=ifelse(QB >= 1, 1,0)) %>% 
  mutate(QC=rowSums(across(Q1_C1:Q1_C4),na.rm=TRUE)) %>% 
  mutate(스포츠관람활동여부=ifelse(QC >= 1, 1,0)) %>% 
  mutate(QD=rowSums(across(Q1_D1:Q1_D18),na.rm=TRUE)) %>% 
  mutate(스포츠참여활동여부=ifelse(QD >= 1, 1,0)) %>% 
  mutate(QE=rowSums(across(Q1_E1:Q1_E11),na.rm=TRUE)) %>% 
  mutate(관광활동여부=ifelse(QE >= 1, 1,0)) %>% 
  mutate(QF=rowSums(across(Q1_F1:Q1_F22),na.rm=TRUE)) %>% 
  mutate(취미오락활동여부=ifelse(QF >= 1, 1,0)) %>% 
  mutate(QG=rowSums(across(Q1_G1:Q1_G9),na.rm=TRUE)) %>% 
  mutate(휴식활동여부=ifelse(QG >= 1, 1,0)) %>% 
  mutate(QH=rowSums(across(Q1_H1:Q1_H9),na.rm=TRUE)) %>% 
  mutate(사회및기타활동여부=ifelse(QH >= 1, 1,0)) %>%
  mutate(한번이상해본여가활동횟수=QA+QB+QC+QD+QE+QF+QG+QH)

view(head(df_2021))
table(df_2021$스포츠관람활동여부)

write.csv(df_2021, file='국민여가활동조사거주지코드정리_cl(2021).csv', row.names=FALSE)
write.csv(all, file='국민여가활동조사변수정리(all).csv', row.names=FALSE)


#여가활동코드정리
les=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/여가활동코드정리/여가활동(.분리필요).csv')
view(les)

les=les %>% 
  mutate(여가활동코드=str_split(여가활동코드명,'\\. ',simplify=TRUE)[,1],
         여가활동유형=str_split(여가활동코드명,'\\. ',simplify=TRUE)[,2]) %>% 
  select(-여가활동코드명)

write.csv(les, 'C:/Users/82102/Desktop/깐부 project/여가활동정리/여가활동코드정리/여가활동.csv',row.names=FALSE)

str_split('1. 전시회 관람 (미술, 사진, 건축, 디자인 등)','\\. ')

#여가공간코드정리
lesarea=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/여가공간코드정리/여가공간(.분리필요).csv')

lesarea=lesarea %>% 
  mutate(여가공간코드=str_split(여가공간코드명,'\\. ',simplify=TRUE)[,1],
               여가공간유형=str_split(여가공간코드명,'\\. ',simplify=TRUE)[,2]) %>% 
  select(-여가공간코드명)

write.csv(lesarea, 'C:/Users/82102/Desktop/깐부 project/여가활동정리/여가공간코드정리/여가공간.csv',row.names=FALSE)
  




