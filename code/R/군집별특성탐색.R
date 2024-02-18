library(tidyverse)
library(ggbreak)
library(gridExtra)
les=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/여가활동코드정리/여가활동.csv')
df_2021=read.csv('국민여가활동조사거주지코드정리_cl(2021).csv')

#1. 유형별 여가활동 참여 활동 여부 비율
#1-1 문화예술활동관람참여
p1=df_2021 %>% 
  mutate(활동여부=ifelse(문화예술활동관람참여여부==1,'해봄','안해봄')) %>% 
  select(활동여부,cluster) %>% 
  group_by(cluster,활동여부) %>%
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(cluster), y=비율, fill=활동여부),
           stat='identity',
           position='dodge')+ 
  labs(x='cluster', title='문화예술활동관람참여여부')+
  scale_y_break(c(0,0.3))

#1-2 문화예술참여활동여부
p2=df_2021 %>% 
  mutate(활동여부=ifelse(문화예술참여활동여부==1,'해봄','안해봄')) %>% 
  select(활동여부,cluster) %>% 
  group_by(cluster,활동여부) %>%
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(cluster), y=비율, fill=활동여부),
           stat='identity',
           position='dodge')+ 
  labs(x='cluster', title='문화예술참여활동여부')

#1-3 스포츠관람활동여부
p3=df_2021 %>% 
  mutate(활동여부=ifelse(스포츠관람활동여부==1,'해봄','안해봄')) %>% 
  select(활동여부,cluster) %>% 
  group_by(cluster,활동여부) %>%
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(cluster), y=비율, fill=활동여부),
           stat='identity',
           position='dodge')+ 
  labs(x='cluster', title='스포츠관람활동여부')+
  scale_y_break(c(0,0.3))

#1-4 스포츠참여활동여부
p4=df_2021 %>% 
  mutate(활동여부=ifelse(스포츠참여활동여부==1,'해봄','안해봄')) %>% 
  select(활동여부,cluster) %>% 
  group_by(cluster,활동여부) %>%
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(cluster), y=비율, fill=활동여부),
           stat='identity',
           position='dodge')+ 
  labs(x='cluster', title='스포츠참여활동여부')+
  scale_y_break(c(0,0.3))

grid.arrange(p1,p2,p3,p4,nrow=2)


#1-5 관광활동여부
p5=df_2021 %>% 
  mutate(활동여부=ifelse(관광활동여부==1,'해봄','안해봄')) %>% 
  select(활동여부,cluster) %>% 
  group_by(cluster,활동여부) %>%
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(cluster), y=비율, fill=활동여부),
           stat='identity',
           position='dodge')+ 
  labs(x='cluster', title='관광활동여부')

#1-6 취미오락활동여부
p6=df_2021 %>% 
  mutate(활동여부=ifelse(취미오락활동여부==1,'해봄','안해봄')) %>% 
  select(활동여부,cluster) %>% 
  group_by(cluster,활동여부) %>%
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(cluster), y=비율, fill=활동여부),
           stat='identity',
           position='dodge')+ 
  labs(x='cluster', title='취미오락활동여부')

#1-7 휴식활동여부
p7=df_2021 %>% 
  mutate(활동여부=ifelse(휴식활동여부==1,'해봄','안해봄')) %>% 
  select(활동여부,cluster) %>% 
  group_by(cluster,활동여부) %>%
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(cluster), y=비율, fill=활동여부),
           stat='identity',
           position='dodge')+ 
  labs(x='cluster', title='휴식활동여부')

#1-8 사회및기타활동여부
p8=df_2021 %>% 
  mutate(활동여부=ifelse(사회및기타활동여부==1,'해봄','안해봄')) %>% 
  select(활동여부,cluster) %>% 
  group_by(cluster,활동여부) %>%
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(cluster), y=비율, fill=활동여부),
           stat='identity',
           position='dodge')+ 
  labs(x='cluster', title='사회및기타활동여부')

grid.arrange(p5,p6,p7,p8, nrow=2)

#2. 한번 이상 해본 여가활동 종류수
df_2021 %>% 
  mutate(cluster=as.factor(cluster)) %>% 
  group_by(cluster) %>%
  summarise(문화예술활동관람=mean(QA,na.rm=TRUE),
                    문화예술참여활동=mean(QB,na.rm=TRUE),
                    스포츠관람활동=mean(QC,na.rm=TRUE),
                    스포츠참여활동=mean(QD,na.rm=TRUE),
                    관광활동=mean(QE,na.rm=TRUE),
                    취미오락활동=mean(QF,na.rm=TRUE),
                    휴식활동=mean(QG,na.rm=TRUE),
                    사회및기타활동=mean(QH,na.rm=TRUE)) %>% 
  pivot_longer(
    cols=문화예술활동관람:사회및기타활동,
    names_to = '여가활동유형',
    values_to = '평균') %>% 
  ggplot()+
  geom_bar(aes(x=fct_relevel(여가활동유형,c('문화예술활동관람','문화예술참여활동','스포츠관람활동','스포츠참여활동','관광활동','취미오락활동','휴식활동','사회및기타활동')), y=평균, fill=cluster),
           stat='identity',
           position='dodge')+
  labs(x='여가활동유형', title='한번 이상 해본 여가활동수 유형별 평균')
  
#3.적절하다고 생각하는 여가 비용
df_2021 %>% 
  mutate(cluster=as.factor(cluster)) %>% 
  group_by(cluster) %>% 
  summarise(여가비용평균=mean(Q10, na.rm=TRUE)) %>% 
  ggplot()+
  geom_bar(aes(x=cluster, y=여가비용평균, fill=cluster),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(title='적절하다고 생각하는 여가비용 평균')+
  scale_y_break(c(0,150000))
  
#4.여가 희망 시간과 평균 여가시간 비교
df_2021 %>% 
  mutate(cluster=as.factor(cluster)) %>% 
  group_by(cluster) %>% 
  summarise(평균여가시간_평일=mean(Q13_1_1, na.rm=TRUE),
                  희망여가시간_평일=mean(Q13_4_1, na.rm=TRUE),
                  평균여가시간_휴일=mean(Q13_1_2, na.rm=TRUE),
                  희망여가시간_휴일=mean(Q13_4_2, na.rm=TRUE)
                  ) %>% 
  pivot_longer(cols=평균여가시간_평일:희망여가시간_휴일,
                names_to = '평균및희망여가시간',
                values_to = '시간') %>% 
  ggplot()+
  geom_bar(aes(x=cluster, y=시간, fill=평균및희망여가시간),
           col='white',
           stat='identity',
           position='dodge')+
  labs(title='여가 희망 시간과 평균 여가시간 비교')+
  scale_y_break(c(0,3))+
  scale_fill_manual(values = c("lightblue", "cornflowerblue", "lightsalmon","coral"))


#5.가장 많이 이용하는 여가 공간 및 희망하는 여가 공간
lesarea=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/여가공간코드정리/여가공간.csv')
#cluster_1
#많이 이용하는 여가공간
df_2021 %>%
  filter(cluster==1) %>% 
  select(Q16_1,Q16_2,Q16_3) %>% 
  pivot_longer(cols=Q16_1:Q16_3,
               names_to='많이 이용하는 여가 공간 순위',
               values_to='여가공간코드') %>% 
  left_join(lesarea,by='여가공간코드') %>%
  filter(!is.na(여가공간코드)) %>% 
  group_by(여가공간유형) %>%
  summarise(횟수=n()) %>% 
  arrange(-횟수) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가공간유형,횟수), y=횟수, fill=여가공간유형),
           stat='identity')+
  coord_flip()+
  labs(title='cluster1의 가장 많이 이용하는 여가 공간(1+2+3순위)',x='여가공간유형')+
  theme(legend.position = 'none')
    
#희망하는 여가공간
df_2021 %>%
  filter(cluster==1) %>% 
  select(Q17_1,Q17_2,Q17_3) %>% 
  pivot_longer(cols=Q17_1:Q17_3,
               names_to='희망하는 여가 공간 순위',
               values_to='여가공간코드') %>% 
  left_join(lesarea,by='여가공간코드') %>%
  filter(!is.na(여가공간코드)) %>% 
  group_by(여가공간유형) %>%
  summarise(횟수=n()) %>% 
  arrange(-횟수) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가공간유형,횟수), y=횟수, fill=여가공간유형),
           stat='identity')+
  coord_flip()+
  labs(title='cluster1의 가장 희망하는 여가 공간(1+2+3순위)',x='여가공간유형')+
  theme(legend.position = 'none')


#cluster_2
#많이 이용하는 여가공간
df_2021 %>%
  filter(cluster==2) %>% 
  select(Q16_1,Q16_2,Q16_3) %>% 
  pivot_longer(cols=Q16_1:Q16_3,
               names_to='많이 이용하는 여가 공간 순위',
               values_to='여가공간코드') %>% 
  left_join(lesarea,by='여가공간코드') %>%
  filter(!is.na(여가공간코드)) %>% 
  group_by(여가공간유형) %>%
  summarise(횟수=n()) %>% 
  arrange(-횟수) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가공간유형,횟수), y=횟수, fill=여가공간유형),
           stat='identity')+
  coord_flip()+
  labs(title='cluster2의 가장 많이 이용하는 여가 공간(1+2+3순위)',x='여가공간유형')+
  theme(legend.position = 'none')

#희망하는 여가공간
df_2021 %>%
  filter(cluster==2) %>% 
  select(Q17_1,Q17_2,Q17_3) %>% 
  pivot_longer(cols=Q17_1:Q17_3,
               names_to='희망하는 여가 공간 순위',
               values_to='여가공간코드') %>% 
  left_join(lesarea,by='여가공간코드') %>%
  filter(!is.na(여가공간코드)) %>% 
  group_by(여가공간유형) %>%
  summarise(횟수=n()) %>% 
  arrange(-횟수) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가공간유형,횟수), y=횟수, fill=여가공간유형),
           stat='identity')+
  coord_flip()+
  labs(title='cluster2의 가장 희망하는 여가 공간(1+2+3순위)',x='여가공간유형')+
  theme(legend.position = 'none')

#cluster_3
#많이 이용하는 여가공간
df_2021 %>%
  filter(cluster==3) %>% 
  select(Q16_1,Q16_2,Q16_3) %>% 
  pivot_longer(cols=Q16_1:Q16_3,
               names_to='많이 이용하는 여가 공간 순위',
               values_to='여가공간코드') %>% 
  left_join(lesarea,by='여가공간코드') %>%
  filter(!is.na(여가공간코드)) %>% 
  group_by(여가공간유형) %>%
  summarise(횟수=n()) %>% 
  arrange(-횟수) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가공간유형,횟수), y=횟수, fill=여가공간유형),
           stat='identity')+
  coord_flip()+
  labs(title='cluster3의 가장 많이 이용하는 여가 공간(1+2+3순위)',x='여가공간유형')+
  theme(legend.position = 'none')

#희망하는 여가공간
df_2021 %>%
  filter(cluster==3) %>% 
  select(Q17_1,Q17_2,Q17_3) %>% 
  pivot_longer(cols=Q17_1:Q17_3,
               names_to='희망하는 여가 공간 순위',
               values_to='여가공간코드') %>% 
  left_join(lesarea,by='여가공간코드') %>%
  filter(!is.na(여가공간코드)) %>% 
  group_by(여가공간유형) %>%
  summarise(횟수=n()) %>% 
  arrange(-횟수) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가공간유형,횟수), y=횟수, fill=여가공간유형),
           stat='identity')+
  coord_flip()+
  labs(title='cluster3의 가장 희망하는 여가 공간(1+2+3순위)',x='여가공간유형')+
  theme(legend.position = 'none')


#cluster_4
#많이 이용하는 여가공간
df_2021 %>%
  filter(cluster==4) %>% 
  select(Q16_1,Q16_2,Q16_3) %>% 
  pivot_longer(cols=Q16_1:Q16_3,
               names_to='많이 이용하는 여가 공간 순위',
               values_to='여가공간코드') %>% 
  left_join(lesarea,by='여가공간코드') %>%
  filter(!is.na(여가공간코드)) %>% 
  group_by(여가공간유형) %>%
  summarise(횟수=n()) %>% 
  arrange(-횟수) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가공간유형,횟수), y=횟수, fill=여가공간유형),
           stat='identity')+
  coord_flip()+
  labs(title='cluster4의 가장 많이 이용하는 여가 공간(1+2+3순위)',x='여가공간유형')+
  theme(legend.position = 'none')

#희망하는 여가공간
df_2021 %>%
  filter(cluster==4) %>% 
  select(Q17_1,Q17_2,Q17_3) %>% 
  pivot_longer(cols=Q17_1:Q17_3,
               names_to='희망하는 여가 공간 순위',
               values_to='여가공간코드') %>% 
  left_join(lesarea,by='여가공간코드') %>%
  filter(!is.na(여가공간코드)) %>% 
  group_by(여가공간유형) %>%
  summarise(횟수=n()) %>% 
  arrange(-횟수) %>% 
  head(20) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가공간유형,횟수), y=횟수, fill=여가공간유형),
           stat='identity')+
  coord_flip()+
  labs(title='cluster4의 가장 희망하는 여가 공간(1+2+3순위)',x='여가공간유형')+
  theme(legend.position = 'none')




#6.여가정책중요도순위
df_2021 %>%
  select(Q26_1_1,Q26_1_2,Q26_1_3,cluster) %>% 
  pivot_longer(cols=Q26_1_1:Q26_1_3,
               names_to='여가 정책 중요도 순위',
               values_to='여가정책유형') %>%
  mutate(여가정책유형=ifelse(여가정책유형==1,'다양한 여가시설',
                             ifelse(여가정책유형==2,'질 좋은 여가프로그램 개발 및 보급',
                                          ifelse(여가정책유형==3,'여가와 관련한 전문인력 양성 및 배치',
                                                       ifelse(여가정책유형==4,'여가관련 동호회 육성 및 지원',ifelse(여가정책유형==5,'소외계층 위한 여가생활 지원',ifelse(여가정책유형==6,'보다 나은 여가생활을 위해 관련 법규와 제도를 개선','공휴일과 휴가를 법적으로 보장'))))))) %>% 
  filter(!is.na(여가정책유형)) %>%
  group_by(cluster,여가정책유형) %>%
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=cluster, y=비율, fill=fct_reorder(여가정책유형,-비율)),
           stat='identity',
           position='dodge')+
  labs(title='여가정책중요도',x='여가정책유형', fill='여가정책유형')

table(df_2021$Q26_1_3, useNA = 'ifany')

#8. 여가생활 불만족 이유
df_2021 %>% 
  mutate(cluster=as.factor(cluster)) %>% 
  mutate(여가생활불만족이유=ifelse(Q30_1==1,'시간 부족',
                          ifelse(Q30_1==2,'경제적부담',
                                 ifelse(Q30_1==3,'여가정보및프로그램부족',
                                        ifelse(Q30_1==4,'여가시설부족',
                                               ifelse(Q30_1==5,'여가함께즐길사람없어서',
                                                      ifelse(Q30_1==6,'이전 경험 부족 및 할 줄아는게 없어서','기타'))))))) %>%
  filter(!is.na(여가생활불만족이유)) %>% 
  group_by(cluster,여가생활불만족이유) %>% 
  summarise(횟수=n()) %>% 
  mutate(비율=횟수/sum(횟수)) %>% 
  ggplot()+
  geom_bar(aes(x=cluster, y=비율, fill=fct_reorder(여가생활불만족이유,-비율)),
           stat='identity',
           position='dodge')+
  labs(title='여가생활 불만족 이유',x='여가생활불만족이유',fill='여가생활불만족이유')
