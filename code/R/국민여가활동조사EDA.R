rm(list=ls())
library(haven)
library(tidyverse)
library(foreign)
library(ggbreak)


#국민여가활동조사 데이터 불러오기
all=read.csv('국민여가활동조사변수정리(all).csv')
df_2021=read.csv('국민여가활동조사거주지코드정리_cl(2021).csv')

#1.일과 여가 균형따른 행복지수 평균 차이
all %>%
  mutate(일과여가의균형정도=as.factor(Q28)) %>%
  mutate(일과여가의균형정도=ifelse(일과여가의균형정도=='1','일에집중',
                          ifelse(일과여가의균형정도=='7','여가에집중',
                                          ifelse(일과여가의균형정도=='4','균형',일과여가의균형정도)))) %>% 
  mutate(일과여가의균형정도=fct_relevel(일과여가의균형정도,c('일에집중','2','3','균형','5','6'))) %>% 
  group_by(일과여가의균형정도) %>% 
  summarise(행복지수=mean(현재.행복.수준, na.rm=TRUE)) %>% 
  ggplot()+
  geom_bar(aes(x=일과여가의균형정도,y=행복지수, fill=일과여가의균형정도),
           stat='identity')+
  scale_y_break(c(0,6), space = 0)+
  ggtitle('          일과여가의균형정도에 따른 행복지수')


#2.가장 많이 참여한 여가활동 1~5순위 합

les=read.csv('C:/Users/82102/Desktop/깐부 project/여가활동정리/여가활동코드정리/여가활동.csv')

all %>% 
  select(Q2_1_1,Q2_1_2,Q2_1_3,Q2_1_4,Q2_1_5) %>% 
  pivot_longer(
    cols=Q2_1_1:Q2_1_5,
    names_to = '여가활동유형순위',
    values_to = '여가활동코드')%>% 
  left_join(les,by='여가활동코드') %>% 
  group_by(여가활동유형) %>% 
  summarise(횟수=n()) %>%
  arrange(-횟수) %>% 
  head(10) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가활동유형,횟수),y=횟수, fill=여가활동유형),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(x='여가활동유형', title='가장 많이 참여한 여가활동 1~5순위 합')+
  coord_flip()

#3. 가장 만족스런 여가활동 1~3순위
all %>% 
  select(Q4_1,Q4_2,Q4_3) %>% 
  pivot_longer(
    cols=Q4_1:Q4_3,
    names_to = '여가활동유형순위',
    values_to = '여가활동코드')%>% 
  left_join(les,by='여가활동코드') %>% 
  group_by(여가활동유형) %>% 
  summarise(횟수=n()) %>%
  arrange(-횟수) %>% 
  head(10) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가활동유형,횟수),y=횟수, fill=여가활동유형),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(x='여가활동유형', title='가장 만족스런 여가활동 1~3순위 합')+
  coord_flip()


#4.가장 많이 평일에 참여한 여가활동 및 희망하는 여가활동
all %>% 
  select(Q11_1_A,Q11_1_B,Q11_1_C,Q11_1_D,Q11_1_E,Q11_1_F,Q11_1_G,Q11_1_H) %>% 
  pivot_longer(
    cols=Q11_1_A:Q11_1_H,
    names_to = '여가활동유형순위',
    values_to = '여가활동코드')%>% 
  left_join(les,by='여가활동코드') %>%
  filter(!is.na(여가활동유형)) %>% 
  group_by(여가활동유형) %>% 
  summarise(횟수=n()) %>%
  arrange(-횟수) %>% 
  head(10) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가활동유형,횟수),y=횟수, fill=여가활동유형),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(x='여가활동유형', title='가장 많이 평일에 참여한 여가활동')+
  coord_flip()

all %>% 
  select(Q11_3_A,Q11_3_B,Q11_3_C,Q11_3_D,Q11_3_E,Q11_3_F,Q11_3_G,Q11_3_H) %>% 
  pivot_longer(
    cols=Q11_3_A:Q11_3_H,
    names_to = '여가활동유형순위',
    values_to = '여가활동코드')%>% 
  left_join(les,by='여가활동코드') %>%
  filter(!is.na(여가활동유형)) %>% 
  group_by(여가활동유형) %>% 
  summarise(횟수=n()) %>%
  arrange(-횟수) %>% 
  head(10) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가활동유형,횟수),y=횟수, fill=여가활동유형),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(x='여가활동유형', title='가장 많이 평일에 희망한 여가활동')+
  coord_flip()

#5.가장 많이 휴일에 참여한 여가활동 및 희망하는 여가활동
all %>% 
  select(Q12_1_A,Q12_1_B,Q12_1_C,Q12_1_D,Q12_1_E,Q12_1_F,Q12_1_G,Q12_1_H) %>% 
  pivot_longer(
    cols=Q12_1_A:Q12_1_H,
    names_to = '여가활동유형순위',
    values_to = '여가활동코드')%>% 
  left_join(les,by='여가활동코드') %>%
  filter(!is.na(여가활동유형)) %>% 
  group_by(여가활동유형) %>% 
  summarise(횟수=n()) %>%
  arrange(-횟수) %>% 
  head(10) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가활동유형,횟수),y=횟수, fill=여가활동유형),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(x='여가활동유형', title='가장 많이 휴일에 참여한 여가활동')+
  coord_flip()

all %>% 
  select(Q12_3_A,Q12_3_B,Q12_3_C,Q12_3_D,Q12_3_E,Q12_3_F,Q12_3_G,Q12_3_H) %>% 
  pivot_longer(
    cols=Q12_3_A:Q12_3_H,
    names_to = '여가활동유형순위',
    values_to = '여가활동코드')%>% 
  left_join(les,by='여가활동코드') %>%
  filter(!is.na(여가활동유형)) %>% 
  group_by(여가활동유형) %>% 
  summarise(횟수=n()) %>%
  arrange(-횟수) %>% 
  head(10) %>% 
  ggplot()+
  geom_bar(aes(x=fct_reorder(여가활동유형,횟수),y=횟수, fill=여가활동유형),
           stat='identity')+
  theme(legend.position = 'none')+
  labs(x='여가활동유형', title='가장 많이 휴일에 희망한 여가활동')+
  coord_flip()





















