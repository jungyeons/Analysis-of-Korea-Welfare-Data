install.packages("foreign")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("haven")
install.packages("readxl")
install.packages("xlsx")

library(foreign)
library(dplyr)
library(ggplot2)
library(haven)
library(readxl)
library(xlsx)

raw_data <- read_spss("Koweps_hpc16_2021_beta1.sav")
View(raw_data)




# 1. 나이, 성별에 의한 자원봉사활동 횟수 연평균 분석

volunteer_data <- raw_data
volunteer_data <- rename(volunteer_data, birth = h16_g4,gender = h16_g3, volunteer_activities = p1604_6)


# 모름/무응답 결측치 처리
volunteer_data$volunteer_activities <- ifelse(volunteer_data$volunteer_activities == 999, NA, volunteer_data$volunteer_activities)

# 태어난 연도를 통해 나이를 추가
volunteer_data$age <- 2021 - volunteer_data$birth + 1



volunteer_data <- volunteer_data %>%
  mutate(gender_type = ifelse(volunteer_data$gender == 1, "남자", ifelse(volunteer_data$gender == 2, "여자", NA)))

age_household_volunteer <- volunteer_data %>%
  filter(!is.na(volunteer_activities)) %>%
  group_by(age, gender_type) %>%
  summarise(mean_volunteer = mean(volunteer_activities))

ggplot(data = age_household_volunteer, aes(x = age, y = mean_volunteer, col=gender_type)) + geom_line()+ ggtitle("나이,  성별에  따른 자원봉사활동 연간 횟수 평균 분석") + xlab("나이") + ylab("자원봉사활동 연간횟수 평균") + labs(col = "성별")







# 2. 지역에 따른 집의 가격 
region_data <- raw_data
region_data <- rename(region_data, region = h16_reg7, price = h1606_6)
region_data$price <- ifelse(region_data$price == 0 | region_data$price == 999999, NA, region_data$price)

region_data$region <- ifelse(region_data$region == 1, "서울", ifelse(region_data$region == 2, "인천,경기", ifelse(region_data$region == 3, "부산,경남,울산", ifelse(region_data$region == 4, "대구.경북", ifelse(region_data$region == 5, "대전,충남", ifelse(region_data$region == 6, "강원,충북",ifelse(region_data$region == 7, "광주,전북,전남,제주도", NA  )))))))

region_price <- region_data %>%
  filter(!is.na(region) & !is.na(price)) %>%
  select(region, price)

ggplot(data = region_price, aes(x = region, y = price))+ geom_boxplot(width=0.8, outlier.size=1, outlier.shape=16, outlier.colour="blue") + stat_summary(fun="mean", geom="point", shape=22, size=1, fill="blue") + ggtitle("지역구분에 따른 집의 가격") + xlab("지역") + ylab("집 가격(단위: 만원)")  + scale_y_continuous(limits = c(0, 2e+05))









# 3. 장애 종류에 따른 월급 분석
disability_data <- raw_data  
disability_data <- rename(disability_data, disability_type = h16_g8, income = p1602_8aq1)
#결측치 제거(0은 비장애인임, 16은 비등록 장애인이라서 종류를 파악할 수 없어 제거)
disability_data$disability_type <- ifelse(disability_data$disability_type==0 |disability_data$disability_type==16,NA,disability_data$disability_type)
#결측치 제거된 것 확인
table(disability_data$disability_type)
#이름부여 및 데이터 가공
disability_data$disability_type <-ifelse(disability_data$disability_type == 1 | disability_data$disability_type ==2|disability_data$disability_type ==3|disability_data$disability_type == 4| disability_data$disability_type == 5| disability_data$disability_type == 13, "외부 신체기능의 장애", ifelse(disability_data$disability_type == 9| disability_data$disability_type == 10| disability_data$disability_type == 11| disability_data$disability_type == 12| disability_data$disability_type == 14| disability_data$disability_type == 15,"내부 기관의 장애", ifelse(disability_data$disability_type == 6| disability_data$disability_type == 7| disability_data$disability_type == 8,"정신적 장애",NA)))
#이상치 결측 처리(월급이 1~9998 사이 나타나기 때문에 0이거나 9999를 이상치 처리(코드북에 따라 모름/무응답이 9999임))
disability_data$income <- ifelse(disability_data %in% c(0,9999), NA,disability_data$income)
#장애 종류에 따른 평균 표 만듦
disability_income <- disability_data %>%
 filter(!is.na(disability_type)) %>%
 filter(!is.na(income)) %>%
 group_by(disability_type)%>%
 summarise(mean_income = mean(income))

disability_income
ggplot(data=disability_income,aes(x=disability_type,y=mean_income))+geom_col()

