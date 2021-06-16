#r 실행 버젼유의
#64비트에서 실행해야 에러가 적게남
# 첨부된 ojdbc6.jar 파일을 다운로드 받아서 C:\ 아래에 두세요.
install.packages("rJava")
install.packages("RJDBC")  

library(rJava)
library(RJDBC)  
library(dplyr)

#데이터베이스 연결 설정
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver"
                   , classPath="C://Database/oracle_db/ojdbc6.jar")

conn <- dbConnect(jdbcDriver, 
                  "jdbc:oracle:thin:@localhost:1521/xe", "system", "oracle")

#sql을 사용해서 r로 데이터 가져오기
df_happiness <- dbGetQuery(conn, "SELECT * FROM system.happiness")


# 나라별 group_by 한 것 (연도 삭제한 것) 
happiness_new <- df_happiness %>% 
  group_by(COUNTRY) %>% 
  summarise(HAPPINESS = mean(HAPPINESS),
            GDP = mean(GDP),
            SUPPORT = mean(SUPPORT),
            HEALTHY = mean(HEALTHY),
            FREEDOM = mean(FREEDOM),
            DONATION = mean(DONATION),
            CORRUPTION = mean(CORRUPTION),
            POSITIVE = mean(POSITIVE),
            NEGATIVE = mean(NEGATIVE),
            )
#View(happiness_new)
str(happiness_new)

#### 구글 차트 연동 ####
library(googleVis)
library(dplyr)
#### 나라별 행복도 막대 그래프 ####

country_happiness  <- happiness_new[, c(1,2)]
country_happiness_arrange <- arrange(country_happiness, HAPPINESS)
# 나라별 행복도 상위 12개국
barplot <- gvisColumnChart(tail(country_happiness_arrange, 12),
                           options=list(title="Top 12 countries in happiness by country",
                           height=400,
                           weight=500))
plot(barplot)
# 나라별 행복도 하위 12개국
barplot <- gvisColumnChart(head(country_happiness_arrange, 12),
                           options=list(title="The bottom 12 countries in happiness by country",
                                        height=400,
                                        weight=500))
plot(barplot)


#########################################################################
#### 지도 시각화 (나라별 행복도)####
#View(happiness_new)
geochart <- gvisGeoChart(happiness_new, locationvar="COUNTRY", 
                 colorvar="HAPPINESS", 
                 options=list(projection="kavrayskiy-vii")) 
plot(geochart)

#### 지도 시각화 (나라별 기부율)####
#View(happiness_new)
geochart <- gvisGeoChart(happiness_new, locationvar="COUNTRY", 
                         colorvar="DONATION", 
                         options=list(projection="kavrayskiy-vii")) 
plot(geochart)
#### 지도 시각화 (나라별 삶 선택에 대한 자유도)####
#View(happiness_new)
geochart <- gvisGeoChart(happiness_new, locationvar="COUNTRY", 
                         colorvar="FREEDOM", 
                         options=list(projection="kavrayskiy-vii")) 
plot(geochart)

#########################################################################
#### 버블차트 시각화 ####
#GDP 상위 15개국 / 버블 차트 
#- 축
#    - X 축 : GDP
#    - Y 축 : 행복도
#    - 원 크기 : 부패에 대한 인식 (높을수록 부패했다고 느낌)
df_donation <- arrange(happiness_new, desc(GDP))
#View(df_donation)
bubbleChart <- gvisBubbleChart(head(df_donation, 15), idvar = "COUNTRY", 
                       xvar = "GDP", 
                       yvar = "HAPPINESS", 
                       colorvar = "COUNTRY", 
                       sizevar = "CORRUPTION", 
                       options = list(title="Top 15 countries in GDP",width=600, height=600,colorAxis="{colors: ['lightblue', 'blue']}"))
plot(bubbleChart)
####################################################################################
#### 산점도 그래프 ####
library(ggplot2)
g2 <- happiness_new %>% 
  group_by(COUNTRY) %>% 
  summarise(HAPPINESS,
            GDP
  )

###3.(1)1인당 GDP - 행복도 (나라별) 산점도 그래프 ####
p1 <- ggplot(data = happiness_new, aes(x = GDP, y = HAPPINESS )) + geom_point()

###3.(2) 사회적 지원 - 행복도 (나라별) ####
p2 <- ggplot(data = happiness_new, aes(x = SUPPORT, y = HAPPINESS)) + geom_point(
)

###3.(3) 인생 선택의 자유 - 행복도 ####
p3 <- ggplot(data = happiness_new, aes(x = FREEDOM, y = HAPPINESS)) + geom_point()

###3.(4) 관대(기부) - 행복도 ####
p4 <- ggplot(data = happiness_new, aes(x = DONATION, y = HAPPINESS)) + geom_point()

###3.(5) 부패에 대한 인식 - 행복도 #### 
p5 <- ggplot(data = happiness_new, aes(x = CORRUPTION, y = HAPPINESS)) + geom_point()

###3.(6) 1인당 GDP - 기대 수명 ####
p6 <- ggplot(data = happiness_new, aes(x = GDP, y = HEALTHY)) + geom_point()

# 많은 사람들이 자신의 나라가 부패했다고 여긴다

#### 3.(7) 상관계수 구하기 ####

# gdp와 happy
cor(df_happiness$GDP, df_happiness$HAPPINESS, use='complete.obs', method='pearson')

# 사회적 지원과 happy
cor(df_happiness$SUPPORT, df_happiness$HAPPINESS, use='complete.obs', method='pearson')

# 인생 선택의 자유와 happy
cor(df_happiness$FREEDOM, df_happiness$HAPPINESS, use='complete.obs', method='pearson')

# 기부와 happy
cor(df_happiness$DONATION, df_happiness$HAPPINESS, use='complete.obs', method='pearson')

# 부패에 대한 인식과 happy
cor(df_happiness$CORRUPTION, df_happiness$HAPPINESS, use='complete.obs', method='pearson')

# gdp와 기대수명
cor(df_happiness$GDP, df_happiness$HEALTHY, use='complete.obs', method='pearson')


### 3.(8) 제목 및 축 이름 변경
p1 <- p1 + labs(title = "GDP에 따른 행복도",
                subtitle="0.7717",
                x = "GDP", y = "happy")

p2 <- p2 + labs(title = "사회적 지원에 따른 행복도", 
                subtitle="0.7097", x = "support",y = "")

p3 <- p3 + labs(title = "선택의 자유에 따른 행복도", 
                subtitle="0.5212", x = "freedom", y = "")

p4 <- p4 + labs(title = "기부에 따른 행복도", 
                subtitle="0.0287", x = "donation", y = "")

p5 <- p5 + labs(title = "부패에 대한 인식에 따른 행복도", subtitle="-0.4118",
                x = "corruption", y = "")

p6 <- p6 + labs(title = "GDP에 따른 기대수명", subtitle="0.5570",
                x = "GDP", y = "healthy")
#### 3.(8) 창 분할 #####
#install.packages("gridExtra")
library(gridExtra)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=6)




####################################################################################
#### 우리나라 data의 활용 ####
## 라이브러리
library(ggplot2)
library(dygraphs)
library(xts)

## 우리나라의 데이터만 추출
korea_happiness <- df_happiness %>%
  filter(COUNTRY == "South Korea")
#View(korea_happiness)

## 날짜를 따로 파생변수 유닉스로 추가하였음
korea_happiness$YEARS <- c(38718
                           ,39083
                           ,39448
                           ,39814
                           ,40179
                           ,40544
                           ,40909
                           ,41275
                           ,41640
                           ,42005
                           ,42370
                           ,42736
                           ,43101
                           ,43466
                           ,43831
)

## GDP와 행복도 시간순서의 속성을 지니는 xts타입으로 변경.
## 받은 숫자를 DATE 타입으로 캐스팅함.
## GDP - happy
korea_GDP <- xts(korea_happiness$GDP/1.75, order.by = as.Date.numeric(korea_happiness$YEARS, origin = "1900-01-01"))
korea_happy <- xts(korea_happiness$HAPPINESS, order.by = as.Date.numeric(korea_happiness$YEARS, origin = "1900-01-01"))

## 두 그래프 결합
korea_GDP_happy <-cbind(korea_GDP, korea_happy)

## 그래프에 이름을 붙여줌
colnames(korea_GDP_happy) <-c("GDP", "HAPPINESS")
head(korea_GDP_happy)

## 그래프를 그림
dygraph(korea_GDP_happy) %>% dyRangeSelector()


## suppot - happy
korea_surpport <- xts(korea_happiness$SUPPORT, order.by = as.Date.numeric(korea_happiness$YEARS, origin = "1900-01-01"))
korea_happy <- xts(korea_happiness$HAPPINESS/7.5, order.by = as.Date.numeric(korea_happiness$YEARS, origin = "1900-01-01"))

## 두 그래프 결합
korea_surpport_happy <-cbind(korea_surpport, korea_happy)

## 그래프에 이름을 붙여줌
colnames(korea_GDP_happy) <-c("surpport", "HAPPINESS")
head(korea_surpport_happy)

## 그래프를 그림
dygraph(korea_surpport_happy) %>% dyRangeSelector()
