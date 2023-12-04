apriori_article<-function(){
  library(KoNLP)
  library(arulesViz)
  library(wordcloud2)
  library(stringr)
  library(arules)
  library(dplyr)
  library(ggplot2)
  library(webshot)
  library(png)
  library(htmlwidgets)
  setwd('d:\\data\\Project')
  arti<-read.csv('d:\\data\\Project\\d\\R_refine_need.csv',header=F)[-3]
  noun <- sapply(arti[2], extractNoun, USE.NAMES=F)
  for (i in 1:length(noun)){
    noun[[i]]<-gsub('자영','자영업',noun[[i]])
    noun[[i]]<-gsub('업자','자영업',noun[[i]])
    noun[[i]]<-gsub('비대','비대면',noun[[i]])
  }
  
  noun_list<-unlist(noun)
  nouns<-Filter(function(x){nchar(x)>=2},noun_list)
  sublist<-c('서울','경제','관련','국회','올해','지난해','가입','확인','자료','정도','경기','구축','대상','지대','산업','취약'
             ,'계층','특수','한국판','건설','처방','시대','베이징','기준','취재','차관','상담',
             '사업','활용','극복','사회')
  for (rl in sublist){nouns<-gsub(rl,"",nouns)}
  
  nouns <- str_replace_all(nouns, "[^[:alpha:]]","")
  
  
  nouns<-nouns[nouns!=""]
  
  # 단어 table 정렬
  nouncount<-sort(table(nouns),decreasing = T)
  nouncount<-nouncount[nouncount>2]
  collist<-names(nouncount)
  contents <- c()
  #데이터프레임 만들기
  for(i in 1:length(noun)) {
    inter <- intersect(noun[[i]] , collist)
    contents <- rbind(contents ,table(inter)[collist])
  }
  #워드 클라우드 그리기기
  
  wordcount<-nouncount[nouncount>10]
  imgFile<-wordcloud2(wordcount,size=0.3,backgroundColor = 'black',color=rainbow(50),shape='pentagon')
  saveWidget(imgFile,'wc.html',selfcontained = F)
  webshot('wc.html','wc.png',delay=5,vwidth=480,vheight=480)
  # 저장 파일 불러오기
  # p = readPNG("wc.png")
  # windows(width=589, height=600)
  # par(mar=c(1.1, 1.1, 1.1, 1.1))  # margin 의 line number 를 지정, bottom, left, top, right 순.
  # plot(0,0, type='n', xlim=c(1,600), ylim=c(1,589), axes=FALSE, xlab='', ylab='', asp=1)    # 빈그림 그리기, 배경이 흰색으로 변함.
  # rasterImage(p, 1, 1, 600, 589)    # rasterImage(이미지데이터, 왼쪽, 아래, 오른쪽, 위)
  contents[which(is.na(contents))] <- 0
  colnames(contents)<-collist
  
  
  trans<-as.matrix(contents,"Transaction")
  
  while(1){
    n<-readline('연관 규칙을 확인하려면 1 아니면 0을 입력하세요 : ')
    if (n=='1'){
      sup<-as.numeric(readline('하한 지지도를 입력해주세요 : '))
      article_rule <- apriori(trans , parameter = list(supp = sup , conf = 0.5 , target = "rules"))
      res1<-inspect(sort(article_rule,decreasing = T))
      res1<-res1[,-8]
      result<-transform(res1,IS=sqrt(lift*support))
      result[1:30,]
      yn<-readline('세부 연관 규칙을 확인하시겠습니까?(Y/N) : ')
      if(toupper(yn)=='Y'){
        hs<-readline('검색어 기준을 입력하세요(lhs/rhs) : ')
        if(hs=='lhs'){
          sn<-as.numeric(readline('검색할 단어의 개수를 입력하세요 : '))
          sl<-c()
          for (i in 1:sn){
            snn<-readline('검색어 : ')
            sl<-append(sl,snn)
          }
          res<-inspect(subset(article_rule,lhs %in% sl))
          res<-res[,-8]
          res<-transform(res,IS=sqrt(lift*support))
          # if (nrow(res)>15){
          #   res<-res[1:15,]
          # }
          cat('\n<',sl,'>을 포함하는 연관규칙은 다음과 같습니다.\n')
          print(res[order(-res$IS),])
          
        }
        if(hs=='rhs'){
          snn<-readline('검색어 : ')
          res<-inspect(subset(article_rule,rhs %in% snn))
          res<-res[,-8]
          
          res<-transform(res,IS=sqrt(lift*support))
          res[order(-res$IS),]
          # if (nrow(res)>15){
          #   res<-res[1:15,]
          # }
          cat('\n<',snn,'>을 포함하는 연관규칙은 다음과 같습니다.\n')
          print(res[order(-res$IS),])
        }
        
        # 그래프로 규칙 나타내기
        if(nrow(res)>15){
          df<-as.data.frame(res[1:15,])
        }else{df<-as.data.frame(res)}
        colnames(df)[2]<-'Direction'
        
        df<-df%>% arrange(-IS)
        dev.new()
        
        
        plot(df %>%
               
               mutate(Rules = paste0(lhs,Direction,rhs)) %>%
               
               
               top_n(n=15, wt = IS) %>%
               
               arrange(-IS) %>%
               
               ggplot() +
               
               geom_point(aes(x = reorder(Rules, IS), y = IS, size = IS, col = IS),
                          
                          stat = 'identity') +
               
               scale_color_gradientn(colours = c("66B2FF", "#044C99")) +
               
               scale_size_continuous(range = c(4,10)) +
               
               ylab("신뢰도") +xlab("규칙") +
               
               guides(size = FALSE, col = FALSE) +
               
               theme_bw() +
               
               theme(text = element_text(size = 15, face = "bold")) +
               
               coord_flip())
        
        ans<-readline('네트워크형태의 연관도를 확인하시겠습니까? (Y/N)')
        if (toupper(ans)=='Y'){
          # net_tab<-nouncount
          # net_name<-names(net_tab)
          # net_noun<- c()
          # for(i in 1:length(noun)) {
          #   inter <- intersect(noun[[i]] , net_name)
          #   net_noun <- rbind(net_noun ,table(inter)[net_name])
          # }
          # net_noun[which(is.na(net_noun))] <- 0
          # colnames(net_noun)<-net_name
          
          net_article_rule <- apriori(trans , parameter = list(supp = 0.07 , conf = 0.3 , target = "rules"))
          dev.new()
          plot(net_article_rule[1:50,],
               
               method='graph',
               
               control=list(type='items'),
               
               vertex.label.cex=0.01,
               edge.arrow.width=0.1,
               edge.arrow.size=0.05,
               vertex.size=0.5,
               vertex.label.dist=0,
               edge.width=0.01,
               edge.arrow.width=2)
        }
      }
    }else{break}
  }
}
apriori_article()

