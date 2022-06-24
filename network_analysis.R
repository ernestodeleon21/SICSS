


#Loading Data ##########

library(academictwitteR)
library(tidyverse)



load('C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/90k_tweets_musk.rda')



tweetsmusk2$text
#tm = tweetsmusk2 %>% mutate(ments = str_extract(text, "(?<=^|\\s)@[^\\s]+"))


table(tweetsmusk2$lang)

#Preprocessing ##########

mylist = list()

for(a in c(1:90000)){
  b = tweetsmusk2$entities$mentions[[a]][[4]]
  if(is.null(b)){
      b = 'NA'
      }
  #final <- list(final, b
  mylist[[a]]<-b
}



tweetsmusk2$ments2 = mylist

coms = list()

for(a in c(1:90000)){
  b = length(mylist[[a]])
  coms = append(coms,b)
}

max(coms)



tweetsmusk2 = tweetsmusk2 %>% mutate(ments2 = gsub("c", "", ments2))
tweetsmusk2 = tweetsmusk2 %>% mutate(ments2 = gsub("\"", "", ments2))
tweetsmusk2 = tweetsmusk2 %>% mutate(ments2 = gsub("[()]", "", ments2))
tweetsmusk2 = tweetsmusk2 %>% mutate(ments2 = gsub(" ", "", ments2))




tm0 = tweetsmusk2 %>% dplyr::filter(ments2 != 'NA') %>% dplyr::select(author_id, ments2)






tm = tm0 %>% 
   separate(ments2,c("a1","a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10"),sep=",")





tm2 = tm  %>%
  pivot_longer(!c(author_id), names_to = "cols", values_to = "mentions") %>%
  dplyr::select("author_id", "mentions") %>% dplyr::filter(!is.na(mentions))

  
#sum.ment.2 = tm2 %>% group_by(mentions) %>% summarize(n=n())

#tm2.2 = left_join(tm2, sum.ment.2)


#tm2.3 = tm2.2 %>% filter(n > 10)


#cr = tweetsmusk2 %>% group_by(author_id) %>% summarize(n=n())

#write_csv(cr,'C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/author_ids.csv' )

############# Analysis ####################

library(tidyverse)
library(igraph)
require(ForceAtlas2)
library(RColorBrewer)
library(classInt)






#tm2.3.sum = tm2.3 %>% group_by(mentions) %>% summarize(n=n(), lang =lang)


tm2.sum = tm2 %>% group_by(mentions) %>% summarize(n=n())#, lang =lang)

#no Musk
#tm2.m = tm2 %>% filter(mentions != '44196397')
#tm2.sum = tm2.m %>% group_by(mentions) %>% summarize(n=n())


mentions.ig <- graph_from_edgelist(
            as.matrix(
                tm2[,c("author_id","mentions")]
                )
            )


main.component <- function(graph, ...) {
  cl <- clusters(graph, ...)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}

#mentions.ig.main = main.component(mentions.ig)

mentions.ig.main = mentions.ig


samp.attr <- data.frame( #Making arributes
  author_id = V(mentions.ig.main)$name,
  node.seq = 1:length(V(mentions.ig.main)$name))
  #)# %>% left_join(tm2,
            #by = c("author_id", "mentions"))




sampt.attr2 = merge(samp.attr, tm2.sum, by.x="author_id", by.y="mentions", all.x=T, all.y=F)

sampt.attr2 = sampt.attr2 %>% arrange(node.seq)


sampt.attr2 = sampt.attr2 %>% mutate(n = ifelse(is.na(n), 0, n))





makeTransparent = function(..., alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}



langs = data.frame(table(sampt.attr2$lang))

table(sampt.attr2$lang)

sampt.attr2 <- sampt.attr2 %>%
  mutate(lang.color = case_when(lang == "en" ~ "#0087DC",
                                lang == "pt" ~ "#D46A4C",
                                lang == 'es' ~ "#E4003B",
                                lang == "qme" ~ "#FDBB30",
                                lang == "hu" ~ "#005B54",
                                TRUE ~ "#FFFF00"))

sampt.attr2 = sampt.attr2 %>% mutate(lang = ifelse(is.na(lang), 'other', lang))





table(sampt.attr2$lang.color)

table((sampt.attr2$n^(1/3)))



table(log(1+sampt.attr2$n))



pdf("C:/Users/ernes/OneDrive - Universitaet Bern/IKMB/PhD Classes/Summer Schools 2022/SICSS/project/main_comp_lang_all_sqrt.pdf", width = 12)

plot(simplify(mentions.ig.main), 
     vertex.label = NA,
     edge.width =0.1,
     vertex.size = sqrt(sampt.attr2),
     #vertex.size = sampt.attr2$n^(1/3),
     #vertex.size = log(1+sampt.attr2$n),
     #vertex.size = 2,
     #vertex.color = factor(sampt.attr2$lang),
     vertex.color = sampt.attr2$lang.color,
     edge.arrow.size = 0)
dev.off()


colnames(tm2)

samp.attr <- data.frame( #Making arributes
  usermentions = V(mentions.ig.main)$name,
  node.seq = 1:length(V(mentions.ig.main)$name),
  degree.out = degree(mentions.ig.main, mode = "out"),
  between.dir = betweenness(mentions.ig.main, directed = T,normalized = T),
  between.undir = betweenness(mentions.ig.main, directed = F, normalized = T)
  ) 




# explore centrality scores
# who are the most central nodes?
# what is the shape of the degree distribution?
# how does in degree vary by party (e.g. are members of any party more central?)
# how could we add other centrality measures, e.g. betweenness?
# how can we create a summary dataset using dplyr?



hist(samp.attr$degree.out)
hist(samp.attr$degree.out[samp.attr$degree.out>0])

ggplot(samp.attr, aes(y = party, x = degree.out))+
  geom_boxplot()

ggplot(samp.attr, aes(y = party, x = between.dir))+
  geom_boxplot()

ggplot(samp.attr, aes(y = party, x = between.undir))+
  geom_boxplot()

plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = sqrt(1 + samp.attr$degree.out),
     vertex.color = samp.attr$party.color,
     edge.arrow.size = 0)


plot(simplify(ig.samp), 
     vertex.label = NA, 
     vertex.size = sqrt(2 + samp.attr$between.undir*1000),
     vertex.color = samp.attr$party.color,
     edge.arrow.size = 0)






