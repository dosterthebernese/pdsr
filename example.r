library(ggthemes)
library(ggplot2)
library(ggsci)
library(scales)
library(reshape2)
#library(hrbrthemes)
library(xtable)
library(dplyr)
library(treemapify)
library(ClusterR)
library(gtools)
library(RPostgreSQL)
library(DBI)

# sudo apt-get install libpq-dev
# sudo R
# install.packages('RPostgreSQL')
# install.packages('RPostgres')

print('foo')

db <- 'tradellama'  #provide the name of your db
host_db <- 'localhost' #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
db_port <- '5432'  # or any other port specified by the DBA
db_user <- 'postgres'  
db_password <- 'Z3tonium'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
tables <- dbListTables(con) 
print(con)
print(tables)
dydxd <- as.data.frame(dbGetQuery(con, 'SELECT d.asset_pair, dd.dydx_id FROM dydx d, dydxd dd where d.id = dd.dydx_id'))
df <- data.frame("AssetPair"=as.character(dydxd[[1]]),"ID"=as.numeric(dydxd[[2]]))
print(df)






# process_me <- list.files(path='~/unprocessed_cluster_bombs',pattern="nfrpv.csv")


# for(ifile in process_me) {

#   ifile_with_dir = paste("~/unprocessed_cluster_bombs/",ifile,sep="")
#   print(ifile_with_dir)

#   theda <- read.csv(ifile_with_dir)

#   thedaframed <- data.frame("Market"=as.character(theda[[1]]),
#     "Min"=as.character(theda[[2]]),
#     "Max"=as.character(theda[[3]]),
#     "Dur"=as.numeric(theda[[4]]),
#     "NFR"=as.numeric(theda[[5]]),
#     "T10VOL"=as.numeric(theda[[6]]),
#     "FUTPERF"=as.numeric(theda[[7]]),
#     "OID"=as.numeric(theda[[8]]),
#     "Snapshot"=as.character(theda[[9]]),
#     "Price"=as.numeric(theda[[10]])
#     )



#   for(i in unique(thedaframed$Market) %>% sort) {

#     print(i)

#     dumb_hack_1 <- unique(thedaframed$Min)
#     dumb_hack_2 <- unique(thedaframed$Max)
#     dumb_hack_3 <- unique(thedaframed$Dur)

#     md <- filter(thedaframed,Market==i)
#     mdlite <- data.frame(
#       "NFR"=as.numeric(md[[5]])*100,
#       "T10VOL"=as.numeric(md[[6]]),
#       "FUTPERF"=as.numeric(md[[7]])
#       )

#     first_price = md$Price[1]
#     last_price = md$Price[nrow(md)]
#     interval_return = (last_price - first_price) / first_price

#     std = sd(md$Price)
#     mea = mean(md$Price)
#     vol = std / mea
#     interval_volatility = vol


#     dumber <- sprintf("%s", "Data from DYDX")
#     better_title <- sprintf("%s p: %f v: %f kmeans 3d", i, interval_return, interval_volatility)

#     res <- KMeans_arma(
#       mdlite,
#       clusters = 4,
#       n_iter = 10,
#       seed_mode = "random_subset",
#       verbose = FALSE,
#       CENTROIDS = NULL,
#       seed = 1
#     )


#     pr = predict_KMeans(mdlite, res, threads = 1)

# #    print(pr)

#     mdlite$col4 <- pr 
#     colnames(mdlite) <- c('NFR','T10VOL','FUTPERF','Cluster')


#     ggplot(mdlite, aes(x=FUTPERF,y=NFR)) +
#       geom_point(aes(color=factor(Cluster), size=T10VOL), alpha=0.3) +
#       # R will tell you if your limits cause you to lose data, so you might have to change these.
#       # But you set these because you need apples apples and now not using facet (due to size)
#       xlim(-.10,.10) +
#       ylim(-.01,.01) +
#       theme_tufte() +
#       guides(alpha=FALSE) +
#   #    scale_y_continuous(labels = scales::label_number_si()) +
#   #    facet_grid(vars(Market)) +
#       scale_color_simpsons() +
#       scale_fill_simpsons() +
#        labs(y="", x="", title=better_title, caption=dumber) +
#       theme(
#         axis.title.x = element_text(color="#e3120b", size=12, face="bold", margin = margin(t = 10, r = 20, b = 0, l = 0)),
#         axis.title.y = element_text(color="#e3120b", size=12, face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 1)
#         )

#   #  ggsave(sprintf("%s/%s-%s-%s-%s","clusterdb",i,dumb_hack_1,dumb_hack_2,"dydxmarkets-triple-nfrpv.svg"), width=8, height=6, dpi=300,  units="in")
#     ggsave(sprintf("%s/%s-%s-%s-%s","clusterdb",i,dumb_hack_1,dumb_hack_2,"dydxmarkets-triple-nfrpv.png"), width=8, height=6, dpi=300,  units="in")



#     ggplot(mdlite, aes(x=FUTPERF,y=NFR)) +
#       geom_point(aes(color=factor(Cluster), size=T10VOL), alpha=0.3, show.legend=FALSE) +
#       # R will tell you if your limits cause you to lose data, so you might have to change these.
#       # But you set these because you need apples apples and now not using facet (due to size)
#       xlim(-.10,.10) +
#       ylim(-.01,.01) +
#       theme_tufte() +
#       guides(alpha=FALSE) +
#       scale_color_simpsons(guide=FALSE) +
#       scale_fill_simpsons(guide=FALSE) +
#        labs(y="", x="", title=i, caption="") +
#       theme(
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank()
#         )

#   #  ggsave(sprintf("%s/%s-%s-%s-%s","clusterdb",i,dumb_hack_1,dumb_hack_2,"dydxmarkets-triple-nfrpv.svg"), width=8, height=6, dpi=300,  units="in")
#     ggsave(sprintf("%s/%s-%s-%s-%s","thumbnails",i,dumb_hack_1,dumb_hack_2,"dydxmarkets-triple-nfrpv.png"), width=4, height=3, dpi=300,  units="in")




#   }





# }



