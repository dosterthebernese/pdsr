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
dydxd <- as.data.frame(dbGetQuery(con, 'SELECT d.asset_pair, dd.dydx_id, dd.trailing_2h_standard_deviation, dd.delta_10min FROM dydx d, dydxd dd where d.id = dd.dydx_id'))
#df <- data.frame("AssetPair"=as.character(dydxd[[1]]),"ID"=as.numeric(dydxd[[2]]))
df <- data.frame("T2STD"=as.numeric(dydxd[[3]]),"D10M"=as.numeric(dydxd[[4]]))
print(df)

res <- GMM(
  df,
  gaussian_comps = 4,
  dist_mode = "eucl_dist",
  seed_mode = "random_subset",
  km_iter = 10,
  em_iter = 5,
  verbose = FALSE,
  var_floor = 1e-10,
  seed = 1
)

pr = predict(res, newdata = df)

df$col3 <- pr 
print(df)
colnames(df) <- c('T2STD','D10M','Cluster')

ggplot(df, aes(x=T2STD,y=D10M)) +
  geom_point(aes(color=factor(Cluster), alpha=0.3)) +
  xlim(-.10,.10) +
  ylim(-.10,.10) +
  theme_tufte() +
  guides(alpha=FALSE) +
  scale_color_simpsons() +
  scale_fill_simpsons() +
   labs(y="", x="", title="tmp", caption="tmp") +
  theme(
    axis.title.x = element_text(color="#e3120b", size=12, face="bold", margin = margin(t = 10, r = 20, b = 0, l = 0)),
    axis.title.y = element_text(color="#e3120b", size=12, face="bold"),
    axis.text.x = element_text(angle = 90, vjust = 1)
    )

#  ggsave(sprintf("%s/%s-%s-%s-%s","clusterdb",i,dumb_hack_1,dumb_hack_2,"dydxmarkets-triple-negvdpv.svg"), width=8, height=6, dpi=300,  units="in")
ggsave(sprintf("%s","tmp.png"), width=8, height=6, dpi=300,  units="in")





