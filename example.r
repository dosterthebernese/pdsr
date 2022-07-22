library(ggthemes)
library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)
library(ClusterR)
library(gtools)
library(RPostgreSQL)
library(DBI)

mainDir <- "."
subDir <- "publish"

ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)

db <- 'tradellama'  #provide the name of your db
host_db <- 'localhost' #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
db_port <- '5432'  # or any other port specified by the DBA
db_user <- 'postgres'  
db_password <- 'Z3tonium'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
tables <- dbListTables(con) 
print(con)
print(tables)
dydxd <- as.data.frame(dbGetQuery(con, 'SELECT d.asset_pair, dd.dydx_id, d.as_of, (dd.trailing_standard_deviation - dd.trailing_halved_standard_deviation) / dd.trailing_standard_deviation, dd.delta FROM dydx d, dydxd dd where d.id = dd.dydx_id order by as_of'))


for(i in unique(dydxd$asset_pair) %>% sort) {

  print(i)

  dydxdf <- filter(dydxd,asset_pair==i)
  min_as_of = min(dydxd$as_of)
  max_as_of = max(dydxd$as_of)
  print(min_as_of)
  print(max_as_of)
  caption_text = sprintf("%s %s %s", "KM GMM", min_as_of, max_as_of)
#  caption_text = sprintf(%s %s %s, "K-Means Gaussian Mixture Model", min_as_of, max_as_of)

  df <- data.frame("T10MIN5MINDSTD"=as.numeric(dydxdf[[4]]),"D10M"=as.numeric(dydxdf[[5]]))

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
#  print(df)
  colnames(df) <- c('T10MIN5MINDSTD','D10M','Cluster')

  ggplot(df, aes(x=T10MIN5MINDSTD,y=D10M)) +
    geom_point(aes(color=factor(Cluster), alpha=0.3)) +
    #BTC
    # xlim(-2.0,2.0) +
    # ylim(-5.0,5.0) +
    theme_tufte() +
    guides(alpha="none") +
    scale_color_simpsons() +
    scale_fill_simpsons() +
     labs(y="10 Min Future Perormance Delta", x="10 Min vs 5Min Trailing Vol", title=i, caption=caption_text) +
    theme(
      axis.title.x = element_text(color="#e3120b", size=12, face="bold", margin = margin(t = 10, r = 20, b = 0, l = 0)),
      axis.title.y = element_text(color="#e3120b", size=12, face="bold"),
      axis.text.x = element_text(angle = 90, vjust = 1)
      )

#  ggsave(sprintf("%s/%s-%s-%s-%s","clusterdb",i,dumb_hack_1,dumb_hack_2,"dydxmarkets-triple-negvdpv.svg"), width=8, height=6, dpi=300,  units="in")
  ggsave(sprintf("%s/%s-%s","publish",i,"g2d-t10mint5mind-f10md.svg"), width=8, height=6, dpi=300,  units="in")
  ggsave(sprintf("%s/%s-%s","publish",i,"g2d-t10mint5mind-f10md.png"), width=8, height=6, dpi=300,  units="in")

  dfcsv <- data.frame("AsOf"=as.character(dydxdf[[3]]), "T10MIN5MINDSTD"=as.numeric(dydxdf[[4]]),"D10M"=as.numeric(dydxdf[[5]]))

  write.csv(dfcsv,sprintf("%s/%s-%s","publish",i,"g2d-t10mint5mind-f10md.csv"), row.names = FALSE)

}






