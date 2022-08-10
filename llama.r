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
subDir <- sprintf("%s", "publish")
ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)

db <- 'tradellama'  #provide the name of your db
#host_db <- '35.226.13.55' #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
# better to run local
host_db <- 'localhost' #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
db_port <- '5432'  # or any other port specified by the DBA
db_user <- 'postgres'  
#db_password <- ''
db_password <- 'Z3tonium'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
tables <- dbListTables(con) 
print(con)
print(tables)


# gtedate = "\'2022-08-01 00:00:00 +00:00\'"
# ltdate = "\'2022-08-02 00:00:00 +00:00\'"

# gtedate = "\'2022-08-02 00:00:00 +00:00\'"
# ltdate = "\'2022-08-03 00:00:00 +00:00\'"

# gtedate = "\'2022-08-03 00:00:00 +00:00\'"
# ltdate = "\'2022-08-04 00:00:00 +00:00\'"

# gtedate = "\'2022-08-04 00:00:00 +00:00\'"
# ltdate = "\'2022-08-05 00:00:00 +00:00\'"

# gtedate = "\'2022-08-05 00:00:00 +00:00\'"
# ltdate = "\'2022-08-06 00:00:00 +00:00\'"

gtedate = "\'2022-08-06 00:00:00 +00:00\'"
ltdate = "\'2022-08-07 00:00:00 +00:00\'"



qry_text = sprintf("SELECT d.asset_pair, dd.dydx_id, d.as_of, 
  (dd.trailing_standard_deviation - dd.trailing_halved_standard_deviation) / dd.trailing_standard_deviation, 
  dd.delta, 
  d.index_price 
  FROM dydx d, dydxd dd 
  where d.id = dd.dydx_id and 
  as_of >= %s and 
  as_of < %s 
  order by as_of", gtedate,ltdate)


dydxd <- as.data.frame(dbGetQuery(con, qry_text))

for(i in unique(dydxd$asset_pair) %>% sort) {

  print(i)

  dydxdf <- filter(dydxd,asset_pair==i)
  min_as_of = min(dydxd$as_of)
  min_as_of_list = strsplit(as.character(min_as_of), " ")
  max_as_of = max(dydxd$as_of)
  max_as_of_list = strsplit(as.character(max_as_of), " ")
  print(min_as_of)
  print(max_as_of)

  print("assumes you're doing NO greater than 24 hours")
  folder = min_as_of_list[[1]][1] 
  tr = sprintf("%s-%s",min_as_of_list[[1]][2],max_as_of_list[[1]][2])
  print(folder)
  print(tr)

  mainDir <- "."
  subDir <- sprintf("%s/%s", "publish", folder)
  ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)

  caption_text = sprintf("%s %s %s", "KM GMM", min_as_of, max_as_of)
#  caption_text = sprintf(%s %s %s, "K-Means Gaussian Mixture Model", min_as_of, max_as_of)

  df <- data.frame("VOL"=as.numeric(dydxdf[[4]]),"DEL"=as.numeric(dydxdf[[5]]))

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
  colnames(df) <- c('VOL','DEL','Cluster')

  ggplot(df, aes(x=VOL,y=DEL)) +
    geom_point(aes(color=factor(Cluster), alpha=0.3)) +
    #You will have to work on this
    xlim(-4.0,4.0) +
    ylim(-4.0,4.0) +
    theme_tufte() +
    guides(alpha="none") +
    scale_color_simpsons() +
    scale_fill_simpsons() +
     labs(y="Delta", x="Vol", title=i, caption=caption_text) +
    theme(
      axis.title.x = element_text(color="#e3120b", size=12, face="bold", margin = margin(t = 10, r = 20, b = 0, l = 0)),
      axis.title.y = element_text(color="#e3120b", size=12, face="bold"),
      axis.text.x = element_text(angle = 90, vjust = 1)
      )

#  ggsave(sprintf("%s/%s-%s-%s-%s","clusterdb",i,dumb_hack_1,dumb_hack_2,"dydxmarkets-triple-negvdpv.svg"), width=8, height=6, dpi=300,  units="in")
  ggsave(sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"g2d-vol-del.svg"), width=8, height=6, dpi=300,  units="in")
  ggsave(sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"g2d-vol-del.png"), width=8, height=6, dpi=300,  units="in")

  dfcsv <- data.frame("AsOf"=as.character(dydxdf[[3]]), "VOL"=as.numeric(dydxdf[[4]]),"DEL"=as.numeric(dydxdf[[5]]))

  write.csv(dfcsv,sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"g2d-vol-del.csv"), row.names = FALSE)


  dfts <- data.frame("AsOf"=as.POSIXct(dydxdf[[3]], format="%Y-%m-%dT%H:%M:%S"), "Price"=as.numeric(dydxdf[[6]]))

  md <- filter(dydxd,asset_pair==i)

  first_price = dfts$Price[1]
  last_price = dfts$Price[nrow(md)]
  interval_return = (last_price - first_price) / first_price

  std = sd(dfts$Price)
  mea = mean(dfts$Price)
  vol = std / mea
  interval_volatility = vol

  caption_text = sprintf("%s %s", min_as_of, max_as_of)


  ggplot(dfts, aes(x=AsOf,y=Price)) +
    geom_line(size=1) +
    theme_tufte() +
    scale_color_simpsons() +
    scale_fill_simpsons() +
     labs(y="", x="", title=i, caption=caption_text) +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y = element_text(color="#e3120b", size=12, face="bold")
      )

  ggsave(sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"ts.svg"), width=8, height=6, dpi=300,  units="in")
  ggsave(sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"ts.png"), width=8, height=6, dpi=300,  units="in")




}

qry_text = sprintf("SELECT d.asset_pair, dd.dydx_id, d.as_of, 
  (dd.trailing_standard_deviation - dd.trailing_halved_standard_deviation) / dd.trailing_standard_deviation, 
  dd.delta, 
  d.index_price 
  FROM dydx d, dydxd dd 
  where d.id = dd.dydx_id and 
  dd.trailing_average < dd.trailing_halved_average and
  as_of >= %s and 
  as_of < %s 
  order by as_of", gtedate,ltdate)


dydxd <- as.data.frame(dbGetQuery(con, qry_text))


for(i in unique(dydxd$asset_pair) %>% sort) {

  print(i)

  dydxdf <- filter(dydxd,asset_pair==i)

  min_as_of = min(dydxd$as_of)
  min_as_of_list = strsplit(as.character(min_as_of), " ")
  max_as_of = max(dydxd$as_of)
  max_as_of_list = strsplit(as.character(max_as_of), " ")
  print(min_as_of)
  print(max_as_of)

  print("assumes you're doing NO greater than 24 hours")
  folder = min_as_of_list[[1]][1] 
  tr = sprintf("%s%s",min_as_of_list[[1]][2],max_as_of_list[[1]][2])
  print(folder)
  print(tr)

  mainDir <- "."
  subDir <- sprintf("%s/%s", "publish",folder)
  ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)

  caption_text = sprintf("%s %s %s", "KM GMM", min_as_of, max_as_of)
#  caption_text = sprintf(%s %s %s, "K-Means Gaussian Mixture Model", min_as_of, max_as_of)

  df <- data.frame("VOL"=as.numeric(dydxdf[[4]]),"DEL"=as.numeric(dydxdf[[5]]))

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
  colnames(df) <- c('VOL','DEL','Cluster')

  ggplot(df, aes(x=VOL,y=DEL)) +
    geom_point(aes(color=factor(Cluster), alpha=0.3)) +
    xlim(-4.0,4.0) +
    ylim(-4.0,4.0) +
    theme_tufte() +
    guides(alpha="none") +
    scale_color_simpsons() +
    scale_fill_simpsons() +
     labs(y="Delta", x="Vol", title=i, caption=caption_text) +
    theme(
      axis.title.x = element_text(color="#e3120b", size=12, face="bold", margin = margin(t = 10, r = 20, b = 0, l = 0)),
      axis.title.y = element_text(color="#e3120b", size=12, face="bold"),
      axis.text.x = element_text(angle = 90, vjust = 1)
      )

#  ggsave(sprintf("%s/%s-%s-%s-%s","clusterdb",i,dumb_hack_1,dumb_hack_2,"dydxmarkets-triple-negvdpv.svg"), width=8, height=6, dpi=300,  units="in")
  ggsave(sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"g2d-vol-del-avg-up.svg"), width=8, height=6, dpi=300,  units="in")
  ggsave(sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"g2d-vol-del-avg-up.png"), width=8, height=6, dpi=300,  units="in")

  dfcsv <- data.frame("AsOf"=as.character(dydxdf[[3]]), "VOL"=as.numeric(dydxdf[[4]]),"DEL"=as.numeric(dydxdf[[5]]))

  write.csv(dfcsv,sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"g2d-vol-del-avg-up.csv"), row.names = FALSE)

}



qry_text = sprintf("SELECT d.asset_pair, dd.dydx_id, d.as_of, 
  (dd.trailing_standard_deviation - dd.trailing_halved_standard_deviation) / dd.trailing_standard_deviation, 
  dd.delta, 
  d.index_price 
  FROM dydx d, dydxd dd 
  where d.id = dd.dydx_id and 
  dd.trailing_average > dd.trailing_halved_average and
  as_of >= %s and 
  as_of < %s 
  order by as_of", gtedate,ltdate)

dydxd <- as.data.frame(dbGetQuery(con, qry_text))


for(i in unique(dydxd$asset_pair) %>% sort) {

  print(i)

  dydxdf <- filter(dydxd,asset_pair==i)

  min_as_of = min(dydxd$as_of)
  min_as_of_list = strsplit(as.character(min_as_of), " ")
  max_as_of = max(dydxd$as_of)
  max_as_of_list = strsplit(as.character(max_as_of), " ")
  print(min_as_of)
  print(max_as_of)

  print("assumes you're doing NO greater than 24 hours")
  folder = min_as_of_list[[1]][1] 
  tr = sprintf("%s%s",min_as_of_list[[1]][2],max_as_of_list[[1]][2])
  print(folder)
  print(tr)

  mainDir <- "."
  subDir <- sprintf("%s/%s", "publish",folder)
  ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)

  caption_text = sprintf("%s %s %s", "KM GMM", min_as_of, max_as_of)
#  caption_text = sprintf(%s %s %s, "K-Means Gaussian Mixture Model", min_as_of, max_as_of)

  df <- data.frame("VOL"=as.numeric(dydxdf[[4]]),"DEL"=as.numeric(dydxdf[[5]]))

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
  colnames(df) <- c('VOL','DEL','Cluster')

  ggplot(df, aes(x=VOL,y=DEL)) +
    geom_point(aes(color=factor(Cluster), alpha=0.3)) +
    xlim(-4.0,4.0) +
    ylim(-4.0,4.0) +
    theme_tufte() +
    guides(alpha="none") +
    scale_color_simpsons() +
    scale_fill_simpsons() +
     labs(y="Delta", x="Vol", title=i, caption=caption_text) +
    theme(
      axis.title.x = element_text(color="#e3120b", size=12, face="bold", margin = margin(t = 10, r = 20, b = 0, l = 0)),
      axis.title.y = element_text(color="#e3120b", size=12, face="bold"),
      axis.text.x = element_text(angle = 90, vjust = 1)
      )

#  ggsave(sprintf("%s/%s-%s-%s-%s","clusterdb",i,dumb_hack_1,dumb_hack_2,"dydxmarkets-triple-negvdpv.svg"), width=8, height=6, dpi=300,  units="in")
  ggsave(sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"g2d-vol-del-avg-down.svg"), width=8, height=6, dpi=300,  units="in")
  ggsave(sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"g2d-vol-del-avg-down.png"), width=8, height=6, dpi=300,  units="in")

  dfcsv <- data.frame("AsOf"=as.character(dydxdf[[3]]), "VOL"=as.numeric(dydxdf[[4]]),"DEL"=as.numeric(dydxdf[[5]]))

  write.csv(dfcsv,sprintf("%s/%s/%s-%s-%s","publish",folder,i,tr,"g2d-vol-del-avg-down.csv"), row.names = FALSE)



}

