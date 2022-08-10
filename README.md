sudo apt install r-base-core  

sudo apt-get install libgmp-dev

sudo apt-get install libpq-dev

sudo apt-get install libfontconfig1-dev

sudo R  

install.packages("ggthemes")  

install.packages("ggsci")  

install.packages("dplyr")  

install.packages("ClusterR")  

install.packages("RPostgreSQL")  

install.packages("RPostgres")  

install.packages("svglite")  

### hack for gifs

convert -delay 100 publish/*/BTC*del.png  btc-out-convert.gif  
convert -delay 100 publish/*/ETH*del.png  eth-out-convert.gif  
convert -delay 100 publish/*/SOL*del.png  sol-out-convert.gif  