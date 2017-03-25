library(ggplot2)

x <- c(2,5)
y <- c(2,5)
num_points <- 200
sd <- .1

df <- data.frame()
for (xi in x) {
  for (yi in y) {
    df <- rbind(df, data.frame(x=rnorm(num_points,xi,sd), y=rnorm(num_points,yi,sd)))
  }
}

ggplot(df, aes(x=x, y=y)) + geom_point(size=.2) + theme_classic() + labs(title="4 Blobs")
ggsave("charts/kmeans1.png", width=3, height=3)

vis_clusters <- function(k, title) {
  cl <- kmeans(df,k, nstart=10)
  df$cluster=factor(cl$cluster)
  centers=as.data.frame(cl$centers)
  
  ggplot(data=df, aes(x=x, y=y, color=cluster)) + 
    geom_point(size=.2) +
    geom_point(data=centers, aes(x=x,y=y, color='Center')) +
    geom_point(data=centers, aes(x=x,y=y, color='Center'), 
               size=30, alpha=.3, show.legend=FALSE) +
    theme_classic() + theme(legend.position = "none") +
    labs(title=title)
}

vis_clusters(4, "K-Means k=4")
ggsave("charts/kmeans2.png", width=3, height=3)

vis_clusters(3, "K-Means k=3")
ggsave("charts/kmeans3.png", width=3, height=3)

vis_clusters(10, "K-Means k=10")
ggsave("charts/kmeans4.png", width=3, height=3)

elbow <- function(n) {
  sse <- c()
  for (k in 1:n) {
    cl <- kmeans(df,k, nstart=10)
    sse <- c(sse, cl$tot.withinss)
  }
  data.frame(k=1:n,sse=sse)
}

ggplot(elbow(10), aes(x=k,y=sse)) + 
  geom_line() +
  theme_minimal() +
  scale_x_continuous(breaks=1:10) +
  labs(y="Total Within Cluster Sum of Squared Error", title="Elbow")
ggsave("charts/kmeans5.png", width=3, height=3)