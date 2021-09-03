# Import Data
library(arules)
transaksi_tabular <- read.transactions(file="https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv", 
                                       format="single", sep="\t", cols=c(1,2), skip=1)


# Insight Top 10
data_item <- itemFrequency(transaksi_tabular, type = "absolute")
data_item <- sort(data_item, decreasing = TRUE)
data_item <- data_item[1:10]
data_item <- data.frame("Nama Produk" = names(data_item), "Jumlah" = data_item, row.names = NULL)
print(data_item)

# Bottom 10
data_bottom <- itemFrequency(transaksi_tabular, type = "absolute")
data_bottom <- sort(data_bottom, decreasing = FALSE)
data_bottom <- data_bottom[1:10]
data_bottom <- data.frame("Nama Produk" = names(data_bottom), "Jumlah" = data_bottom, row.names = NULL)
write.csv(data_bottom, file = "bottom10_item_retail.txt")
print(data_bottom)

# Kombinasi Produk Penjualan yang Menarik
mba <- apriori(transaksi_tabular,parameter = list(supp = 10/length(transaksi_tabular), 
                                                  confidence = 0.5, minlen = 2, maxlen = 3))

mba <- head(sort(mba, decreasing = TRUE, by="lift"), n=10)
write(mba, file = "kombinasi_retail.txt")
inspect(mba)

# Mencari Paket Produk yang bisa dipasangkan dengan Item Slow-Moving dengan conf = 0.1
mba <- apriori(transaksi_tabular,parameter = list(supp = 10/length(transaksi_tabular), 
                                                  confidence = 0.1, minlen = 2, maxlen = 3))

A <- subset(mba,rhs %in% "Tas Makeup")
B <- subset(mba,rhs %in% "Baju Renang Pria Anak-anak")

A <- head(sort(A, by="lift", decreasing=TRUE),n=3L)
B <- head(sort(L, by="lift", decreasing=TRUE),n=3L)

AB <- c(A,B)
inspect(AB)
