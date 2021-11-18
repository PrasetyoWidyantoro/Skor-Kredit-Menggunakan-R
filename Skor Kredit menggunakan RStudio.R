#Data Preparation untuk Class Variable
library("openxlsx")
library("C50")
library("reshape2")

#Mempersiapkan data
dataCreditRating <- read.xlsx(xlsxFile = "credit_scoring.xlsx")
str(dataCreditRating)

#Melakukan konversi kolom risk_rating menjadi factor
dataCreditRating$risk_rating <- as.factor(dataCreditRating$risk_rating)

#Melihat struktur setelah konversi
str(dataCreditRating)

#Menghilangkan beberapa variable input dari dataset 
input_columns <- c("durasi_pinjaman_bulan", "jumlah_tanggungan")
datafeed <- dataCreditRating[ , input_columns ]
str(datafeed)

#Mempersiapkan porsi index acak untuk training dan testing set
set.seed(100)
indeks_training_set <- sample(900, 800)

#Membuat dan menampilkan training set dan testing set
input_training_set <- datafeed[indeks_training_set,]
class_training_set <- dataCreditRating[indeks_training_set,]$risk_rating
input_testing_set <- datafeed[-indeks_training_set,]

str(input_training_set)
str(class_training_set)
str(input_testing_set)

#Menghasilkan Model dengan Fungsi C5.0
#menghasilkan dan menampilkan summary model
risk_rating_model <- C5.0(input_training_set, class_training_set)
summary(risk_rating_model)

#Menghasilkan Plot dari Model C5.0
#menghasilkan dan menampilkan summary model
risk_rating_model <- C5.0(input_training_set, class_training_set)
plot(risk_rating_model)

#Label dari Class
#menghasilkan model
risk_rating_model <- C5.0(input_training_set, class_training_set, control = C5.0Control(label="Risk Rating"))
summary(risk_rating_model)

#Jumlah Data dan Variable yang Digunakan
# Mempersiapkan training dan testing set
set.seed(100) # untuk menyeragamkan hasil random antar tiap komputer
indeks_training_set <- sample(900, 780)

# Membuat dan menampilkan training set dan testing set
input_training_set <- datafeed[indeks_training_set,]
class_training_set <- dataCreditRating[indeks_training_set,]$risk_rating
input_testing_set <- datafeed[-indeks_training_set,]

# menghasilkan model
risk_rating_model <- C5.0(input_training_set, class_training_set, control = C5.0Control(label="Risk Rating"))
summary(risk_rating_model)

#Merubah Label Class Variable
#Mempersiapkan class dan input variables 
dataCreditRating$risk_rating[dataCreditRating$risk_rating == "1"]  <-  "satu"
dataCreditRating$risk_rating[dataCreditRating$risk_rating == "2"]  <-  "dua"
dataCreditRating$risk_rating[dataCreditRating$risk_rating == "3"]  <-  "tiga"
dataCreditRating$risk_rating[dataCreditRating$risk_rating == "4"]  <-  "empat"
dataCreditRating$risk_rating[dataCreditRating$risk_rating == "5"]  <-  "lima"
dataCreditRating$risk_rating <- as.factor(dataCreditRating$risk_rating) 
input_columns <- c("durasi_pinjaman_bulan", "jumlah_tanggungan")
datafeed <- dataCreditRating[ , input_columns ]

#Mempersiapkan training dan testing set
set.seed(100) #untuk menyeragamkan hasil random antar tiap komputer
indeks_training_set <- sample(900, 800)

#Membuat dan menampilkan training set dan testing set
input_training_set <- datafeed[indeks_training_set,]
class_training_set <- dataCreditRating[indeks_training_set,]$risk_rating
input_testing_set <- datafeed[-indeks_training_set,]

#menghasilkan model
risk_rating_model <- C5.0(input_training_set, class_training_set, control = C5.0Control(label="Risk Rating"))
summary(risk_rating_model)

#Evaluasi Model 
#Menggunakan Fungsi Predict
#menggunakan model untuk prediksi testing set
predict(risk_rating_model, input_testing_set)

#menyimpan hasil prediksi testing set ke dalam kolom hasil_prediksi
input_testing_set$risk_rating <- dataCreditRating[-indeks_training_set,]$risk_rating
input_testing_set$hasil_prediksi <- predict(risk_rating_model, input_testing_set)

# menampilkan variable input_testing_set
print(input_testing_set)

#Membuat Table Confusion Matrix
#membuat confusion matrix
dcast(hasil_prediksi ~ risk_rating, data=input_testing_set)

#Jumlah Data dengan Prediksi Benar
#Menghitung jumlah prediksi yang benar
nrow(input_testing_set[input_testing_set$risk_rating==input_testing_set$hasil_prediksi,])

#Jumlah Data dengan Prediksi Salah
#Menghitung jumlah prediksi yang salah
nrow(input_testing_set[input_testing_set$risk_rating!=input_testing_set$hasil_prediksi,])

#Membuat data frame aplikasi baru
aplikasi_baru <- data.frame(jumlah_tanggungan = 6, durasi_pinjaman_bulan = 12)
print(aplikasi_baru)

#melakukan prediksi
predict(risk_rating_model, aplikasi_baru)

#Merubah Durasi Pinjaman
#Membuat data frame aplikasi baru
aplikasi_baru <- data.frame(jumlah_tanggungan = 6, durasi_pinjaman_bulan = 64 )

#melakukan prediksi
predict(risk_rating_model, aplikasi_baru)

