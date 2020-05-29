remove(list=ls())
setwd("D:/BKP/Backup Pendrive/UFMG/Disciplinas/Estatistica experimental/Videos/_Dicas/Folha acerola")
library(EBImage)
im=readImage("imagem.jpg")
plot(im)
ref=readImage("Referencia.jpg")
plot(ref)
fundo=readImage("fundo.jpg")
plot(fundo)
folhas=readImage("folhas.jpg")
plot(folhas)

mref=cbind(c(ref@.Data[,,1]),c(ref@.Data[,,2]),c(ref@.Data[,,3]))
mref=mref[sample(1:nrow(mref)),]
mref=mref[1:20000,]
colnames(mref)=c("R","G","B")

mfundo=cbind(c(fundo@.Data[,,1]),c(fundo@.Data[,,2]),c(fundo@.Data[,,3]))
mfundo=mfundo[sample(1:nrow(mfundo)),]
mfundo=mfundo[1:20000,]
colnames(mfundo)=c("R","G","B")

mfolhas=cbind(c(folhas@.Data[,,1]),c(folhas@.Data[,,2]),c(folhas@.Data[,,3]))
mfolhas=mfolhas[sample(1:nrow(mfolhas)),]
mfolhas=mfolhas[1:20000,]
colnames(mfolhas)=c("R","G","B")

#install.packages("RSNNS")
library(RSNNS)
Mat=rbind(cbind(mfolhas,1,0,0),cbind(mfundo,0,1,0),cbind(mref,0,0,1))
Mat=Mat[sample(1:nrow(Mat)),]
Mat2=splitForTrainingAndTest(Mat[,1:3],Mat[,4:6],ratio = 0.3)

modelo=mlp(Mat2$inputsTrain,Mat2$targetsTrain,size=c(5,3))

Pred=round(predict(modelo,newdata = Mat2$inputsTest),0)
mean((Pred%*%c(1:3))==(Mat2$targetsTest%*%c(1:3)))*100

?EBImage
im2=resize(im,200)
plot(im2)
mim=cbind(c(im2@.Data[,,1]),c(im2@.Data[,,2]),c(im2@.Data[,,3]))
Pred2=round(predict(modelo,newdata = mim),0)
NumPixelRef=sum(Pred2[,3])

mat3=matrix(Pred2[,1],ncol=ncol(im2@.Data[,,1]))
mat3b=bwlabel(mat3)
Shape=computeFeatures.shape(mat3b)
ID=Shape[,1]>50
Area=Shape[ID,1]
AreaCor=Area*46.75/NumPixelRef
Coord=computeFeatures.moment(mat3b)
Coord=Coord[ID,]

plot(im2)
text(Coord[,1],Coord[,2],round(AreaCor,2),col="blue",cex=0.8)
