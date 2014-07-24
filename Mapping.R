library(RgoogleMaps)
library(splancs)
library(calibrate)

data = read.csv("UCDP GED v.1.1-2011.csv", h=T, sep=",")
data$total_deaths = data$Deaths_A + data$Deaths_B + data$Civilian_Deaths + data$Unknown # add a new variable named total_deaths
data=data[-10829,] ## remove the conflict happened in France ##
dat=as.matrix(data)

## separeate data year by year ##
j=1
data1989 = matrix(ncol=39, nrow=804)
for (i in 1:21860) {
  if (dat[i,2]==1989) {
    data1989[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1990 = matrix(ncol=39, nrow=1264)
for (i in 1:21860) {
  if (dat[i,2]==1990) {
    data1990[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1991 = matrix(ncol=39, nrow=766)
for (i in 1:21860) {
  if (dat[i,2]==1991) {
    data1991[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1992 = matrix(ncol=39, nrow=991)
for (i in 1:21860) {
  if (dat[i,2]==1992) {
    data1992[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1993 = matrix(ncol=39, nrow=1410)
for (i in 1:21860) {
  if (dat[i,2]==1993) {
    data1993[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1994 = matrix(ncol=39, nrow=1442)
for (i in 1:21860) {
  if (dat[i,2]==1994) {
    data1994[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1995 = matrix(ncol=39, nrow=1043)
for (i in 1:21860) {
  if (dat[i,2]==1995) {
    data1995[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1996 = matrix(ncol=39, nrow=854)
for (i in 1:21860) {
  if (dat[i,2]==1996) {
    data1996[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1997 = matrix(ncol=39, nrow=827)
for (i in 1:21860) {
  if (dat[i,2]==1997) {
    data1997[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1998 = matrix(ncol=39, nrow=1235)
for (i in 1:21860) {
  if (dat[i,2]==1998) {
    data1998[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data1999 = matrix(ncol=39, nrow=1082)
for (i in 1:21860) {
  if (dat[i,2]==1999) {
    data1999[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2000 = matrix(ncol=39, nrow=1058)
for (i in 1:21860) {
  if (dat[i,2]==2000) {
    data2000[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2001 = matrix(ncol=39, nrow=658)
for (i in 1:21860) {
  if (dat[i,2]==2001) {
    data2001[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2002 = matrix(ncol=39, nrow=1180)
for (i in 1:21860) {
  if (dat[i,2]==2002) {
    data2002[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2003 = matrix(ncol=39, nrow=1005)
for (i in 1:21860) {
  if (dat[i,2]==2003) {
    data2003[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2004 = matrix(ncol=39, nrow=1059)
for (i in 1:21860) {
  if (dat[i,2]==2004) {
    data2004[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2005 = matrix(ncol=39, nrow=700)
for (i in 1:21860) {
  if (dat[i,2]==2005) {
    data2005[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2006 = matrix(ncol=39, nrow=709)
for (i in 1:21860) {
  if (dat[i,2]==2006) {
    data2006[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2007 = matrix(ncol=39, nrow=876)
for (i in 1:21860) {
  if (dat[i,2]==2007) {
    data2007[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2008 = matrix(ncol=39, nrow=888)
for (i in 1:21860) {
  if (dat[i,2]==2008) {
    data2008[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2009 = matrix(ncol=39, nrow=1295)
for (i in 1:21860) {
  if (dat[i,2]==2009) {
    data2009[j,]=dat[i,]
    j=j+1
  }
  else{}
}

j=1
data2010 = matrix(ncol=39, nrow=714)
for (i in 1:21860) {
  if (dat[i,2]==2010) {
    data2010[j,]=dat[i,]
    j=j+1
  }
  else{}
}

## find the number of conflicts in each interval and name them ##
red = sum(data$total_deaths >= 500)
yellow = sum(data$total_deaths >= 20 & data$total_deaths < 500)
blue = sum(data$total_deaths >= 0 & data$total_deaths < 20)
color = c(blue, yellow, red); color

## sort the matrix based on total deaths ##
matr1 = matrix(c(data$Lat, data$Lon, data$total_deaths), ncol=3)
sortmag=matr1[order(matr1[,3])]
lat1=sortmag[,1]
lon1=sortmag[,2]

## determine the map size, draw the map and add legend ##

center = c(mean(data$Lat), mean(data$Lon))
zoom <- min(MaxZoom(range(data$Lat), range(data$Lon)))
MyMap <- GetMap(center=center, zoom=zoom,markers = '&markers=color:red',
                destfile = "Africa.png");
tmp <- PlotOnStaticMap(MyMap, lat=data$Lat, lon=data$Lon , destfile = "Africa.png",
                       cex=rep(c(0.5, 1.5, 2.5), color), 
                       pch=20,
                       col=rep(c('blue', 'yellow', 'red'), color))
legend("bottomright", legend=c("500+", "20-500", "0-20"), col=c("red", "yellow", "blue"), pch=20, pt.cex=c(2.5, 1.5, 0.5))
text(x=0, y=-280, labels="Conflicts in Africa")

## generate 2d kernel smoothing graph ##

bdw = sqrt(bw.nrd(lon1)^2+bw.nrd(lat1)^2)  ## default bandwidth
bdry = matrix(c(-20,-35,-20,40,60,40,60,-35,-20,-35),ncol=2,byrow=T)
b = as.points(lon1, lat1)
z = kernel2d(b, bdry, 4, nx=85, ny=75); image(z, col=gray((64:20)/64), xlab="longitude", ylab="latitude"); points(b, pch=".")

data9195 = rbind(data1991, data1992, data1993, data1994, data1995) ## combine data in 5 year interval ##
lat9195 = as.numeric(data9195[,23]); lon9195 = as.numeric(data9195[,24])
bdw9195 = sqrt(bw.nrd(lon9195)^2+bw.nrd(lat9195)^2)  ## default bandwidth
b9195 = as.points(lon9195, lat9195)

data9600 = rbind(data1996, data1997, data1998, data1999, data2000)
lat9600 = as.numeric(data9600[,23]); lon9600 = as.numeric(data9600[,24])
bdw9600 = sqrt(bw.nrd(lon9600)^2+bw.nrd(lat9600)^2)  ## default bandwidth
b9600 = as.points(lon9600, lat9600)

data0105 = rbind(data2001, data2002, data2003, data2004, data2005)
lat0105 = as.numeric(data0105[,23]); lon0105 = as.numeric(data0105[,24])
bdw0105 = sqrt(bw.nrd(lon0105)^2+bw.nrd(lat0105)^2)  ## default bandwidth
b0105 = as.points(lon0105, lat0105)

data0610 = rbind(data2006, data2007, data2008, data2009, data2010)
lat0610 = as.numeric(data0610[,23]); lon0610 = as.numeric(data0610[,24])
bdw0610 = sqrt(bw.nrd(lon0610)^2+bw.nrd(lat0610)^2)  ## default bandwidth
b0610 = as.points(lon0610, lat0610)

nation = read.table("http://geocommons.com/overlays/7674.csv", h=T, sep=",") ## read the data of nation names and their locations ##
nation1 = nation[c(1,2,6,12,17,27,42,43,44,49),] ## pick some important nations ##

par(mfrow=c(2,2))
image(z, col=gray((64:20)/64), main="1989-2010", xlab="longitude", ylab="latitude"); points(b, pch=".")
points(nation1$longitude, nation1$latitude)
textxy(nation1$longitude, nation1$latitude, nation1$Country, dcol="blue")
image(z9195, col=gray((64:20)/64), main="(a) 1991-1995", xlab="longitude", ylab="latitude"); points(b9195, pch=".")
points(nation$longitude, nation$latitude)
textxy(nation1$longitude, nation1$latitude, nation1$Country, dcol="blue") ## mark nations' names ##
image(z9600, col=gray((64:20)/64), main="(b) 1996-2000", xlab="longitude", ylab="latitude"); points(b9600, pch=".")
points(nation$longitude, nation$latitude)
textxy(nation1$longitude, nation1$latitude, nation1$Country, dcol="blue")
image(z0105, col=gray((64:20)/64), main="(c) 2001-2005", xlab="longitude", ylab="latitude"); points(b0105, pch=".")
points(nation$longitude, nation$latitude)
textxy(nation1$longitude, nation1$latitude, nation1$Country, dcol="blue")

par(mfrow=c(1,1))
image(z0610, col=gray((64:20)/64), main="(d) 2006-2010", xlab="longitude", ylab="latitude"); points(b0610, pch=".")
points(nation1$longitude, nation1$latitude)
textxy(nation1$longitude, nation1$latitude, nation1$Country, dcol="blue")
