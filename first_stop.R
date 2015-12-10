buscheckz<-function(x,y,z)
{
  
 #读取数据、合并bus和route  
  
gps<-read.csv("/home/public/data/GPS_DATA.csv",header=TRUE,sep = ",") 
bus<-read.csv("/home/public/data/AFC_DATA.csv",header=TRUE,sep = ",") 
route<-read.csv("/home/public/data/BUS_ROUTE_DIC.csv",header=TRUE,sep = ",")
gpst<-unique(gps) 
gpsr<-merge(gpst,route,by="bus_id")
busr<-merge(bus,route,by="bus_id")

#循环

x<-data.frame()
routeid<-unique(busr$route_id)

for(route_id in routeid){
  
  #bus
  p<-busr[busr[,"route_id"]==route_id,]    #routeid
  route1<-p[order(p[,1],p[,4],p[,5]),]
  
  
  #gps
  a<-gpsr[gpsr[,"route_id"]==route_id,]
  gps1<-a[order(a[,1],a[,2],a[,3]),]
  
    y<-data.frame()
    busid<-unique(route1$bus_id)

    for(id in busid){                       #busid
        b<-route1[route1$bus_id==id,]
        g<-gps1[gps1$bus_id==id,]
    
        z<-data.frame()
        dday<-unique(b$day)

           for(d in dday){                  #day
              br<-b[b$day==d,]
              gr<-g[g$day==d,]
              brt<-br$time
              grt<-gr$time
              gra<-data.frame(grt)
              bra<-data.frame(brt)
      
             if(nrow(gra)<10|nrow(bra)<10){break}   #某天某辆车的gps的time记录小于10条的情况 不予考虑。
      
             else{

              
              dis<-dist(brt)
              hc<-hclust(dis,"single")            #按上车刷卡时间间距聚类，根据文献间隔大于48秒算一站，并排序。
              ctree<-cutree(hc,h=48)   
              
              
              knn<-get.knnx(grt,brt)
              q<-grt[nrow=knn$nn.index[,1]]
              bind<-cbind(q,ctree) 
              abind<-data.frame(bind)
              s<-c(knn$nn.dist[,1]==min(knn$nn.dist[,1]))   #knn最领近算法关联筛选afc的time和gps的time
              j<-abind[which(s),]    
        
              buniq<-unique(j)
              merg<-merge(gr,buniq,by.x="time",by.y="q")     
              }
      
           z<-rbind(z,merg)
        }
    
         y<-rbind(y,z)
    }
 
     x<-rbind(x,y)
  
     #print(x)
 }

RESULT_STOP_LIST<-data.frame(route_id=x$route_id,direction=1,sequence=x$ctree,lng=x$lng,lat=x$lat)
write.table(RESULT_STOP_LIST,file="/home/user021/RESULT_STOP_LIST.csv",col.names = F,quote = F)

}
