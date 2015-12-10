alighting<-function(x,y){
  
  #读取、合并数据
  gps<-read.csv("/home/public/data/GPS_DATA.csv",header=TRUE,sep = ",") 
  bus<-read.csv("/home/public/data/AFC_DATA.csv",header=TRUE,sep = ",") 
  route<-read.csv("/home/public/data/BUS_ROUTE_DIC.csv",header=TRUE,sep = ",")
  gpst<-unique(gps) 
  gpsr<-merge(gpst,route,by="bus_id")
  busr<-merge(bus,route,by="bus_id")
  

  #循环
  x<-data.frame()
  routeid<-unique(busr$route_id)
  for(route_id in routeid){                      #routeid
    
    #bus
    p<-busr[busr[,"route_id"]==route_id,]
    route1<-p[order(p[,1],p[,4],p[,5]),]
    
    
    #gps
    a<-gpsr[gpsr[,"route_id"]==route_id,]
    gps1<-a[order(a[,1],a[,2],a[,3]),]
    
    y<-data.frame()
    busid<-unique(route1$bus_id)
    for(id in busid){                          #busid
      b<-route1[route1$bus_id==id,]
      g<-gps1[gps1$bus_id==id,]
      
      dday<-unique(b$day)
      z<-data.frame()
      for(d in dday){                      #day
        br<-b[b$day==d,]
        gr<-g[g$day==d,]
        
        
        brt<-br$time
        grt<-gr$time
        gra<-data.frame(grt)
        bra<-data.frame(brt)
        
        if(nrow(gra)<10|nrow(bra)<10){break}           #某天某辆车的gps的time记录小于10条的情况 不予考虑。
        else{
          
          knn<-get.knnx(grt,brt)
          q<-gr[knn$nn.index[,1],]
          bind<-cbind(q,guid=br$guid,card_id=br$card_id,time1=br$time) # bind<-cbind(q,cut)  #knn最邻近距离筛选出与所有guid的时间最接近的gps时间和坐标信息
          abind<-data.frame(bind)
          
        }
        
        z<-rbind(z,abind)
        
      }
      y<-rbind(y,z)
      
    }
    
    x<-rbind(x,y)
   
  }
  
  card<-group_by(x,card_id)
  ar<-arrange(card,day,time1)
  
  lng1<-cbind(ar,lng1=lead(ar$lng))
  lat1<-cbind(lng1,lat1=lead(ar$lat))     #假设理想情况：下次上车站点是上次上车的下车站点。方法：lead-lag
  
  alighting<-data.frame(lat1$guid,lat1$lng1,lat1$lat1)
  write.table(alighting,file="/home/user021/RESULT_ALIGHT_LIST.csv",row.names=F,col.names = F,quote = F)
}
