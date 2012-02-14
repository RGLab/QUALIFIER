mapping<-read.csv("/loc/no-backup/mike/rv144_mapping/mapping.csv")
master<-read.csv("/loc/no-backup/Raphael/rv144_master_wk26.csv")

mapping1<-merge(mapping,master,by.x="ptid",by.y="pin")

write.csv(mapping1,file="/loc/no-backup/mike/rv144_mapping/mapping1.csv")

		

mapping1<-read.csv("/loc/no-backup/mike/rv144_mapping/mapping1.csv")
mapping1$visit<-factor(mapping1$visit,labels=c("Pre","Post"))

write.csv(subset(mapping1,lab=="B"),file="/loc/no-backup/mike/rv144_mapping/mapping_B.csv",row.names=F)
write.csv(subset(mapping1,lab=="cc"),file="/loc/no-backup/mike/rv144_mapping/mapping_cc.csv",row.names=F)
write.csv(subset(mapping1,lab=="T"),file="/loc/no-backup/mike/rv144_mapping/mapping_T.csv",row.names=F)
