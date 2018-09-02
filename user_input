## program to take input from users and put it into array



n<-readline("give the no . of data you have: ")
a<-NULL
i<-1
while(i<=n)
{
  z<-10^length(i)
  k<-i%%z
  if(k==1 && i!=11)
    print(paste0("give me ", i,"st data " ))
  if(k==2 && i!=12)
    print(paste0("give me ", i,"nd data " ))
  if(k==3 && i!=13)
    print(paste0("give me ", i,"rd data " ))
  if(k==4 || k==5 || k==6 || k==7 || k==8 ||k==9 ||k==0 || i==11||i==12||i==13)
    print(paste0("give me ", i,"th data " ))
  b<-(readline("=>"))
  ##code if you want to edit any entry
  ##feel free to press edit at any poin to edit any date
  if(b=="edit")
  {
    print(a)
    pos<-readline("tell me the position of the data you want to be edited:")
    pos<-as.integer(pos)
    l<-1:i==pos
    if(sum(l)==1)
    {
      edit.type<-readline("What do you want delete or update:")
      if(edit.type=="delete")
      {
        a<-a[-pos]
        i<-i-1
        cat(paste("your new data is \n",a,"\n"))
      }
      if(edit.type=="update")
      {
        print(paste("give me new number you want to put at ",a[pos]))
        new<-readline("=>")
        a[pos]<-new
        cat(paste("your new data is \n",a,"\n"))
      }
    }
  }
  
  if(b!="edit")
  {
    a<-c(a,b)
    i<-i+1
  }
  
} 

print("your input  is stored in vector named a")
