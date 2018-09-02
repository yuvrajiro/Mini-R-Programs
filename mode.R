rahul.ka.mode.function<-function(x)
{
  d<-NULL;
  counter<-0;
  mode<-NULL;
  a<-NULL
  b<-NULL
  c<-NULL
  d<-NULL
  e<-NULL
  f<-NULL
  g<-NULL
  h<-NULL
  for(i in x)
  {
    a<-i==x
    b<-sum(a)
    d<-c(d,b)
  };
  for(i in d)
  {
    a<-i>=d
    if(sum(a)==length(d))
    {
      e<-i
      
    }
    else
    {
      
    }
  };
  for(i in d)
  {
    if(e==i)
    {
      counter<-counter+1
      f<-c(f,counter)
    }
    else
    {
      counter<-counter+1
    }
  };
  for(i in 1:length(f))
  {
    g<-x[f[i]]
    h<-c(h,g)
  };
  
  mode<-unique(h);
  mode;
}