library(igraph)
library(ggplot2)

par(mfrow=c(2,3))

set.seed(5)

#Erdos Renyi model
e<-sample_gnp(100,1/100)
plot(e,layout=layout_nicely(e),vertex.label=NA, vertex.size=7,vertex.color="blue",main= "Erdos-Renyi")

#Barabasi Albert model
b<-sample_pa(100,directed=F)
plot(b,layout=layout_nicely(b),vertex.label=NA, vertex.size=7,vertex.color="red",main= "Barabasi-Albert")

#Watts Strogatz model 
w<-sample_smallworld(1,100,1,0.1)
plot(w,layout=layout_nicely(w),vertex.label=NA, vertex.size=7,vertex.color="green",main= "Watts-Strogatz")

#Apply SIR model to Erdos Renyi
sir_e<-sir(e,beta=1,gamma=1)
plot(sir_e,color="aliceblue",median_color="cadetblue2",quantile_color="darkblue")

#Apply SIR model to Barbasi Albert
sir_b<-sir(b,beta=1,gamma=1)
plot(sir_b,color="coral",median_color="brown1",quantile_color="brown3")

#Apply SIR model to Watss Strogatz
sir_w<-sir(w,beta=1,gamma=1)
plot(sir_w,color="darkolivegreen1",median_color="darkgreen",quantile_color="darkolivegreen3")

#Tests with different beta and gamma scalars for ER
par(mfrow=c(1,3))
sir_e2<-sir(e,beta=2,gamma=3)
plot(sir_e2,color="aliceblue",median_color="cadetblue2",quantile_color="darkblue")
sir_e3<-sir(e,beta=3,gamma=1)
plot(sir_e3,color="aliceblue",median_color="cadetblue2",quantile_color="darkblue")
mtext("Erdos-Renyi",side=3,line=2)
sir_e4<-sir(e,beta=4,gamma=2)
plot(sir_e4,color="aliceblue",median_color="cadetblue2",quantile_color="darkblue")

#Tests with different beta and gamma scalars for BA
sir_b2<-sir(b,beta=2,gamma=3)
plot(sir_b2,color="coral",median_color="brown1",quantile_color="brown3")
sir_b3<-sir(e,beta=3,gamma=1)
plot(sir_b3,color="coral",median_color="brown1",quantile_color="brown3")
mtext("Barbasi-Albert",side=3,line=2)
sir_b4<-sir(e,beta=4,gamma=2)
plot(sir_b4,color="coral",median_color="brown1",quantile_color="brown3")

#Tests with different beta and gamma scalars for WS
sir_w2<-sir(w,beta=2,gamma=3)
plot(sir_w2,color="darkolivegreen1",median_color="darkgreen",quantile_color="darkolivegreen3")
sir_w3<-sir(e,beta=3,gamma=1)
plot(sir_w3,color="darkolivegreen1",median_color="darkgreen",quantile_color="darkolivegreen3")
mtext("Watss-Strogatz",side=3,line=2)
sir_w4<-sir(e,beta=4,gamma=2)
plot(sir_w4,color="darkolivegreen1",median_color="darkgreen",quantile_color="darkolivegreen3")