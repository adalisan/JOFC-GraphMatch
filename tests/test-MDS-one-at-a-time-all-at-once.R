oos<-TRUE
d.start <-1


Y.embed.gower.all <-Embed.Nodes (D.omnibus,
                          in.sample.ind,
                          oos, 
                          d.start,
                          wt.equalize  =  FALSE,
                          separability.entries.w  =  FALSE,
                          assume.matched.for.oos   =   FALSE ,
                          w.vals  =  0.99,
                          oos.embed.n.at.a.time   =   sum(!in.sample.ind)/2,
                          mds.init.method="gower")

Y.embed.gower.one <-Embed.Nodes (D.omnibus,
                                 in.sample.ind,
                                 oos, 
                                 d.start,
                                 wt.equalize  =  FALSE,
                                 separability.entries.w  =  FALSE,
                                 assume.matched.for.oos   =   FALSE ,
                                 w.vals  =  0.99,
                                 oos.embed.n.at.a.time   =   1,
                                 mds.init.method="gower")
Y.embed.random.all <-Embed.Nodes (D.omnibus,
                                 in.sample.ind,
                                 oos, 
                                 d.start,
                                 wt.equalize  =  FALSE,
                                 separability.entries.w  =  FALSE,
                                 assume.matched.for.oos   =   FALSE ,
                                 w.vals  =  0.99,
                                 oos.embed.n.at.a.time   =   sum(!in.sample.ind)/2,
                                 mds.init.method="random")
Y.embed.random.once <-Embed.Nodes (D.omnibus,
                                 in.sample.ind,
                                 oos, 
                                 d.start,
                                 wt.equalize  =  FALSE,
                                 separability.entries.w  =  FALSE,
                                 assume.matched.for.oos   =   FALSE ,
                                 w.vals  =  0.99,
                                 oos.embed.n.at.a.time   =   1,
                                 mds.init.method="random")