# simuclustfactor 0.0.3

* This version presents 5 clustering-factorial decomposition methods of which, 3 were fully implemented. Thus `TWCFTA`, `TWFCTA` and `CT3Clus` models.
* `TWCFTA` is implemented for sequential/tandem clustering-factorial decomposition.
* `TWFCTA` is implemented for sequential/tandem factorial-clustering technique.
* `T3Clus` is the simultaneous version of the TWCFTA implementation.
* `3FKMeans` is the simultaneous version of the TWFCTA implementation.
* `CT3Clus` is the convex combination of the simultaneous implementation of T3Clus and 3FKMeans models which integrates a parameter $\alpha$ to switch among the `T3Clus` (simultaneous TWCFTA when $\alpha=1$), `3FKMeans` (simultaneous TWFCTA when $\alpha=0$) and a weighted version of T3Clus and 3FKMeans when $0<\alpha<1$.
