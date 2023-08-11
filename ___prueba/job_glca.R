
require(glca)
invisible("glca format")
# We excluded otras_sus3_mod because it had only one value
f_preds2<- item(sus_principal_mod, otras_sus1_mod, otras_sus2_mod) ~ 1

# f <- cbind(sus_principal_mod, otras_sus1_mod, otras_sus2_mod, otras_sus3_mod)~1
# gss.lc2 <- poLCA(f,mydata_preds3,nclass=2)
# lca203 <- glca(f_preds2, data = mydata_preds3, nclass = 3, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)


seed<-2125

old <- Sys.time()

print(old)

lca202 <- glca(f_preds2, data = mydata_preds3, nclass = 2, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
#43 minutes each more or less
lca203 <- glca(f_preds2, data = mydata_preds3, nclass = 3, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca204 <- glca(f_preds2, data = mydata_preds3, nclass = 4, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca205 <- glca(f_preds2, data = mydata_preds3, nclass = 5, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca206 <- glca(f_preds2, data = mydata_preds3, nclass = 6, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca207 <- glca(f_preds2, data = mydata_preds3, nclass = 7, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca208 <- glca(f_preds2, data = mydata_preds3, nclass = 8, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca209 <- glca(f_preds2, data = mydata_preds3, nclass = 9, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca210 <- glca(f_preds2, data = mydata_preds3, nclass = 10, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca211 <- glca(f_preds2, data = mydata_preds3, nclass = 11, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca212 <- glca(f_preds2, data = mydata_preds3, nclass = 12, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca213 <- glca(f_preds2, data = mydata_preds3, nclass = 13, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)
lca214 <- glca(f_preds2, data = mydata_preds3, nclass = 14, seed = seed, verbose = FALSE, n.init = 5e1, decreasing=T, maxiter = 1e4,testiter = 500)

gof2<-
  gofglca(lca202, lca203, lca204, lca205, lca206, lca207, lca208, lca209, lca210, lca211, lca212, lca213, lca214, test = "chisq")

new_med<-(Sys.time())
paste0("The model took ",round(new_med-old,2)," until every LCA was computed")
print(new_med)