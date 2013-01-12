medSTCTrain <-
function (documents, labels, ntopics, class_num, initial_c, lambda, rho, nfolds, delta_ell, supervised=TRUE, 
primal_svm=1, var_max_iter=20, convergence=1e-4, em_max_iter=100, em_convergence=1e-4, 
svm_alg_type=2, res_file="overall-res_supervised.txt", output_dir=".") 
{
    retval <- structure(.Call("medSTCTrain", documents, labels, as.integer(ntopics), as.integer(class_num), as.double(initial_c),
    							as.double(lambda),as.double(rho), as.integer(nfolds), as.double(delta_ell),
    							as.logical(supervised),as.logical(primal_svm),as.integer(var_max_iter), 
    							as.double(convergence), as.integer(em_max_iter), as.double(em_convergence),
    							as.integer(svm_alg_type), res_file, output_dir)
    							, names=c("dir")	 
    
    					)
    retval
}

medSTCTest <-
function (documents, labels, modelDir) 
{
    retval<-structure(.Call("medSTCTest", documents, labels, modelDir))
}