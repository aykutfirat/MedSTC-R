medSTC <-
function (documents, mlabels, ntopics, class_num, initial_c, lambda, rho, nfolds, delta_ell, supervised=TRUE, 
primal_svm=1, var_max_iter=20, convergence=1e-4, em_max_iter=100, em_convergence=1e-4, 
svm_alg_type=2, res_file="overall-res_supervised.txt", output_dir=".") 
{

	integerLabels<-as.integer(factor(mlabels))-1L 
    model<-vector('list',2)
    model$modelState<- structure(.Call("medSTCTrain", documents, integerLabels, as.integer(ntopics), as.integer(class_num), as.double(initial_c),
    							as.double(lambda),as.double(rho), as.integer(nfolds), as.double(delta_ell),
    							as.logical(supervised),as.logical(primal_svm),as.integer(var_max_iter), 
    							as.double(convergence), as.integer(em_max_iter), as.double(em_convergence),
    							as.integer(svm_alg_type), res_file, output_dir) 
    					)
    model$ntopics=ntopics 
    model$class_num=class_num
    model$initial_c=initial_c
    model$lambda=lambda
    model$rho=rho
    model$nfolds=nfolds
    model$delta_ell=delta_ell
    model$supervised=supervised
	model$primal_svm=primal_svm
	model$var_max_iter=var_max_iter
	model$convergence=convergence
	model$em_max_iter=em_max_iter
	model$em_convergence=em_convergence 
	model$svm_alg_type=svm_alg_type
	model$res_file=res_file
	model$output_dir=output_dir
	class(model)="medSTC"
    model
}

predict.medSTC <-
function (model, documents, mlabels) 
{
	result<-vector('list',2)
    integerLabels<-as.integer(factor(mlabels))-1L 
    retval<-structure(.Call("medSTCTest",model$modelState, documents, integerLabels, as.integer(model$ntopics), 
    	as.integer(model$class_num), as.double(model$initial_c),as.double(model$lambda),as.double(model$rho), 
    	as.integer(model$nfolds), as.double(model$delta_ell),
		as.logical(model$supervised),as.logical(model$primal_svm),as.integer(model$var_max_iter), 
		as.double(model$convergence), as.integer(model$em_max_iter), as.double(model$em_convergence),
		as.integer(model$svm_alg_type), model$res_file, model$output_dir))
		colnames(retval)<-sort(unique(mlabels))
		result$scores<-retval
		result$assignments<-colnames(retval)[apply(retval,1,which.max)]
		result
}
