// (C) Copyright 2011, Jun Zhu (junzhu [at] cs [dot] cmu [dot] edu)

// This file is part of MedSTC.

// MedSTC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2 of the License, or (at your
// option) any later version.

// MedSTC is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA

#include <sys/stat.h>
#include <sys/types.h>
#include <vector>
#include <map>
#include <string>
#include "../SVMLight/svm_common.h"
#include "MedSTC.h"
#include <R.h>
#include <Rdefines.h>
using namespace std;

#define CHECK(VAL, TYPE) if (!is##TYPE(VAL)) { \
    error(#VAL " must be a(n) " #TYPE "."); \
  }

#define CHECKLEN(VAL, TYPE, LEN) if (!is##TYPE(VAL) || length(VAL) != LEN) { \
    error(#VAL " must be a length " #LEN " " #TYPE "."); \
  }

#define CHECKMATROW(VAL, TYPE, NROW) if (!isMatrix(VAL) || !is##TYPE(VAL) || NUMROWS(VAL) != NROW) { \
    error(#VAL " must be a matrix with " #NROW " rows of type " #TYPE "."); \
  }
#define NUMROWS(MAT) (INTEGER(GET_DIM(MAT))[0])
#define NUMCOLS(MAT) (INTEGER(GET_DIM(MAT))[1])
int file_exist (char *filename)
{
  struct stat   buffer;   
  return (stat (filename, &buffer) == 0);
}

void read_data_from_R(Corpus* c, SEXP documents,int nd, int labels[])
{

	int nw, word, count;
	int num_terms = 0;
	Document* docs = (Document*) malloc(sizeof(Document)*(nd));

	for (int i=0; i < nd; i++)
	{
		SEXP document = VECTOR_ELT(documents, i);
        CHECKMATROW(document, Integer, 2);
        nw = INTEGER(GET_DIM(document))[1];

        docs[i].length = nw;
        docs[i].total = 0;
        docs[i].words = (int*)malloc(sizeof(int)*nw);
		docs[i].counts = (int*)malloc(sizeof(int)*nw);
		docs[i].gndlabel = labels[i];
		docs[i].lossAugLabel = -1;
        for (int ww=0; ww< nw; ww++){
        	word = INTEGER(document)[ww * 2];

      		count = INTEGER(document)[ww * 2 + 1];
      		

			word = word - OFFSET;
			docs[i].words[ww] = word;
			docs[i].counts[ww] = count;
			docs[i].total += count;
			if (word >= num_terms) num_terms = word + 1; 

        }
	} 	
	c->docs=docs;
	c->num_docs = nd;
	c->num_terms = num_terms;
}
extern "C" {
SEXP medSTCTrain(SEXP documents_,
			 SEXP labels_,
			 SEXP ntopics_,
			 SEXP class_num_,
			 SEXP initial_c_,
			 SEXP lambda_,
			 SEXP rho_,
			 SEXP nfolds_,
			 SEXP delta_ell_,
			 SEXP supervised_,
			 SEXP primal_svm_,
			 SEXP var_max_iter_,
			 SEXP convergence_,
			 SEXP em_max_iter_,
			 SEXP em_convergence_,
			 SEXP svm_alg_type_,
			 SEXP res_file_,
			 SEXP output_dir_){
  			
  			GetRNGstate();
  			CHECK(documents_, NewList);
			int nd = length(documents_);
			int labels[nd];
			if (!isNull(labels_)) {
    			if (length(labels_) != nd) {
      				error("class labels must have same length as documents.");
    			}
    			CHECKLEN(labels_, Integer, nd);	
    			for (int i=0; i<nd;i++){
    				labels[i]=INTEGER(labels_)[i];
    				}
    		}
    		Params param;
			CHECKLEN(ntopics_, Integer, 1);
  			param.NTOPICS  = INTEGER(ntopics_)[0];
  			CHECKLEN(class_num_, Integer, 1);
  			param.NLABELS = INTEGER(class_num_)[0];
  			CHECKLEN(initial_c_, Real, 1);
  			param.INITIAL_C = REAL(initial_c_)[0];
			CHECKLEN(lambda_, Real, 1);
  			param.LAMBDA = REAL(lambda_)[0];
  			CHECKLEN(rho_, Real, 1);
  			param.RHO= REAL(rho_)[0];
  			CHECKLEN(nfolds_, Integer, 1);
  			param.NFOLDS = INTEGER(nfolds_)[0];
  			CHECKLEN(delta_ell_, Real, 1);
  			param.DELTA_ELL = REAL(delta_ell_)[0];
  			CHECKLEN(supervised_, Logical, 1);
  			param.SUPERVISED= LOGICAL(supervised_)[0];
  			CHECKLEN(primal_svm_, Logical, 1);
  			param.PRIMALSVM = LOGICAL(primal_svm_)[0];
  			CHECKLEN(var_max_iter_, Integer, 1);
  			param.VAR_MAX_ITER = INTEGER(var_max_iter_)[0];
  			CHECKLEN(convergence_, Real, 1);
  			param.VAR_CONVERGED = REAL(convergence_)[0];
  			CHECKLEN(em_max_iter_, Integer, 1);
  			param.EM_MAX_ITER = INTEGER(em_max_iter_)[0];
  			CHECKLEN(em_convergence_, Real, 1);
  			param.EM_CONVERGED = REAL(em_convergence_)[0];
  			CHECKLEN(svm_alg_type_, Integer, 1);
  			param.SVM_ALGTYPE = INTEGER(svm_alg_type_)[0];
  			
  			char res_file[512];
  			sprintf(param.res_filename,"%s",CHAR(STRING_ELT(res_file_,0)));
  			char output_dir[512];
  			sprintf(output_dir,"%s",CHAR(STRING_ELT(output_dir_,0)));
 			Corpus *c = new Corpus();
		    read_data_from_R(c,documents_,nd,labels);	
			char dir[512];
			sprintf(dir, "%s/s%d_c%d_f%d_s%d", output_dir, param.NTOPICS, (int) param.INITIAL_C,param.NFOLDS, param.SUPERVISED);
			mkdir(dir,0755);
			char paramFile[512];
			sprintf(paramFile, "%s/param.dat", dir);
			sprintf(param.train_filename,"*");
			sprintf(param.test_filename,"*");
			param.save_settings(paramFile);
			MedSTC model;
			model.train("random", dir, c, &param);
			delete c;
			SEXP directory;
            PROTECT(directory = allocVector(STRSXP, 1));
            SET_STRING_ELT(directory, 0, mkChar(dir));
         	UNPROTECT(1);
			return directory;
}
}
extern "C" {
SEXP medSTCTest(SEXP documents_,SEXP labels_,SEXP dir_){
  			char dir[512];
  			sprintf(dir,"%s",CHAR(STRING_ELT(dir_,0)));
  			CHECK(documents_, NewList);
			int nd = length(documents_);
			int labels[nd];
			if (!isNull(labels_)) {
    			if (length(labels_) != nd) {
      				error("class labels must have same length as documents.");
    			}
    			CHECKLEN(labels_, Integer, nd);	
    			for (int i=0; i<nd;i++){
    				labels[i]=INTEGER(labels_)[i];
    				}
    		}
    		Params param;
    		char paramFile[512];
			sprintf(paramFile, "%s/param.dat", dir);
			if (file_exist(paramFile)) param.read_settings(paramFile);
			else error("param.dat file is not found in %s",dir);	
 			Corpus *c = new Corpus();
			read_data_from_R(c,documents_,nd,labels);			
			MedSTC evlModel;
			double dAcc = evlModel.sparse_coding(dir, c, &param);
			Rprintf("Accuracy: %.3f\n", dAcc);
			delete c;	
			
			SEXP retval;
  			PROTECT(retval = allocMatrix(REALSXP, nd, param.NLABELS));
  			for (int i=0; i < c->num_docs; i++){
  				for (int j=0; j<param.NLABELS; j++)
  				REAL(retval)[i+c->num_docs*j] =  c->docs[i].scores[j];
         	}
         	UNPROTECT(1);
			return retval;	
  	
}
}

